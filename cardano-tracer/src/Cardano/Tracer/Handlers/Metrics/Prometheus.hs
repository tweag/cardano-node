{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Cardano.Logging.Prometheus.Exposition (renderExpositionFromSampleWith)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.MetaTrace

import           Prelude hiding (head)

import           Control.Monad (join)
import           Data.Aeson (ToJSON (..), encode, pairs, (.=))
import qualified Data.ByteString as ByteString
import           Data.Functor ((<&>))
import qualified Data.Map as Map (Map, empty)
import           Data.Maybe
import           Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings, runSettings)
import           System.Metrics as EKG (Store, sampleAll)
import           System.Time.Extra (sleep)

-- | Runs a simple HTTP server that listens on @endpoint@.
--
--   At the root, it lists the connected nodes, either as HTML or JSON, depending
--   on the request's 'Accept: ' header.
--
--   Routing is dynamic, depending on the connected nodes. A valid URL is derived
--   from the nodeName configured for the connecting node. E.g. a node name
--   of `127.0.0.1:30004` will result in the route `/12700130004` which
--   renders that node's Prometheus / OpenMetrics text exposition:
--
-- # TYPE Mem_resident_int gauge
-- # HELP Mem_resident_int Kernel-reported RSS (resident set size)
-- Mem_resident_int 103792640
-- # TYPE RTS_gcMajorNum_int gauge
-- # HELP RTS_gcMajorNum_int Major GCs
-- RTS_gcMajorNum_int 4
-- # TYPE rts_gc_num_bytes_usage_samples counter
-- rts_gc_num_bytes_usage_samples 4
-- # TYPE remainingKESPeriods_int gauge
-- remainingKESPeriods_int 62
-- # TYPE rts_gc_bytes_copied counter
-- rts_gc_bytes_copied 17114384
-- # TYPE nodeCannotForge_int gauge
-- # HELP nodeCannotForge_int How many times was this node unable to forge [a block]?
--
--  The `/targets` path can be used for Prometheus HTTP service discovery. This lets
--  Prometheus dynamically discover all connected nodes, and scrape their metrics.
--  Below is a minimal example of a corresponding job definition that goes into the
--  `prometheus.yml` configuration:
--
--    - job_name: "cardano-tracer"
--
--      metrics_path: '/targets'
--
--      http_sd_configs:
--      - url: 'http://127.0.0.1:3200/targets'  # <-- Your cardano-tracer's real hostname:prometheus port
--
--      relabel_configs:
--      - source_labels: [__address__]
--        target_label: __param_target
--      - source_labels: [__param_target]
--        target_label: instance
--      - target_label: __address__
--        replacement: 127.0.0.1:3200           # <-- Your cardano-tracer's real hostname:prometheus port
--
--  In cardano-tracer's config, you can optionally provide labels that will be attached to all discovered
--  targets (default is no labels):
--    "prometheusLabels": {
--        "<labelname>": "<labelvalue>", ...
--      }
runPrometheusServer
  :: TracerEnv
  -> Endpoint
  -> IO RouteDictionary
  -> IO ()
runPrometheusServer tracerEnv endpoint computeRoutes_autoUpdate = do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.1
  -- If everything is okay, the function 'simpleHttpServe' never returns.
  -- But if there is some problem, it never throws an exception, but just stops.
  -- So if it stopped - it will be re-started.
  traceWith teTracer TracerStartedPrometheus
    { ttPrometheusEndpoint = endpoint
    }
  runSettings (setEndpoint endpoint defaultSettings) do
    renderPrometheus computeRoutes_autoUpdate noSuffix teMetricsHelp promLabels
  where
    TracerEnv
      { teTracer
      , teConfig = TracerConfig { metricsNoSuffix, prometheusLabels }
      , teMetricsHelp
      } = tracerEnv

    noSuffix    = or @Maybe metricsNoSuffix
    promLabels  = fromMaybe Map.empty prometheusLabels

renderPrometheus
  :: IO RouteDictionary
  -> Bool
  -> [(Text, Builder)]
  -> Map.Map Text Text
  -> Application
renderPrometheus computeRoutes_autoUpdate noSuffix helpTextDict promLabels request send = do
  routeDictionary :: RouteDictionary <-
    computeRoutes_autoUpdate

  case pathInfo request of

    [] ->
      send $ uncurry (responseLBS status200) $ if wantsJson
        then (contentHdrJSON    , renderJson routeDictionary)
        else (contentHdrUtf8Html, renderListOfConnectedNodes "Prometheus metrics" routeDictionary)

    ["targets"]
      | wantsJson, isNothing targetQueryParam
      -> serviceDiscovery routeDictionary

      | Just target <- targetQueryParam
      -> case lookup target (getRouteDictionary routeDictionary) of
          Just (store :: EKG.Store, _) -> metricsExposition store
          Nothing                      -> notFound target

      | otherwise
      -> badRequest

    route:_
      | Just (store :: EKG.Store, _) <- lookup route (getRouteDictionary routeDictionary)
     -> metricsExposition store

      | otherwise
     -> notFound route

  where
    acceptHeader :: Maybe ByteString.ByteString
    acceptHeader = lookup hAccept $ requestHeaders request

    wantsJson, wantsOpenMetrics :: Bool
    wantsJson         = all @Maybe ("application/json"             `ByteString.isInfixOf`) acceptHeader
    wantsOpenMetrics  = all @Maybe ("application/openmetrics-text" `ByteString.isInfixOf`) acceptHeader

    targetQueryParam :: Maybe Text
    targetQueryParam = fmap T.decodeUtf8 $ join $ lookup "target" (queryString request)

    metricsExposition store = do
      metrics <- getMetricsFromNode noSuffix helpTextDict store
      send $ responseBuilder status200
        (if wantsOpenMetrics then contentHdrOpenMetrics else contentHdrPrometheus)
        (TL.encodeUtf8Builder metrics)

    serviceDiscovery (RouteDictionary routeDict) =
      let slugs = map fst routeDict
      in send $ responseLBS status200 contentHdrJSON $
        encode [PSD (slugs, promLabels) | (not . null) slugs]

    notFound t = send $ responseLBS status404 contentHdrUtf8Text $
      "Not found: " <> (TL.encodeUtf8 . TL.fromStrict) t
    badRequest = send $ responseLBS status400 contentHdrUtf8Text
      "Bad Request"

getMetricsFromNode
  :: Bool
  -> [(Text, Builder)]
  -> EKG.Store
  -> IO TL.Text
getMetricsFromNode noSuffix helpTextDict ekgStore =
  sampleAll ekgStore <&> renderExpositionFromSampleWith helpTextDict noSuffix


newtype PrometheusServiceDiscovery = PSD ([Text], Map.Map Text Text)

instance ToJSON PrometheusServiceDiscovery where
  toJSON _ = error "ToJSON.toJSON(PrometheusServiceDiscovery): implementation error"

  toEncoding (PSD (slugs, labelMap)) = pairs $
       ("targets" .= slugs)
    <> ("labels"  .= labelMap)
