{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.CardanoTracer
  ( CardanoTracerConf (..)
  , withCardanoTracer
  ) where


import           Testnet.Filepath
import           Cardano.Tracer.Configuration

import           Prelude

import           Data.Aeson (encodeFile)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Process.Run (procCardanoTracer)

import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H

data CardanoTracerConf = CardanoTracerConf
  { tempAbsPath   :: FilePath
  , testnetMagic :: Int
  , logPath :: FilePath
  , logFormat :: LogFormat
  } deriving (Eq, Show)

mkConfig :: CardanoTracerConf -> Int -> FilePath -> TracerConfig
mkConfig CardanoTracerConf { testnetMagic, logPath, logFormat } port socketPath = TracerConfig
  { networkMagic = fromIntegral testnetMagic
  , network = AcceptAt $ LocalPipe socketPath
  , loRequestNum = Nothing
  , ekgRequestFreq = Nothing
  , hasEKG = Nothing
  , hasPrometheus = Just $ Endpoint "127.0.0.1" port $ Just False
  , hasTimeseries = Nothing
  , tlsCertificate = Nothing
  , hasForwarding = Nothing
  , logging = LoggingParams logPath FileMode logFormat :| []
  , rotation = Nothing
  , verbosity = Nothing
  , metricsNoSuffix = Nothing
  , metricsHelp = Nothing
  , resourceFreq = Nothing
  , ekgRequestFull = Nothing
  , prometheusLabels = Nothing
  }

withCardanoTracer :: CardanoTracerConf -> (FilePath -> Integration ()) -> Integration ()
withCardanoTracer conf@CardanoTracerConf{tempAbsPath} k = do
  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath
      socketPath = undefined

  nodeStdoutFile <- H.noteTempFile logDir "cardano-tracer.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "cardano-tracer.stderr.log"

  hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

  [prometheusPort] <- H.evalIO $ IO.allocateRandomPorts 1
  let configFile = undefined
  H.evalIO $ encodeFile configFile $ mkConfig conf prometheusPort socketPath

  cp <- procCardanoTracer $
    [ "--config", configFile
    ]

  (_, _, _, hProcess, _) <- H.createProcess $ cp
    { IO.std_in = IO.CreatePipe
    , IO.std_out = IO.UseHandle hNodeStdout
    , IO.std_err = IO.UseHandle hNodeStderr
    , IO.cwd = Just tempBaseAbsPath
    }

  H.onFailure $ H.evalIO $ IO.terminateProcess hProcess
  H.noteShow_ =<< H.getPid hProcess

  H.evalIO $ putStrLn $ "Prometheus is running at http://localhost:" <> show prometheusPort
  k socketPath

  H.evalIO $ IO.terminateProcess hProcess
