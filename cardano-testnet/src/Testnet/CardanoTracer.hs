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
import           Cardano.Node.Testnet.Paths (defaultSocketName)
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
  { tempAbsPath :: FilePath
  , testnetMagic :: Int
  , logFormat :: LogFormat
  } deriving (Eq, Show)

mkConfig :: CardanoTracerConf -> Int -> FilePath ->  FilePath -> TracerConfig
mkConfig CardanoTracerConf { testnetMagic, logFormat } port logFile socketFile = TracerConfig
  { networkMagic = fromIntegral testnetMagic
  , network = AcceptAt $ LocalPipe socketFile
  , loRequestNum = Nothing
  , ekgRequestFreq = Nothing
  , hasEKG = Nothing
  , hasPrometheus = Just $ Endpoint "127.0.0.1" port $ Just False
  , hasTimeseries = Nothing
  , tlsCertificate = Nothing
  , hasForwarding = Nothing
  , logging = LoggingParams logFile FileMode logFormat :| []
  , rotation = Nothing
  , verbosity = Nothing
  , metricsNoSuffix = Nothing
  , metricsHelp = Nothing
  , resourceFreq = Nothing
  , ekgRequestFull = Nothing
  , prometheusLabels = Nothing
  }

withCardanoTracer :: CardanoTracerConf -> (FilePath -> Integration r) -> Integration r
withCardanoTracer conf@CardanoTracerConf{tempAbsPath} k = do
  let tmpPath = TmpAbsolutePath tempAbsPath
      logDir = makeLogDir tmpPath
      tempBaseAbsPath = makeTmpBaseAbsPath tmpPath

  nodeStdoutFile <- H.noteTempFile logDir "cardano-tracer.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "cardano-tracer.stderr.log"
  logFile <- H.noteTempFile logDir "cardano-tracer.log"
  socketFile <- H.noteTempFile (makeSocketDir tmpPath) defaultSocketName
  configFile <- H.noteTempFile tempAbsPath "cardano-tracer-config.json"

  hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

  [prometheusPort] <- H.evalIO $ IO.allocateRandomPorts 1
  H.evalIO $ encodeFile configFile $ mkConfig conf prometheusPort logFile socketFile

  cp <- procCardanoTracer
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
  r <- k socketFile

  H.evalIO $ IO.terminateProcess hProcess
  pure r
