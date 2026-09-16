{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.CardanoTracer
  ( CardanoTracerConf (..)
  , withCardanoTracer
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Process.Run (procCardanoTracer)

import qualified Hedgehog as H
import           Hedgehog.Extras (Integration)
import           Hedgehog.Extras.Stock (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H

data CardanoTracerConf = CardanoTracerConf
  { testnetMagic  :: Int
  } deriving (Eq, Show)

withCardanoTracer :: CardanoTracerConf -> [String] -> (String -> Integration ()) -> Integration ()
withCardanoTracer
    CardanoTracerConf
      { testnetMagic
      } args f = do
  let logDir = makeLogDir $ TmpAbsolutePath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath

  nodeStdoutFile <- H.noteTempFile logDir "cardano-tracer.stdout.log"
  nodeStderrFile <- H.noteTempFile logDir "cardano-tracer.stderr.log"

  hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

  [prometheusPort] <- H.evalIO $ maybe (IO.allocateRandomPorts 1) (pure . (:[])) maybePort

  cp <- procCardanoTracer $
    [ "--config", unFile configPath
    ] <> args

  (_, _, _, hProcess, _) <- H.createProcess $ cp
    { IO.std_in = IO.CreatePipe
    , IO.std_out = IO.UseHandle hNodeStdout
    , IO.std_err = IO.UseHandle hNodeStderr
    , IO.cwd = Just tempBaseAbsPath
    }

  H.onFailure $ H.evalIO $ IO.terminateProcess hProcess
  H.noteShow_ =<< H.getPid hProcess

  let uriBase = "http://localhost:" <> show prometheusPort
  f uriBase

  H.evalIO $ IO.terminateProcess hProcess
