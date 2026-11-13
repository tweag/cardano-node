module Main where

import           Cardano.CLI.Environment (getEnvCli)
import qualified Cardano.Crypto.Init as Crypto

import qualified Options.Applicative as Opt

import           Parsers.Run (opts, pref, runTestnetCmd)

import           System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  envCli <- getEnvCli
  tNetCmd <- Opt.customExecParser pref (opts envCli)
  runTestnetCmd tNetCmd
