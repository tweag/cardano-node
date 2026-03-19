{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Options.Applicative
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStrLn, stderr)

import           Test.Consensus.Genesis.Tests (GenesisTestKey)
import           Test.Consensus.Genesis.TestSuite
import           Test.Consensus.Genesis.TestSuite.SmallKey
import           Test.Consensus.PeerSimulator.Tests (SmokeTestKey)
data Command = ListClasses

options :: ParserInfo Command
options =
  info
    (commandP <**> helper)
    ( mconcat
        [ fullDesc
        , progDesc "Generate conformance tests"
        , header "conformance-test-gen - A conformance test generator"
        ]
    )

commandP :: Parser Command
commandP =
  subparser
    ( command
        "list-classes"
        ( info
            (pure ListClasses)
            (progDesc "List available conformance test keys")
        )
    )

parseOptions :: [String] -> IO Command
parseOptions args =
  case execParserPure defaultPrefs options args of
    Success cmd -> pure cmd
    Failure failure -> do
      let (msg, _) = renderFailure failure "conformance-test-gen"
      hPutStrLn stderr msg
      exitFailure
    CompletionInvoked compl -> do
      msg <- execCompletion compl "conformance-test-gen"
      putStr msg
      exitSuccess

main :: IO ()
main = do
  cmd <- getArgs >>= parseOptions
  case cmd of
    ListClasses -> mapM_ (putStrLn . keyName) $ suiteKeys genesisTestSuite <> suiteKeys smokeTestSuite

-- | All available test key names, derived from the key type definitions.
-- No hardcoded strings: the datatype names and constructor names come
-- entirely from the 'GHC.Generics.Generic' representations of the key
-- types.
availableTestKeys :: [String]
availableTestKeys =
  fmap keyName (getAllKeys @GenesisTestKey)
    <> fmap keyName (getAllKeys @SmokeTestKey)
