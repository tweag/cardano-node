{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Options.Applicative
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStrLn, stderr)

import qualified Test.Consensus.Genesis.Tests as Genesis
import           Test.Consensus.Genesis.TestSuite.SmallKey
import qualified Test.Consensus.PeerSimulator.Tests as PeerSimulator

import           KeyName (keyName)

-- NOTE [Proposed TestSuite extension]
-- The 'availableTestKeys' function below is currently implemented by
-- directly calling 'getAllKeys' from 'SmallKey' for each key type, and
-- then rendering the keys with 'keyName' from 'KeyName'.  This is a
-- bit of a leaky abstraction, as it relies on the fact that the key types
-- used in the test suites are 'SmallKey's, and that the 'keyName'
-- function is available to render them.  A more elegant solution would be
-- to add a method to the 'TestSuite' abstraction itself, which would allow
-- us to extract the key enumeration from a suite value, without relying on
-- the 'SmallKey' abstraction: For example, we could add a method like this:
--
-- @suiteKeys :: TestSuite blk key -> [key]@
--
-- Then we could not expose the SmallKey module at all.

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
    ListClasses -> mapM_ putStrLn availableTestKeys

-- | All available test key names, derived from the key type definitions.
-- No hardcoded strings: the datatype names and constructor names come
-- entirely from the 'GHC.Generics.Generic' representations of the key
-- types.
availableTestKeys :: [String]
availableTestKeys =
  fmap keyName (getAllKeys @Genesis.GenesisTestKey)
    <> fmap keyName (getAllKeys @PeerSimulator.SmokeTestKey)
