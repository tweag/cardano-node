{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Foldable (find, traverse_)
import           Options.Applicative (CompletionResult (execCompletion), Parser, ParserInfo,
                   ParserResult (CompletionInvoked, Failure, Success), argument, command,
                   defaultPrefs, eitherReader, execParserPure, fullDesc, header, help, helper,
                   info, long, metavar, optional, option, progDesc, renderFailure,
                   strOption, subparser, (<**>))
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

import           Test.Consensus.Genesis.Setup (ConformanceTest (..))
import           Test.Consensus.Genesis.TestSuite (at, getTest, keyName, suiteKeys)
import qualified Test.Consensus.Genesis.TestSuite.All as All
import           Test.Consensus.Serialize (ReifiedTestCase (..), Seed (..))
import           Test.QuickCheck.Random (newQCGen)
import           Test.Util.TestBlock (TestBlock)

data Command
  = ListClasses
  | Generate GenerateOpts

data GenerateOpts = GenerateOpts
  { genSeed   :: Maybe Seed
  , genOutput :: Maybe FilePath
  , genKey    :: All.TestKey
  }

parseKey :: String -> Either String All.TestKey
parseKey s = maybe (Left $ "Unknown test class: " <> s) Right $
  find ((== s) . keyName) allKeys

parseSeed :: String -> Either String Seed
parseSeed s = maybe (Left $ "Invalid seed: " <> s) (Right . Seed) $ readMaybe s

generateP :: Parser GenerateOpts
generateP = GenerateOpts
  <$> optional (option (eitherReader parseSeed)
        (mconcat
          [ long "seed"
          , metavar "SEED"
          , help "Seed for test case generation"
          ]))
  <*> optional (strOption
        (mconcat
          [ long "output"
          , metavar "FILE"
          , help "Output file"
          ]))
  <*> argument (eitherReader parseKey)
        (mconcat
          [ metavar "CLASS"
          , help "Test class, as given by `list-classes`"
          ])

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
    <> command
        "generate"
        ( info
            (Generate <$> generateP <**> helper)
            (progDesc "Generate a serialized conformance test case")
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
    ListClasses -> traverse_ (putStrLn . keyName) allKeys
    Generate opts -> do
      seed <- maybe (fmap Seed newQCGen) pure $ genSeed opts
      let key = genKey opts
          ct = getTest $ at (All.testSuite @TestBlock) key
          rtc = ReifiedTestCase
            { rtcTestKey = key
            , rtcTestVersion = ctVersion ct
            , rtcShrinkIndex = mempty
            , rtcSeed = seed
            }
      maybe BSL.putStrLn BSL.writeFile (genOutput opts) $ encode rtc

allKeys :: [All.TestKey]
allKeys = suiteKeys $ All.testSuite @TestBlock

