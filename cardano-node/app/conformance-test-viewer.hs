{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import           Prelude hiding (lookup)

import           Control.Error.Util (failWith)
import           Control.Monad.Except (ExceptT(), runExceptT, throwError, MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import           Data.Aeson (eitherDecode, FromJSON(..), ToJSON(..), withText)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (groupBy)
import qualified Data.Text as T () -- TODO: Waiting for test key parsing.
import qualified ExitCodes as Exit
import           Options.Applicative
import qualified Ouroboros.Network.AnchoredFragment as AF
import           ShrinkIndex
import           System.Environment (getArgs)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           Test.Consensus.Genesis.Setup (ConformanceTest(..))
import           Test.Consensus.Genesis.Tests (GenesisTestKey, testSuite)
import           Test.Consensus.Genesis.TestSuite (at, getTest)
import           Test.Consensus.OrphanInstances ()
import           Test.Consensus.PeerSimulator.StateView (StateView(..))
import           Test.Consensus.PointSchedule (GenesisTest(..), GenesisTestFull)
import qualified Test.Consensus.Serialize as Serialize
import qualified Test.QuickCheck.Gen as QC
import           Text.Read (readEither)
import           Test.Util.TestBlock (TestBlock)

nameString, versionString :: String
nameString    = "conformance-test-viewer"
versionString = "v0.0"



data Options = Options
  { optInputPath    :: Maybe FilePath -- ^ Where to read input (default stdin)
  , optShrinkIndex  :: ShrinkIndex    -- ^ Which shrink of the input to analyze
  , optOutputPath   :: Maybe FilePath -- ^ Where to write output (default stdout)
  , optMode         :: Mode
  } deriving (Eq, Show)

data Mode
  = ShowDescendant
  deriving (Eq, Show)

data TestCaseType
  = IntTC | StringTC | GenesisTestTC
  deriving (Eq, Show)

options :: ParserInfo Options
options = info
  (optionParser <**> helper <**> simpleVersioner versionString) $
  mconcat
    [ fullDesc
    , progDesc "A tool for viewing shrunken test cases."
    , header "viewer - a conformance test viewer"
    ]

optionParser :: Parser Options
optionParser = Options
  <$> (optional $ argument str
    (mconcat
        [ metavar "FILE_PATH"
        , help "File path for the test case file (JSON)"
        ]))
  <*> (option (eitherReader parseShrinkIndexOption)
    (long "shrink-index" <> mconcat
        [ short 'i'
        , metavar "SHRINK_INDEX"
        , value (path [])
        , help "An index pointing to a shrunken test case"
        ]))
  <*> (optional $ option str
    (long "output" <> mconcat
        [ metavar "FILE_PATH"
        , help "File path for writing output"
        ]))
  <*> pure ShowDescendant



main :: IO ()
main = do
  opts <- getArgs >>= getOptions

  result <- runExceptT $ do
    testCase <- getInputTestCase (optInputPath opts)
    shrinkResult <- analyzeShrinkTree (optMode opts) (optShrinkIndex opts) testCase
    writeOutputTestCase (optOutputPath opts) shrinkResult

  case result of
    Right () -> pure ()
    Left err -> do
      hPutStr stderr (err <> "\n")
      Exit.exitWithStatus Exit.BadUsage



-- | This utility is parametric over the test case type, but at run time
-- we have to instantiate the input at a specific type. To achieve this
-- we hide the concrete type `a` behind an existential.
data ViewableTestCase where
  TestCase
    :: (a ~ GenesisTestFull blk, AF.HasHeader blk, Show blk)
    => Serialize.ReifiedTestCase GenesisTestKey Serialize.BlockRep
    -> a -> (a -> [a]) -> ViewableTestCase



-- | Parse command line options
getOptions :: [String] -> IO Options
getOptions args = do
  case execParserPure defaultPrefs options args of
    Success opts -> pure opts
    Failure failure -> do
      let (msg, _) = renderFailure failure nameString
      hPutStrLn stderr msg
      Exit.exitWithStatus Exit.BadUsage
    CompletionInvoked compl -> do
      execCompletion compl nameString >>= putStr
      Exit.exitWithStatus Exit.Success

getInputTestCase
  :: (MonadError String m, MonadIO m)
  => Maybe FilePath -> m ViewableTestCase
getInputTestCase inputPath = do
  input <- liftIO $ maybe BS.getContents BS.readFile inputPath
  rawJson <- case eitherDecode input of
    Right ok -> pure (ok :: Aeson.Value)
    Left err -> throwError $ "Input decoding error: " <> err

  _rawKey <- case rawJson of
    Aeson.Object o -> case Aeson.lookup "key" o of
      Just (Aeson.String k) -> pure k
      _ -> throwError "Malformed JSON: missing string field \"key\""
    _ -> throwError "Malformed JSON: must be an object"
  testKey <- error "getInputTestCase: test key parsing not yet implemented"
    -- TODO(nbloomf): fix this once test keys are implemented
    -- case parseKeyName (T.unpack rawKey) of
    --  Just (k :: GenesisTestKey) -> pure k
    --  Nothing -> throwError $ "Unrecognized test case key: " <> T.unpack rawKey

  reifiedTestCase :: Serialize.ReifiedTestCase GenesisTestKey Serialize.BlockRep
    <- case eitherDecode input of
      Right ok -> pure ok
      Left err -> throwError $ "Input decoding error: " <> err

  let
    -- The StateView is not used in the existing shrinkers, but if it ever
    -- is we will need to update the serialized test cases to include it.
    -- See @ouroboros-consensus-diffusion:Test.Consensus.PointSchedule.Shrinking@
    stateView :: StateView TestBlock
    stateView = error "getInputTestCase: Cannot construct an accurate StateView"

    generator = ctGenerator conformanceTest
    Serialize.Seed seed = Serialize.rtcSeed reifiedTestCase
    conformanceTest = getTest . at testSuite $ testKey
    -- QuickCheck's default size is 30, which we adjust to get the initial test case.
    genesisTest = QC.unGen generator seed (ctMaxSize conformanceTest 30)
    shrinker val = ctShrinker conformanceTest val stateView

  pure $ TestCase reifiedTestCase genesisTest shrinker

-- | Analyze the shrink tree of a value of any viewable test case;
-- returns the resulting test case.
analyzeShrinkTree
  :: (Monad m)
  => Mode -> ShrinkIndex -> ViewableTestCase -> ExceptT String m ViewableTestCase
analyzeShrinkTree mode shrinkIndex (TestCase key testCase shrinker) =
  let makeTestCase x = TestCase key x shrinker
  in fmap makeTestCase $ case mode of
    ShowDescendant -> failWith "Descendant does not exist. :(" $
      lookup shrinkIndex $ ShrinkIndex.makeShrinkTree shrinker testCase

writeOutputTestCase
  :: (MonadIO m) => Maybe FilePath -> ViewableTestCase -> m ()
writeOutputTestCase outputPath (TestCase reifiedTestCase testCase _) = do
  let
    Serialize.ReifiedTestCase {..} = reifiedTestCase
    bytes = encodePretty $ Serialize.serializeReifiedTestCase
      Serialize.FormatVersionOne $ Serialize.toReifiedTestCase rtcTestKey rtcTestVersion
        (gtBlockTree testCase) (gtSchedule testCase) rtcShrinkIndex rtcSeed
  liftIO $ case outputPath of
    Nothing -> BS.putStr bytes >> putStrLn ""
    Just oPath -> BS.writeFile oPath bytes



-- Very permissive; given a string, interpret any maximal contiguous
-- substring of digits as a number and all other characters as delimiters.
--   parseShrinkIndex "1,2,34"    == Right path [1,2,34]
--   parseShrinkIndex "1,,2,,,34" == Right path [1,2,34]
--   parseShrinkIndex "1 2 34"    == Right path [1,2,34]
--   parseShrinkIndex "o1w2o34w"  == Right path [1,2,34]
parseShrinkIndexOption :: String -> Either String ShrinkIndex
parseShrinkIndexOption =
  fmap path . traverse readEither . filter (all isDigit) . groupBy bothDigits
  where bothDigits = on (&&) isDigit
