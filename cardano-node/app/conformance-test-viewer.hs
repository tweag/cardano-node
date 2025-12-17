{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Prelude hiding (lookup)

import           Control.Error.Util (failWith)
import           Control.Monad.Except (ExceptT(), runExceptT, throwError, MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import           Data.Aeson (eitherDecode, FromJSON(..), ToJSON(..))
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.Proxy
import           Data.List (groupBy)
import qualified ExitCodes as Exit
import           Options.Applicative
import           Ouroboros.Consensus.Byron.Ledger.Block
import           ShrinkIndex
import           System.Environment (getArgs)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           Test.Consensus.OrphanInstances ()
import           Test.Consensus.PointSchedule (GenesisTest, PointSchedule)
import           Test.QuickCheck (Arbitrary(..))
import           Text.Read (readEither)

nameString, versionString :: String
nameString    = "conformance-test-viewer"
versionString = "v0.0"



data Options = Options
  { optInputPath    :: Maybe FilePath -- ^ Where to read input (default stdin)
  , optShrinkIndex  :: ShrinkIndex    -- ^ Which shrink of the input to analyze
  , optOutputPath   :: Maybe FilePath -- ^ Where to write output (default stdout)
  , optTestCaseType :: TestCaseType   -- ^ For testing; specify a test case type
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
  <*> (option (eitherReader parseTestCaseType)
    (long "type" <> mconcat
      [ short 't'
      , value GenesisTestTC
      , metavar "TYPE"
      , help "Which type of test case to parse"
      ]))
  <*> pure ShowDescendant



main :: IO ()
main = do
  args <- getArgs
  opts <- getOptions args

  result <- runExceptT $ do
    testCase <- getInputTestCase (optTestCaseType opts) (optInputPath opts)
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
  TestCase :: (FromJSON a, ToJSON a, Arbitrary a) => a -> ViewableTestCase



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

-- | Determine from the program options what type the input test
-- case should be instantiated at, and then read it.
getInputTestCase
  :: (MonadError String m, MonadIO m)
  => TestCaseType -> Maybe FilePath -> m ViewableTestCase
getInputTestCase testCaseType inputPath = do
  case testCaseType of
    IntTC         -> readInputTestCase (Proxy :: Proxy Int)    inputPath
    StringTC      -> readInputTestCase (Proxy :: Proxy String) inputPath
    GenesisTestTC -> error "Genesis test not yet implemented!" -- readInputTestCase @(GenesisTest ByronBlock (PointSchedule ByronBlock)) -- TODO

-- | Read and parse a JSON-encoded test case either
-- from a file or from stdin.
readInputTestCase
  :: forall a m. (MonadError String m, MonadIO m)
  => (ToJSON a, FromJSON a, Arbitrary a)
  => Proxy a -> Maybe FilePath -> m ViewableTestCase
readInputTestCase _ inputPath = do
  input <- liftIO $ maybe BS.getContents BS.readFile inputPath
  fmap TestCase $ case eitherDecode input of
    Right ok -> pure (ok :: a)
    Left err -> throwError $ "Input decoding error: " <> err

-- | Analyze the shrink tree of a value of any type that
-- implements `Arbitrary`, `FromJSON`, and `ToJSON`;
-- returns the resulting test case.
analyzeShrinkTree
  :: (Monad m)
  => Mode -> ShrinkIndex -> ViewableTestCase -> ExceptT String m ViewableTestCase
analyzeShrinkTree mode shrinkIndex (TestCase testCase) = fmap TestCase $
  case mode of
    ShowDescendant -> failWith "Descendant does not exist. :(" $
      lookup shrinkIndex $ arbitraryShrinkTree testCase

writeOutputTestCase
  :: (MonadIO m) => Maybe FilePath -> ViewableTestCase -> m ()
writeOutputTestCase outputPath (TestCase testCase) = do
  let bytes = encodePretty testCase
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

parseTestCaseType :: String -> Either String TestCaseType
parseTestCaseType symbol = case symbol of
  "int"     -> Right IntTC
  "string"  -> Right StringTC
  "genesis" -> Right GenesisTestTC
  _ -> Left $ "Unrecognized test case type \"" <> symbol <> "\""
