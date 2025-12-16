{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import           Prelude hiding (lookup)

import           Control.Monad.Except (ExceptT(), runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT(), asks, runReaderT)
import           Data.Aeson (eitherDecode, encode, FromJSON(..), ToJSON(..))
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
import           System.Exit
import           System.IO (hPutStr, hPutStrLn, hPutChar, stderr)
import           Test.Consensus.OrphanInstances ()
import           Test.Consensus.PointSchedule (GenesisTest, PointSchedule)
import           Test.QuickCheck (Arbitrary(..), Gen)
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
  <$> (argument (Just <$> str)
    (mconcat
        [ metavar "FILE_PATH"
        , value Nothing
        , help "File path for the test case file (JSON)"
        ]))
  <*> (option (eitherReader parseShrinkIndexOption)
    (long "shrink-index" <> mconcat
        [ short 'i'
        , metavar "SHRINK_INDEX"
        , value (path [])
        , help "An index pointing to a shrunken test case"
        ]))
  <*> (option (Just <$> str)
    (long "output" <> mconcat
        [ metavar "FILE_PATH"
        , value Nothing
        , help "File path writing output"
        ]))
  <*> (option (eitherReader parseTestCaseType)
    (long "type" <> mconcat
      [ short 't'
      , value GenesisTestTC
      , metavar "TYPE"
      , help "Which type of test case to parse"
      ]))
  <*> pure ShowDescendant



-- Helper monad for the main function, providing the
-- options environment and early exit.
type ViewerM a = ReaderT Options (ExceptT String IO) a

runViewerM :: Options -> ViewerM a -> IO (Either String a)
runViewerM opts = runExceptT . flip runReaderT opts

runWithHandler :: ViewerM a -> (String -> IO a) -> Options -> IO a
runWithHandler act handle opts =
  runViewerM opts act >>= either handle pure


main :: IO ()
main = getArgs >>= getOptions
  >>= runWithHandler (getInputTestCase >>= analyzeShrinkTree >>= writeOutputTestCase)
        (\err -> hPutStr stderr (err <> "\n") >> Exit.exitWithStatus Exit.InternalError)



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
  :: ViewerM ViewableTestCase
getInputTestCase = do
  testCaseType <- asks optTestCaseType
  case testCaseType of
    IntTC         -> readInputTestCase @Int
    StringTC      -> readInputTestCase @String
    -- GenesisTestTC -> readInputTestCase @(GenesisTest ByronBlock (PointSchedule ByronBlock)) -- TODO

-- | Read and parse a JSON-encoded test case either
-- from a file or from stdin.
readInputTestCase
  :: forall a. (ToJSON a, FromJSON a, Arbitrary a)
  => ViewerM ViewableTestCase
readInputTestCase = do
  input <- asks optInputPath
    >>= (liftIO . maybe BS.getContents BS.readFile)

  TestCase <$> case eitherDecode input of
    Right ok -> pure (ok :: a)
    Left err -> throwError $ "Input decoding error: " <> err

-- | Analyze the shrink tree of a value of any type that
-- implements `Arbitrary`, `FromJSON`, and `ToJSON`;
-- returns the resulting test case.
analyzeShrinkTree :: ViewableTestCase -> ViewerM ViewableTestCase
analyzeShrinkTree (TestCase testCase) = do
  mode <- asks optMode
  TestCase <$> case mode of
    ShowDescendant -> do
      shrinkIndex <- asks optShrinkIndex
      maybe (throwError "Descendant does not exist. :(") pure $
        lookup shrinkIndex $ arbitraryShrinkTree testCase

writeOutputTestCase :: ViewableTestCase -> ViewerM ()
writeOutputTestCase (TestCase testCase) = do
  let bytes = encodePretty testCase
  outputPath <- asks optOutputPath
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
parseTestCaseType str = case str of
  "int"     -> Right IntTC
  "string"  -> Right StringTC
  "genesis" -> Right GenesisTestTC
  _ -> Left $ "Unrecognized test case type \"" <> str <> "\""
