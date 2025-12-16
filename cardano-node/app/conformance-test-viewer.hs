{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Prelude hiding (lookup)

import           Control.Monad (when)
import           Data.Aeson (eitherDecode, encode, FromJSON, ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.Proxy
import           Data.List (groupBy)
import qualified ExitCodes as Exit
import           Options.Applicative
import           ShrinkIndex
import           System.Environment (getArgs)
import           System.Exit
import           System.IO (hPutStr, hPutStrLn, hPutChar, stderr)
import           Test.QuickCheck (Arbitrary)
import           Text.Read (readEither)



main :: IO ()
main = do
  opts <- getArgs >>= getOptions

  when (optShowVersion opts) $
    showVersion >> Exit.exitWithStatus Exit.Success
  
  case optDebug opts of
    True  -> analyzeTreeOf (Proxy :: Proxy Int) opts
    False -> analyzeTreeOf (Proxy :: Proxy Int) opts -- For now just a dummy implementation



-- Analyze the shrink tree of a value of any type that
-- implements `Arbitrary`, `FromJSON`, and `ToJSON`.
analyzeTreeOf
  :: forall a. (ToJSON a, FromJSON a, Arbitrary a)
  => Proxy a -> Options -> IO ()
analyzeTreeOf _ opts = do
  input <- case optInputPath opts of
    Nothing -> BS.getContents
    Just iPath -> BS.readFile iPath
    
  testCase <- case eitherDecode input of
    Right ok -> pure (ok :: a)
    Left err -> do
      errPutStrLn $ "Input decoding error: " <> err
      exitFailure
  
  let tree = arbitraryShrinkTree testCase

  case optMode opts of
    ShowDescendant -> do
      let result = lookup (optShrinkIndex opts) tree
      descendant <- case result of
        Nothing -> do
          errPutStrLn $ "Descendant does not exist. :("
          exitFailure
        Just ok -> pure ok
  
      let bytes = if optPrettyPrint opts
            then encodePretty descendant
            else encode descendant
        
      case optOutputPath opts of
        Nothing -> BS.putStr bytes >> putStrLn ""
        Just oPath -> BS.writeFile oPath bytes



getOptions :: [String] -> IO Options
getOptions args = do
  case execParserPure defaultPrefs options args of
    Success opts -> pure opts
    Failure failure -> do
      let (msg, _) = renderFailure failure nameString
      hPutStrLn stderr msg
      Exit.exitWithStatus Exit.BadUsage
    CompletionInvoked compl -> do
      msg <- execCompletion compl nameString
      putStr msg
      Exit.exitWithStatus Exit.Success



data Mode
  = ShowDescendant
  deriving (Eq, Show)

options :: ParserInfo Options
options = info (infoOption "INFO" (long "help" <> short '?') <*> optionParser) $ mconcat
  [ fullDesc
  ]

optionParser :: Parser Options
optionParser = Options
  <$> flag False True
    (long "version")
  <*> (option auto
    (long "input" <> mconcat
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
  <*> (option auto
    (long "output" <> mconcat
        [ metavar "FILE_PATH"
        , value Nothing
        , help "File path writing output"
        ]))
  <*> flag True False
    (long "notpretty")
  <*> flag False True
    (long "debug")
  <*> pure ShowDescendant

data Options = Options
  { optShowVersion :: Bool           -- ^ Show version info?
  , optInputPath   :: Maybe FilePath
  , optShrinkIndex :: ShrinkIndex
  , optOutputPath  :: Maybe FilePath
  , optPrettyPrint :: Bool           -- ^ Pretty print output?
  , optDebug       :: Bool           -- ^ For testing
  , optMode        :: Mode
  } deriving (Eq, Show)



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



-- Like putStrLn, but for stderr.
errPutStrLn :: String -> IO ()
errPutStrLn msg = hPutStr stderr msg >> hPutChar stderr '\n'

showVersion :: IO ()
showVersion = putStrLn versionString

nameString :: String
nameString = "conformance-test-viewer"

versionString :: String
versionString = nameString <> "-0.0"
