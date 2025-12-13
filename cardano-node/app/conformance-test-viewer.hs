{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds -Wno-unused-imports #-}

module Main (main) where

import           Prelude hiding (lookup)

import           Control.Monad ((>=>))
import           Data.Aeson (eitherDecode, encode, FromJSON, ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import           Data.Char (isDigit)
import           Data.Proxy
import           Data.List (groupBy)
import           ShrinkIndex
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit
import           System.IO (hPutStr, hPutChar, stderr)
import           Test.QuickCheck (Arbitrary)
import           Text.Read (readEither)



main :: IO ()
main = do
  opts <- getOptions

  if optShowHelp opts
    then showUsage >> exitSuccess
    else pure ()
    
  if optShowVersion opts
    then showVersion >> exitSuccess
    else pure ()
  
  if optDebug opts
    then analyzeTreeOf (Proxy :: Proxy Int) opts
    else analyzeTreeOf (Proxy :: Proxy Int) opts -- For now just a dummy implementation

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



getOptions :: IO Options
getOptions = do
  argv <- getArgs

  let (actions, params, errs) = getOpt Permute options argv
  
  if null errs
    then pure ()
    else do
      errPutStrLn "option error(s):"
      mapM_ errPutStrLn errs
      showUsage >> exitFailure
    
  -- Options as parsed only from the flags.
  opts <- case compose actions defaultOpts of
    Right ok -> pure ok
    Left err -> do
      errPutStrLn $ "Option parsing error: " <> err
      showUsage >> exitFailure

  -- Adjust to account for the input path being passed as
  -- an argument instead of a flag.
  case params of
    [] -> pure opts
    [inputPath] -> pure $ opts { optInputPath = Just inputPath }
    _ -> do
      errPutStrLn $ "unrecognized arguments: " <> show params
      showUsage >> exitFailure



-- This is Kleisli `mconcat`.
compose :: (Monad m) => [a -> m a] -> a -> m a
compose = foldr (>=>) return

data Mode
  = ShowDescendant
  deriving (Eq, Show)

data Options = Options
  { optShowHelp    :: Bool           -- ^ Show usage?
  , optShowVersion :: Bool           -- ^ Show version info?
  , optInputPath   :: Maybe FilePath
  , optShrinkIndex :: ShrinkIndex
  , optOutputPath  :: Maybe FilePath
  , optPrettyPrint :: Bool
  , optDebug       :: Bool
  , optMode        :: Mode
  } deriving (Eq, Show)

defaultOpts :: Options
defaultOpts = Options
  { optShowHelp    = False
  , optShowVersion = False
  , optInputPath   = Nothing
  , optShrinkIndex = mempty
  , optOutputPath  = Nothing
  , optPrettyPrint = True
  , optDebug       = False
  , optMode        = ShowDescendant
  }

options :: [OptDescr (Options -> Either String Options)]
options =
  [ let munge opts = pure $ opts { optShowHelp = True }
    in Option ['?'] ["help"] (NoArg munge)
        "show usage"

  , let munge opts = pure $ opts { optShowVersion = True }
    in Option [] ["version"] (NoArg munge)
        "show version information"

  , let munge mPath opts = pure $ opts { optInputPath = mPath }
    in Option [] ["input"] (OptArg munge "FILE")
        "read from FILE (default is stdin)"

  , let
      munge d opts = do
        ix <- parseShrinkIndexOption d
        pure $ opts { optShrinkIndex = ix }
    in
      Option [] ["shrink-index"] (ReqArg munge "STRING")
        "comma delimited list of natural numbers, e.g. '1,3,3,7' (default is the empty list)"
      
  , let munge mPath opts = pure $ opts { optOutputPath = mPath }
    in Option [] ["output"] (OptArg munge "FILE")
        "write to FILE (default is stdout)"

  , let munge opts = pure $ opts { optPrettyPrint = False }
    in Option [] ["notpretty"] (NoArg munge)
        "do not pretty print output"
        
  , let munge opts = pure $ opts { optDebug = True }
    in Option [] ["debug"] (NoArg munge)
        "Use trees of integers instead of test scripts"
  ]

-- Very permissive; given a string, interpret any maximal contiguous
-- substring of digits as a number and all other characters as delimiters.
--   parseShrinkIndex "1,2,34"    == Right path [1,2,34]
--   parseShrinkIndex "1,,2,,,34" == Right path [1,2,34]
--   parseShrinkIndex "1 2 34"    == Right path [1,2,34]
--   parseShrinkIndex "o1w2o34w"  == Right path [1,2,34]
parseShrinkIndexOption :: String -> Either String ShrinkIndex
parseShrinkIndexOption =
  fmap path . sequenceA . fmap readEither . filter (all isDigit) . groupBy bothDigits
  where bothDigits u v = isDigit u && isDigit v



-- Like putStrLn, but for stderr.
errPutStrLn :: String -> IO ()
errPutStrLn msg = hPutStr stderr msg >> hPutChar stderr '\n'

showUsage :: IO ()
showUsage = do
  let header = "USAGE: " <> nameString <> " [--OPTION...] [PATH]"
  putStrLn $ usageInfo header options

showVersion :: IO ()
showVersion = putStrLn versionString

nameString :: String
nameString = "conformance-test-viewer"

versionString :: String
versionString = nameString <> "-0.0"
