{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command line argument parser for the test runner.
module Options (parseOptions, Options (..)) where

import           Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)

import           Options.Applicative (CompletionResult (execCompletion), Parser, ParserInfo,
                   ParserResult (CompletionInvoked, Failure), auto, defaultPrefs, execParserPure,
                   fullDesc, header, help, helper, info, long, metavar, option, progDesc,
                   renderFailure, short, strArgument, strOption, value, (<**>))
import qualified Options.Applicative as O
import           System.IO (hPutStrLn, stderr)

import           ExitCodes
import           ShrinkIndex

data Options = Options
  { optTestFile :: FilePath
  , optOutputTopologyFile :: FilePath
  , optSocketPath :: FilePath
  , optPort :: PortNumber
  , optMinimalTestOutput :: Maybe FilePath
  , optShrinkIndex :: Maybe ShrinkIndex
  }

options :: ParserInfo Options
options =
  info
    (optsP <**> helper)
    ( mconcat
        [ fullDesc
        , progDesc
            ( mconcat
                [ "Locally simulate peers described by the TEST_FILE "
                , "to tests a node's resulting state for consensus"
                ]
            )
        , header "runner - A conformance test runner"
        ]
    )

optsP :: Parser Options
optsP = do
  optTestFile <- strArgument $ metavar "TEST_FILE"
  optOutputTopologyFile <-
    strOption $
      mconcat
        [ long "topology-file"
        , short 't'
        , metavar "FILE_PATH"
        , value "topology.file"
        , help "File path for the testing topology file (JSON)"
        ]

  optPort <-
    option auto $
      mconcat
        [ long "port"
        , short 'p'
        , metavar "PORT_NUMBER"
        , value 3001
        , help "Starting port for simulated peers"
        ]

  optMinimalTestOutput <-
    O.optional $ strOption $
      mconcat
        [ long "minimal-test-output"
        , short 'm'
        , metavar "FILE_PATH"
        , help "File path for the minimal counterexample test file"
        ]

  optShrinkIndex <-
    O.optional $ fmap path $ option auto $
      mconcat
        [ long "shrink-index"
        , short 'i'
        , metavar "SHRINK_INDEX"
        , help "An index pointing to a shrunk test case"
        ]

  optSocketPath <-
    strOption $
      mconcat
        [ long "socket-path"
        , short 's'
        , metavar "FILEPATH"
        , help "Filepath to a Unix domain socket for communicating to the NUT"
        ]


  pure Options{..}

parseOptions :: [String] -> IO (Options)
parseOptions args =
  case execParserPure defaultPrefs options args of
    O.Success opts -> pure opts
    Failure failure -> do
      let (msg, _) = renderFailure failure "conformance-test-runner"
      hPutStrLn stderr msg
      exitWithStatus BadUsage
    CompletionInvoked compl -> do
      -- Completion handler
      msg <- execCompletion compl "conformance-test-runner"
      putStr msg
      exitWithStatus Success
