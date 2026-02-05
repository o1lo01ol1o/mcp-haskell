{-# LANGUAGE OverloadedStrings #-}

module Main where

import BuildInfo (gitBranch, gitCommitDate, gitCommitHash, gitIsDirty)
import Cabal.Config (CabalServerConfig (..), defaultServerConfig, loadConfig)
import Cabal.Wrapper (runCabalWrapper)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server.Cabal (runCabalServer)
import qualified MCP.SDK.Logging as SDKLog
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Utils.Logging (LogLevel (..), logError, logInfo, setLogLevel)

data Action
  = ActionRun (Maybe FilePath)
  | ActionShowHelp
  | ActionShowVersion

data Options = Options
  { optAction :: Action
  , optLogLevel :: LogLevel
  }

defaultOptions :: Options
defaultOptions = Options {optAction = ActionRun Nothing, optLogLevel = Warn}

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--internal-cabal-wrapper" : wrapperArgs -> runCabalWrapper wrapperArgs
    _ -> runCLI args

runCLI :: [String] -> IO ()
runCLI args =
  case parseArgs args of
    Left err -> do
      putStrLn err
      exitFailure
    Right opts -> do
      let level = optLogLevel opts
      setLogLevel level
      SDKLog.setLogLevel (toSdkLevel level)
      case optAction opts of
        ActionShowHelp -> showHelp
        ActionShowVersion -> showVersion
        ActionRun Nothing -> runWithDefaultConfig
        ActionRun (Just path) -> runWithConfig path

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go :: Options -> [String] -> Either String Options
    go opts [] = Right opts
    go opts ("--help" : _) = Right opts {optAction = ActionShowHelp}
    go opts ("--version" : _) = Right opts {optAction = ActionShowVersion}
    go opts ("--config" : path : rest) = go opts {optAction = ActionRun (Just path)} rest
    go _ ("--config" : []) = Left "Missing argument for --config"
    go opts ("--log-level" : lvl : rest) =
      case parseLogLevel lvl of
        Just l -> go opts {optLogLevel = l} rest
        Nothing -> Left $ "Unknown log level: " ++ lvl
    go _ ("--log-level" : []) = Left "Missing argument for --log-level"
    go _ (unknown : _) = Left $ "Unknown argument: " ++ unknown

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel raw =
  case map toLower raw of
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "warning" -> Just Warn
    "error" -> Just Error
    _ -> Nothing

toSdkLevel :: LogLevel -> SDKLog.LogLevel
toSdkLevel Debug = SDKLog.Debug
toSdkLevel Info = SDKLog.Info
toSdkLevel Warn = SDKLog.Warn
toSdkLevel Error = SDKLog.Error

runWithDefaultConfig :: IO ()
runWithDefaultConfig = do
  currentDir <- getCurrentDirectory
  let config = defaultServerConfig {defaultWorkspace = currentDir}
  logInfo "Starting MCP-Cabal server with default configuration"
  logInfo $ "Working directory: " <> T.pack (show currentDir)
  logBuildInfo
  runServerWithResources config

runWithConfig :: FilePath -> IO ()
runWithConfig configPath = do
  configResult <- loadConfig configPath
  case configResult of
    Nothing -> do
      logError $ "Failed to load configuration from: " <> T.pack (show configPath)
      logInfo "Using default configuration instead"
      runWithDefaultConfig
    Just config -> do
      logInfo $ "Loaded configuration from: " <> T.pack (show configPath)
      runServerWithResources config

runServerWithResources :: CabalServerConfig -> IO ()
runServerWithResources config = do
  logInfo "Initializing Cabal server with signal handling and resource management"
  runCabalServer config

showHelp :: IO ()
showHelp =
  putStrLn $
    unlines
      [ "mcp-cabal - Model Context Protocol server for cabal test integration"
      , ""
      , "Usage:"
      , "  mcp-cabal                        Start with default configuration"
      , "  mcp-cabal --config FILE         Start with configuration from FILE"
      , "  mcp-cabal --log-level LEVEL     Set log level (debug|info|warn|error)"
      , "  mcp-cabal --help                Show this help message"
      , "  mcp-cabal --version             Show version information"
      , ""
      , "Description:"
      , "  This MCP server provides integration with cabal test for"
      , "  real-time test execution monitoring. It exposes tools to start,"
      , "  stop, and monitor cabal test processes, and provides access to"
      , "  test output through the MCP protocol."
      , ""
      , "Available MCP Tools:"
      , "  cabal-test-start     - Start a new cabal test process for a project"
      , "  cabal-test-stop      - Stop a running cabal test process"
      , "  cabal-test-status    - Get status of a cabal test process"
      , "  cabal-test-messages  - Get output from a cabal test process"
      , "  cabal-test-list      - List known cabal test processes"
      , ""
      , "Configuration:"
      , "  Configuration files should be in JSON format. See documentation"
      , "  for available options and examples."
      , "  The default log level is 'warn'. Use --log-level to adjust verbosity."
      , ""
      , "Examples:"
      , "  # Start server in current directory"
      , "  mcp-cabal"
      , ""
      , "  # Start with custom configuration"
      , "  mcp-cabal --config ~/.config/mcp-cabal/config.json"
      ]

logBuildInfo :: IO ()
logBuildInfo = do
  logInfo $ "Built from git commit: " <> T.pack gitCommitHash <> if gitIsDirty then " (dirty)" else ""
  logInfo $ "Git branch: " <> T.pack gitBranch
  logInfo $ "Build date: " <> T.pack gitCommitDate

showVersion :: IO ()
showVersion =
  putStrLn $
    unlines
      [ "mcp-cabal version 0.1.0.0"
      , "Model Context Protocol server for cabal test integration"
      , ""
      , "Built with:"
      , "  - GHC 9.8.4"
      , "  - MCP Protocol v2025-06-18"
      , "  - Git commit: " ++ gitCommitHash ++ if gitIsDirty then " (dirty)" else ""
      , "  - Git branch: " ++ gitBranch
      , "  - Build date: " ++ gitCommitDate
      , ""
      , "Copyright (c) 2025 o1lo01ol1o"
      , "Licensed under MIT License"
      ]
