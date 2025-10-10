{-# LANGUAGE OverloadedStrings #-}

module Main where

import BuildInfo (gitBranch, gitCommitDate, gitCommitHash, gitIsDirty)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import GHCID.Config (GHCIDServerConfig (..), defaultServerConfig, loadConfig)
import MCP.Server.GHCID (runGHCIDServer)
import GHCID.Wrapper (runGhcidWrapper)
import qualified MCP.SDK.Logging as SDKLog
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Utils.Logging (LogLevel (..), logError, logInfo, setLogLevel)

-- | CLI action to perform.
data Action
  = ActionRun (Maybe FilePath)
  | ActionShowHelp
  | ActionShowVersion

-- | Parsed CLI options.
data Options = Options
  { optAction :: Action
  , optLogLevel :: LogLevel
  }

-- | Default CLI options (warn-level logging, run with default config).
defaultOptions :: Options
defaultOptions = Options {optAction = ActionRun Nothing, optLogLevel = Warn}

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--internal-ghcid-wrapper" : wrapperArgs -> runGhcidWrapper wrapperArgs
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

-- | Run server with default configuration
runWithDefaultConfig :: IO ()
runWithDefaultConfig = do
  currentDir <- getCurrentDirectory
  let config = defaultServerConfig {defaultWorkspace = currentDir}

  logInfo "Starting MCP-GHCID server with default configuration"
  logInfo $ "Working directory: " <> T.pack (show currentDir)
  logBuildInfo

  runServerWithResources config

-- | Run server with configuration from file
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

-- | Run server with proper resource management and signal handling
runServerWithResources :: GHCIDServerConfig -> IO ()
runServerWithResources config = do
  logInfo "Initializing GHCID server with signal handling and resource management"

  -- Run the server; signal handling is installed inside the server to ensure
  -- it has access to the live registry, so we just launch it here.
  runGHCIDServer config

-- | Show help message
showHelp :: IO ()
showHelp =
  putStrLn $
    unlines
      [ "mcp-ghcid - Model Context Protocol server for ghcid integration"
      , ""
      , "Usage:"
      , "  mcp-ghcid                        Start with default configuration"
      , "  mcp-ghcid --config FILE         Start with configuration from FILE"
      , "  mcp-ghcid --log-level LEVEL     Set log level (debug|info|warn|error)"
      , "  mcp-ghcid --help                Show this help message"
      , "  mcp-ghcid --version             Show version information"
      , ""
      , "Description:"
      , "  This MCP server provides integration with ghcid (GHCi daemon) for"
      , "  real-time Haskell compilation monitoring. It exposes tools to start,"
      , "  stop, and monitor ghcid processes, and provides access to compiler"
      , "  messages and build status through the MCP protocol."
      , ""
      , "Available MCP Tools:"
      , "  ghcid-start     - Start a new ghcid process for a project"
      , "  ghcid-stop      - Stop a running ghcid process"
      , "  ghcid-restart   - Restart a ghcid process"
      , "  ghcid-status    - Get status of a ghcid process"
      , "  ghcid-messages  - Get compiler messages from ghcid"
      , "  ghcid-clear     - Clear messages from a ghcid process"
      , "  ghcid-list      - List all active ghcid processes"
      , ""
      , "Configuration:"
      , "  Configuration files should be in JSON format. See documentation"
      , "  for available options and examples."
      , "  The default log level is 'warn'. Use --log-level to adjust verbosity."
      , ""
      , "Examples:"
      , "  # Start server in current directory"
      , "  mcp-ghcid"
      , ""
      , "  # Start with custom configuration"
      , "  mcp-ghcid --config ~/.config/mcp-ghcid/config.json"
      ]

-- | Log build information for debugging
logBuildInfo :: IO ()
logBuildInfo = do
  logInfo $ "Built from git commit: " <> T.pack gitCommitHash <> if gitIsDirty then " (dirty)" else ""
  logInfo $ "Git branch: " <> T.pack gitBranch
  logInfo $ "Build date: " <> T.pack gitCommitDate

-- | Show version information
showVersion :: IO ()
showVersion =
  putStrLn $
    unlines
      [ "mcp-ghcid version 0.1.0.0"
      , "Model Context Protocol server for ghcid integration"
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
