{-# LANGUAGE OverloadedStrings #-}

module Main where

import BuildInfo (gitBranch, gitCommitDate, gitCommitHash, gitIsDirty)
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import GHCID.Config (GHCIDServerConfig (..), defaultServerConfig, loadConfig)
import GHCID.Signals (ShutdownConfig (..), defaultShutdownConfig, withGracefulShutdown)
import MCP.Server.GHCID (runGHCIDServer)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import Utils.Logging (logError, logInfo)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runWithDefaultConfig
    ["--config", configPath] -> runWithConfig configPath
    ["--help"] -> showHelp
    ["--version"] -> showVersion
    _ -> do
      putStrLn "Invalid arguments. Use --help for usage information."
      exitFailure

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

  -- Create shutdown configuration
  let shutdownConfig =
        defaultShutdownConfig
          { shutdownGracePeriod = 30,
            shutdownForceDelay = 10,
            shutdownLogOutput = True,
            shutdownExitCode = ExitSuccess
          }

  -- Run the server with signal handling (SDK handles resource management)
  withGracefulShutdown
    shutdownConfig
    Nothing -- No custom registry needed - SDK manages this
    (runGHCIDServer config)

-- | Show help message
showHelp :: IO ()
showHelp =
  putStrLn $
    unlines
      [ "mcp-ghcid - Model Context Protocol server for ghcid integration",
        "",
        "Usage:",
        "  mcp-ghcid                    Start with default configuration",
        "  mcp-ghcid --config FILE     Start with configuration from FILE",
        "  mcp-ghcid --help            Show this help message",
        "  mcp-ghcid --version         Show version information",
        "",
        "Description:",
        "  This MCP server provides integration with ghcid (GHCi daemon) for",
        "  real-time Haskell compilation monitoring. It exposes tools to start,",
        "  stop, and monitor ghcid processes, and provides access to compiler",
        "  messages and build status through the MCP protocol.",
        "",
        "Available MCP Tools:",
        "  ghcid.start     - Start a new ghcid process for a project",
        "  ghcid.stop      - Stop a running ghcid process",
        "  ghcid.restart   - Restart a ghcid process",
        "  ghcid.status    - Get status of a ghcid process",
        "  ghcid.messages  - Get compiler messages from ghcid",
        "  ghcid.clear     - Clear messages from a ghcid process",
        "  ghcid.list      - List all active ghcid processes",
        "",
        "Configuration:",
        "  Configuration files should be in JSON format. See documentation",
        "  for available options and examples.",
        "",
        "Examples:",
        "  # Start server in current directory",
        "  mcp-ghcid",
        "",
        "  # Start with custom configuration",
        "  mcp-ghcid --config ~/.config/mcp-ghcid/config.json"
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
      [ "mcp-ghcid version 0.1.0.0",
        "Model Context Protocol server for ghcid integration",
        "",
        "Built with:",
        "  - GHC 9.8.4",
        "  - MCP Protocol v2025-06-18",
        "  - Git commit: " ++ gitCommitHash ++ if gitIsDirty then " (dirty)" else "",
        "  - Git branch: " ++ gitBranch,
        "  - Build date: " ++ gitCommitDate,
        "",
        "Copyright (c) 2025 o1lo01ol1o",
        "Licensed under MIT License"
      ]
