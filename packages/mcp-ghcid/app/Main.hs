{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, ExitCode(ExitSuccess))
import System.Directory (getCurrentDirectory)
import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T

import MCP.Server.GHCID (runGHCIDServer)
import GHCID.Config (defaultServerConfig, GHCIDServerConfig(..), loadConfig)
import GHCID.Signals (withGracefulShutdown, defaultShutdownConfig, ShutdownConfig(..))
import GHCID.ProcessRegistry (createProcessRegistry, shutdownProcessRegistry)
import MCP.Router.GHCID (createGHCIDRouter, shutdownGHCIDRouter, defaultRouterConfig)
import Utils.Logging (logInfo, logError)

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
  let config = defaultServerConfig { defaultWorkspace = currentDir }
  
  logInfo "Starting MCP-GHCID server with default configuration"
  logInfo $ "Working directory: " <> T.pack (show currentDir)
  
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
  let shutdownConfig = defaultShutdownConfig 
        { shutdownGracePeriod = 30
        , shutdownForceDelay = 10
        , shutdownLogOutput = True
        , shutdownExitCode = ExitSuccess
        }
  
  -- Initialize resources with proper cleanup
  bracket
    initializeResources
    cleanupResources
    (runServerMain config shutdownConfig)
  where
    initializeResources = do
      logInfo "Initializing process registry and router"
      registry <- createProcessRegistry
      router <- createGHCIDRouter defaultRouterConfig
      return (registry, router)
    
    cleanupResources (registry, router) = do
      logInfo "Cleaning up server resources"
      shutdownGHCIDRouter router
      shutdownProcessRegistry registry
      logInfo "Server cleanup complete"
    
    runServerMain config shutdownConfig (registry, router) = do
      logInfo "Starting MCP-GHCID server with graceful shutdown handling"
      
      -- Run the server with signal handling
      withGracefulShutdown 
        shutdownConfig 
        (Just registry) 
        (Just router) 
        (runGHCIDServer config)

-- | Show help message
showHelp :: IO ()
showHelp = putStrLn $ unlines
  [ "mcp-ghcid - Model Context Protocol server for ghcid integration"
  , ""
  , "Usage:"  
  , "  mcp-ghcid                    Start with default configuration"
  , "  mcp-ghcid --config FILE     Start with configuration from FILE"
  , "  mcp-ghcid --help            Show this help message"
  , "  mcp-ghcid --version         Show version information"
  , ""
  , "Description:"
  , "  This MCP server provides integration with ghcid (GHCi daemon) for"
  , "  real-time Haskell compilation monitoring. It exposes tools to start,"
  , "  stop, and monitor ghcid processes, and provides access to compiler"
  , "  messages and build status through the MCP protocol."
  , ""
  , "Available MCP Tools:"
  , "  ghcid.start     - Start a new ghcid process for a project"
  , "  ghcid.stop      - Stop a running ghcid process"
  , "  ghcid.restart   - Restart a ghcid process" 
  , "  ghcid.status    - Get status of a ghcid process"
  , "  ghcid.messages  - Get compiler messages from ghcid"
  , "  ghcid.clear     - Clear messages from a ghcid process"
  , "  ghcid.list      - List all active ghcid processes"
  , ""
  , "Configuration:"
  , "  Configuration files should be in JSON format. See documentation"
  , "  for available options and examples."
  , ""
  , "Examples:"
  , "  # Start server in current directory"
  , "  mcp-ghcid"
  , ""  
  , "  # Start with custom configuration"
  , "  mcp-ghcid --config ~/.config/mcp-ghcid/config.json"
  ]

-- | Show version information
showVersion :: IO ()
showVersion = putStrLn $ unlines
  [ "mcp-ghcid version 0.1.0.0"
  , "Model Context Protocol server for ghcid integration"
  , ""
  , "Built with:"
  , "  - GHC 9.8.4"
  , "  - MCP Protocol v2025-03-26"
  , ""
  , "Copyright (c) 2025 o1lo01ol1o"
  , "Licensed under MIT License"
  ]