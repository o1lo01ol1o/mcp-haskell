{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Server.Cabal
  ( runCabalServer
  , CabalServerState(..)
  , initializeCabalServerState
  , withCabalServerState
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket, throw, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Cabal.Config (CabalServerConfig (..))
import Cabal.ProcessRegistry (ProcessRegistry, createProcessRegistry, shutdownProcessRegistry)
import Cabal.Signals (ShutdownConfig (..), defaultShutdownConfig, withGracefulShutdown)
import MCP.SDK.Capabilities (ServerCapabilityBuilder (..), ServerCapabilities, ToolsCapability (..), buildServerCapabilities)
import MCP.SDK.Error (MCPError (..))
import qualified MCP.SDK.Server as Server
import MCP.SDK.Server.API (registerTool)
import MCP.SDK.Server.Monad (ServerM, runServerM)
import MCP.SDK.Transport (wrapTransport)
import MCP.SDK.Transport.Stdio (createStdioTransport)
import MCP.SDK.Types hiding (serverName, serverVersion)
import MCP.Tools.Cabal (executeCabalTool)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (stdin, stdout)
import System.Posix.Process (exitImmediately, getParentProcessID)
import System.Posix.Types (CPid (..))
import qualified Utils.Logging as Log

-- | Cabal Server State
data CabalServerState = CabalServerState
  { cabalRegistry :: ProcessRegistry
  , serverConfig :: CabalServerConfig
  }

-- | Initialize Cabal server state
initializeCabalServerState :: CabalServerConfig -> IO CabalServerState
initializeCabalServerState config = do
  registry <- createProcessRegistry
  return
    CabalServerState
      { cabalRegistry = registry
      , serverConfig = config
      }

-- | Bracketed server state initialization that guarantees registry shutdown.
withCabalServerState :: CabalServerConfig -> (CabalServerState -> IO a) -> IO a
withCabalServerState config =
  bracket (initializeCabalServerState config) (shutdownProcessRegistry . cabalRegistry)

parentCheckIntervalMicros :: Int
parentCheckIntervalMicros = 1 * 1000000

-- | Monitor the parent process; if it exits unexpectedly, shut down the registry.
withParentDeathWatcher :: ProcessRegistry -> IO a -> IO a
withParentDeathWatcher registry action = do
  initialParent <- getParentProcessID
  let CPid rawParent = initialParent
  if rawParent <= 1
    then action
    else bracket (async (watch initialParent)) cancel (const action)
  where
    watch parentPid = loop
      where
        loop = do
          threadDelay parentCheckIntervalMicros
          currentParent <- getParentProcessID
          if currentParent == parentPid
            then loop
            else do
              Log.logWarn "Detected parent process exit; shutting down cabal registry"
              shutdownProcessRegistry registry
              exitImmediately (ExitFailure 130)

-- | Run Cabal MCP server using the SDK.
runCabalServer :: CabalServerConfig -> IO ()
runCabalServer config = do
  Log.logInfo "Starting Cabal MCP server with SDK integration"
  let shutdownConfig =
        defaultShutdownConfig
          { shutdownGracePeriod = 30
          , shutdownForceDelay = 10
          , shutdownLogOutput = True
          , shutdownExitCode = ExitSuccess
          }
  result <- try @MCPError $
    withCabalServerState config $ \cabalState -> do
      withParentDeathWatcher (cabalRegistry cabalState) $
        withGracefulShutdown shutdownConfig (Just $ cabalRegistry cabalState) $ do
        transport <- liftIO $ createStdioTransport stdin stdout
        let sdkConfig =
              Server.defaultServerConfig
                { serverInstructions = Just (instructionsMessage config)
                }
        serverResult <-
          liftIO $
            Server.finalizeServer $
              Server.withServerConfig sdkConfig $
                Server.withServerCapabilities createServerCapabilities $
                  Server.withServerInfo (serverName config) (serverVersion config) $
                    Server.withServerTransport (wrapTransport transport) $
                      Server.buildServer
        case serverResult of
          Left err -> throw err
          Right serverEnv -> do
            liftIO $ runServerM serverEnv $ registerCabalTools cabalState
            liftIO $ Server.runServer serverEnv
  case result of
    Left mcpError -> do
      Log.logError $ "Cabal MCP server failed: " <> T.pack (show mcpError)
      Log.logError $ "Cabal MCP server error: " <> T.pack (show mcpError)
    Right _ ->
      Log.logInfo "Cabal MCP server completed successfully"

-- | Create server capabilities.
createServerCapabilities :: ServerCapabilities
createServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability Nothing
      , scbResources = Nothing
      , scbPrompts = Nothing
      , scbLogging = Nothing
      , scbExperimental = Nothing
      }

-- | Register all Cabal tools with the server.
registerCabalTools :: CabalServerState -> ServerM ()
registerCabalTools cabalState = do
  liftIO $ Log.logInfo "Registering Cabal tools"

  let startTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "cabal-test-start"
                , toolDescription =
                    Just
                      "Start a new cabal test process (default uses an isolated --builddir under $TMPDIR/mcp-cache/cabal-test/ or MCP_CACHE_DIR)"
                , toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("URI of the cabal file or project directory" :: Text)
                                  ]
                            , "work_dir"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("Working directory for the project" :: Text)
                                  ]
                            , "target"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("Optional cabal test target (e.g. test:unit)" :: Text)
                                  ]
                            , "options"
                                .= object
                                  [ "type" .= ("object" :: Text)
                                  , "description" .= ("Additional cabal test options" :: Text)
                                  ]
                            ]
                      , "required" .= (["cabal_uri", "work_dir"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty
                }
          , tdHandler = Just $ handleStartCabalTest cabalState
          }
  registerTool "cabal-test-start" startTool

  let stopTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "cabal-test-stop"
                , toolDescription = Just "Stop a running cabal test process"
                , toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("URI of the cabal file or project to stop" :: Text)
                                  ]
                            , "force"
                                .= object
                                  [ "type" .= ("boolean" :: Text)
                                  , "description" .= ("Force stop the process" :: Text)
                                  ]
                            ]
                      , "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty
                }
          , tdHandler = Just $ handleStopCabalTest cabalState
          }
  registerTool "cabal-test-stop" stopTool

  let statusTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "cabal-test-status"
                , toolDescription = Just "Get status of a cabal test process"
                , toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("URI of the cabal file or project to check" :: Text)
                                  ]
                            ]
                      , "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty
                }
          , tdHandler = Just $ handleStatusCabalTest cabalState
          }
  registerTool "cabal-test-status" statusTool

  let messagesTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "cabal-test-messages"
                , toolDescription = Just "Get output from a cabal test process"
                , toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text)
                                  , "description" .= ("URI of the cabal file or project" :: Text)
                                  ]
                            , "filter"
                                .= object
                                  [ "type" .= ("object" :: Text)
                                  , "description" .= ("Filter to apply to messages" :: Text)
                                  ]
                            , "count"
                                .= object
                                  [ "type" .= ("integer" :: Text)
                                  , "description" .= ("Maximum number of messages to return" :: Text)
                                  ]
                            ]
                      , "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty
                }
          , tdHandler = Just $ handleMessagesCabalTest cabalState
          }
  registerTool "cabal-test-messages" messagesTool

  let listTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "cabal-test-list"
                , toolDescription = Just "List all known cabal test processes"
                , toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text)
                      , "properties"
                          .= object
                            [ "include_status"
                                .= object
                                  [ "type" .= ("boolean" :: Text)
                                  , "description" .= ("Include status information for each process" :: Text)
                                  ]
                            ]
                      ] of
                      Object obj -> obj
                      _ -> KM.empty
                }
          , tdHandler = Just $ handleListCabalTest cabalState
          }
  registerTool "cabal-test-list" listTool

  liftIO $ Log.logInfo "All Cabal tools registered successfully"

handleStartCabalTest :: CabalServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStartCabalTest cabalState _ctx args = do
  liftIO $ Log.logInfo "Handling cabal-test-start request"
  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for cabal-test-start"
      Just argsObj -> return $ ToolsCallRequest "cabal-test-start" (Just argsObj)
    executeCabalTool (cabalRegistry cabalState) toolsCallReq
  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "cabal-test-start failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "cabal-test-start completed successfully"
      return $ Right response

handleStopCabalTest :: CabalServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStopCabalTest cabalState _ctx args = do
  liftIO $ Log.logInfo "Handling cabal-test-stop request"
  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for cabal-test-stop"
      Just argsObj -> return $ ToolsCallRequest "cabal-test-stop" (Just argsObj)
    executeCabalTool (cabalRegistry cabalState) toolsCallReq
  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "cabal-test-stop failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "cabal-test-stop completed successfully"
      return $ Right response

handleStatusCabalTest :: CabalServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStatusCabalTest cabalState _ctx args = do
  liftIO $ Log.logInfo "Handling cabal-test-status request"
  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for cabal-test-status"
      Just argsObj -> return $ ToolsCallRequest "cabal-test-status" (Just argsObj)
    executeCabalTool (cabalRegistry cabalState) toolsCallReq
  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "cabal-test-status failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "cabal-test-status completed successfully"
      return $ Right response

handleMessagesCabalTest :: CabalServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleMessagesCabalTest cabalState _ctx args = do
  liftIO $ Log.logInfo "Handling cabal-test-messages request"
  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for cabal-test-messages"
      Just argsObj -> return $ ToolsCallRequest "cabal-test-messages" (Just argsObj)
    executeCabalTool (cabalRegistry cabalState) toolsCallReq
  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "cabal-test-messages failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "cabal-test-messages completed successfully"
      return $ Right response

handleListCabalTest :: CabalServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleListCabalTest cabalState _ctx args = do
  liftIO $ Log.logInfo "Handling cabal-test-list request"
  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> return $ ToolsCallRequest "cabal-test-list" Nothing
      Just argsObj -> return $ ToolsCallRequest "cabal-test-list" (Just argsObj)
    executeCabalTool (cabalRegistry cabalState) toolsCallReq
  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "cabal-test-list failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "cabal-test-list completed successfully"
      return $ Right response
