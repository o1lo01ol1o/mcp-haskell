{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Server.GHCID
  ( runGHCIDServer,
    GHCIDServerState (..),
    initializeGHCIDServerState,
  )
where

import Control.Exception (throw, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
-- MCP SDK imports

-- Internal imports
import GHCID.Config (GHCIDServerConfig (..))
import GHCID.ProcessRegistry (ProcessRegistry, createProcessRegistry)
import MCP.SDK.Capabilities (ServerCapabilityBuilder (..), ToolsCapability (..), buildServerCapabilities)
import MCP.SDK.Error (MCPError (..))
import MCP.SDK.Server
import MCP.SDK.Server.API (registerTool)
import MCP.SDK.Server.Monad (ServerM, runServerM)
import MCP.SDK.Transport (wrapTransport)
import MCP.SDK.Transport.Stdio (createStdioTransport)
import MCP.SDK.Types hiding (serverName, serverVersion)
import MCP.Tools.GHCID (executeGHCIDTool)
import System.IO (stdin, stdout)
import qualified Utils.Logging as Log

-- | GHCID Server State
data GHCIDServerState = GHCIDServerState
  { ghcidRegistry :: ProcessRegistry,
    serverConfig :: GHCIDServerConfig
  }

-- | Initialize GHCID server state
initializeGHCIDServerState :: GHCIDServerConfig -> IO GHCIDServerState
initializeGHCIDServerState config = do
  registry <- createProcessRegistry
  return
    GHCIDServerState
      { ghcidRegistry = registry,
        serverConfig = config
      }

-- | Run GHCID MCP server using the SDK
runGHCIDServer :: GHCIDServerConfig -> IO ()
runGHCIDServer config = do
  Log.logInfo "Starting GHCID MCP server with SDK integration"

  result <- try @MCPError $ do
    -- Initialize server state
    ghcidState <- liftIO $ initializeGHCIDServerState config

    -- Create stdio transport
    transport <- liftIO $ createStdioTransport stdin stdout

    -- Build server with SDK
    serverResult <-
      liftIO $
        finalizeServer $
          withServerConfig defaultServerConfig $
            withServerCapabilities createServerCapabilities $
              withServerInfo (serverName config) (serverVersion config) $
                withServerTransport (wrapTransport transport) $
                  buildServer

    case serverResult of
      Left err -> throw err
      Right serverEnv -> do
        -- Register GHCID tools
        liftIO $ runServerM serverEnv $ registerGHCIDTools ghcidState

        -- Run the server
        liftIO $ runServer serverEnv
  case result of
    Left mcpError -> do
      Log.logError $ "GHCID MCP server failed: " <> T.pack (show mcpError)
      Log.logError $ "GHCID MCP server error: " <> T.pack (show mcpError)
    Right _ ->
      Log.logInfo "GHCID MCP server completed successfully"

-- | Create server capabilities
createServerCapabilities :: Capabilities
createServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability Nothing,
        scbResources = Nothing,
        scbPrompts = Nothing,
        scbLogging = Nothing, -- We handle our own logging
        scbExperimental = Nothing
      }

-- | Register all GHCID tools with the server
registerGHCIDTools :: GHCIDServerState -> ServerM ()
registerGHCIDTools ghcidState = do
  liftIO $ Log.logInfo "Registering GHCID tools"

  -- Register start GHCID tool
  let startTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.start",
                  toolDescription = Just "Start a new GHCID process for a Haskell project",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("URI of the cabal file or project directory" :: Text)
                                  ],
                              "work_dir"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("Working directory for the GHCID process" :: Text)
                                  ],
                              "options"
                                .= object
                                  [ "type" .= ("object" :: Text),
                                    "description" .= ("Additional GHCID options" :: Text)
                                  ]
                            ],
                        "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleStartGHCID ghcidState
          }

  registerTool "ghcid.start" startTool

  -- Register stop GHCID tool
  let stopTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.stop",
                  toolDescription = Just "Stop a running GHCID process",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("URI of the cabal file or project to stop" :: Text)
                                  ],
                              "force"
                                .= object
                                  [ "type" .= ("boolean" :: Text),
                                    "description" .= ("Force stop the process" :: Text)
                                  ]
                            ],
                        "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleStopGHCID ghcidState
          }

  registerTool "ghcid.stop" stopTool

  -- Register restart GHCID tool
  let restartTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.restart",
                  toolDescription = Just "Restart a GHCID process",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("URI of the cabal file or project to restart" :: Text)
                                  ],
                              "work_dir"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("New working directory (optional)" :: Text)
                                  ]
                            ],
                        "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleRestartGHCID ghcidState
          }

  registerTool "ghcid.restart" restartTool

  -- Register status GHCID tool
  let statusTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.status",
                  toolDescription = Just "Get status of a GHCID process",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("URI of the cabal file or project to check" :: Text)
                                  ]
                            ],
                        "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleStatusGHCID ghcidState
          }

  registerTool "ghcid.status" statusTool

  -- Register messages GHCID tool
  let messagesTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.messages",
                  toolDescription = Just "Get compilation messages from a GHCID process",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "cabal_uri"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("URI of the cabal file or project" :: Text)
                                  ],
                              "filter"
                                .= object
                                  [ "type" .= ("object" :: Text),
                                    "description" .= ("Filter to apply to messages" :: Text)
                                  ],
                              "count"
                                .= object
                                  [ "type" .= ("integer" :: Text),
                                    "description" .= ("Maximum number of messages to return" :: Text)
                                  ]
                            ],
                        "required" .= (["cabal_uri"] :: [Text])
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleMessagesGHCID ghcidState
          }

  registerTool "ghcid.messages" messagesTool

  -- Register list GHCID tool
  let listTool =
        ToolDefinition
          { tdTool =
              Tool
                { toolNameField = "ghcid.list",
                  toolDescription = Just "List all running GHCID processes",
                  toolInputSchema =
                    case object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "include_status"
                                .= object
                                  [ "type" .= ("boolean" :: Text),
                                    "description" .= ("Include status information for each process" :: Text)
                                  ]
                            ]
                      ] of
                      Object obj -> obj
                      _ -> KM.empty -- Fallback to empty schema if object construction fails
                },
            tdHandler = Just $ handleListGHCID ghcidState
          }

  registerTool "ghcid.list" listTool

  liftIO $ Log.logInfo "All GHCID tools registered successfully"

-- | Handle start GHCID tool calls
handleStartGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStartGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.start request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for ghcid.start"
      Just argsObj -> return $ ToolsCallRequest "ghcid.start" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.start failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.start completed successfully"
      return $ Right response

-- | Handle stop GHCID tool calls
handleStopGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStopGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.stop request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for ghcid.stop"
      Just argsObj -> return $ ToolsCallRequest "ghcid.stop" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.stop failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.stop completed successfully"
      return $ Right response

-- | Handle restart GHCID tool calls
handleRestartGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleRestartGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.restart request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for ghcid.restart"
      Just argsObj -> return $ ToolsCallRequest "ghcid.restart" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.restart failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.restart completed successfully"
      return $ Right response

-- | Handle status GHCID tool calls
handleStatusGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStatusGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.status request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for ghcid.status"
      Just argsObj -> return $ ToolsCallRequest "ghcid.status" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.status failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.status completed successfully"
      return $ Right response

-- | Handle messages GHCID tool calls
handleMessagesGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleMessagesGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.messages request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> throw $ ValidationError "Missing arguments for ghcid.messages"
      Just argsObj -> return $ ToolsCallRequest "ghcid.messages" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.messages failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.messages completed successfully"
      return $ Right response

-- | Handle list GHCID tool calls
handleListGHCID :: GHCIDServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleListGHCID ghcidState _ctx args = do
  liftIO $ Log.logInfo "Handling ghcid.list request"

  result <- liftIO $ try @MCPError $ do
    toolsCallReq <- case args of
      Nothing -> return $ ToolsCallRequest "ghcid.list" Nothing
      Just argsObj -> return $ ToolsCallRequest "ghcid.list" (Just argsObj)

    executeGHCIDTool (ghcidRegistry ghcidState) toolsCallReq

  case result of
    Left mcpError -> do
      liftIO $ Log.logError $ "ghcid.list failed: " <> T.pack (show mcpError)
      return $ Left mcpError
    Right response -> do
      liftIO $ Log.logInfo "ghcid.list completed successfully"
      return $ Right response
