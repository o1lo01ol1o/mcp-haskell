{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Server.Obelisk
  ( runObeliskServer
  ) where

import Control.Exception (throw, try)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad.Reader (ask)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import MCP.SDK.Capabilities
import MCP.SDK.Error
import qualified MCP.SDK.Server as Server
import MCP.SDK.Server.Monad (ServerM, runServerM, ServerEnv(..), ServerConfig(..))
import MCP.SDK.Transport (wrapTransport)
import MCP.SDK.Transport.Stdio (createStdioTransport)
import MCP.SDK.Types
import MCP.Tools.Obelisk (executeObeliskTool)
import qualified Obelisk.Config as Config
import Obelisk.ProcessRegistry
import System.IO (stdin, stdout, stderr)
import qualified Data.Text.IO as TIO
import MCP.SDK.Server.State (scTools, RegisteredTool(..))
import qualified Data.Map.Strict as Map

-- | Internal server state holding the process registry.
data ObeliskServerState = ObeliskServerState
  { obRegistry :: ProcessRegistry
  }

runObeliskServer :: Config.ObeliskServerConfig -> IO ()
runObeliskServer config = do
  TIO.hPutStrLn stderr "Starting Obelisk MCP server"

  result <- try @MCPError $ do
    registry <- liftIO createRegistry
    let state = ObeliskServerState registry

    transport <- liftIO $ createStdioTransport stdin stdout

    let serverConfig =
          Server.defaultServerConfig
            { serverInstructions = Just (Config.serverInstructions config)
            }

    serverEnvResult <-
      liftIO $
        Server.finalizeServer $
          Server.withServerConfig serverConfig $
            Server.withServerCapabilities obCapabilities $
              Server.withServerInfo (Config.serverName config) (Config.serverVersion config) $
                Server.withServerTransport (wrapTransport transport) $
                  Server.buildServer

    serverEnv <- case serverEnvResult of
      Left err -> throw err
      Right env -> pure env

    liftIO $ runServerM serverEnv $ registerObeliskTools state
    liftIO $ Server.runServer serverEnv

  case result of
    Left err -> TIO.hPutStrLn stderr $ "Obelisk MCP server failed: " <> T.pack (show err)
    Right _ -> TIO.hPutStrLn stderr "Obelisk MCP server exited"

registerToolSilently :: Text -> ToolDefinition ServerM -> ServerM ()
registerToolSilently name def = do
  env <- ask
  let ctx = serverContext env
      registered = RegisteredTool { rtDefinition = def, rtEnabled = True }
  liftIO $ atomically $ modifyTVar' (scTools ctx) (Map.insert name registered)

obCapabilities :: Capabilities
obCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability Nothing
      , scbResources = Nothing
      , scbPrompts = Nothing
      , scbLogging = Nothing
      , scbExperimental = Nothing
      }

registerObeliskTools :: ObeliskServerState -> ServerM ()
registerObeliskTools state = do
  let startTool = ToolDefinition
        { tdTool = Tool
            { toolNameField = "obelisk.start"
            , toolDescription = Just "Start ob watch for an Obelisk project"
            , toolInputSchema = startSchema
            }
        , tdHandler = Just $ callHandler state "obelisk.start"
        }

      stopTool = ToolDefinition
        { tdTool = Tool
            { toolNameField = "obelisk.stop"
            , toolDescription = Just "Stop ob watch for an Obelisk project"
            , toolInputSchema = startSchema
            }
        , tdHandler = Just $ callHandler state "obelisk.stop"
        }

      statusTool = ToolDefinition
        { tdTool = Tool
            { toolNameField = "obelisk.status"
            , toolDescription = Just "Get status for an ob watch process"
            , toolInputSchema = startSchema
            }
        , tdHandler = Just $ callHandler state "obelisk.status"
        }

      messagesTool = ToolDefinition
        { tdTool = Tool
            { toolNameField = "obelisk.messages"
            , toolDescription = Just "Fetch recent output from ob watch"
            , toolInputSchema = messagesSchema
            }
        , tdHandler = Just $ callHandler state "obelisk.messages"
        }

      listTool = ToolDefinition
        { tdTool = Tool
            { toolNameField = "obelisk.list"
            , toolDescription = Just "List active ob watch processes"
            , toolInputSchema = listSchema
            }
        , tdHandler = Just $ callHandler state "obelisk.list"
        }

  mapM_ (uncurry registerToolSilently)
    [ ("obelisk.start", startTool)
    , ("obelisk.stop", stopTool)
    , ("obelisk.status", statusTool)
    , ("obelisk.messages", messagesTool)
    , ("obelisk.list", listTool)
    ]

startSchema :: Object
startSchema = case object
  [ "type" .= ("object" :: Text)
  , "properties" .= object
      [ "projectPath" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Absolute path to the Obelisk project" :: Text)
          ]
      ]
  , "required" .= (["projectPath"] :: [Text])
  ] of
  Object o -> o
  _ -> KM.empty

listSchema :: Object
listSchema = case object
  [ "type" .= ("object" :: Text)
  , "properties" .= object []
  ] of
  Object o -> o
  _ -> KM.empty

messagesSchema :: Object
messagesSchema = case object
  [ "type" .= ("object" :: Text)
  , "properties" .= object
      [ "projectPath" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Absolute path to the Obelisk project" :: Text)
          ]
      , "limit" .= object
          [ "type" .= ("integer" :: Text)
          , "description" .= ("Maximum number of lines to return" :: Text)
          ]
      ]
  , "required" .= (["projectPath"] :: [Text])
  ] of
  Object o -> o
  _ -> KM.empty

callHandler :: ObeliskServerState -> Text -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
callHandler state toolName _ctx args = do
  liftIO $ TIO.hPutStrLn stderr $ "Handling obelisk tool: " <> toolName
  result <- liftIO $ try @MCPError $ do
    reqArgs <- case args of
      Nothing -> throw $ ValidationError $ "Missing arguments for " <> toolName
      Just obj -> pure $ ToolsCallRequest toolName (Just obj)
    executeObeliskTool (obRegistry state) reqArgs
  pure $ either Left Right result
