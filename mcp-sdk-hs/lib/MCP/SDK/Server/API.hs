{-# LANGUAGE OverloadedStrings #-}

module MCP.SDK.Server.API
  ( registerTool,
    removeTool,
    registerPrompt,
    removePrompt,
    registerResource,
    removeResource,
    sendToolListChanged,
    sendPromptListChanged,
    sendResourceListChanged,
    sendListChanged,
    createMessage,
    elicitInput
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', newEmptyTMVarIO, readTVarIO, takeTMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Result (..), fromJSON, object, toJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import MCP.SDK.Error (MCPError (..))
import MCP.SDK.Protocol
  ( JSONRPCMessage (..),
    JSONRPCNotificationMessage (..),
    JSONRPCRequestMessage (..),
  )
import MCP.SDK.Server.Monad
  ( ServerEnv (..),
    ServerM,
  )
import MCP.SDK.Server.State
  ( RegisteredPrompt (..),
    RegisteredResource (..),
    RegisteredTool (..),
    scPrompts,
    scResources,
    scTools,
  )
import MCP.SDK.Transport (sendMessage)
import MCP.SDK.Types
  ( ClientCapabilities (..),
    CreateMessageRequest,
    CreateMessageResponse,
    ElicitRequest,
    ElicitResponse,
    PromptDefinition,
    RequestId (..),
    Resource,
    ToolDefinition,
  )
import System.Timeout (timeout)

-- | Registers a new tool with the server.
-- If a tool with the same name already exists, it will be overwritten.
-- Sends a `tools/listChanged` notification to clients that support it.
registerTool :: Text -> ToolDefinition ServerM -> ServerM ()
registerTool name def = do
  ctx <- fmap serverContext ask
  let registeredTool =
        RegisteredTool
          { rtDefinition = def,
            rtEnabled = True -- Tools are enabled by default
          }
  liftIO $ atomically $ modifyTVar' (scTools ctx) (Map.insert name registeredTool)
  sendToolListChanged

-- | Removes a tool from the server.
-- Sends a `tools/listChanged` notification to clients that support it.
removeTool :: Text -> ServerM ()
removeTool name = do
  ctx <- fmap serverContext ask
  liftIO $ atomically $ modifyTVar' (scTools ctx) (Map.delete name)
  sendToolListChanged

-- | Registers a new prompt with the server.
registerPrompt :: Text -> PromptDefinition ServerM -> ServerM ()
registerPrompt name def = do
  ctx <- fmap serverContext ask
  let registeredPrompt =
        RegisteredPrompt
          { rpDefinition = def,
            rpEnabled = True
          }
  liftIO $ atomically $ modifyTVar' (scPrompts ctx) (Map.insert name registeredPrompt)
  sendPromptListChanged

-- | Removes a prompt from the server.
removePrompt :: Text -> ServerM ()
removePrompt name = do
  ctx <- fmap serverContext ask
  liftIO $ atomically $ modifyTVar' (scPrompts ctx) (Map.delete name)
  sendPromptListChanged

-- | Registers a new resource with the server.
registerResource :: Text -> Resource -> ServerM ()
registerResource uri resource = do
  ctx <- fmap serverContext ask
  let registeredResource =
        RegisteredResource
          { rrResource = resource,
            rrEnabled = True
          }
  liftIO $ atomically $ modifyTVar' (scResources ctx) (Map.insert uri registeredResource)
  sendResourceListChanged

-- | Removes a resource from the server.
removeResource :: Text -> ServerM ()
removeResource uri = do
  ctx <- fmap serverContext ask
  liftIO $ atomically $ modifyTVar' (scResources ctx) (Map.delete uri)
  sendResourceListChanged

-- Internal helper functions to send notifications

sendToolListChanged :: ServerM ()
sendToolListChanged = sendListChanged "tools/listChanged"

sendPromptListChanged :: ServerM ()
sendPromptListChanged = sendListChanged "prompts/listChanged"

sendResourceListChanged :: ServerM ()
sendResourceListChanged = sendListChanged "resources/listChanged"

-- Generic notification sender
-- TODO: Check client capabilities to see if they support listChanged notifications.
sendListChanged :: Text -> ServerM ()
sendListChanged method = do
  env <- ask
  -- Use case expression to extract transport from GADT
  case env of
    ServerEnv transport _ _ _ _ _ _ _ _ -> do
      let notification = JSONRPCNotification $ JSONRPCNotificationMessage method (object [])
      _ <- liftIO $ sendMessage transport notification
      return ()

-- | Sends a `sampling/createMessage` request to the client and awaits a response.
createMessage :: CreateMessageRequest -> ServerM (Either MCPError CreateMessageResponse)
createMessage req = do
  env <- ask
  mClientCaps <- liftIO $ readTVarIO (clientCapabilities env)
  -- 1. Check if client supports sampling
  case mClientCaps of
    Nothing -> return $ Left $ ProtocolError "Client capabilities not yet known."
    Just caps ->
      case clientSampling caps of
        Nothing -> return $ Left $ InvalidCapability "Client does not support sampling/createMessage"
        Just _ -> do
          -- 2. Generate a new request ID
          reqIdUUID <- liftIO nextRandom
          let requestIdValue = RequestIdText (toText reqIdUUID)
          -- 3. Create a TMVar to wait for the response
          responseVar <- liftIO newEmptyTMVarIO
          liftIO $ atomically $ modifyTVar' (serverPendingRequests env) (Map.insert requestIdValue responseVar)
          -- 4. Send the request
          let jsonRpcReq = JSONRPCRequest (JSONRPCRequestMessage requestIdValue "sampling/createMessage" (toJSON req))
          case env of
            ServerEnv transport _ _ _ _ _ _ _ _ -> do
              _ <- liftIO $ sendMessage transport jsonRpcReq
              -- 5. Wait for the response with a timeout (30 seconds default)
              let timeoutMicros = 30000000
              result <- liftIO $ timeout timeoutMicros $ atomically $ takeTMVar responseVar
              -- 6. Clean up and process result
              liftIO $ atomically $ modifyTVar' (serverPendingRequests env) (Map.delete requestIdValue)
              case result of
                Nothing -> return $ Left $ RequestTimeout "sampling/createMessage"
                Just (Left err) -> return $ Left err
                Just (Right value) ->
                  case fromJSON value of
                    Success resp -> return $ Right resp
                    Error msg -> return $ Left $ ParseError $ "Failed to parse CreateMessage-Response: " <> T.pack msg

-- | Sends an `elicitation/create` request to the client and awaits a response.
elicitInput :: ElicitRequest -> ServerM (Either MCPError ElicitResponse)
elicitInput req = do
  env <- ask
  -- NOTE: The specification does not define a specific capability for elicitation.
  -- We assume that if a client supports tools, it should support elicitation.
  -- 1. Generate a new request ID
  reqIdUUID <- liftIO nextRandom
  let requestIdValue = RequestIdText (toText reqIdUUID)
  -- 2. Create a TMVar to wait for the response
  responseVar <- liftIO newEmptyTMVarIO
  liftIO $ atomically $ modifyTVar' (serverPendingRequests env) (Map.insert requestIdValue responseVar)
  -- 3. Send the request
  let jsonRpcReq = JSONRPCRequest (JSONRPCRequestMessage requestIdValue "elicitation/create" (toJSON req))
  case env of
    ServerEnv transport _ _ _ _ _ _ _ _ -> do
      _ <- liftIO $ sendMessage transport jsonRpcReq
      -- 4. Wait for the response with a timeout (30 seconds default)
      let timeoutMicros = 30000000
      result <- liftIO $ timeout timeoutMicros $ atomically $ takeTMVar responseVar
      -- 5. Clean up and process result
      liftIO $ atomically $ modifyTVar' (serverPendingRequests env) (Map.delete requestIdValue)
      case result of
        Nothing -> return $ Left $ RequestTimeout "elicitation/create"
        Just (Left err) -> return $ Left err
        Just (Right value) ->
          case fromJSON value of
            Success resp -> return $ Right resp
            Error msg -> return $ Left $ ParseError $ "Failed to parse ElicitResponse: " <> T.pack msg
