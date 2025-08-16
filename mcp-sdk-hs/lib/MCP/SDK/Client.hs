{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.SDK.Client where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Result (..), ToJSON, Value, fromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import MCP.SDK.Error
import MCP.SDK.Protocol
import MCP.SDK.Request
import MCP.SDK.Transport
import MCP.SDK.Types

-- | MCP Client state
data MCPClient t = MCPClient
  { clientTransport :: t,
    clientState :: TVar ClientState,
    clientCapabilities :: Capabilities,
    clientInfo :: ClientInfo
  }

data ClientState
  = Uninitialized
  | Initializing
  | Ready ServerInfo Capabilities
  | Closed
  deriving (Eq, Show)

-- | Create a new MCP client
newMCPClient :: (Transport t) => t -> ClientInfo -> Capabilities -> IO (MCPClient t)
newMCPClient transport info caps = do
  state <- newTVarIO Uninitialized
  return
    MCPClient
      { clientTransport = transport,
        clientState = state,
        clientCapabilities = caps,
        clientInfo = info
      }

instance (Transport t) => RequestHandler (MCPClient t) where
  executeRequest client ctx request = liftIO $ do
    state <- readTVarIO (clientState client)
    case state of
      Closed -> return $ Left (TransportError "Client is closed")
      Uninitialized -> return $ Left (ProtocolError "Client not initialized")
      Initializing -> return $ Left (ProtocolError "Client is initializing")
      Ready _ _ -> sendRequestInternal client ctx request

-- | Initialize the client with the server
initializeClient :: (Transport t) => MCPClient t -> IO (Either MCPError (ServerInfo, Capabilities))
initializeClient client = do
  currentState <- readTVarIO (clientState client)
  case currentState of
    Ready serverInfo caps -> return $ Right (serverInfo, caps)
    Closed -> return $ Left (TransportError "Client is closed")
    _ -> do
      atomically $ writeTVar (clientState client) Initializing

      let initReq = InitializeRequest "2024-11-05" (clientCapabilities client) (clientInfo client)
      result <- case makeInitializeRequest "2024-11-05" (clientCapabilities client) (clientInfo client) of
        Left err -> return $ Left err
        Right req -> sendRequestInternal client defaultContext req

      case result of
        Left err -> do
          atomically $ writeTVar (clientState client) Uninitialized
          return $ Left err
        Right resp -> do
          atomically $ writeTVar (clientState client) (Ready (respServerInfo resp) (respCapabilities resp))
          return $ Right (respServerInfo resp, respCapabilities resp)

-- | Close the client
closeClient :: (Transport t) => MCPClient t -> IO ()
closeClient client = do
  atomically $ writeTVar (clientState client) Closed
  closeTransport (clientTransport client)

-- | Check if client is ready
isClientReady :: MCPClient t -> IO Bool
isClientReady client = do
  state <- readTVarIO (clientState client)
  return $ case state of
    Ready _ _ -> True
    _ -> False

-- | Send request with full type safety
sendRequestInternal ::
  forall t m.
  (Transport t, ToJSON (RequestType m)) =>
  MCPClient t ->
  RequestContext ->
  MCPRequest m ->
  IO (Either MCPError (ResponseType m))
sendRequestInternal client ctx request = do
  -- Generate request ID
  reqId <- generateUniqueRequestId

  -- Encode request to JSON-RPC
  let jsonRpcReq = encodeRequest reqId request
  let jsonRpcMsg = JSONRPCRequest jsonRpcReq

  -- Send request through transport
  sendResult <- sendMessage (clientTransport client) jsonRpcMsg
  case sendResult of
    Left err -> return $ Left err
    Right () -> do
      -- Wait for response (simplified - real implementation would use pending requests)
      responseResult <- receiveMessage (clientTransport client)
      case responseResult of
        Left err -> return $ Left err
        Right (JSONRPCResponse resp) -> do
          -- Decode response based on request type
          return $ decodeRequestResponse request resp
        Right _ -> return $ Left (ProtocolError "Expected response, got different message type")

-- | Type-safe response decoder that preserves GADT type information
decodeRequestResponse :: MCPRequest m -> JSONRPCResponseMessage -> Either MCPError (ResponseType m)
decodeRequestResponse req resp = case req of
  InitializeReq _ -> decodeResponse @'Initialize Initialize resp
  ToolsListReq _ -> decodeResponse @'ToolsList ToolsList resp
  ToolsCallReq _ -> decodeResponse @'ToolsCall ToolsCall resp
  ResourcesListReq _ -> decodeResponse @'ResourcesList ResourcesList resp
  ResourcesReadReq _ -> decodeResponse @'ResourcesRead ResourcesRead resp
  PromptsGetReq _ -> decodeResponse @'PromptsGet PromptsGet resp
  PromptsListReq _ -> decodeResponse @'PromptsList PromptsList resp
  PingReq _ -> decodeResponse @'Ping Ping resp

-- | Helper to extract method from GADT request
getMethodFromRequest :: MCPRequest m -> Method
getMethodFromRequest (InitializeReq _) = Initialize
getMethodFromRequest (ToolsListReq _) = ToolsList
getMethodFromRequest (ToolsCallReq _) = ToolsCall
getMethodFromRequest (ResourcesListReq _) = ResourcesList
getMethodFromRequest (ResourcesReadReq _) = ResourcesRead
getMethodFromRequest (PromptsGetReq _) = PromptsGet
getMethodFromRequest (PromptsListReq _) = PromptsList
getMethodFromRequest (PingReq _) = Ping

-- | Generate unique request ID
generateUniqueRequestId :: IO RequestId
generateUniqueRequestId = do
  uuid <- UUID.nextRandom
  return $ RequestId $ T.pack $ UUID.toString uuid

-- | High-level client API functions

-- | List available tools
listTools :: (Transport t) => MCPClient t -> Maybe Text -> IO (Either MCPError ToolsListResponse)
listTools client cursor = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else executeRequest client defaultContext (makeToolsListRequest cursor)

-- | Call a tool
callTool :: (Transport t) => MCPClient t -> Text -> Maybe Value -> IO (Either MCPError ToolsCallResponse)
callTool client toolName args = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else case makeToolsCallRequest toolName (fmap toObject args) of
      Left err -> return $ Left err
      Right req -> executeRequest client defaultContext req
  where
    toObject val = case fromJSON val of
      Success obj -> obj
      Error _ -> mempty

-- | List available resources
listResources :: (Transport t) => MCPClient t -> Maybe Text -> IO (Either MCPError ResourcesListResponse)
listResources client cursor = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else executeRequest client defaultContext (makeResourcesListRequest cursor)

-- | Read a resource
readResource :: (Transport t) => MCPClient t -> Text -> IO (Either MCPError ResourcesReadResponse)
readResource client uri = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else case makeResourcesReadRequest uri of
      Left err -> return $ Left err
      Right req -> executeRequest client defaultContext req

-- | List available prompts
listPrompts :: (Transport t) => MCPClient t -> Maybe Text -> IO (Either MCPError PromptsListResponse)
listPrompts client cursor = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else executeRequest client defaultContext (makePromptsListRequest cursor)

-- | Get a prompt
getPrompt :: (Transport t) => MCPClient t -> Text -> Maybe Value -> IO (Either MCPError PromptsGetResponse)
getPrompt client promptName args = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else case makePromptsGetRequest promptName (fmap toObject args) of
      Left err -> return $ Left err
      Right req -> executeRequest client defaultContext req
  where
    toObject val = case fromJSON val of
      Success obj -> obj
      Error _ -> mempty

-- | Ping the server
pingServer :: (Transport t) => MCPClient t -> IO (Either MCPError PingResponse)
pingServer client = do
  ready <- isClientReady client
  if not ready
    then return $ Left (ProtocolError "Client not initialized")
    else executeRequest client defaultContext makePingRequest

-- | Client builder pattern for easy configuration
data ClientBuilder t = ClientBuilder
  { cbTransport :: Maybe t,
    cbClientInfo :: Maybe ClientInfo,
    cbCapabilities :: Maybe Capabilities,
    cbTimeout :: Maybe Int
  }

-- | Start building a client
buildClient :: ClientBuilder t
buildClient = ClientBuilder Nothing Nothing Nothing Nothing

-- | Set transport for client
withTransport :: t -> ClientBuilder t -> ClientBuilder t
withTransport transport builder = builder {cbTransport = Just transport}

-- | Set client info
withClientInfo :: Text -> Text -> ClientBuilder t -> ClientBuilder t
withClientInfo name version builder =
  builder {cbClientInfo = Just (ClientInfo name version)}

-- | Set capabilities
withCapabilities :: Capabilities -> ClientBuilder t -> ClientBuilder t
withCapabilities caps builder = builder {cbCapabilities = Just caps}

-- | Set timeout
withTimeout :: Int -> ClientBuilder t -> ClientBuilder t
withTimeout timeout builder = builder {cbTimeout = Just timeout}

-- | Finalize client creation
finalizeClient :: (Transport t) => ClientBuilder t -> IO (Either MCPError (MCPClient t))
finalizeClient ClientBuilder {..} = do
  transport <-
    maybe
      (return $ Left $ ValidationError "Transport is required")
      (return . Right)
      cbTransport
  clientInfo <-
    maybe
      (return $ Left $ ValidationError "Client info is required")
      (return . Right)
      cbClientInfo
  let capabilities = maybe defaultCapabilities id cbCapabilities

  case (transport, clientInfo) of
    (Right t, Right info) -> Right <$> newMCPClient t info capabilities
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err

-- | Default capabilities for clients
defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities Nothing Nothing
