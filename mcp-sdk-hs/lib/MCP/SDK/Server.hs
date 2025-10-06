{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.SDK.Server where

import Control.Concurrent.STM (atomically, newTVarIO, putTMVar, readTVar, readTVarIO, writeTVar)
import Control.Exception.Safe (bracket, catch, finally)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN, logWarnN)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.Aeson (Object, Result (..), ToJSON, Value, fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import MCP.SDK.Error
import MCP.SDK.Protocol
import qualified MCP.SDK.Server.API as API
import MCP.SDK.Server.Monad (LogLevel (..), ServerConfig (..), ServerEnv (..), ServerHandlers (..), ServerM, ServerState (..), runServerM)
import qualified MCP.SDK.Server.Monad as Monad
import MCP.SDK.Server.State
import MCP.SDK.Transport (SomeTransport, Transport (..), disconnect)
import qualified MCP.SDK.Transport as Transport
import MCP.SDK.Types hiding (elicitInput)
import MCP.SDK.Types.Auth (AuthInfo)

-- | Create a new MCP server with monad transformer stack
newMCPServer :: (Transport t) => t -> Implementation -> Capabilities -> ServerHandlers -> ServerConfig -> IO (ServerEnv (ServerContext ServerM))
newMCPServer transport info caps handlers config = do
  state <- newTVarIO ServerUninitialized
  context <- newServerContext
  pending <- newTVarIO Map.empty
  clientCaps <- newTVarIO Nothing
  return
    ServerEnv
      { serverTransport = transport,
        serverState = state,
        serverCapabilities = caps,
        serverInfo = info,
        serverHandlers = handlers,
        serverConfig = config,
        serverContext = context,
        serverPendingRequests = pending,
        clientCapabilities = clientCaps
      }

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { configName = "mcp-haskell-server",
      configVersion = "0.1.0",
      serverMaxConnections = 100,
      serverRequestTimeout = 30,
      serverLogLevel = Info,
      serverAuthMiddleware = Nothing,
      serverInstructions = Nothing
    }

-- | Logging helper functions that respect server config
logDebugS :: Text -> ServerM ()
logDebugS msg = do
  config <- asks serverConfig
  when (serverLogLevel config <= Debug) $ logDebugN msg

logInfoS :: Text -> ServerM ()
logInfoS msg = do
  config <- asks serverConfig
  when (serverLogLevel config <= Info) $ logInfoN msg

logWarnS :: Text -> ServerM ()
logWarnS msg = do
  config <- asks serverConfig
  when (serverLogLevel config <= Warn) $ logWarnN msg

logErrorS :: Text -> ServerM ()
logErrorS msg = do
  config <- asks serverConfig
  when (serverLogLevel config <= Monad.Error) $ logErrorN msg

-- | Run the server with proper resource management
runServer :: ServerEnv (ServerContext ServerM) -> IO (Either MCPError ())
runServer env = runServerM env $ do
  logInfoS "Starting MCP server"
  result <- catch (runServerLoop) handleServerError
  logInfoS "MCP server stopped"
  return result
  where
    handleServerError :: MCPError -> ServerM (Either MCPError ())
    handleServerError e = do
      logErrorS $ "Server error: " <> T.pack (show e)
      return (Left e)

-- | Main server event loop in ServerM
runServerLoop :: ServerM (Either MCPError ())
runServerLoop = do
  env <- ask
  liftIO $ atomically $ writeTVar (serverState env) ServerUninitialized
  logInfoS "Server initialized, starting message loop"

  bracket
    initializeServerResources
    cleanupServerResources
    (\_ -> messageLoop)
  where
    initializeServerResources = do
      logDebugS "Initializing server resources"
      return ()

    cleanupServerResources _ = do
      logDebugS "Cleaning up server resources"
      env <- ask
      liftIO $ atomically $ writeTVar (serverState env) ServerClosed

-- | Message processing loop
messageLoop :: ServerM (Either MCPError ())
messageLoop = do
  env <- ask
  case env of
    ServerEnv transport _ _ _ _ _ _ pendingRequests _ -> do
      forever $ do
        logDebugS "Waiting for incoming message"
        -- Receive incoming message
        msgResult <- liftIO $ receiveMessage transport
        case msgResult of
          Left err -> do
            logErrorS $ "Transport receive error: " <> T.pack (show err)
            return ()
          Right msg -> do
            -- Check if it's a response to a server-initiated request
            isResponseToPending <- case msg of
              JSONRPCResponse (JSONRPCResponseMessage respId mResult mError) -> do
                mVar <- liftIO $ atomically $ Map.lookup respId <$> readTVar pendingRequests
                case mVar of
                  Just var -> do
                    logDebugS $ "Received response for server-initiated request: " <> requestIdToText respId
                    let responseVal = case (mResult, mError) of
                          (Just result, _) -> Right result
                          (_, Just err) -> Left $ ProtocolError (errorMessage err)
                          _ -> Left $ ProtocolError "Invalid response from client"
                    liftIO $ atomically $ putTMVar var responseVal
                    return True
                  Nothing -> return False
              _ -> return False
            -- If not, handle it as a regular incoming message
            unless isResponseToPending $
              handleIncomingServerMessage msg

-- | Handle incoming messages in ServerM
handleIncomingServerMessage :: JSONRPCMessage -> ServerM ()
handleIncomingServerMessage msg = do
  env <- ask
  let config = serverConfig env
  case serverAuthMiddleware config of
    Nothing -> processMessage msg Nothing
    Just authMiddleware -> do
      authResult <- authMiddleware msg
      case authResult of
        Left err -> case msg of
          JSONRPCRequest req -> do
            let reqIdText = requestIdToText (reqId req)
            logWarnS $ "Authentication failed for request: " <> reqIdText
            sendResponse (reqId req) (Left err :: Either MCPError Value)
          _ -> logWarnS "Authentication failed for incoming message."
        Right authInfo -> processMessage msg (Just authInfo)
  where
    processMessage m mAuthInfo = case m of
      JSONRPCRequest req -> do
        logDebugS $ "Handling request: " <> reqMethod req
        handleRequest req mAuthInfo
      JSONRPCNotification notif -> do
        logDebugS $ "Handling notification: " <> notifMethod notif
        handleNotification notif mAuthInfo
      JSONRPCResponse _ -> do
        logWarnS "Server received unexpected response"

-- | Handle incoming requests in ServerM
handleRequest :: JSONRPCRequestMessage -> Maybe AuthInfo -> ServerM ()
handleRequest req mAuthInfo = do
  let requestId = reqId req
  let method = reqMethod req
  let params = reqParams req

  logDebugS $ "Processing " <> method <> " request with ID: " <> requestIdToText requestId

  let wrapAndSend :: (ToJSON a) => ServerM (Either MCPError a) -> ServerM ()
      wrapAndSend action = do
        result <- action
        sendResponse requestId result

  case method of
    "initialize" -> wrapAndSend $ handleInitialize params
    "tools/list" -> wrapAndSend $ handleToolsListRequest params
    "tools/call" -> wrapAndSend $ handleToolsCallRequest params mAuthInfo
    "resources/list" -> wrapAndSend $ handleResourcesListRequest params
    "resources/read" -> wrapAndSend $ handleResourcesReadRequest params
    "prompts/list" -> wrapAndSend $ handlePromptsListRequest params
    "prompts/get" -> wrapAndSend $ handlePromptsGetRequest params
    "ping" -> wrapAndSend $ handlePingRequest params
    "completion/complete" -> wrapAndSend $ handleCompleteRequest params
    _ -> do
      logWarnS $ "Unsupported method: " <> method
      sendResponse requestId (Left (UnsupportedMethod method) :: Either MCPError Value)

-- | Send JSON-RPC response
sendResponse :: (ToJSON a) => RequestId -> Either MCPError a -> ServerM ()
sendResponse reqId result = do
  env <- ask
  case env of
    ServerEnv transport _ _ _ _ _ _ _ _ -> do
      let response = case result of
            Left err -> JSONRPCResponse (JSONRPCResponseMessage reqId Nothing (Just $ errorToJSONRPCError err))
            Right val -> JSONRPCResponse (JSONRPCResponseMessage reqId (Just $ toJSON val) Nothing)
      sendResult <- liftIO $ sendMessage transport response
      case sendResult of
        Left err -> logErrorS $ "Failed to send response: " <> T.pack (show err)
        Right () -> logDebugS $ "Response sent for request: " <> requestIdToText reqId

-- | Handle notifications in ServerM
handleNotification :: JSONRPCNotificationMessage -> Maybe AuthInfo -> ServerM ()
handleNotification notif _mAuthInfo = do
  logInfoS $ "Received notification: " <> notifMethod notif

-- Extend this for specific notification handling as needed.
-- The auth info is passed here for consistency, in case a notification
-- handler needs it in the future.

-- | Handle initialize request in ServerM
handleInitialize :: Value -> ServerM (Either MCPError InitializeResponse)
handleInitialize params = do
  case fromJSON params of
    Aeson.Error msg -> do
      logErrorS $ "Failed to parse initialize request: " <> T.pack msg
      return $ Left $ ParseError $ T.pack msg
    Success (InitializeRequest requestedVersion caps clientInfo) -> do
      logInfoS $ "Initializing session with client: " <> clientName clientInfo

      -- Update server state
      env <- ask
      liftIO $ atomically $ do
        writeTVar (serverState env) (ServerReady clientInfo caps)
        writeTVar (clientCapabilities env) (Just caps)

      let negotiatedVersion =
            if requestedVersion `elem` supportedProtocolVersions
              then requestedVersion
              else latestProtocolVersion

      let response =
            InitializeResponse
              { respProtocolVersion = negotiatedVersion,
                respCapabilities = serverCapabilities env,
                respServerInfo = serverInfo env,
                respInstructions = serverInstructions (serverConfig env)
              }

      logInfoS "Session initialized successfully"
      return $ Right response

-- | Handle tools/list request in ServerM
handleToolsListRequest :: Value -> ServerM (Either MCPError ToolsListResponse)
handleToolsListRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> do
      logWarnS "Tools/list requested before initialization"
      return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady _ _ -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (ToolsListRequest cursor) -> do
          logDebugS "Handling tools/list request"
          ctx <- asks serverContext
          -- TODO: Implement pagination using the cursor
          toolsMap <- liftIO $ readTVarIO (scTools ctx)
          let tools = map (tdTool . rtDefinition) . filter rtEnabled . Map.elems $ toolsMap
          let response =
                ToolsListResponse
                  { tools = V.fromList tools,
                    nextCursor = Nothing
                  }
          return $ Right response

-- | Handle tools/call request in ServerM
handleToolsCallRequest :: Value -> Maybe AuthInfo -> ServerM (Either MCPError ToolsCallResponse)
handleToolsCallRequest params mAuthInfo = do
  state <- getServerState
  case state of
    ServerUninitialized -> do
      logWarnS "Tools/call requested before initialization"
      return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady {} -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (ToolsCallRequest name args) -> do
          logDebugS $ "Handling tools/call request for tool: " <> name
          ctx <- asks serverContext
          toolsMap <- liftIO $ readTVarIO (scTools ctx)
          let toolContext = ToolHandlerContext API.elicitInput mAuthInfo
          case Map.lookup name toolsMap of
            Just (RegisteredTool (ToolDefinition _ (Just handler)) True) ->
              handler toolContext args
            Just (RegisteredTool (ToolDefinition _ Nothing) True) ->
              return $ Left $ UnsupportedMethod $ "Tool '" <> name <> "' has no handler"
            Just (RegisteredTool _ False) ->
              return $ Left $ PermissionDenied ("Tool '" <> name <> "' is disabled")
            Nothing ->
              return $ Left $ UnsupportedMethod ("Tool '" <> name <> "' not found")

-- | Handle resources/list request in ServerM
handleResourcesListRequest :: Value -> ServerM (Either MCPError ResourcesListResponse)
handleResourcesListRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady _ _ -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (ResourcesListRequest cursor) -> do
          logDebugS "Handling resources/list request"
          ctx <- asks serverContext
          -- TODO: Implement pagination using the cursor
          resourcesMap <- liftIO $ readTVarIO (scResources ctx)
          let resources = map rrResource . filter rrEnabled . Map.elems $ resourcesMap
          let response =
                ResourcesListResponse
                  { resources = V.fromList resources,
                    resourcesNextCursor = Nothing
                  }
          return $ Right response

-- | Handle resources/read request in ServerM
handleResourcesReadRequest :: Value -> ServerM (Either MCPError ResourcesReadResponse)
handleResourcesReadRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady _ _ -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (ResourcesReadRequest uri) -> do
          logDebugS $ "Handling resources/read request for: " <> uri
          ctx <- asks serverContext
          resourcesMap <- liftIO $ readTVarIO (scResources ctx)
          case Map.lookup uri resourcesMap of
            Just (RegisteredResource res True) -> do
              -- Assuming static resource content is its name, as Resource type has no content field.
              let content = TextContent (resourceName res)
              let response = ResourcesReadResponse {resourceContents = V.singleton content}
              return $ Right response
            Just _ -> return $ Left $ PermissionDenied ("Resource '" <> uri <> "' is disabled") -- Resource is disabled
            Nothing -> do
              -- Static resource not found, try to match templates
              templatesMap <- liftIO $ readTVarIO (scResourceTemplates ctx)
              let templates = Map.elems templatesMap
              -- Find the first template that matches and successfully produces a response
              findFirstMatch templates uri
  where
    findFirstMatch [] uri = return $ Left $ ResourceNotFound uri
    findFirstMatch (template : ts) uri = do
      case matchTemplate (rtPattern template) uri of
        Just vars -> do
          -- If template matches, call its handler. If it fails, try the next one.
          result <- rtHandler template vars
          case result of
            Right _ -> return result
            Left _ -> findFirstMatch ts uri
        Nothing -> findFirstMatch ts uri -- No match, try next template
    matchTemplate :: Text -> Text -> Maybe (Map.Map Text Text)
    matchTemplate pattern uri =
      let pParts = T.splitOn "/" pattern
          uParts = T.splitOn "/" uri
       in if length pParts == length uParts
            then fmap Map.fromList (go pParts uParts)
            else Nothing
      where
        go [] [] = Just []
        go (p : ps) (u : us) = do
          tailMatch <- go ps us
          if "{" `T.isPrefixOf` p && "}" `T.isSuffixOf` p
            then
              let varName = T.drop 1 $ T.dropEnd 1 p
               in Just $ (varName, u) : tailMatch
            else
              if p == u
                then Just tailMatch
                else Nothing
        go _ _ = Nothing

-- | Handle prompts/list request in ServerM
handlePromptsListRequest :: Value -> ServerM (Either MCPError PromptsListResponse)
handlePromptsListRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady _ _ -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (PromptsListRequest cursor) -> do
          logDebugS "Handling prompts/list request"
          ctx <- asks serverContext
          -- TODO: Implement pagination using the cursor
          promptsMap <- liftIO $ readTVarIO (scPrompts ctx)
          let prompts = map (pdPrompt . rpDefinition) . filter rpEnabled . Map.elems $ promptsMap
          let response =
                PromptsListResponse
                  { prompts = V.fromList prompts,
                    promptsNextCursor = Nothing
                  }
          return $ Right response

-- | Handle prompts/get request in ServerM
handlePromptsGetRequest :: Value -> ServerM (Either MCPError PromptsGetResponse)
handlePromptsGetRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady _ _ -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (PromptsGetRequest name args) -> do
          logDebugS $ "Handling prompts/get request for: " <> name
          ctx <- asks serverContext
          promptsMap <- liftIO $ readTVarIO (scPrompts ctx)
          case Map.lookup name promptsMap of
            Just (RegisteredPrompt (PromptDefinition _ messages _) True) -> do
              -- TODO: Implement argument interpolation for prompt content
              let response = PromptsGetResponse {promptResponseDescription = Nothing, promptMessages = messages}
              return $ Right response
            Just _ -> return $ Left $ PermissionDenied ("Prompt '" <> name <> "' is disabled") -- Prompt is disabled
            Nothing -> return $ Left $ UnsupportedMethod ("Prompt '" <> name <> "' not found")

-- | Handle ping request in ServerM
handlePingRequest :: Value -> ServerM (Either MCPError PingResponse)
handlePingRequest _ = do
  state <- getServerState
  case state of
    ServerUninitialized -> return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady {} -> do
      logDebugS "Handling ping request"
      return $ Right PingResponse

-- | Helper to get current server state
handleCompleteRequest :: Value -> ServerM (Either MCPError CompleteResponse)
handleCompleteRequest params = do
  state <- getServerState
  case state of
    ServerUninitialized -> do
      logWarnS "completion/complete requested before initialization"
      return $ Left $ ProtocolError "Server not initialized"
    ServerClosed -> return $ Left $ ProtocolError "Server closed"
    ServerReady {} -> do
      case fromJSON params of
        Aeson.Error msg -> return $ Left $ ParseError $ T.pack msg
        Success (CompleteRequest ref arg ctx) -> do
          logDebugS "Handling completion/complete request"
          env <- ask
          let serverCtx = serverContext env
          let argName = caName arg
          let argValue = caValue arg
          let contextArgs = ccArguments ctx
          completions <- case ref of
            PromptRef promptName -> do
              promptsMap <- liftIO $ readTVarIO (scPrompts serverCtx)
              case Map.lookup promptName promptsMap of
                Just (RegisteredPrompt (PromptDefinition _ _ completers) True) ->
                  case Map.lookup argName completers of
                    Just completer -> completer contextArgs argValue
                    _ -> return []
                _ -> return [] -- Prompt not found or has no arguments
            ResourceRef templateName -> do
              templatesMap <- liftIO $ readTVarIO (scResourceTemplates serverCtx)
              case Map.lookup templateName templatesMap of
                Just (ResourceTemplate _ _ completers) ->
                  case Map.lookup argName completers of
                    Just completer -> completer contextArgs argValue
                    _ -> return [] -- No completer for this argument name
                _ -> return [] -- Template not found
          return $ Right $ CompleteResponse (V.fromList completions)

getServerState :: ServerM ServerState
getServerState = do
  env <- ask
  liftIO $ readTVarIO (serverState env)

-- | Convert MCP error to JSON-RPC error
errorToJSONRPCError :: MCPError -> JSONRPCError
errorToJSONRPCError err =
  JSONRPCError
    { errorCode = MCP.SDK.Error.errorCodeToInt $ MCP.SDK.Error.mcpErrorToCode err,
      errorMessage = errorToMessage err,
      errorData = Nothing
    }
  where
    errorToMessage (ProtocolError msg) = msg
    errorToMessage (ParseError msg) = msg
    errorToMessage (ValidationError msg) = msg
    errorToMessage (TransportError msg) = msg
    errorToMessage TimeoutError = "Request timed out"
    errorToMessage (RequestTimeout msg) = "Request timed out: " <> msg
    errorToMessage ConnectionClosed = "Connection closed"
    errorToMessage (UnsupportedMethod method) = "Unsupported method: " <> method
    errorToMessage (InvalidCapability cap) = "Invalid capability: " <> cap
    errorToMessage (ResourceNotFound res) = "Resource not found: " <> res
    errorToMessage (ToolExecutionError msg) = "Tool execution error: " <> msg
    errorToMessage (PermissionDenied msg) = "Permission denied: " <> msg

-- mcpErrorToCode function is now provided by MCP.SDK.Error module

-- Error codes are now defined in MCP.SDK.Error module

-- | Close the server gracefully
closeServer :: ServerEnv (ServerContext ServerM) -> ServerM ()
closeServer env = do
  case env of
    ServerEnv transport state _ _ _ _ _ _ _ -> do
      logInfoS "Closing MCP server"
      liftIO $ atomically $ writeTVar state ServerClosed
      liftIO $ disconnect transport

-- | Check if server is ready
isServerReady :: ServerEnv (ServerContext ServerM) -> ServerM Bool
isServerReady env = do
  state <- liftIO $ readTVarIO (serverState env)
  return $ case state of
    ServerReady _ _ -> True
    _ -> False

-- | Server builder pattern for easy configuration
data ServerBuilder = ServerBuilder
  { sbTransport :: Maybe SomeTransport,
    sbServerInfo :: Maybe Implementation,
    sbCapabilities :: Maybe Capabilities,
    sbHandlers :: Maybe ServerHandlers,
    sbConfig :: Maybe ServerConfig
  }

-- | Start building a server
buildServer :: ServerBuilder
buildServer = ServerBuilder Nothing Nothing Nothing Nothing Nothing

-- | Set transport for server
withServerTransport :: SomeTransport -> ServerBuilder -> ServerBuilder
withServerTransport transport builder = builder {sbTransport = Just transport}

-- | Set server info
withServerInfo :: Text -> Text -> ServerBuilder -> ServerBuilder
withServerInfo name version builder =
  builder {sbServerInfo = Just (ServerInfo name version)}

-- | Set server capabilities
withServerCapabilities :: Capabilities -> ServerBuilder -> ServerBuilder
withServerCapabilities caps builder = builder {sbCapabilities = Just caps}

-- | Set server handlers
withServerHandlers :: ServerHandlers -> ServerBuilder -> ServerBuilder
withServerHandlers handlers builder = builder {sbHandlers = Just handlers}

-- | Set server configuration
withServerConfig :: ServerConfig -> ServerBuilder -> ServerBuilder
withServerConfig config builder = builder {sbConfig = Just config}

-- | Finalize server creation
finalizeServer :: ServerBuilder -> IO (Either MCPError (ServerEnv (ServerContext ServerM)))
finalizeServer ServerBuilder {..} =
  case sbTransport of
    Nothing -> return $ Left $ ValidationError "Transport is required"
    Just (Transport.SomeTransport t) ->
      case sbServerInfo of
        Nothing -> return $ Left $ ValidationError "Server info is required"
        Just info -> do
          let capabilities = maybe defaultServerCapabilities id sbCapabilities
          let handlers = maybe defaultServerHandlers id sbHandlers
          let config = maybe defaultServerConfig id sbConfig
          Right <$> newMCPServer t info capabilities handlers config

-- | Default capabilities for servers
defaultServerCapabilities :: Capabilities
defaultServerCapabilities = Capabilities Nothing Nothing

-- | Default handlers (all return not implemented, but now in ServerM)
defaultServerHandlers :: ServerHandlers
defaultServerHandlers = ServerHandlers {}
