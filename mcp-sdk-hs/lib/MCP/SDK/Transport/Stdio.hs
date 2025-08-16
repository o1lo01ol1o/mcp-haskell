{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.SDK.Transport.Stdio
  ( StdioTransport,
    newStdioTransport,
    runStdioTransport,
    -- Client-specific types and functions
    ClientConfig (..),
    ClientState (..),
    ClientM,
    runClientM,
    runStdioClient,
    sendClientRequest,
    defaultClientConfig,
    -- Server-specific types and functions
    ServerConfig (..),
    ServerState (..),
    ServerM,
    runServerM,
    runStdioServer,
    defaultServerConfig,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, catch)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebugN, logErrorN, logInfoN, logWarnN, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Aeson (Result (..), Value, decode, encode, fromJSON, object, toJSON, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import MCP.SDK.Error
import MCP.SDK.Protocol
import MCP.SDK.Transport
import MCP.SDK.Types
import qualified StmContainers.Map as StmMap
import System.IO (Handle, hFlush, hGetLine, hIsEOF, hPutStrLn, stderr, stdin, stdout)
import System.Timeout (timeout)

-- | Stdio-based transport implementation
data StdioTransport = StdioTransport
  { stInput :: Handle,
    stOutput :: Handle,
    stState :: TransportState,
    stMessageQueue :: TBQueue JSONRPCMessage
  }

instance Eq StdioTransport where
  (StdioTransport _ _ s1 _) == (StdioTransport _ _ s2 _) = s1 == s2

-- | Client-specific configuration
data ClientConfig = ClientConfig
  { clientRequestTimeout :: Int, -- Request timeout in seconds
    clientRetryAttempts :: Int, -- Number of retry attempts
    clientMaxPendingRequests :: Int, -- Maximum concurrent pending requests
    clientName :: Text -- Client identifier for logging
  }
  deriving (Eq, Show)

-- | Client state for request correlation and timeout management
data ClientState = ClientState
  { csPendingRequests :: StmMap.Map RequestId (UTCTime, TMVar (Either MCPError JSONRPCResponseMessage)),
    csRequestCount :: TVar Int,
    csConnectionRetries :: TVar Int
  }

-- | Client monad stack: ReaderT for config + LoggingT for logging + IO for effects
newtype ClientM a = ClientM
  {unClientM :: ReaderT (ClientConfig, ClientState, StdioTransport) (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ClientConfig, ClientState, StdioTransport),
      MonadLogger
    )

-- | Server-specific configuration
data ServerConfig = ServerConfig
  { serverMaxConcurrentRequests :: Int, -- Maximum concurrent request handlers
    serverRequestQueueSize :: Int, -- Size of request processing queue
    serverShutdownTimeout :: Int, -- Graceful shutdown timeout in seconds
    serverName :: Text -- Server identifier for logging
  }
  deriving (Eq, Show)

-- | Server state for request handling and resource management
data ServerState = ServerState
  { ssActiveRequests :: StmMap.Map RequestId (TMVar (Either MCPError JSONRPCResponseMessage)),
    ssRequestCount :: TVar Int,
    ssLastActivity :: TVar UTCTime,
    ssShuttingDown :: TVar Bool,
    ssRequestQueue :: TBQueue JSONRPCRequestMessage
  }

-- | Server monad stack: ReaderT for config + LoggingT for logging + IO for effects
newtype ServerM a = ServerM
  {unServerM :: ReaderT (ServerConfig, ServerState, StdioTransport) (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ServerConfig, ServerState, StdioTransport),
      MonadLogger
    )

-- | Helper to create transport state and queue
createTransportBase :: TransportConfig -> IO (TransportState, TBQueue JSONRPCMessage)
createTransportBase config = do
  state <- initTransportState config
  queue <- newTBQueueIO (fromIntegral $ transportBufferSize config)
  return (state, queue)

-- | Helper to build StdioTransport with given handles
buildStdioTransport :: Handle -> Handle -> TransportState -> TBQueue JSONRPCMessage -> StdioTransport
buildStdioTransport input output state queue =
  StdioTransport
    { stInput = input,
      stOutput = output,
      stState = state,
      stMessageQueue = queue
    }

-- | Create a new stdio transport
newStdioTransport :: TransportConfig -> IO StdioTransport
newStdioTransport config = do
  (state, queue) <- createTransportBase config
  return $ buildStdioTransport stdin stdout state queue

-- | Create stdio transport with custom handles
newStdioTransportWithHandles :: Handle -> Handle -> TransportConfig -> IO StdioTransport
newStdioTransportWithHandles input output config = do
  (state, queue) <- createTransportBase config
  return $ buildStdioTransport input output state queue

instance Transport StdioTransport where
  sendMessage transport msg = liftIO $ do
    connected <- readTVarIO (tsConnected $ stState transport)
    if connected
      then catch (sendMessageImpl transport msg) handleIOError
      else return $ Left (TransportError "Transport not connected")

  receiveMessage transport = liftIO $ do
    connected <- readTVarIO (tsConnected $ stState transport)
    if connected
      then do
        result <- atomically $ readTBQueue (stMessageQueue transport)
        return $ Right result
      else return $ Left (TransportError "Transport not connected")

  closeTransport transport = liftIO $ do
    atomically $ writeTVar (tsConnected $ stState transport) False

  isConnected transport =
    liftIO $
      readTVarIO (tsConnected $ stState transport)

-- | Internal message sending implementation
sendMessageImpl :: StdioTransport -> JSONRPCMessage -> IO (Either MCPError ())
sendMessageImpl StdioTransport {..} msg = do
  let jsonBytes = encode msg
  let jsonString = L8.unpack jsonBytes
  catch
    ( do
        hPutStrLn stOutput jsonString
        hFlush stOutput
        return $ Right ()
    )
    (\e -> return $ Left $ TransportError $ T.pack $ show (e :: IOException))

-- | Helper to handle IOException and convert to MCPError
handleIOError :: IOException -> IO (Either MCPError a)
handleIOError e = return $ Left (TransportError $ T.pack $ show e)

-- | Start the stdio transport with proper resource management
runStdioTransport :: StdioTransport -> IO () -> IO (Either MCPError ())
runStdioTransport transport@StdioTransport {..} action = do
  bracket
    (startTransport transport)
    (\_ -> stopTransport transport)
    ( \startResult -> case startResult of
        Left err -> return $ Left err
        Right _ -> do
          action
          return $ Right ()
    )

-- | Start transport operations
startTransport :: StdioTransport -> IO (Either MCPError ())
startTransport transport@StdioTransport {..} = do
  atomically $ writeTVar (tsConnected stState) True
  -- Start async message receiver
  void $ forkIO $ receiveLoop transport
  return $ Right ()

-- | Stop transport operations
stopTransport :: StdioTransport -> IO ()
stopTransport transport@StdioTransport {..} = do
  atomically $ writeTVar (tsConnected stState) False

-- | Message receiving loop
receiveLoop :: StdioTransport -> IO ()
receiveLoop transport@StdioTransport {..} = do
  forever $ do
    connected <- readTVarIO (tsConnected stState)
    when connected $ do
      result <- catch (receiveMessageLine transport) handleIOError
      case result of
        Left err -> hPutStrLn stderr $ "Receive error: " ++ show err
        Right msg -> atomically $ writeTBQueue stMessageQueue msg

-- | Receive a single message line from input
receiveMessageLine :: StdioTransport -> IO (Either MCPError JSONRPCMessage)
receiveMessageLine StdioTransport {..} = do
  eof <- hIsEOF stInput
  if eof
    then return $ Left ConnectionClosed
    else do
      line <- hGetLine stInput
      let lineBS = L8.pack line
      case decode lineBS of
        Nothing -> return $ Left $ ParseError "Failed to parse JSON message"
        Just msg -> return $ Right msg

-- | Default client configuration
defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { clientRequestTimeout = 30,
      clientRetryAttempts = 3,
      clientMaxPendingRequests = 100,
      clientName = "mcp-haskell-client"
    }

-- | Default server configuration
defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { serverMaxConcurrentRequests = 50,
      serverRequestQueueSize = 200,
      serverShutdownTimeout = 10,
      serverName = "mcp-haskell-server"
    }

-- | Create initial client state
newClientState :: IO ClientState
newClientState = do
  pending <- StmMap.newIO
  count <- newTVarIO 0
  retries <- newTVarIO 0
  return
    ClientState
      { csPendingRequests = pending,
        csRequestCount = count,
        csConnectionRetries = retries
      }

-- | Create initial server state
newServerState :: ServerConfig -> IO ServerState
newServerState config = do
  active <- StmMap.newIO
  count <- newTVarIO 0
  now <- getCurrentTime
  activity <- newTVarIO now
  shutdown <- newTVarIO False
  queue <- newTBQueueIO (fromIntegral $ serverRequestQueueSize config)
  return
    ServerState
      { ssActiveRequests = active,
        ssRequestCount = count,
        ssLastActivity = activity,
        ssShuttingDown = shutdown,
        ssRequestQueue = queue
      }

-- | Run the Client monad
runClientM :: ClientConfig -> ClientState -> StdioTransport -> ClientM a -> IO a
runClientM config state transport action =
  runStdoutLoggingT $ runReaderT (unClientM action) (config, state, transport)

-- | Run the Server monad
runServerM :: ServerConfig -> ServerState -> StdioTransport -> ServerM a -> IO a
runServerM config state transport action =
  runStdoutLoggingT $ runReaderT (unServerM action) (config, state, transport)

-- | Client-specific request sending with timeout and correlation
sendClientRequest :: RequestId -> JSONRPCRequestMessage -> ClientM (Either MCPError JSONRPCResponseMessage)
sendClientRequest reqId req = do
  (config, state, transport) <- ask
  logInfoN $ "Sending request: " <> T.pack (show reqId)

  -- Check pending request limit
  pendingCount <- liftIO $ atomically $ StmMap.size (csPendingRequests state)
  if pendingCount >= clientMaxPendingRequests config
    then return $ Left $ TransportError "Too many pending requests"
    else do
      -- Create response variable and register request
      responseVar <- liftIO newEmptyTMVarIO
      now <- liftIO getCurrentTime
      let timeoutTime = addUTCTime (fromIntegral $ clientRequestTimeout config) now

      liftIO $ atomically $ do
        StmMap.insert (timeoutTime, responseVar) reqId (csPendingRequests state)
        modifyTVar' (csRequestCount state) (+ 1)

      -- Send request
      sendResult <- liftIO $ sendMessage transport (JSONRPCRequest req)
      case sendResult of
        Left err -> do
          logErrorN $ "Failed to send request: " <> T.pack (show err)
          liftIO $ atomically $ StmMap.delete reqId (csPendingRequests state)
          return $ Left err
        Right () -> do
          -- Wait for response with timeout
          liftIO $ atomically $ do
            response <- readTMVar responseVar
            StmMap.delete reqId (csPendingRequests state)
            return response

-- | Process incoming responses for client
processClientResponse :: JSONRPCResponseMessage -> ClientM ()
processClientResponse resp = do
  (_, state, _) <- ask
  let reqId = respId resp
  logDebugN $ "Processing response for request: " <> T.pack (show reqId)

  liftIO $ atomically $ do
    maybePending <- StmMap.lookup reqId (csPendingRequests state)
    case maybePending of
      Nothing -> return () -- Unknown response
      Just (_, responseVar) -> putTMVar responseVar (Right resp)

-- | Queue server request for processing
queueServerRequest :: JSONRPCRequestMessage -> ServerM Bool
queueServerRequest req = do
  (_, state, _) <- ask
  liftIO $ atomically $ do
    full <- isFullTBQueue (ssRequestQueue state)
    if full
      then return False
      else do
        writeTBQueue (ssRequestQueue state) req
        return True

-- | Process server requests from queue
processServerRequests :: (JSONRPCRequestMessage -> ServerM (Either MCPError JSONRPCResponseMessage)) -> ServerM ()
processServerRequests handler = do
  (config, state, transport) <- ask
  logInfoN "Starting server request processor"

  forever $ do
    -- Check if shutting down
    shutting <- liftIO $ readTVarIO (ssShuttingDown state)
    when shutting $ do
      logInfoN "Server shutting down, stopping request processor"
      return ()

    -- Get next request from queue
    maybeReq <- liftIO $ atomically $ do
      empty <- isEmptyTBQueue (ssRequestQueue state)
      if empty
        then return Nothing
        else Just <$> readTBQueue (ssRequestQueue state)

    case maybeReq of
      Nothing -> liftIO $ threadDelay 100000 -- 100ms wait
      Just req -> do
        let requestId = reqId req
        logInfoN $ "Processing request: " <> T.pack (show requestId)

        -- Check concurrent request limit
        activeCount <- liftIO $ atomically $ StmMap.size (ssActiveRequests state)
        if activeCount >= serverMaxConcurrentRequests config
          then do
            logWarnN "Server at request capacity, rejecting request"
            let errorResp =
                  JSONRPCResponseMessage
                    { respId = requestId,
                      respResult = Nothing,
                      respError = Just $ JSONRPCError (-32000) "Server busy" Nothing
                    }
            liftIO $ void $ sendMessage transport (JSONRPCResponse errorResp)
          else do
            -- Handle request
            responseVar <- liftIO newEmptyTMVarIO
            liftIO $ atomically $ do
              StmMap.insert responseVar requestId (ssActiveRequests state)
              modifyTVar' (ssRequestCount state) (+ 1)

            -- Process request
            result <- handler req
            let response = case result of
                  Left err ->
                    JSONRPCResponseMessage
                      { respId = requestId,
                        respResult = Nothing,
                        respError = Just $ JSONRPCError (-32603) (T.pack $ show err) Nothing
                      }
                  Right resp -> resp

            -- Send response and cleanup
            liftIO $ do
              void $ sendMessage transport (JSONRPCResponse response)
              atomically $ do
                putTMVar responseVar (Right response)
                StmMap.delete requestId (ssActiveRequests state)

-- | Graceful server shutdown
shutdownServer :: ServerM ()
shutdownServer = do
  (config, state, _) <- ask
  logInfoN "Initiating graceful server shutdown"
  liftIO $ atomically $ writeTVar (ssShuttingDown state) True

  -- Wait for active requests with timeout
  let timeoutMicros = serverShutdownTimeout config * 1000000
  liftIO $ void $ timeout timeoutMicros $ do
    atomically $ do
      isEmpty <- StmMap.null (ssActiveRequests state)
      when (not isEmpty) retry

-- | Get client statistics
getClientStats :: ClientM (Int, Int, Int)
getClientStats = do
  (_, state, _) <- ask
  liftIO $ atomically $ do
    pendingCount <- StmMap.size (csPendingRequests state)
    totalRequests <- readTVar (csRequestCount state)
    retries <- readTVar (csConnectionRetries state)
    return (pendingCount, totalRequests, retries)

-- | Get server statistics
getServerStats :: ServerM (Int, Int, Int, Bool)
getServerStats = do
  (_, state, _) <- ask
  liftIO $ atomically $ do
    activeCount <- StmMap.size (ssActiveRequests state)
    totalRequests <- readTVar (ssRequestCount state)
    queueSize <- lengthTBQueue (ssRequestQueue state)
    shuttingDown <- readTVar (ssShuttingDown state)
    return (activeCount, totalRequests, fromIntegral queueSize, shuttingDown)

-- | High-level stdio transport runner for clients
runStdioClient :: ClientConfig -> (ClientM a) -> IO (Either MCPError a)
runStdioClient config clientAction = do
  transport <- newStdioTransport defaultTransportConfig
  clientState <- newClientState

--   runStdioTransport transport $ do
    -- Start response processor
    void $ forkIO $ runClientM config clientState transport $ forever $ do
      (_, _, transport') <- ask
      msgResult <- liftIO $ receiveMessage transport'
      case msgResult of
        Left err -> logErrorN $ "Client receive error: " <> T.pack (show err)
        Right (JSONRPCResponse resp) -> processClientResponse resp
        Right _ -> return () -- Ignore other message types

    -- Basic client monitoring
    void $ forkIO $ runClientM config clientState transport $ forever $ do
      liftIO $ threadDelay 30000000 -- Check every 30 seconds
      (pending, total, retries) <- getClientStats
      logInfoN $ "Client stats - Pending: " <> T.pack (show pending) <> ", Total: " <> T.pack (show total)

    -- Run client action and return result
    result <- runClientM config clientState transport clientAction
    return $ Right result

-- | High-level stdio transport runner for servers
runStdioServer :: ServerConfig -> (ServerM a) -> IO (Either MCPError a)
runStdioServer config serverAction = do
  transport <- newStdioTransport defaultTransportConfig
  serverState <- newServerState config

--   runStdioTransport transport $ do
    -- Start request processor
    void $ forkIO $ runServerM config serverState transport $ forever $ do
      (_, state, transport') <- ask
      msgResult <- liftIO $ receiveMessage transport'
      case msgResult of
        Left err -> logErrorN $ "Server receive error: " <> T.pack (show err)
        Right (JSONRPCRequest req) -> void $ queueServerRequest req
        Right _ -> return () -- Ignore other message types

    -- Start stats reporter
    void $ forkIO $ runServerM config serverState transport $ forever $ do
      liftIO $ threadDelay 60000000 -- Report every minute
      (active, total, queued, shutting) <- getServerStats
      logInfoN $
        "Server stats - Active: "
          <> T.pack (show active)
          <> ", Total: "
          <> T.pack (show total)
          <> ", Queued: "
          <> T.pack (show queued)

    -- Run server action and return result
    result <- runServerM config serverState transport serverAction
    return $ Right result
