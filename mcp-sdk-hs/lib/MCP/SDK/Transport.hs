{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module MCP.SDK.Transport 
  ( Transport(..)
  , TransportConfig(..)
  , TransportState(..)
  , SomeTransport(..)
  , defaultTransportConfig
  , initTransportState
  , registerPendingRequest
  , resolvePendingRequest
  , connect
  , disconnect
  , handleIncomingMessage
  , RecoveryStrategy(..)
  , getRecoveryStrategy
  , wrapTransport
  , withSomeTransport
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import MCP.SDK.Error
import MCP.SDK.Protocol
import MCP.SDK.Types

-- | Abstract transport interface
class Transport t where
  -- | Send a message through the transport
  sendMessage :: MonadIO m => t -> JSONRPCMessage -> m (Either MCPError ())
  
  -- | Receive a message from the transport (blocking)
  receiveMessage :: MonadIO m => t -> m (Either MCPError JSONRPCMessage)
  
  -- | Close the transport
  closeTransport :: MonadIO m => t -> m ()
  
  -- | Check if transport is connected
  isConnected :: MonadIO m => t -> m Bool

-- | Transport configuration
data TransportConfig = TransportConfig
  { transportTimeout :: Int      -- Timeout in seconds
  , transportBufferSize :: Int   -- Buffer size for messages
  , transportRetries :: Int      -- Number of retry attempts
  } deriving (Eq, Show)

-- | Default transport configuration
defaultTransportConfig :: TransportConfig
defaultTransportConfig = TransportConfig
  { transportTimeout = 30
  , transportBufferSize = 8192
  , transportRetries = 3
  }

-- | Transport state for connection management
data TransportState = TransportState
  { tsConnected :: TVar Bool
  , tsPendingRequests :: TVar [(RequestId, TMVar (Either MCPError Value))]
  , tsConfig :: TransportConfig
  } deriving Eq

-- | Initialize transport state
initTransportState :: TransportConfig -> IO TransportState
initTransportState config = do
  connected <- newTVarIO False
  pending <- newTVarIO []
  return TransportState
    { tsConnected = connected
    , tsPendingRequests = pending
    , tsConfig = config
    }

-- | Register a pending request
registerPendingRequest :: TransportState -> RequestId -> STM (TMVar (Either MCPError Value))
registerPendingRequest state requestIdValue = do
  var <- newEmptyTMVar
  modifyTVar' (tsPendingRequests state) ((requestIdValue, var):)
  return var

-- | Resolve a pending request
resolvePendingRequest :: TransportState -> RequestId -> Either MCPError Value -> STM Bool
resolvePendingRequest state requestIdValue result = do
  pending <- readTVar (tsPendingRequests state)
  case lookup requestIdValue pending of
    Nothing -> return False
    Just var -> do
      let remaining = filter ((/= requestIdValue) . fst) pending
      writeTVar (tsPendingRequests state) remaining
      putTMVar var result
      return True

-- | Connection lifecycle management
connect :: Transport t => t -> IO (Either MCPError ())
connect transport = do
  connected <- isConnected transport
  if connected
    then return (Right ())
    else return (Left (TransportError "Failed to connect"))

disconnect :: Transport t => t -> IO ()
disconnect = closeTransport

-- | Message handling utilities
handleIncomingMessage :: TransportState -> JSONRPCMessage -> IO ()
handleIncomingMessage state msg = case msg of
  JSONRPCResponse resp -> do
    let requestIdValue = respId resp
    result <- case respError resp of
      Just err -> return $ Left (ProtocolError (errorMessage err))
      Nothing -> case respResult resp of
        Just val -> return $ Right val
        Nothing -> return $ Left (ProtocolError "No result in response")
    
    resolved <- atomically $ resolvePendingRequest state requestIdValue result
    if not resolved
      then putStrLn $ "Warning: Received response for unknown request: " ++ show requestIdValue
      else return ()
  
  JSONRPCNotification _ -> do
    -- Handle notifications (could be extended for specific notification types)
    return ()
  
  JSONRPCRequest _ -> do
    -- For client transports, unexpected requests should be logged
    putStrLn "Warning: Received unexpected request on client transport"
    return ()

-- | Error recovery strategies
data RecoveryStrategy
  = NoRecovery
  | RetryWithBackoff Int  -- milliseconds
  | ReconnectAndRetry
  deriving (Eq, Show)

-- | Determine recovery strategy based on error type
getRecoveryStrategy :: MCPError -> RecoveryStrategy
getRecoveryStrategy ConnectionClosed = ReconnectAndRetry
getRecoveryStrategy TimeoutError = RetryWithBackoff 1000
getRecoveryStrategy (TransportError _) = RetryWithBackoff 500
getRecoveryStrategy _ = NoRecovery

-- | Existential wrapper for Transport to allow storage without knowing the connection type
data SomeTransport = forall conn. Transport conn => SomeTransport conn

-- | Wrap a Transport in the existential wrapper
wrapTransport :: Transport conn => conn -> SomeTransport
wrapTransport = SomeTransport

-- | Extract a Transport from the existential wrapper with a continuation
withSomeTransport :: SomeTransport -> (forall conn. Transport conn => conn -> r) -> r
withSomeTransport (SomeTransport transport) k = k transport
