{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines the core Server monad and its environment, separating it
-- from the main server logic to break module cycles.
module MCP.SDK.Server.Monad
  ( ServerM (..),
    runServerM,
    ServerEnv (..),
    ServerConfig (..),
    LogLevel (..),
    ServerState (..),
    ServerHandlers (..),
  )
where

import Control.Concurrent.STM (TMVar, TVar)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import MCP.SDK.Error (MCPError)
import MCP.SDK.Server.Middleware (AuthMiddleware)
import MCP.SDK.Server.State (ServerContext)
import MCP.SDK.Transport (Transport)
import MCP.SDK.Types (Capabilities, ClientInfo, Implementation, RequestId)

-- | Server Monad Transformer Stack: ReaderT + LoggingT + IO
newtype ServerM a = ServerM
  {unServerM :: ReaderT (ServerEnv (ServerContext ServerM)) (LoggingT IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ServerEnv (ServerContext ServerM)),
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | Server Configuration Environment
data ServerEnv ctx where
  ServerEnv ::
    (Transport t) =>
    { serverTransport :: t,
      serverState :: TVar ServerState,
      serverCapabilities :: Capabilities,
      serverInfo :: Implementation,
      serverHandlers :: ServerHandlers,
      serverConfig :: ServerConfig,
      serverContext :: ctx,
      serverPendingRequests :: TVar (Map RequestId (TMVar (Either MCPError Value))),
      clientCapabilities :: TVar (Maybe Capabilities)
    } ->
    ServerEnv ctx

-- | Server Configuration
data ServerConfig = ServerConfig
  { configName :: Text,
    configVersion :: Text,
    serverMaxConnections :: Int,
    serverRequestTimeout :: Int,
    serverLogLevel :: LogLevel,
    serverAuthMiddleware :: Maybe (AuthMiddleware ServerM)
  }

-- | Log levels
data LogLevel = Debug | Info | Warn | Error deriving (Eq, Show, Ord)

-- | Server state machine
data ServerState
  = ServerUninitialized
  | ServerReady ClientInfo Capabilities
  | ServerClosed
  deriving (Eq, Show)

-- | Server request handlers in ServerM monad.
-- This is now just a placeholder but is kept for structure.
data ServerHandlers = ServerHandlers {}

-- | Run the Server monad
runServerM :: ServerEnv (ServerContext ServerM) -> ServerM a -> IO a
runServerM env action = runStdoutLoggingT $ runReaderT (unServerM action) env
