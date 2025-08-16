{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.SDK.Request
  ( -- Smart constructors (only way to create valid requests)
    makeInitializeRequest,
    makeToolsListRequest,
    makeToolsCallRequest,
    makeResourcesListRequest,
    makeResourcesReadRequest,
    makePromptsGetRequest,
    makePromptsListRequest,
    makePingRequest,
    -- Other exports
    RequestContext (..),
    defaultContext,
    RequestHandler (..),
    generateRequestId,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import MCP.SDK.Error
import MCP.SDK.Protocol
import MCP.SDK.Types

-- | Smart constructors that parse and validate, ensuring only valid requests can be created
makeInitializeRequest :: Text -> Capabilities -> ClientInfo -> Either MCPError (MCPRequest 'Initialize)
makeInitializeRequest pv caps info
  | T.null pv = Left (validationError "protocolVersion" "cannot be empty")
  | otherwise = Right $ InitializeReq (InitializeRequest pv caps info)

makeToolsListRequest :: Maybe Text -> MCPRequest 'ToolsList
makeToolsListRequest cursor = ToolsListReq (ToolsListRequest cursor)

makeToolsCallRequest :: Text -> Maybe Object -> Either MCPError (MCPRequest 'ToolsCall)
makeToolsCallRequest name args
  | T.null name = Left (validationError "name" "tool name cannot be empty")
  | otherwise = Right $ ToolsCallReq (ToolsCallRequest name args)

makeResourcesListRequest :: Maybe Text -> MCPRequest 'ResourcesList
makeResourcesListRequest cursor = ResourcesListReq (ResourcesListRequest cursor)

makeResourcesReadRequest :: Text -> Either MCPError (MCPRequest 'ResourcesRead)
makeResourcesReadRequest uri
  | T.null uri = Left (validationError "uri" "resource URI cannot be empty")
  | otherwise = Right $ ResourcesReadReq (ResourcesReadRequest uri)

makePromptsGetRequest :: Text -> Maybe Object -> Either MCPError (MCPRequest 'PromptsGet)
makePromptsGetRequest name args
  | T.null name = Left (validationError "name" "prompt name cannot be empty")
  | otherwise = Right $ PromptsGetReq (PromptsGetRequest name args)

makePromptsListRequest :: Maybe Text -> MCPRequest 'PromptsList
makePromptsListRequest cursor = PromptsListReq (PromptsListRequest cursor)

makePingRequest :: MCPRequest 'Ping
makePingRequest = PingReq PingRequest

-- | Request execution context
data RequestContext = RequestContext
  { requestTimeout :: Int, -- timeout in seconds
    requestRetries :: Int -- number of retries
  }
  deriving (Eq, Show)

-- | Default request context
defaultContext :: RequestContext
defaultContext =
  RequestContext
    { requestTimeout = 30,
      requestRetries = 0
    }

-- | Type-safe request handler interface
class RequestHandler h where
  -- | Execute a request with full type safety
  executeRequest ::
    (MonadIO m, ToJSON (RequestType method)) =>
    h ->
    RequestContext ->
    MCPRequest method ->
    m (Either MCPError (ResponseType method))

-- | Utility function to create request ID
generateRequestId :: (MonadIO m) => m RequestId
generateRequestId = do
  uuid <- liftIO nextRandom
  let reqId = "req-" <> toString uuid
  return (RequestId (T.pack reqId))
