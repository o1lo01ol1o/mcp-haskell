{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.SDK.Error
  ( MCPError (..),
    ErrorCode (..),
    errorCodeToInt,
    mcpErrorToCode,
    validationError,
    transportError,
  )
where

import Control.Exception (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Comprehensive MCP error types following correct-by-construction principles
data MCPError
  = ProtocolError Text -- JSON-RPC protocol errors
  | ParseError Text -- JSON parsing errors
  | ValidationError Text -- Request validation errors
  | TransportError Text -- Transport layer errors
  | TimeoutError -- Request timeout
  | RequestTimeout Text -- Specific request timeout with context
  | ConnectionClosed -- Connection unexpectedly closed
  | UnsupportedMethod Text -- Method not supported by server/client
  | InvalidCapability Text -- Capability mismatch
  | ResourceNotFound Text -- Requested resource doesn't exist
  | ToolExecutionError Text -- Tool execution failed
  | PermissionDenied Text -- Access denied
  deriving (Eq, Show, Generic)

instance Exception MCPError

-- | Standard JSON-RPC error codes
data ErrorCode
  = ParseErrorCode -- -32700
  | InvalidRequestCode -- -32600
  | MethodNotFoundCode -- -32601
  | InvalidParamsCode -- -32602
  | InternalErrorCode -- -32603
  | ServerErrorCode Int -- -32000 to -32099
  deriving (Eq, Show)

errorCodeToInt :: ErrorCode -> Int
errorCodeToInt ParseErrorCode = -32700
errorCodeToInt InvalidRequestCode = -32600
errorCodeToInt MethodNotFoundCode = -32601
errorCodeToInt InvalidParamsCode = -32602
errorCodeToInt InternalErrorCode = -32603
errorCodeToInt (ServerErrorCode code) = code

-- | Convert MCP errors to appropriate JSON-RPC error codes
mcpErrorToCode :: MCPError -> ErrorCode
mcpErrorToCode (ProtocolError _) = InvalidRequestCode
mcpErrorToCode (ParseError _) = ParseErrorCode
mcpErrorToCode (ValidationError _) = InvalidParamsCode
mcpErrorToCode (UnsupportedMethod _) = MethodNotFoundCode
mcpErrorToCode (ResourceNotFound _) = ServerErrorCode (-32001)
mcpErrorToCode (ToolExecutionError _) = ServerErrorCode (-32002)
mcpErrorToCode (PermissionDenied _) = ServerErrorCode (-32003)
mcpErrorToCode _ = InternalErrorCode

-- | Smart constructor for validation errors with context
validationError :: Text -> Text -> MCPError
validationError field reason = ValidationError $ field <> ": " <> reason

-- | Smart constructor for transport errors with context
transportError :: Text -> MCPError
transportError = TransportError
