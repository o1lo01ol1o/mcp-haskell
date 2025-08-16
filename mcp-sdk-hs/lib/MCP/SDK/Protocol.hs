{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.SDK.Protocol where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.SDK.Error
import MCP.SDK.Types

-- | JSON-RPC 2.0 message types
data JSONRPCMessage
  = JSONRPCRequest JSONRPCRequestMessage
  | JSONRPCResponse JSONRPCResponseMessage
  | JSONRPCNotification JSONRPCNotificationMessage
  deriving (Eq, Show)

data JSONRPCRequestMessage = JSONRPCRequestMessage
  { reqId :: RequestId,
    reqMethod :: Text,
    reqParams :: Value
  }
  deriving (Eq, Show)

data JSONRPCResponseMessage = JSONRPCResponseMessage
  { respId :: RequestId,
    respResult :: Maybe Value,
    respError :: Maybe JSONRPCError
  }
  deriving (Eq, Show)

data JSONRPCNotificationMessage = JSONRPCNotificationMessage
  { notifMethod :: Text,
    notifParams :: Value
  }
  deriving (Eq, Show)

data JSONRPCError = JSONRPCError
  { errorCode :: Int,
    errorMessage :: Text,
    errorData :: Maybe Value
  }
  deriving (Eq, Show)

-- | Convert method types to their string representations
methodToText :: Method -> Text
methodToText Initialize = "initialize"
methodToText ToolsList = "tools/list"
methodToText ToolsCall = "tools/call"
methodToText ResourcesList = "resources/list"
methodToText ResourcesRead = "resources/read"
methodToText PromptsGet = "prompts/get"
methodToText PromptsList = "prompts/list"
methodToText Ping = "ping"
methodToText Complete = "completion/complete"
methodToText SamplingCreateMessage = "sampling/createMessage"
methodToText ElicitationCreate = "elicitation/create"
methodToText (Notification name) = name

-- | Parse method from text
textToMethod :: Text -> Either MCPError Method
textToMethod "initialize" = Right Initialize
textToMethod "tools/list" = Right ToolsList
textToMethod "tools/call" = Right ToolsCall
textToMethod "resources/list" = Right ResourcesList
textToMethod "resources/read" = Right ResourcesRead
textToMethod "prompts/get" = Right PromptsGet
textToMethod "prompts/list" = Right PromptsList
textToMethod "ping" = Right Ping
textToMethod other = Right (Notification other)

-- | Encode MCP request to JSON-RPC
encodeRequest ::
  forall m.
  (ToJSON (RequestType m)) =>
  RequestId ->
  MCPRequest m ->
  JSONRPCRequestMessage
encodeRequest reqId req = JSONRPCRequestMessage reqId method params
  where
    (method, params) = case req of
      InitializeReq r -> (methodToText Initialize, toJSON r)
      ToolsListReq r -> (methodToText ToolsList, toJSON r)
      ToolsCallReq r -> (methodToText ToolsCall, toJSON r)
      ResourcesListReq r -> (methodToText ResourcesList, toJSON r)
      ResourcesReadReq r -> (methodToText ResourcesRead, toJSON r)
      PromptsGetReq r -> (methodToText PromptsGet, toJSON r)
      PromptsListReq r -> (methodToText PromptsList, toJSON r)
      PingReq r -> (methodToText Ping, toJSON r)
      CompleteReq r -> (methodToText Complete, toJSON r)
      SamplingCreateMessageReq r -> (methodToText SamplingCreateMessage, toJSON r)
      ElicitationCreateReq r -> (methodToText ElicitationCreate, toJSON r)

-- | Decode JSON-RPC response based on expected method
decodeResponse ::
  forall m.
  (FromJSON (ResponseType m)) =>
  Method ->
  JSONRPCResponseMessage ->
  Either MCPError (ResponseType m)
decodeResponse expectedMethod resp
  | Just err <- respError resp = Left (ProtocolError (errorMessage err))
  | Just result <- respResult resp =
      case fromJSON result of
        Success val -> Right val
        Error msg -> Left (ParseError (T.pack msg))
  | otherwise = Left (ProtocolError "No result or error in response")

-- | JSON instances for JSON-RPC types
instance FromJSON JSONRPCMessage where
  parseJSON = withObject "JSONRPCMessage" $ \o -> do
    hasId <- (o .: "id" :: Parser Value) >> pure True <|> pure False
    hasResult <- (o .: "result" :: Parser Value) >> pure True <|> pure False
    hasError <- (o .: "error" :: Parser Value) >> pure True <|> pure False

    if hasId && (hasResult || hasError)
      then JSONRPCResponse <$> parseJSON (Object o)
      else
        if hasId
          then JSONRPCRequest <$> parseJSON (Object o)
          else JSONRPCNotification <$> parseJSON (Object o)

instance ToJSON JSONRPCMessage where
  toJSON (JSONRPCRequest req) = toJSON req
  toJSON (JSONRPCResponse resp) = toJSON resp
  toJSON (JSONRPCNotification notif) = toJSON notif

instance FromJSON JSONRPCRequestMessage where
  parseJSON = withObject "JSONRPCRequestMessage" $ \o ->
    JSONRPCRequestMessage
      <$> o .: "id"
      <*> o .: "method"
      <*> o .: "params"

instance ToJSON JSONRPCRequestMessage where
  toJSON (JSONRPCRequestMessage reqId method params) =
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= reqId,
        "method" .= method,
        "params" .= params
      ]

instance FromJSON JSONRPCResponseMessage where
  parseJSON = withObject "JSONRPCResponseMessage" $ \o ->
    JSONRPCResponseMessage
      <$> o .: "id"
      <*> o .:? "result"
      <*> o .:? "error"

instance ToJSON JSONRPCResponseMessage where
  toJSON (JSONRPCResponseMessage respId result err) =
    object $
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= respId
      ]
        ++ maybe [] (\r -> ["result" .= r]) result
        ++ maybe [] (\e -> ["error" .= e]) err

instance FromJSON JSONRPCNotificationMessage where
  parseJSON = withObject "JSONRPCNotificationMessage" $ \o ->
    JSONRPCNotificationMessage
      <$> o .: "method"
      <*> o .: "params"

instance ToJSON JSONRPCNotificationMessage where
  toJSON (JSONRPCNotificationMessage method params) =
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "method" .= method,
        "params" .= params
      ]

instance FromJSON JSONRPCError where
  parseJSON = withObject "JSONRPCError" $ \o ->
    JSONRPCError
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

instance ToJSON JSONRPCError where
  toJSON (JSONRPCError code msg dat) =
    object $
      [ "code" .= code,
        "message" .= msg
      ]
        ++ maybe [] (\d -> ["data" .= d]) dat
