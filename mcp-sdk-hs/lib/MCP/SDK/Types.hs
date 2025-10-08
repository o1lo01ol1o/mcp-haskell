{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.SDK.Types where

import Data.Aeson
import Data.Aeson.KeyMap (insert)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Hashable (Hashable (..))
import Data.Map.Strict (Map)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import MCP.SDK.Error (MCPError)
import MCP.SDK.Types.Auth (AuthInfo)

-- | Latest protocol version supported by this SDK.
latestProtocolVersion :: Text
latestProtocolVersion = "2025-06-18"

-- | Default negotiated version when the requested version is unsupported.
defaultNegotiatedProtocolVersion :: Text
defaultNegotiatedProtocolVersion = "2025-03-26"

-- | Ordered list of protocol versions understood by this SDK.
supportedProtocolVersions :: [Text]
supportedProtocolVersions =
  [ latestProtocolVersion
  , defaultNegotiatedProtocolVersion
  , "2024-11-05"
  , "2024-10-07"
  ]

-- | Request ID type for JSON-RPC
data RequestId
  = RequestIdText Text
  | RequestIdNumber Integer
  deriving (Eq, Show, Ord)

instance Hashable RequestId where
  hashWithSalt salt (RequestIdText t) = hashWithSalt salt (0 :: Int, t)
  hashWithSalt salt (RequestIdNumber n) = hashWithSalt salt (1 :: Int, n)

requestIdToText :: RequestId -> Text
requestIdToText (RequestIdText t) = t
requestIdToText (RequestIdNumber n) = T.pack (show n)

instance FromJSON RequestId where
  parseJSON (String t) = pure (RequestIdText t)
  parseJSON (Number n) =
    case floatingOrInteger n of
      Left (_ :: Double) -> fail "Request id must be an integer"
      Right (i :: Integer) -> pure $ RequestIdNumber i
  parseJSON v = typeMismatch "RequestId" v

instance ToJSON RequestId where
  toJSON (RequestIdText t) = String t
  toJSON (RequestIdNumber n) = Number (fromInteger n)


-- | Method names as a closed type family
data Method
  = Initialize
  | ToolsList
  | ToolsCall
  | ResourcesList
  | ResourcesRead
  | PromptsGet
  | PromptsList
  | Ping
  | Complete
  | SamplingCreateMessage
  | ElicitationCreate
  | Notification Text -- for extensibility
  deriving (Eq, Show)

-- | Type family mapping methods to their request types
type family RequestType (m :: Method) where
  RequestType 'Initialize = InitializeRequest
  RequestType 'ToolsList = ToolsListRequest
  RequestType 'ToolsCall = ToolsCallRequest
  RequestType 'ResourcesList = ResourcesListRequest
  RequestType 'ResourcesRead = ResourcesReadRequest
  RequestType 'PromptsGet = PromptsGetRequest
  RequestType 'PromptsList = PromptsListRequest
  RequestType 'Ping = PingRequest
  RequestType 'Complete = CompleteRequest
  RequestType 'SamplingCreateMessage = CreateMessageRequest
  RequestType 'ElicitationCreate = ElicitRequest

-- | Type family mapping methods to their response types
type family ResponseType (m :: Method) where
  ResponseType 'Initialize = InitializeResponse
  ResponseType 'ToolsList = ToolsListResponse
  ResponseType 'ToolsCall = ToolsCallResponse
  ResponseType 'ResourcesList = ResourcesListResponse
  ResponseType 'ResourcesRead = ResourcesReadResponse
  ResponseType 'PromptsGet = PromptsGetResponse
  ResponseType 'PromptsList = PromptsListResponse
  ResponseType 'Ping = PingResponse
  ResponseType 'Complete = CompleteResponse
  ResponseType 'SamplingCreateMessage = CreateMessageResponse
  ResponseType 'ElicitationCreate = ElicitResponse

-- | GADT ensuring type-safe request/response pairing
data MCPRequest (m :: Method) where
  InitializeReq :: InitializeRequest -> MCPRequest 'Initialize
  ToolsListReq :: ToolsListRequest -> MCPRequest 'ToolsList
  ToolsCallReq :: ToolsCallRequest -> MCPRequest 'ToolsCall
  ResourcesListReq :: ResourcesListRequest -> MCPRequest 'ResourcesList
  ResourcesReadReq :: ResourcesReadRequest -> MCPRequest 'ResourcesRead
  PromptsGetReq :: PromptsGetRequest -> MCPRequest 'PromptsGet
  PromptsListReq :: PromptsListRequest -> MCPRequest 'PromptsList
  PingReq :: PingRequest -> MCPRequest 'Ping
  CompleteReq :: CompleteRequest -> MCPRequest 'Complete
  SamplingCreateMessageReq :: CreateMessageRequest -> MCPRequest 'SamplingCreateMessage
  ElicitationCreateReq :: ElicitRequest -> MCPRequest 'ElicitationCreate

deriving instance Show (MCPRequest m)

-- | Core MCP data types
data ClientInfo = ClientInfo
  { clientName :: Text,
    clientVersion :: Text
  }
  deriving (Eq, Show)

data ServerInfo = ServerInfo
  { serverName :: Text,
    serverVersion :: Text
  }
  deriving (Eq, Show)

-- | Implementation information (alias for consistency with MCP spec)
type Implementation = ServerInfo

data Capabilities = Capabilities
  { experimental :: Maybe Object,
    sampling :: Maybe Object
  }
  deriving (Eq, Show)

-- | Request types
data InitializeRequest = InitializeRequest
  { initProtocolVersion :: Text,
    initCapabilities :: Capabilities,
    initClientInfo :: ClientInfo
  }
  deriving (Eq, Show)

data ToolsListRequest = ToolsListRequest
  { toolsCursor :: Maybe Text
  }
  deriving (Eq, Show)

data ToolsCallRequest = ToolsCallRequest
  { toolName :: Text,
    toolArguments :: Maybe Object
  }
  deriving (Eq, Show)

data ResourcesListRequest = ResourcesListRequest
  { resourcesCursor :: Maybe Text
  }
  deriving (Eq, Show)

data ResourcesReadRequest = ResourcesReadRequest
  { resourceUri :: Text
  }
  deriving (Eq, Show)

data PromptsGetRequest = PromptsGetRequest
  { promptName :: Text,
    promptGetArguments :: Maybe Object
  }
  deriving (Eq, Show)

data PromptsListRequest = PromptsListRequest
  { promptsCursor :: Maybe Text
  }
  deriving (Eq, Show)

data PingRequest = PingRequest deriving (Eq, Show)

data CompletionReference
  = PromptRef Text
  | ResourceRef Text
  deriving (Eq, Show)

data CompletionArgument = CompletionArgument
  { caName :: Text,
    caValue :: Text
  }
  deriving (Eq, Show)

data CompletionContext = CompletionContext
  { ccArguments :: Map Text Text
  }
  deriving (Eq, Show)

data CompleteRequest = CompleteRequest
  { crRef :: CompletionReference,
    crArgument :: CompletionArgument,
    crContext :: CompletionContext
  }
  deriving (Eq, Show)

data CompleteResponse = CompleteResponse
  { cresCompletions :: V.Vector Text
  }
  deriving (Eq, Show)

data CreateMessageRequest = CreateMessageRequest
  { cmrMessages :: V.Vector PromptMessage,
    cmrSamplingParams :: Maybe Object
  }
  deriving (Eq, Show)

data CreateMessageResponse = CreateMessageResponse
  { cresMessage :: PromptMessage
  }
  deriving (Eq, Show)

data ElicitRequest = ElicitRequest
  { erMessage :: Text,
    erInput :: Maybe Object
  }
  deriving (Eq, Show)

data ElicitResponse = ElicitResponse
  { eresValue :: Value
  }
  deriving (Eq, Show)

-- | Response types
data InitializeResponse = InitializeResponse
  { respProtocolVersion :: Text,
    respCapabilities :: Capabilities,
    respServerInfo :: ServerInfo,
    respInstructions :: Maybe Text
  }
  deriving (Eq, Show)

data Tool = Tool
  { toolNameField :: Text,
    toolDescription :: Maybe Text,
    toolInputSchema :: Object
  }
  deriving (Eq, Show)

data ToolHandlerContext m = ToolHandlerContext
  { elicitInput :: ElicitRequest -> m (Either MCPError ElicitResponse),
    authInfo :: Maybe AuthInfo
  }

instance Show (ToolHandlerContext m) where
  show (ToolHandlerContext _ mAuth) =
    "ToolHandlerContext {elicitInput = <function>, authInfo = "
      ++ show mAuth
      ++ "}"

-- | A `ToolDefinition` extends a `Tool` with a handler.
-- The handler is parameterized over a monad `m` to avoid circular dependencies.
-- In practice, `m` will be the `ServerM` monad from the `MCP.SDK.Server` module.
data ToolDefinition m = ToolDefinition
  { tdTool :: Tool,
    tdHandler :: Maybe (ToolHandlerContext m -> Maybe Object -> m (Either MCPError ToolsCallResponse))
  }

-- Note: Cannot derive Eq because of the function type in tdHandler.
instance Show (ToolDefinition m) where
  show (ToolDefinition tool mHandler) =
    "ToolDefinition {tdTool = "
      ++ show tool
      ++ ", tdHandler = "
      ++ case mHandler of
        Nothing -> "Nothing"
        Just _ -> "Just <function>"
      ++ "}"

data ToolsListResponse = ToolsListResponse
  { tools :: V.Vector Tool,
    nextCursor :: Maybe Text
  }
  deriving (Eq, Show)

data ToolCallResult = ToolCallResult
  { toolCallContent :: V.Vector Content,
    toolCallIsError :: Maybe Bool
  }
  deriving (Eq, Show)

data ToolsCallResponse = ToolsCallResponse
  { toolCallResult :: ToolCallResult
  }
  deriving (Eq, Show)

data Resource = Resource
  { resourceUriField :: Text,
    resourceName :: Text,
    resourceDescription :: Maybe Text,
    resourceMimeType :: Maybe Text
  }
  deriving (Eq, Show)

type CompletionHandler m = Map Text Text -> Text -> m [Text]

-- | A template for generating resources dynamically based on a URI pattern.
data ResourceTemplate m = ResourceTemplate
  { rtPattern :: Text,
    rtHandler :: Map Text Text -> m (Either MCPError ResourcesReadResponse),
    rtCompleters :: Map Text (CompletionHandler m)
  }

-- Note: Cannot derive Eq because of the function type in rtHandler.
instance Show (ResourceTemplate m) where
  show (ResourceTemplate pattern _handler _completers) =
    "ResourceTemplate {rtPattern = "
      ++ show pattern
      ++ ", rtHandler = <function>, rtCompleters = <functions>}"

data ResourcesListResponse = ResourcesListResponse
  { resources :: V.Vector Resource,
    resourcesNextCursor :: Maybe Text
  }
  deriving (Eq, Show)

data ResourcesReadResponse = ResourcesReadResponse
  { resourceContents :: V.Vector Content
  }
  deriving (Eq, Show)

data Prompt = Prompt
  { promptNameField :: Text,
    promptDescription :: Maybe Text,
    promptArguments :: Maybe (V.Vector PromptArgument)
  }
  deriving (Eq, Show)

data PromptArgument = PromptArgument
  { argName :: Text,
    argDescription :: Maybe Text,
    argRequired :: Maybe Bool
  }
  deriving (Eq, Show)

-- | A `PromptDefinition` extends a `Prompt` with the messages that constitute
-- its content. This allows for dynamic substitution of arguments into the
-- content when a prompt is requested.
data PromptDefinition m = PromptDefinition
  { pdPrompt :: Prompt,
    pdMessages :: V.Vector PromptMessage,
    pdCompleters :: Map Text (CompletionHandler m)
  }

instance Show (PromptDefinition m) where
  show (PromptDefinition prompt messages _completers) =
    "PromptDefinition {pdPrompt = "
      ++ show prompt
      ++ ", pdMessages = "
      ++ show messages
      ++ ", pdCompleters = <functions>}"

data PromptsListResponse = PromptsListResponse
  { prompts :: V.Vector Prompt,
    promptsNextCursor :: Maybe Text
  }
  deriving (Eq, Show)

data PromptMessage = PromptMessage
  { msgRole :: Role,
    msgContent :: Content
  }
  deriving (Eq, Show)

data PromptsGetResponse = PromptsGetResponse
  { promptResponseDescription :: Maybe Text,
    promptMessages :: V.Vector PromptMessage
  }
  deriving (Eq, Show)

data PingResponse = PingResponse deriving (Eq, Show)

-- | Content types
data Content
  = TextContent Text
  | ImageContent ImageData
  | ResourceContent ResourceRefContent
  deriving (Eq, Show)

data ImageData = ImageData
  { imageData :: Text,
    imageMimeType :: Text
  }
  deriving (Eq, Show)

data ResourceRefContent = ResourceRefContent
  { refUri :: Text
  }
  deriving (Eq, Show)

data Role = User | Assistant | System
  deriving (Eq, Show)

-- JSON instances
instance FromJSON ImageData where
  parseJSON = withObject "ImageData" $ \o ->
    ImageData
      <$> o .: "data"
      <*> o .: "mimeType"

instance ToJSON ImageData where
  toJSON (ImageData dat mime) =
    object
      [ "data" .= dat,
        "mimeType" .= mime
      ]

instance FromJSON ResourceRefContent where
  parseJSON = withObject "ResourceRefContent" $ \o ->
    ResourceRefContent
      <$> o .: "uri"

instance ToJSON ResourceRefContent where
  toJSON (ResourceRefContent uri) =
    object
      [ "uri" .= uri
      ]

instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \o ->
    ClientInfo
      <$> o .: "name"
      <*> o .: "version"

instance ToJSON ClientInfo where
  toJSON (ClientInfo name version) =
    object
      [ "name" .= name,
        "version" .= version
      ]

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o ->
    ServerInfo
      <$> o .: "name"
      <*> o .: "version"

instance ToJSON ServerInfo where
  toJSON (ServerInfo name version) =
    object
      [ "name" .= name,
        "version" .= version
      ]

instance FromJSON Capabilities where
  parseJSON = withObject "Capabilities" $ \o ->
    Capabilities
      <$> o .:? "experimental"
      <*> o .:? "sampling"

instance ToJSON Capabilities where
  toJSON (Capabilities experimental sampling) =
    object
      [ "experimental" .= experimental,
        "sampling" .= sampling
      ]

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    "system" -> pure System
    _ -> fail "Invalid role"

instance ToJSON Role where
  toJSON User = "user"
  toJSON Assistant = "assistant"
  toJSON System = "system"

instance FromJSON Content where
  parseJSON = withObject "Content" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text" -> TextContent <$> o .: "text"
      "image" -> ImageContent <$> parseJSON (Object o)
      "resource" -> ResourceContent <$> parseJSON (Object o)
      _ -> fail "Unknown content type"

instance ToJSON Content where
  toJSON (TextContent text) = object ["type" .= ("text" :: Text), "text" .= text]
  toJSON (ImageContent img) = case toJSON img of
    Object imgObj -> Object $ insert "type" "image" imgObj
    _ -> error "ImageData should serialize to object"
  toJSON (ResourceContent ref) = case toJSON ref of
    Object refObj -> Object $ insert "type" "resource" refObj
    _ -> error "ResourceRefContent should serialize to object"

-- Additional JSON instances for request/response types
instance FromJSON InitializeRequest where
  parseJSON = withObject "InitializeRequest" $ \o ->
    InitializeRequest
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "clientInfo"

instance ToJSON InitializeRequest where
  toJSON (InitializeRequest pv caps info) =
    object
      [ "protocolVersion" .= pv,
        "capabilities" .= caps,
        "clientInfo" .= info
      ]

instance FromJSON InitializeResponse where
  parseJSON = withObject "InitializeResponse" $ \o ->
    InitializeResponse
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "serverInfo"
      <*> o .:? "instructions"

instance ToJSON InitializeResponse where
  toJSON (InitializeResponse pv caps info instructions) =
    object
      ([ "protocolVersion" .= pv,
         "capabilities" .= caps,
         "serverInfo" .= info
       ] ++ maybe [] (\instr -> ["instructions" .= instr]) instructions)

instance FromJSON ToolsListRequest where
  parseJSON Null = pure (ToolsListRequest Nothing)
  parseJSON v =
    withObject "ToolsListRequest" (
      \o ->
        ToolsListRequest
          <$> o .:? "cursor"
      ) v

instance ToJSON ToolsListRequest where
  toJSON (ToolsListRequest cursor) =
    object
      [ "cursor" .= cursor
      ]

instance FromJSON PingRequest where
  parseJSON = withObject "PingRequest" $ \_ -> pure PingRequest

instance ToJSON PingRequest where
  toJSON PingRequest = object []

instance FromJSON CompletionReference where
  parseJSON = withObject "CompletionReference" $ \o -> do
    typ <- o .: "type"
    case typ of
      ("prompt" :: Text) -> PromptRef <$> o .: "name"
      ("resource" :: Text) -> ResourceRef <$> o .: "name"
      _ -> fail "Invalid completion reference type"

instance ToJSON CompletionReference where
  toJSON (PromptRef name) = object ["type" .= ("prompt" :: Text), "name" .= name]
  toJSON (ResourceRef name) = object ["type" .= ("resource" :: Text), "name" .= name]

instance FromJSON CompletionArgument where
  parseJSON = withObject "CompletionArgument" $ \o ->
    CompletionArgument <$> o .: "name" <*> o .: "value"

instance ToJSON CompletionArgument where
  toJSON (CompletionArgument name value) = object ["name" .= name, "value" .= value]

instance FromJSON CompletionContext where
  parseJSON = withObject "CompletionContext" $ \o ->
    CompletionContext <$> o .: "arguments"

instance ToJSON CompletionContext where
  toJSON (CompletionContext args) = object ["arguments" .= args]

instance FromJSON CompleteRequest where
  parseJSON = withObject "CompleteRequest" $ \o ->
    CompleteRequest
      <$> o .: "ref"
      <*> o .: "argument"
      <*> o .: "context"

instance ToJSON CompleteRequest where
  toJSON (CompleteRequest ref arg ctx) =
    object ["ref" .= ref, "argument" .= arg, "context" .= ctx]

instance FromJSON CompleteResponse where
  parseJSON = withObject "CompleteResponse" $ \o ->
    CompleteResponse <$> o .: "completions"

instance ToJSON CompleteResponse where
  toJSON (CompleteResponse completions) = object ["completions" .= completions]

instance FromJSON CreateMessageRequest where
  parseJSON = withObject "CreateMessageRequest" $ \o ->
    CreateMessageRequest
      <$> o .: "messages"
      <*> o .:? "sampling"

instance ToJSON CreateMessageRequest where
  toJSON (CreateMessageRequest messages sampling) =
    object ["messages" .= messages, "sampling" .= sampling]

instance FromJSON CreateMessageResponse where
  parseJSON = withObject "CreateMessageResponse" $ \o ->
    CreateMessageResponse <$> o .: "message"

instance ToJSON CreateMessageResponse where
  toJSON (CreateMessageResponse message) = object ["message" .= message]

instance FromJSON ElicitRequest where
  parseJSON = withObject "ElicitRequest" $ \o ->
    ElicitRequest
      <$> o .: "message"
      <*> o .:? "input"

instance ToJSON ElicitRequest where
  toJSON (ElicitRequest message input) =
    object ["message" .= message, "input" .= input]

instance FromJSON ElicitResponse where
  parseJSON = withObject "ElicitResponse" $ \o ->
    ElicitResponse <$> o .: "value"

instance ToJSON ElicitResponse where
  toJSON (ElicitResponse value) = object ["value" .= value]

instance FromJSON PromptMessage where
  parseJSON = withObject "PromptMessage" $ \o ->
    PromptMessage
      <$> o .: "role"
      <*> o .: "content"

instance ToJSON PromptMessage where
  toJSON (PromptMessage role content) =
    object ["role" .= role, "content" .= content]

instance FromJSON ToolsListResponse where
  parseJSON = withObject "ToolsListResponse" $ \o ->
    ToolsListResponse
      <$> o .: "tools"
      <*> o .:? "nextCursor"

instance ToJSON ToolsListResponse where
  toJSON (ToolsListResponse tools cursor) =
    object ["tools" .= tools, "nextCursor" .= cursor]

instance FromJSON ToolsCallResponse where
  parseJSON = withObject "ToolsCallResponse" $ \o -> do
    content <- o .: "content"
    isError <- o .:? "isError"
    pure $ ToolsCallResponse (ToolCallResult content isError)

instance ToJSON ToolsCallResponse where
  toJSON (ToolsCallResponse (ToolCallResult content isError)) =
    object $
      [ "content" .= content
      ] ++ maybe [] (\flag -> ["isError" .= flag]) isError

instance FromJSON ResourcesListResponse where
  parseJSON = withObject "ResourcesListResponse" $ \o ->
    ResourcesListResponse
      <$> o .: "resources"
      <*> o .:? "nextCursor"

instance ToJSON ResourcesListResponse where
  toJSON (ResourcesListResponse resources cursor) =
    object ["resources" .= resources, "nextCursor" .= cursor]

instance FromJSON ResourcesReadResponse where
  parseJSON = withObject "ResourcesReadResponse" $ \o ->
    ResourcesReadResponse <$> o .: "contents"

instance ToJSON ResourcesReadResponse where
  toJSON (ResourcesReadResponse contents) = object ["contents" .= contents]

instance FromJSON PromptsListResponse where
  parseJSON = withObject "PromptsListResponse" $ \o ->
    PromptsListResponse
      <$> o .: "prompts"
      <*> o .:? "nextCursor"

instance ToJSON PromptsListResponse where
  toJSON (PromptsListResponse prompts cursor) =
    object ["prompts" .= prompts, "nextCursor" .= cursor]

instance FromJSON PromptsGetResponse where
  parseJSON = withObject "PromptsGetResponse" $ \o ->
    PromptsGetResponse
      <$> o .:? "description"
      <*> o .: "messages"

instance ToJSON PromptsGetResponse where
  toJSON (PromptsGetResponse desc messages) =
    object ["description" .= desc, "messages" .= messages]

instance FromJSON PingResponse where
  parseJSON = withObject "PingResponse" $ \_ -> pure PingResponse

instance ToJSON PingResponse where
  toJSON PingResponse = object []

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o ->
    Tool
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .: "inputSchema"

instance ToJSON Tool where
  toJSON (Tool name desc schema) =
    object ["name" .= name, "description" .= desc, "inputSchema" .= schema]

instance FromJSON ToolCallResult where
  parseJSON = withObject "ToolCallResult" $ \o ->
    ToolCallResult
      <$> o .: "content"
      <*> o .:? "isError"

instance ToJSON ToolCallResult where
  toJSON (ToolCallResult content isError) =
    object ["content" .= content, "isError" .= isError]

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o ->
    Resource
      <$> o .: "uri"
      <*> o .: "name"
      <*> o .:? "description"
      <*> o .:? "mimeType"

instance ToJSON Resource where
  toJSON (Resource uri name desc mime) =
    object ["uri" .= uri, "name" .= name, "description" .= desc, "mimeType" .= mime]

instance FromJSON Prompt where
  parseJSON = withObject "Prompt" $ \o ->
    Prompt
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "arguments"

instance ToJSON Prompt where
  toJSON (Prompt name desc args) =
    object ["name" .= name, "description" .= desc, "arguments" .= args]

instance FromJSON PromptArgument where
  parseJSON = withObject "PromptArgument" $ \o ->
    PromptArgument
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "required"

instance ToJSON PromptArgument where
  toJSON (PromptArgument name desc req) =
    object ["name" .= name, "description" .= desc, "required" .= req]

instance FromJSON ToolsCallRequest where
  parseJSON = withObject "ToolsCallRequest" $ \o ->
    ToolsCallRequest
      <$> o .: "name"
      <*> o .:? "arguments"

instance ToJSON ToolsCallRequest where
  toJSON (ToolsCallRequest name args) =
    object ["name" .= name, "arguments" .= args]

instance FromJSON ResourcesListRequest where
  parseJSON = withObject "ResourcesListRequest" $ \o ->
    ResourcesListRequest <$> o .:? "cursor"

instance ToJSON ResourcesListRequest where
  toJSON (ResourcesListRequest cursor) =
    object ["cursor" .= cursor]

instance FromJSON ResourcesReadRequest where
  parseJSON = withObject "ResourcesReadRequest" $ \o ->
    ResourcesReadRequest <$> o .: "uri"

instance ToJSON ResourcesReadRequest where
  toJSON (ResourcesReadRequest uri) =
    object ["uri" .= uri]

instance FromJSON PromptsListRequest where
  parseJSON = withObject "PromptsListRequest" $ \o ->
    PromptsListRequest <$> o .:? "cursor"

instance ToJSON PromptsListRequest where
  toJSON (PromptsListRequest cursor) =
    object ["cursor" .= cursor]

instance FromJSON PromptsGetRequest where
  parseJSON = withObject "PromptsGetRequest" $ \o ->
    PromptsGetRequest
      <$> o .: "name"
      <*> o .:? "arguments"

instance ToJSON PromptsGetRequest where
  toJSON (PromptsGetRequest name args) =
    object ["name" .= name, "arguments" .= args]

-- Add similar instances for all other types...
