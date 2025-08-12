{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.Types where

import Data.Aeson
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show (GShow (..))
import Data.GADT.Show.TH (deriveGShow)
import Data.Kind (Type)
import Data.Some (Some (..))
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import GHC.Generics (Generic)

-- MCP Protocol Version (2025 specification)
mcpVersion :: Text
mcpVersion = "2025-03-26"

-- JSON-RPC 2.0 Message Types
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text,
    method :: Text,
    params :: Maybe Value,
    id :: Maybe Value
  }
  deriving (Generic, Show, Eq)

data JsonRpcResponse = JsonRpcResponse
  { jsonrpc :: Text,
    result :: Maybe Value,
    error :: Maybe JsonRpcError,
    id :: Maybe Value
  }
  deriving (Generic, Show, Eq)

data JsonRpcError = JsonRpcError
  { code :: Int,
    message :: Text,
    errorData :: Maybe Value
  }
  deriving (Generic, Show, Eq)

instance FromJSON JsonRpcRequest where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON JsonRpcRequest where
  toJSON = genericToJSON defaultOptions

instance FromJSON JsonRpcResponse where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON JsonRpcResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o ->
    JsonRpcError
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

-- MCP Result Types for GADT (simplified to avoid forward references)
data InitializeResult = InitializeResult
  { protocolVersion :: Text,
    capabilities :: Value,
    serverInfo :: Value
  }
  deriving (Generic, Show, Eq)

data ListToolsResult = ListToolsResult
  { tools :: Value
  }
  deriving (Generic, Show, Eq)

data ListResourcesResult = ListResourcesResult
  { resources :: Value
  }
  deriving (Generic, Show, Eq)

instance ToJSON JsonRpcError where
  toJSON (JsonRpcError c m d) =
    object
      [ "code" .= c,
        "message" .= m,
        "data" .= d
      ]

-- MCP Capability Types
data ServerCapabilities = ServerCapabilities
  { logging :: Maybe LoggingCapability,
    prompts :: Maybe PromptsCapability,
    resources :: Maybe ResourcesCapability,
    tools :: Maybe ToolsCapability
  }
  deriving (Generic, Show, Eq)

data LoggingCapability = LoggingCapability deriving (Generic, Show, Eq)

data PromptsCapability = PromptsCapability deriving (Generic, Show, Eq)

data ResourcesCapability = ResourcesCapability
  { subscribe :: Maybe Bool,
    listChanged :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

data ToolsCapability = ToolsCapability
  { listChanged :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

instance FromJSON ServerCapabilities where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ServerCapabilities where
  toJSON = genericToJSON defaultOptions

instance FromJSON LoggingCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON LoggingCapability where
  toJSON = genericToJSON defaultOptions

instance FromJSON PromptsCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PromptsCapability where
  toJSON = genericToJSON defaultOptions

instance FromJSON ResourcesCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourcesCapability where
  toJSON = genericToJSON defaultOptions

instance FromJSON ToolsCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ToolsCapability where
  toJSON = genericToJSON defaultOptions

-- MCP Initialize Request/Response
data InitializeRequest = InitializeRequest
  { protocolVersion :: Text,
    capabilities :: ClientCapabilities,
    clientInfo :: ClientInfo
  }
  deriving (Generic, Show, Eq, Ord)

data ClientCapabilities = ClientCapabilities
  { roots :: Maybe RootsCapability,
    sampling :: Maybe SamplingCapability
  }
  deriving (Generic, Show, Eq, Ord)

data RootsCapability = RootsCapability
  { listChanged :: Maybe Bool
  }
  deriving (Generic, Show, Eq, Ord)

data SamplingCapability = SamplingCapability deriving (Generic, Show, Eq, Ord)

data ClientInfo = ClientInfo
  { name :: Text,
    version :: Text
  }
  deriving (Generic, Show, Eq, Ord)

instance FromJSON InitializeRequest where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON InitializeRequest where
  toJSON = genericToJSON defaultOptions

instance FromJSON ClientCapabilities where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ClientCapabilities where
  toJSON = genericToJSON defaultOptions

instance FromJSON RootsCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON RootsCapability where
  toJSON = genericToJSON defaultOptions

instance FromJSON SamplingCapability where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON SamplingCapability where
  toJSON = genericToJSON defaultOptions

instance FromJSON ClientInfo where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ClientInfo where
  toJSON = genericToJSON defaultOptions

-- MCP Tool Types
data Tool = Tool
  { name :: Text,
    description :: Maybe Text,
    inputSchema :: Value
  }
  deriving (Generic, Show, Eq)

instance FromJSON Tool where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Tool where
  toJSON = genericToJSON defaultOptions

data ToolCall = ToolCall
  { name :: Text,
    arguments :: Maybe Value
  }
  deriving (Generic, Show, Eq)

instance FromJSON ToolCall where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ToolCall where
  toJSON = genericToJSON defaultOptions

data ToolResult = ToolResult
  { content :: [ToolContent],
    isError :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

data ToolContent = ToolContent
  { contentType :: Text,
    text :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON ToolResult where
  parseJSON = withObject "ToolResult" $ \o ->
    ToolResult
      <$> o .: "content"
      <*> o .:? "isError"

instance ToJSON ToolResult where
  toJSON (ToolResult c e) =
    object
      [ "content" .= c,
        "isError" .= e
      ]

instance FromJSON ToolContent where
  parseJSON = withObject "ToolContent" $ \o ->
    ToolContent
      <$> o .: "type"
      <*> o .:? "text"

instance ToJSON ToolContent where
  toJSON (ToolContent t txt) =
    object
      [ "type" .= t,
        "text" .= txt
      ]

-- MCP Resource Types
data Resource = Resource
  { uri :: Text,
    name :: Text,
    description :: Maybe Text,
    mimeType :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Resource where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Resource where
  toJSON = genericToJSON defaultOptions

data ResourceContents = ResourceContents
  { contents :: [ResourceContent]
  }
  deriving (Generic, Show, Eq)

data ResourceContent = ResourceContent
  { uri :: Text,
    mimeType :: Maybe Text,
    text :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON ResourceContents where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourceContents where
  toJSON = genericToJSON defaultOptions

instance FromJSON ResourceContent where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ResourceContent where
  toJSON = genericToJSON defaultOptions

-- Haskell-specific Types
data HLSStatus = Running | Stopped | Error Text
  deriving (Generic, Show, Eq)

instance FromJSON HLSStatus where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON HLSStatus where
  toJSON = genericToJSON defaultOptions

data ProjectInfo = ProjectInfo
  { projectRoot :: FilePath,
    cabalFiles :: [FilePath],
    stackFiles :: [FilePath],
    haskellFiles :: [FilePath]
  }
  deriving (Generic, Show, Eq)

instance FromJSON ProjectInfo where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ProjectInfo where
  toJSON = genericToJSON defaultOptions

-- JSON instances for result types
instance FromJSON InitializeResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON InitializeResult where
  toJSON = genericToJSON defaultOptions

instance FromJSON ListToolsResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ListToolsResult where
  toJSON = genericToJSON defaultOptions

instance FromJSON ListResourcesResult where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON ListResourcesResult where
  toJSON = genericToJSON defaultOptions

-- GADT-based MCP Request Protocol
data McpRequest :: Type -> Type where
  -- HLS Management
  GetHlsStatus :: McpRequest HLSStatus
  RestartHlsServer :: McpRequest HLSStatus
  StartHlsServer :: Maybe FilePath -> McpRequest HLSStatus
  StopHlsServer :: McpRequest HLSStatus
  -- LSP Operations
  HoverInfo :: FilePath -> Int -> Int -> McpRequest (Either Text Text)
  GotoDefinition :: FilePath -> Int -> Int -> McpRequest (Either Text [Text])
  FindReferences :: FilePath -> Int -> Int -> McpRequest (Either Text [Text])
  GetDocumentSymbols :: FilePath -> McpRequest (Either Text [Text])
  GetWorkspaceSymbols :: Text -> McpRequest (Either Text [Text])
  GetCompletions :: FilePath -> Int -> Int -> McpRequest (Either Text [Text])
  GetCodeActions :: FilePath -> Int -> Int -> Int -> Int -> McpRequest (Either Text [Text])
  GetCodeLenses :: FilePath -> McpRequest (Either Text [Text])
  FormatDocument :: FilePath -> McpRequest (Either Text Text)
  GetFileDiagnostics :: FilePath -> McpRequest (Either Text [Text])
  ExecuteCommand :: Text -> FilePath -> Int -> Int -> Int -> Int -> McpRequest (Either Text Text)
  OrganizeImports :: FilePath -> McpRequest (Either Text Text)
  InsertImport :: FilePath -> Text -> Maybe Text -> McpRequest (Either Text Text)
  RemoveUnusedImports :: FilePath -> McpRequest (Either Text Text)
  -- MCP Protocol
  Initialize :: InitializeRequest -> McpRequest InitializeResult
  ListTools :: McpRequest ListToolsResult
  ListResources :: McpRequest ListResourcesResult
  ReadResource :: Text -> McpRequest ResourceContents
  -- Version and Health
  GetVersion :: McpRequest Text

deriving instance Show (McpRequest a)

deriving instance Eq (McpRequest a)



-- Generate instances using Template Haskell
$(deriveGShow ''McpRequest)
$(deriveGEq ''McpRequest)
$(deriveGCompare ''McpRequest)
