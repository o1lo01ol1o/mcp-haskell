{-# LANGUAGE OverloadedStrings #-}

module MCP.Protocol where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Types
import MCP.Resources
import MCP.Tools.HLS
import MCP.Tools.Documentation
import MCP.Tools.Diagnostics
import System.IO

-- Protocol Error Codes
parseError, invalidRequest, methodNotFound, invalidParams, internalError :: Int
parseError = -32700
invalidRequest = -32600
methodNotFound = -32601
invalidParams = -32602
internalError = -32603

-- Create JSON-RPC Response
createResponse :: Maybe Value -> Maybe JsonRpcError -> Maybe Value -> JsonRpcResponse
createResponse result err reqId = JsonRpcResponse "2.0" result err reqId

-- Create JSON-RPC Error Response
createErrorResponse :: Int -> Text -> Maybe Value -> Maybe Value -> JsonRpcResponse
createErrorResponse code msg errData reqId = createResponse Nothing (Just jsonRpcErr) reqId
  where
    jsonRpcErr = JsonRpcError code msg errData

-- Create Success Response
createSuccessResponse :: ToJSON a => a -> Maybe Value -> JsonRpcResponse
createSuccessResponse result reqId = createResponse (Just $ toJSON result) Nothing reqId

-- Send JSON-RPC Message
sendMessage :: ToJSON a => a -> IO ()
sendMessage msg = do
  let json = encode msg
  L8.putStr json
  hFlush stdout

-- Read JSON-RPC Message from stdin
readMessage :: IO (Either String JsonRpcRequest)
readMessage = do
  input <- L8.getContents
  case eitherDecode input of
    Left err -> return $ Left err
    Right req -> return $ Right req

-- Handle Initialize Request
handleInitialize :: InitializeRequest -> Maybe Value -> JsonRpcResponse
handleInitialize _initReq reqId = createSuccessResponse initResponse reqId
  where
    initResponse = object
      [ "protocolVersion" .= mcpVersion
      , "capabilities" .= serverCapabilities
      , "serverInfo" .= serverInfo
      ]
    
    serverCapabilities = ServerCapabilities
      { logging = Nothing
      , prompts = Nothing
      , resources = Just (ResourcesCapability (Just True) (Just True))
      , tools = Just (ToolsCapability (Just True))
      }
    
    serverInfo = object
      [ "name" .= ("mcp-hls" :: Text)
      , "version" .= ("0.1.0.0" :: Text)
      ]

-- List Available Tools
listTools :: JsonRpcResponse
listTools = createSuccessResponse toolsResponse Nothing
  where
    toolsResponse = object ["tools" .= availableTools]
    
    availableTools = 
      [ -- HLS Management Tools
        Tool "restart_hls_server" (Just "Restart the Haskell Language Server") restartSchema
      , Tool "start_hls_server" (Just "Start the Haskell Language Server") startSchema
      , Tool "stop_hls_server" (Just "Stop the Haskell Language Server") stopSchema
      , Tool "get_hls_status" (Just "Get HLS server status") statusSchema
      , Tool "show_versions" (Just "Show version information") versionSchema
      
      -- Code Formatting and Analysis Tools
      , Tool "format_code" (Just "Format Haskell code with various formatters") formatSchema
      , Tool "get_diagnostics" (Just "Get diagnostics for a Haskell file") diagnosticsSchema
      , Tool "check_syntax" (Just "Check syntax of a Haskell file") syntaxSchema
      , Tool "hlint_suggestions" (Just "Get HLint suggestions for a file") hlintSchema
      
      -- Documentation Tools
      , Tool "show_documentation" (Just "Show documentation for a module") docSchema
      , Tool "search_haddock" (Just "Search in Haddock documentation") searchSchema
      , Tool "generate_docs" (Just "Generate Haddock documentation for project") genDocsSchema
      , Tool "browse_module_docs" (Just "Browse symbols in a module") browseSchema
      , Tool "get_symbol_info" (Just "Get detailed information about a symbol") symbolSchema
      ]
    
    restartSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      ]
    
    startSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "workingDir" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Working directory for HLS" :: Text)
              ]
          ]
      ]
    
    stopSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      ]
    
    statusSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      ]
    
    versionSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      ]
    
    formatSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the Haskell file to format" :: Text)
              ]
          , "formatter" .= object
              [ "type" .= ("string" :: Text)
              , "enum" .= ["ormolu", "fourmolu", "brittany", "stylish-haskell" :: Text]
              , "description" .= ("Formatter to use" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]
    
    diagnosticsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the Haskell file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]
    
    syntaxSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the Haskell file to check" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]
    
    hlintSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the Haskell file for HLint analysis" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]
    
    docSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "moduleName" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the module to show documentation for" :: Text)
              ]
          ]
      , "required" .= ["moduleName" :: Text]
      ]
    
    searchSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Search query for Haddock documentation" :: Text)
              ]
          ]
      , "required" .= ["query" :: Text]
      ]
    
    genDocsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "projectPath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the project root directory" :: Text)
              ]
          ]
      , "required" .= ["projectPath" :: Text]
      ]
    
    browseSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "moduleName" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the module to browse" :: Text)
              ]
          ]
      , "required" .= ["moduleName" :: Text]
      ]
    
    symbolSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "symbol" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Symbol name to get information for" :: Text)
              ]
          , "moduleName" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Optional module name to search in" :: Text)
              ]
          ]
      , "required" .= ["symbol" :: Text]
      ]

-- List Available Resources
listResources :: IO JsonRpcResponse
listResources = do
  availableResources <- getAvailableResources
  let resourcesResponse = object ["resources" .= availableResources]
  return $ createSuccessResponse resourcesResponse Nothing

-- Process Request
processRequest :: JsonRpcRequest -> IO JsonRpcResponse
processRequest req = case method req of
  "initialize" -> do
    case fromJSON <$> params req of
      Just (Success initReq) -> return $ handleInitialize initReq Nothing
      Just (Data.Aeson.Error _) -> return $ createErrorResponse invalidParams "Invalid initialize parameters" Nothing Nothing
      Nothing -> return $ createErrorResponse invalidParams "Missing initialize parameters" Nothing Nothing
  
  "tools/list" -> return listTools
  
  "resources/list" -> listResources
  
  "tools/call" -> do
    case fromJSON <$> params req of
      Just (Success toolCall) -> do
        result <- handleToolCall toolCall
        return $ createSuccessResponse result Nothing
      Just (Data.Aeson.Error _) -> return $ createErrorResponse invalidParams "Invalid tool call parameters" Nothing Nothing
      Nothing -> return $ createErrorResponse invalidParams "Missing tool call parameters" Nothing Nothing
  
  "resources/read" -> do
    case fromJSON <$> params req of
      Just (Success obj) -> case obj of
        Object o -> case KM.lookup "uri" o of
          Just (String uri) -> do
            result <- handleResourceRead uri
            case result of
              Left err -> return $ createErrorResponse internalError err Nothing Nothing
              Right contents -> return $ createSuccessResponse contents Nothing
          _ -> return $ createErrorResponse invalidParams "Missing or invalid URI parameter" Nothing Nothing
        _ -> return $ createErrorResponse invalidParams "Invalid parameters" Nothing Nothing
      Just (Data.Aeson.Error _) -> return $ createErrorResponse invalidParams "Invalid resource read parameters" Nothing Nothing
      Nothing -> return $ createErrorResponse invalidParams "Missing resource read parameters" Nothing Nothing
  
  _ -> return $ createErrorResponse methodNotFound ("Unknown method: " <> method req) Nothing Nothing

-- Handle Tool Calls
handleToolCall :: ToolCall -> IO ToolResult
handleToolCall (ToolCall toolName toolArgs) = 
  case toolName of
    -- HLS Management Tools
    n | n `Prelude.elem` ["restart_hls_server", "start_hls_server", "stop_hls_server", "get_hls_status", "show_versions"] ->
        handleHLSTool n toolArgs
    
    -- Documentation Tools  
    n | n `Prelude.elem` ["show_documentation", "search_haddock", "generate_docs", "browse_module_docs", "get_symbol_info"] ->
        handleDocumentationTool n toolArgs
    
    -- Diagnostics and Formatting Tools
    n | n `Prelude.elem` ["get_diagnostics", "format_code", "check_syntax", "hlint_suggestions"] ->
        handleDiagnosticsTool n toolArgs
    
    -- Unknown tool
    _ -> return $ ToolResult
      [ ToolContent "text" (Just $ "Unknown tool: " <> toolName) ]
      (Just True)