{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

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
import System.IO (hFlush, stdout, stdin)
import Utils.Logging

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

-- Validate JSON-RPC Request
validateRequest :: JsonRpcRequest -> Either JsonRpcError ()
validateRequest (JsonRpcRequest version methodName _ _)
  | version /= "2.0" = Left $ JsonRpcError invalidRequest "Invalid JSON-RPC version" Nothing
  | T.null methodName = Left $ JsonRpcError invalidRequest "Missing method" Nothing
  | otherwise = Right ()

-- Safe parameter extraction with validation
extractParams :: FromJSON a => JsonRpcRequest -> Either JsonRpcError a
extractParams req = case params req of
  Nothing -> Left $ JsonRpcError invalidParams "Missing parameters" Nothing
  Just p -> case fromJSON p of
    Success val -> Right val
    Data.Aeson.Error err -> Left $ JsonRpcError invalidParams ("Parameter parsing failed: " <> T.pack err) Nothing

-- Create Success Response
createSuccessResponse :: ToJSON a => a -> Maybe Value -> JsonRpcResponse
createSuccessResponse result reqId = createResponse (Just $ toJSON result) Nothing reqId

-- Send JSON-RPC Message
sendMessage :: ToJSON a => a -> IO ()
sendMessage msg = do
  let json = encode msg
  L8.putStrLn json
  hFlush stdout

-- Read JSON-RPC Message from stdin (line-delimited)
readMessage :: IO (Either String JsonRpcRequest)
readMessage = do
  input <- getLine
  case eitherDecode (L8.pack input) of
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

    serverCapabilities =
      let caps = ServerCapabilities Nothing Nothing Nothing Nothing
      in caps { resources = Just (ResourcesCapability (Just True) (Just True))
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

      -- LSP Basic Operations
      , Tool "hover_info" (Just "Get hover information for a symbol") hoverSchema
      , Tool "goto_definition" (Just "Go to definition of a symbol") definitionSchema
      , Tool "find_references" (Just "Find all references to a symbol") referencesSchema
      , Tool "document_symbols" (Just "Get all symbols in a document") documentSymbolsSchema
      , Tool "workspace_symbols" (Just "Search for symbols in the workspace") workspaceSymbolsSchema
      , Tool "get_diagnostics" (Just "Get diagnostics for a Haskell file") diagnosticsSchema
      , Tool "format_document" (Just "Format a Haskell document") formatDocumentSchema
      , Tool "get_completions" (Just "Get code completions at a position") completionsSchema

      -- Code Actions and Refactoring
      , Tool "get_code_actions" (Just "Get available code actions for a range") codeActionsSchema
      , Tool "add_type_signature" (Just "Add missing type signature to a function") addTypeSignatureSchema
      , Tool "extend_import" (Just "Extend an import statement") extendImportSchema
      , Tool "execute_command" (Just "Execute an arbitrary HLS command") executeCommandSchema
      , Tool "retrie_refactor" (Just "Perform refactoring with retrie") retrieRefactorSchema
      , Tool "gadt_conversion" (Just "Convert ADT to GADT") gadtConversionSchema
      , Tool "expand_th_splice" (Just "Expand Template Haskell splice") expandTHSpliceSchema
      , Tool "update_module_name" (Just "Update module name to match file name") updateModuleNameSchema
      , Tool "add_cabal_dependency" (Just "Add a dependency to cabal file") addCabalDependencySchema

      -- Code Lens Operations
      , Tool "get_code_lenses" (Just "Get code lenses for a document") codeLensesSchema
      , Tool "resolve_code_lens" (Just "Resolve a code lens") resolveCodeLensSchema

      -- Advanced Operations
      , Tool "eval_expression" (Just "Evaluate a Haskell expression") evalExpressionSchema
      , Tool "organize_imports" (Just "Organize imports in a file") organizeImportsSchema
      , Tool "insert_import" (Just "Insert a new import") insertImportSchema
      , Tool "remove_unused_imports" (Just "Remove unused imports") removeUnusedImportsSchema
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

    -- LSP Basic Operation Schemas
    hoverSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "line" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Line number (0-indexed)" :: Text)
              ]
          , "character" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Character position (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "line", "character" :: Text]
      ]

    definitionSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "line" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Line number (0-indexed)" :: Text)
              ]
          , "character" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Character position (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "line", "character" :: Text]
      ]

    referencesSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "line" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Line number (0-indexed)" :: Text)
              ]
          , "character" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Character position (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "line", "character" :: Text]
      ]

    documentSymbolsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

    workspaceSymbolsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Search query for symbols" :: Text)
              ]
          ]
      , "required" .= ["query" :: Text]
      ]

    formatDocumentSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file to format" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

    completionsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "line" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Line number (0-indexed)" :: Text)
              ]
          , "character" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Character position (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "line", "character" :: Text]
      ]

    -- Code Actions and Refactoring Schemas
    codeActionsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    addTypeSignatureSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line of function (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character of function (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line of function (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character of function (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    extendImportSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line of import (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character of import (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line of import (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character of import (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    executeCommandSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("HLS command name" :: Text)
              ]
          , "arguments" .= object
              [ "type" .= ("object" :: Text)
              , "description" .= ("Command arguments" :: Text)
              ]
          ]
      , "required" .= ["command", "arguments" :: Text]
      ]

    retrieRefactorSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "lhs" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Left-hand side pattern to replace" :: Text)
              ]
          , "rhs" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Right-hand side replacement" :: Text)
              ]
          ]
      , "required" .= ["filePath", "lhs", "rhs" :: Text]
      ]

    gadtConversionSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line of data type (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character of data type (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line of data type (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character of data type (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    expandTHSpliceSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line of TH splice (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character of TH splice (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line of TH splice (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character of TH splice (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    updateModuleNameSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

    addCabalDependencySchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the cabal file or project" :: Text)
              ]
          , "dependency" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Dependency name to add" :: Text)
              ]
          ]
      , "required" .= ["filePath", "dependency" :: Text]
      ]

    -- Code Lens Schemas
    codeLensesSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

    resolveCodeLensSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "codeLens" .= object
              [ "type" .= ("object" :: Text)
              , "description" .= ("Code lens object to resolve" :: Text)
              ]
          ]
      , "required" .= ["codeLens" :: Text]
      ]

    -- Advanced Operation Schemas
    evalExpressionSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "startLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start line of expression (0-indexed)" :: Text)
              ]
          , "startChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Start character of expression (0-indexed)" :: Text)
              ]
          , "endLine" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End line of expression (0-indexed)" :: Text)
              ]
          , "endChar" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("End character of expression (0-indexed)" :: Text)
              ]
          ]
      , "required" .= ["filePath", "startLine", "startChar", "endLine", "endChar" :: Text]
      ]

    organizeImportsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

    insertImportSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          , "moduleName" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of module to import" :: Text)
              ]
          , "symbol" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Optional specific symbol to import" :: Text)
              ]
          ]
      , "required" .= ["filePath", "moduleName" :: Text]
      ]

    removeUnusedImportsSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "filePath" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to the file" :: Text)
              ]
          ]
      , "required" .= ["filePath" :: Text]
      ]

-- List Available Resources
listResources :: IO JsonRpcResponse
listResources = do
  availableResources <- getAvailableResources
  let resourcesResponse = object ["resources" .= availableResources]
  return $ createSuccessResponse resourcesResponse Nothing

-- Helper function to extract request ID safely
reqId :: JsonRpcRequest -> Maybe Value
reqId (JsonRpcRequest _ _ _ reqIdValue) = reqIdValue

-- Process Request
processRequest :: JsonRpcRequest -> IO JsonRpcResponse
processRequest req = do
  logDebug $ "Processing MCP request: " <> method req
  -- Validate the request first
  case validateRequest req of
    Left err -> do
      logError $ "Request validation failed: " <> message err
      return $ createErrorResponse (code err) (message err) (errorData err) (reqId req)
    Right _ -> do
      result <- case method req of
        "initialize" -> do
          logInfo "Processing initialize request"
          case extractParams req of
            Right initReq -> do
              logInfo "Initialize request parsed successfully"
              return $ handleInitialize initReq (reqId req)
            Left err -> do
              logError $ "Initialize parameter extraction failed: " <> message err
              return $ createErrorResponse (code err) (message err) (errorData err) (reqId req)

        "tools/list" -> do
          logInfo "Processing tools/list request"
          return listTools

        "resources/list" -> do
          logInfo "Processing resources/list request"
          listResources

        "tools/call" -> do
          logInfo "Processing tools/call request"
          case extractParams req of
            Right toolCall@(ToolCall toolName _) -> do
              logInfo $ "Calling tool: " <> toolName
              result <- handleToolCall toolCall
              logInfo $ "Tool call completed successfully: " <> toolName
              return $ createSuccessResponse result (reqId req)
            Left err -> do
              logError $ "Tool call parameter extraction failed: " <> message err
              return $ createErrorResponse (code err) (message err) (errorData err) (reqId req)

        "resources/read" -> do
          logInfo "Processing resources/read request"
          case extractParams req of
            Right obj -> case obj of
              Object o -> case KM.lookup "uri" o of
                Just (String uri) -> do
                  logInfo $ "Reading resource: " <> uri
                  result <- handleResourceRead uri
                  case result of
                    Left err -> do
                      logError $ "Failed to read resource " <> uri <> ": " <> err
                      return $ createErrorResponse internalError err Nothing (reqId req)
                    Right contents -> do
                      logInfo $ "Resource read successfully: " <> uri
                      return $ createSuccessResponse contents (reqId req)
                _ -> do
                  logError "Missing or invalid URI parameter in resource read"
                  return $ createErrorResponse invalidParams "Missing or invalid URI parameter" Nothing (reqId req)
              _ -> do
                logError "Invalid parameters for resource read"
                return $ createErrorResponse invalidParams "Invalid parameters" Nothing (reqId req)
            Left err -> do
              logError $ "Resource read parameter extraction failed: " <> message err
              return $ createErrorResponse (code err) (message err) (errorData err) (reqId req)

        _ -> do
          logWarn $ "Unknown method requested: " <> method req
          return $ createErrorResponse methodNotFound ("Unknown method: " <> method req) Nothing (reqId req)

      logDebug $ "Request processed, returning response"
      return result

-- Handle Tool Calls
handleToolCall :: ToolCall -> IO ToolResult
handleToolCall (ToolCall toolName toolArgs) = do
  logDebug $ "Handling tool call: " <> toolName
  case toolName of
    -- HLS Management Tools
    n | n `Prelude.elem` ["restart_hls_server", "start_hls_server", "stop_hls_server", "get_hls_status", "show_versions"] -> do
        logInfo $ "Executing HLS management tool: " <> n
        handleHLSTool n toolArgs

    -- LSP Basic Operations
    n | n `Prelude.elem` ["hover_info", "goto_definition", "find_references", "document_symbols", "workspace_symbols", "get_diagnostics", "format_document", "get_completions"] -> do
        logInfo $ "Executing LSP operation: " <> n
        handleHLSTool n toolArgs

    -- Code Actions and Refactoring
    n | n `Prelude.elem` ["get_code_actions", "add_type_signature", "extend_import", "execute_command", "retrie_refactor", "gadt_conversion", "expand_th_splice", "update_module_name", "add_cabal_dependency"] -> do
        logInfo $ "Executing code action/refactoring tool: " <> n
        handleHLSTool n toolArgs

    -- Code Lens Operations
    n | n `Prelude.elem` ["get_code_lenses", "resolve_code_lens"] -> do
        logInfo $ "Executing code lens operation: " <> n
        handleHLSTool n toolArgs

    -- Advanced Operations
    n | n `Prelude.elem` ["eval_expression", "organize_imports", "insert_import", "remove_unused_imports"] ->
        handleHLSTool n toolArgs

    -- Documentation Tools
    n | n `Prelude.elem` ["show_documentation", "search_haddock", "generate_docs", "browse_module_docs", "get_symbol_info"] ->
        handleDocumentationTool n toolArgs

    -- Diagnostics and Formatting Tools (legacy compatibility)
    n | n `Prelude.elem` ["format_code", "check_syntax", "hlint_suggestions"] ->
        handleDiagnosticsTool n toolArgs

    -- Unknown tool
    _ -> return $ ToolResult
      [ ToolContent "text" (Just $ "Unknown tool: " <> toolName) ]
      (Just True)
