{-# LANGUAGE OverloadedStrings #-}

module MCP.Tools.HLS 
  ( handleHLSTool
  , getHLSStatus 
  , hlsProcessVar
  , restartHLSServer
  , startHLSServer
  , stopHLSServer
  ) where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified MCP.Types as Types

-- Use Types.HLSStatus from MCP.Types

-- Missing variables that are imported elsewhere
hlsProcessVar :: ()
hlsProcessVar = ()

-- Missing function implementations - These should be moved to HLS.Client eventually  
restartHLSServer :: IO (Either Text Text)
restartHLSServer = return $ Left "HLS restart not implemented"

startHLSServer :: Maybe FilePath -> IO (Either Text Text)
startHLSServer _workDir = return $ Left "HLS start not implemented"

stopHLSServer :: IO (Either Text Text)
stopHLSServer = return $ Left "HLS stop not implemented"

getHLSStatus :: IO Types.HLSStatus
getHLSStatus = return Types.Stopped

getHLSVersion :: IO (Either Text Text)
getHLSVersion = return $ Right "Unknown version"

getHoverInfo :: FilePath -> Int -> Int -> IO (Either Text Text)
getHoverInfo _filePath _line _char = return $ Left "Hover info not implemented"

gotoDefinition :: FilePath -> Int -> Int -> IO (Either Text Text)
gotoDefinition _filePath _line _char = return $ Left "Goto definition not implemented"

findReferences :: FilePath -> Int -> Int -> IO (Either Text Text)
findReferences _filePath _line _char = return $ Left "Find references not implemented"

getDocumentSymbols :: FilePath -> IO (Either Text Text)
getDocumentSymbols _filePath = return $ Left "Document symbols not implemented"

getWorkspaceSymbols :: Text -> IO (Either Text Text)
getWorkspaceSymbols _query = return $ Left "Workspace symbols not implemented"

getFileDiagnostics :: FilePath -> IO (Either Text Text)
getFileDiagnostics _filePath = return $ Left "File diagnostics not implemented"

formatDocument :: FilePath -> IO (Either Text Text)
formatDocument _filePath = return $ Left "Format document not implemented"

getCompletions :: FilePath -> Int -> Int -> IO (Either Text Text)
getCompletions _filePath _line _char = return $ Left "Completions not implemented"

getCodeLenses :: FilePath -> IO (Either Text Text)
getCodeLenses _filePath = return $ Left "Code lenses not implemented"

resolveCodeLens :: Value -> IO (Either Text Text)
resolveCodeLens _codeLensData = return $ Left "Code lens resolve not implemented"

getCodeActions :: FilePath -> Int -> Int -> Int -> Int -> IO (Either Text Text)
getCodeActions _filePath _startLine _startChar _endLine _endChar = return $ Left "Code actions not implemented"

executeHLSCommand :: Text -> FilePath -> Int -> Int -> Int -> Int -> IO (Either Text Text)
executeHLSCommand _command _filePath _startLine _startChar _endLine _endChar = return $ Left "Execute command not implemented"

organizeImports :: FilePath -> IO (Either Text Text)
organizeImports _filePath = return $ Left "Organize imports not implemented"

insertImport :: FilePath -> Text -> Maybe Text -> IO (Either Text Text)
insertImport _filePath _moduleName _symbol = return $ Left "Insert import not implemented"

removeUnusedImports :: FilePath -> IO (Either Text Text)
removeUnusedImports _filePath = return $ Left "Remove unused imports not implemented"

-- Additional missing functions
executeGenericHLSCommand :: Text -> Value -> IO (Either Text Text)
executeGenericHLSCommand _command _args = return $ Left "Generic HLS command execution not implemented"

executeRetrieCommand :: FilePath -> Text -> Text -> IO (Either Text Text)
executeRetrieCommand _filePath _lhs _rhs = return $ Left "Retrie command execution not implemented"

executeModuleNameCommand :: FilePath -> IO (Either Text Text)
executeModuleNameCommand _filePath = return $ Left "Module name command execution not implemented"

executeCabalAddCommand :: FilePath -> Text -> IO (Either Text Text)
executeCabalAddCommand _filePath _dependency = return $ Left "Cabal add command execution not implemented"

-- Handle HLS Tool Calls
handleHLSTool :: Text -> Maybe Value -> IO Types.ToolResult
handleHLSTool toolName maybeArgs = case toolName of
  -- Server Management
  "restart_hls_server" -> restartHLSTool
  "start_hls_server" -> startHLSTool maybeArgs
  "stop_hls_server" -> stopHLSTool
  "get_hls_status" -> getHLSStatusTool
  "show_versions" -> showVersionsTool
  
  -- LSP Basic Operations
  "hover_info" -> hoverInfoTool maybeArgs
  "goto_definition" -> gotoDefinitionTool maybeArgs
  "find_references" -> findReferencesTool maybeArgs
  "document_symbols" -> documentSymbolsTool maybeArgs
  "workspace_symbols" -> workspaceSymbolsTool maybeArgs
  "get_diagnostics" -> getDiagnosticsTool maybeArgs
  "format_document" -> formatDocumentTool maybeArgs
  "get_completions" -> getCompletionsTool maybeArgs
  
  -- Code Actions and Refactoring
  "get_code_actions" -> getCodeActionsTool maybeArgs
  "add_type_signature" -> addTypeSignatureTool maybeArgs
  "extend_import" -> extendImportTool maybeArgs
  "execute_command" -> executeCommandTool maybeArgs
  "retrie_refactor" -> retrieRefactorTool maybeArgs
  "gadt_conversion" -> gadtConversionTool maybeArgs
  "expand_th_splice" -> expandTHSpliceTool maybeArgs
  "update_module_name" -> updateModuleNameTool maybeArgs
  "add_cabal_dependency" -> addCabalDependencyTool maybeArgs
  
  -- Code Lens Operations
  "get_code_lenses" -> getCodeLensesTool maybeArgs
  "resolve_code_lens" -> resolveCodeLensTool maybeArgs
  
  -- Advanced Operations
  "eval_expression" -> evalExpressionTool maybeArgs
  "organize_imports" -> organizeImportsTool maybeArgs
  "insert_import" -> insertImportTool maybeArgs
  "remove_unused_imports" -> removeUnusedImportsTool maybeArgs
  
  _ -> return $ Types.ToolResult
    [ Types.ToolContent "text" (Just $ "Unknown HLS tool: " <> toolName) ]
    (Just True)

-- Restart HLS Server Tool
restartHLSTool :: IO Types.ToolResult
restartHLSTool = do
  result <- restartHLSServer
  case result of
    Left err -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "Error restarting HLS: " <> err) ]
      (Just True)
    Right status -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "HLS server restarted successfully. Status: " <> T.pack (show status)) ]
      Nothing

-- Start HLS Server Tool
startHLSTool :: Maybe Value -> IO Types.ToolResult
startHLSTool maybeArgs = do
  let workDir = parseWorkingDir maybeArgs
  result <- startHLSServer workDir
  case result of
    Left err -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "Error starting HLS: " <> err) ]
      (Just True)
    Right status -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "HLS server started successfully. Status: " <> T.pack (show status)) ]
      Nothing
  where
    parseWorkingDir :: Maybe Value -> Maybe FilePath
    parseWorkingDir Nothing = Nothing
    parseWorkingDir (Just args) = case fromJSON args of
      Success obj -> case parseMaybe (.: "workingDir") obj of
        Just dir -> Just (T.unpack dir)
        Nothing -> Nothing
      Data.Aeson.Error _ -> Nothing

-- Stop HLS Server Tool
stopHLSTool :: IO Types.ToolResult
stopHLSTool = do
  result <- stopHLSServer
  case result of
    Left err -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "Error stopping HLS: " <> err) ]
      (Just True)
    Right status -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just $ "HLS server stopped successfully. Status: " <> T.pack (show status)) ]
      Nothing

-- Get HLS Status Tool
getHLSStatusTool :: IO Types.ToolResult
getHLSStatusTool = do
  status <- getHLSStatus
  let statusText = case status of
        Types.Running -> "HLS server is running"
        Types.Stopped -> "HLS server is stopped"
        Types.Error txt -> "HLS server error: " <> txt

  return $ Types.ToolResult
    [ Types.ToolContent "text" (Just statusText) ]
    Nothing

-- Show Versions Tool
showVersionsTool :: IO Types.ToolResult
showVersionsTool = do
  hlsVersionResult <- getHLSVersion
  let hlsVersion = case hlsVersionResult of
        Left err -> "Error getting HLS version: " <> err
        Right version -> "HLS Version: " <> version
  
  let mcpVersion = "MCP-HLS Version: 0.1.0.0"
  let protocolVersion = "MCP Protocol Version: " <> Types.mcpVersion
  
  return $ Types.ToolResult
    [ Types.ToolContent "text" (Just $ T.unlines [mcpVersion, protocolVersion, hlsVersion]) ]
    Nothing

-- LSP Information Tools
hoverInfoTool :: Maybe Value -> IO Types.ToolResult
hoverInfoTool maybeArgs = do
  case parseFileAndPosition maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, line, character") ]
      (Just True)
    Just (filePath, line, char) -> do
      result <- getHoverInfo filePath line char
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting hover info: " <> err) ]
          (Just True)
        Right hoverText -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just hoverText) ]
          Nothing

gotoDefinitionTool :: Maybe Value -> IO Types.ToolResult
gotoDefinitionTool maybeArgs = do
  case parseFileAndPosition maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, line, character") ]
      (Just True)
    Just (filePath, line, char) -> do
      result <- gotoDefinition filePath line char
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting definition: " <> err) ]
          (Just True)
        Right locations -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Definition locations: " <> T.pack (show locations)) ]
          Nothing

findReferencesTool :: Maybe Value -> IO Types.ToolResult
findReferencesTool maybeArgs = do
  case parseFileAndPosition maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, line, character") ]
      (Just True)
    Just (filePath, line, char) -> do
      result <- findReferences filePath line char
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error finding references: " <> err) ]
          (Just True)
        Right references -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "References: " <> T.pack (show references)) ]
          Nothing

documentSymbolsTool :: Maybe Value -> IO Types.ToolResult
documentSymbolsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- getDocumentSymbols filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting document symbols: " <> err) ]
          (Just True)
        Right symbols -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Document symbols: " <> T.pack (show symbols)) ]
          Nothing

workspaceSymbolsTool :: Maybe Value -> IO Types.ToolResult
workspaceSymbolsTool maybeArgs = do
  case parseQuery maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: query") ]
      (Just True)
    Just query -> do
      result <- getWorkspaceSymbols query
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting workspace symbols: " <> err) ]
          (Just True)
        Right symbols -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Workspace symbols: " <> T.pack (show symbols)) ]
          Nothing

getDiagnosticsTool :: Maybe Value -> IO Types.ToolResult
getDiagnosticsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- getFileDiagnostics filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting diagnostics: " <> err) ]
          (Just True)
        Right diagnostics -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Diagnostics: " <> T.pack (show diagnostics)) ]
          Nothing

formatDocumentTool :: Maybe Value -> IO Types.ToolResult
formatDocumentTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- formatDocument filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error formatting document: " <> err) ]
          (Just True)
        Right formatted -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Document formatted successfully: " <> formatted) ]
          Nothing

getCompletionsTool :: Maybe Value -> IO Types.ToolResult
getCompletionsTool maybeArgs = do
  case parseFileAndPosition maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, line, character") ]
      (Just True)
    Just (filePath, line, char) -> do
      result <- getCompletions filePath line char
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting completions: " <> err) ]
          (Just True)
        Right completions -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Completions: " <> T.pack (show completions)) ]
          Nothing

-- Code Actions and Commands
getCodeActionsTool :: Maybe Value -> IO Types.ToolResult
getCodeActionsTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- getCodeActions filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting code actions: " <> err) ]
          (Just True)
        Right actions -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Code actions: " <> T.pack (show actions)) ]
          Nothing

addTypeSignatureTool :: Maybe Value -> IO Types.ToolResult
addTypeSignatureTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- executeHLSCommand "ghcide-type-lenses:typesignature.add" filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error adding type signature: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Type signature added: " <> T.pack (show response)) ]
          Nothing

extendImportTool :: Maybe Value -> IO Types.ToolResult
extendImportTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- executeHLSCommand "ghcide-extend-import-action:extendImport" filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error extending import: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Import extended: " <> T.pack (show response)) ]
          Nothing

executeCommandTool :: Maybe Value -> IO Types.ToolResult
executeCommandTool maybeArgs = do
  case parseCommandAndArgs maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: command, arguments") ]
      (Just True)
    Just (command, args) -> do
      result <- executeGenericHLSCommand command args
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error executing command: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Command executed: " <> T.pack (show response)) ]
          Nothing

retrieRefactorTool :: Maybe Value -> IO Types.ToolResult
retrieRefactorTool maybeArgs = do
  case parseRetrieArgs maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, lhs, rhs") ]
      (Just True)
    Just (filePath, lhs, rhs) -> do
      result <- executeRetrieCommand filePath lhs rhs
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error executing retrie refactoring: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Retrie refactoring completed: " <> T.pack (show response)) ]
          Nothing

gadtConversionTool :: Maybe Value -> IO Types.ToolResult
gadtConversionTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- executeHLSCommand "gadt:GADT.toGADT" filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error converting to GADT: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "GADT conversion completed: " <> T.pack (show response)) ]
          Nothing

expandTHSpliceTool :: Maybe Value -> IO Types.ToolResult
expandTHSpliceTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- executeHLSCommand "splice:expandTHSpliceInplace" filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error expanding TH splice: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "TH splice expanded: " <> T.pack (show response)) ]
          Nothing

updateModuleNameTool :: Maybe Value -> IO Types.ToolResult
updateModuleNameTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- executeModuleNameCommand filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error updating module name: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Module name updated: " <> T.pack (show response)) ]
          Nothing

addCabalDependencyTool :: Maybe Value -> IO Types.ToolResult
addCabalDependencyTool maybeArgs = do
  case parseCabalDependencyArgs maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, dependency") ]
      (Just True)
    Just (filePath, dependency) -> do
      result <- executeCabalAddCommand filePath dependency
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error adding cabal dependency: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Cabal dependency added: " <> T.pack (show response)) ]
          Nothing

-- Code Lens Operations
getCodeLensesTool :: Maybe Value -> IO Types.ToolResult
getCodeLensesTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- getCodeLenses filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting code lenses: " <> err) ]
          (Just True)
        Right lenses -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Code lenses: " <> T.pack (show lenses)) ]
          Nothing

resolveCodeLensTool :: Maybe Value -> IO Types.ToolResult
resolveCodeLensTool maybeArgs = do
  case parseCodeLensArgs maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: codeLens") ]
      (Just True)
    Just codeLensData -> do
      result <- resolveCodeLens codeLensData
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error resolving code lens: " <> err) ]
          (Just True)
        Right resolved -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Code lens resolved: " <> T.pack (show resolved)) ]
          Nothing

-- Advanced Operations
evalExpressionTool :: Maybe Value -> IO Types.ToolResult
evalExpressionTool maybeArgs = do
  case parseFileAndRange maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, startLine, startChar, endLine, endChar") ]
      (Just True)
    Just (filePath, startLine, startChar, endLine, endChar) -> do
      result <- executeHLSCommand "eval:evalCommand" filePath startLine startChar endLine endChar
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error evaluating expression: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Expression evaluated: " <> T.pack (show response)) ]
          Nothing

organizeImportsTool :: Maybe Value -> IO Types.ToolResult
organizeImportsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- organizeImports filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error organizing imports: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Imports organized: " <> T.pack (show response)) ]
          Nothing

insertImportTool :: Maybe Value -> IO Types.ToolResult
insertImportTool maybeArgs = do
  case parseInsertImportArgs maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: filePath, moduleName, symbol") ]
      (Just True)
    Just (filePath, moduleName, symbol) -> do
      result <- insertImport filePath moduleName symbol
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error inserting import: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Import inserted: " <> T.pack (show response)) ]
          Nothing

removeUnusedImportsTool :: Maybe Value -> IO Types.ToolResult
removeUnusedImportsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      result <- removeUnusedImports filePath
      case result of
        Left err -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error removing unused imports: " <> err) ]
          (Just True)
        Right response -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Unused imports removed: " <> T.pack (show response)) ]
          Nothing

-- Argument parsing helpers
parseFileAndPosition :: Maybe Value -> Maybe (FilePath, Int, Int)
parseFileAndPosition Nothing = Nothing
parseFileAndPosition (Just args) = case fromJSON args of
  Success obj -> do
    filePath <- T.unpack <$> parseMaybe (.: "filePath") obj
    line <- parseMaybe (.: "line") obj
    char <- parseMaybe (.: "character") obj
    return (filePath, line, char)
  Data.Aeson.Error _ -> Nothing

parseFilePath :: Maybe Value -> Maybe FilePath
parseFilePath Nothing = Nothing
parseFilePath (Just args) = case fromJSON args of
  Success obj -> T.unpack <$> parseMaybe (.: "filePath") obj
  Data.Aeson.Error _ -> Nothing

parseQuery :: Maybe Value -> Maybe Text
parseQuery Nothing = Nothing
parseQuery (Just args) = case fromJSON args of
  Success obj -> parseMaybe (.: "query") obj
  Data.Aeson.Error _ -> Nothing

parseFileAndRange :: Maybe Value -> Maybe (FilePath, Int, Int, Int, Int)
parseFileAndRange Nothing = Nothing
parseFileAndRange (Just args) = case fromJSON args of
  Success obj -> do
    filePath <- T.unpack <$> parseMaybe (.: "filePath") obj
    startLine <- parseMaybe (.: "startLine") obj
    startChar <- parseMaybe (.: "startChar") obj
    endLine <- parseMaybe (.: "endLine") obj
    endChar <- parseMaybe (.: "endChar") obj
    return (filePath, startLine, startChar, endLine, endChar)
  Data.Aeson.Error _ -> Nothing

parseCommandAndArgs :: Maybe Value -> Maybe (Text, Value)
parseCommandAndArgs Nothing = Nothing
parseCommandAndArgs (Just args) = case fromJSON args of
  Success obj -> do
    command <- parseMaybe (.: "command") obj
    arguments <- parseMaybe (.: "arguments") obj
    return (command, arguments)
  Data.Aeson.Error _ -> Nothing

parseRetrieArgs :: Maybe Value -> Maybe (FilePath, Text, Text)
parseRetrieArgs Nothing = Nothing
parseRetrieArgs (Just args) = case fromJSON args of
  Success obj -> do
    filePath <- T.unpack <$> parseMaybe (.: "filePath") obj
    lhs <- parseMaybe (.: "lhs") obj
    rhs <- parseMaybe (.: "rhs") obj
    return (filePath, lhs, rhs)
  Data.Aeson.Error _ -> Nothing

parseCabalDependencyArgs :: Maybe Value -> Maybe (FilePath, Text)
parseCabalDependencyArgs Nothing = Nothing
parseCabalDependencyArgs (Just args) = case fromJSON args of
  Success obj -> do
    filePath <- T.unpack <$> parseMaybe (.: "filePath") obj
    dependency <- parseMaybe (.: "dependency") obj
    return (filePath, dependency)
  Data.Aeson.Error _ -> Nothing

parseCodeLensArgs :: Maybe Value -> Maybe Value
parseCodeLensArgs Nothing = Nothing
parseCodeLensArgs (Just args) = case fromJSON args of
  Success obj -> parseMaybe (.: "codeLens") obj
  Data.Aeson.Error _ -> Nothing

parseInsertImportArgs :: Maybe Value -> Maybe (FilePath, Text, Maybe Text)
parseInsertImportArgs Nothing = Nothing
parseInsertImportArgs (Just args) = case fromJSON args of
  Success obj -> do
    filePath <- T.unpack <$> parseMaybe (.: "filePath") obj
    moduleName <- parseMaybe (.: "moduleName") obj
    symbol <- parseMaybe (.:? "symbol") obj
    return (filePath, moduleName, symbol)
  Data.Aeson.Error _ -> Nothing
