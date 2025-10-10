{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Tools.Diagnostics
  ( handleDiagnosticsTool,
    getDiagnosticsTool,
    formatCodeTool,
    checkSyntaxTool,
    hlintSuggestionsTool,
    formatHaskellFile,
    formatWithOrmolu,
    formatWithFourmolu,
    formatWithBrittany,
    formatWithStylish,
    parseFilePath,
    parseFormatArgs,
    typeCheckFile,
    getModuleImports,
    getModuleExports,
    getDiagnosticsFromHLS,
    getDiagnosticsFromGHC
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Process
import MCP.Types
import MCP.Tools.HLS (getHLSStatus)

-- Handle Diagnostics Tool Calls
handleDiagnosticsTool :: Text -> Maybe Value -> IO ToolResult
handleDiagnosticsTool toolName maybeArgs = case toolName of
  "get_diagnostics" -> getDiagnosticsTool maybeArgs
  "format_code" -> formatCodeTool maybeArgs
  "check_syntax" -> checkSyntaxTool maybeArgs
  "hlint_suggestions" -> hlintSuggestionsTool maybeArgs
  _ -> return $ ToolResult
    [ ToolContent "text" (Just $ "Unknown diagnostics tool: " <> toolName) ]
    (Just True)

-- Get Diagnostics Tool
getDiagnosticsTool :: Maybe Value -> IO ToolResult
getDiagnosticsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ ToolResult
      [ ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      -- Try to use HLS first if available, fallback to GHC
      hlsStatus <- getHLSStatus
      case hlsStatus of
        Running -> getDiagnosticsFromHLS filePath
        _ -> getDiagnosticsFromGHC filePath

-- Format Code Tool
formatCodeTool :: Maybe Value -> IO ToolResult
formatCodeTool maybeArgs = do
  case parseFormatArgs maybeArgs of
    Nothing -> return $ ToolResult
      [ ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just (filePath, formatter) -> do
      formatOutcome <- formatHaskellFile filePath formatter
      case formatOutcome of
        Left err -> return $ ToolResult
          [ ToolContent "text" (Just $ "Error formatting code: " <> err) ]
          (Just True)
        Right formatted -> return $ ToolResult
          [ ToolContent "text" (Just $ "Formatted code:\n" <> formatted) ]
          Nothing

-- Check Syntax Tool
checkSyntaxTool :: Maybe Value -> IO ToolResult
checkSyntaxTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ ToolResult
      [ ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      syntaxOutcome <- try $ do
        output <- readProcess "ghc" ["-fno-code", "-fno-warn-missing-signatures", filePath] ""
        return output
      
      case syntaxOutcome of
        Left (ex :: SomeException) -> return $ ToolResult
          [ ToolContent "text" (Just $ "Syntax check failed: " <> T.pack (show ex)) ]
          (Just True)
        Right "" -> return $ ToolResult
          [ ToolContent "text" (Just $ "✓ Syntax check passed for " <> T.pack filePath) ]
          Nothing
        Right output -> return $ ToolResult
          [ ToolContent "text" (Just $ "Syntax issues in " <> T.pack filePath <> ":\n" <> T.pack output) ]
          (Just True)

-- HLint Suggestions Tool
hlintSuggestionsTool :: Maybe Value -> IO ToolResult
hlintSuggestionsTool maybeArgs = do
  case parseFilePath maybeArgs of
    Nothing -> return $ ToolResult
      [ ToolContent "text" (Just "Missing required parameter: filePath") ]
      (Just True)
    Just filePath -> do
      hlintOutcome <- try $ do
        output <- readProcess "hlint" [filePath] ""
        return output
      
      case hlintOutcome of
        Left (ex :: SomeException) -> return $ ToolResult
          [ ToolContent "text" (Just $ "Error running HLint: " <> T.pack (show ex)) ]
          (Just True)
        Right "" -> return $ ToolResult
          [ ToolContent "text" (Just $ "✓ No HLint suggestions for " <> T.pack filePath) ]
          Nothing
        Right output -> return $ ToolResult
          [ ToolContent "text" (Just $ "HLint suggestions for " <> T.pack filePath <> ":\n" <> T.pack output) ]
          Nothing

-- Format Haskell File
formatHaskellFile :: FilePath -> Text -> IO (Either Text Text)
formatHaskellFile filePath formatter = do
  case formatter of
    "ormolu" -> formatWithOrmolu filePath
    "fourmolu" -> formatWithFourmolu filePath
    "brittany" -> formatWithBrittany filePath
    "stylish-haskell" -> formatWithStylish filePath
    _ -> return $ Left $ "Unknown formatter: " <> formatter

-- Format with Ormolu
formatWithOrmolu :: FilePath -> IO (Either Text Text)
formatWithOrmolu filePath = do
  ormoluOutcome <- try $ readProcess "ormolu" [filePath] ""
  case ormoluOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Ormolu error: " <> T.pack (show ex)
    Right output -> return $ Right $ T.pack output

-- Format with Fourmolu
formatWithFourmolu :: FilePath -> IO (Either Text Text)
formatWithFourmolu filePath = do
  fourmoluOutcome <- try $ readProcess "fourmolu" [filePath] ""
  case fourmoluOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Fourmolu error: " <> T.pack (show ex)
    Right output -> return $ Right $ T.pack output

-- Format with Brittany
formatWithBrittany :: FilePath -> IO (Either Text Text)
formatWithBrittany filePath = do
  brittanyOutcome <- try $ do
    fileContent <- readFile filePath
    readProcess "brittany" [] fileContent
  case brittanyOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Brittany error: " <> T.pack (show ex)
    Right output -> return $ Right $ T.pack output

-- Format with Stylish Haskell
formatWithStylish :: FilePath -> IO (Either Text Text)
formatWithStylish filePath = do
  stylishOutcome <- try $ do
    fileContent <- readFile filePath
    readProcess "stylish-haskell" [] fileContent
  case stylishOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Stylish-haskell error: " <> T.pack (show ex)
    Right output -> return $ Right $ T.pack output

-- Parse File Path from Arguments
parseFilePath :: Maybe Value -> Maybe FilePath
parseFilePath Nothing = Nothing
parseFilePath (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> case KM.lookup "filePath" o of
      Just (String path) -> Just (T.unpack path)
      _ -> Nothing
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing

-- Parse Format Arguments
parseFormatArgs :: Maybe Value -> Maybe (FilePath, Text)
parseFormatArgs Nothing = Nothing
parseFormatArgs (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> do
      filePath <- case KM.lookup "filePath" o of
        Just (String path) -> Just (T.unpack path)
        _ -> Nothing
      let formatter = case KM.lookup "formatter" o of
            Just (String fmt) -> fmt
            _ -> "ormolu" -- default
      return (filePath, formatter)
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing

-- Type Check with GHC
typeCheckFile :: FilePath -> IO (Either Text Text)
typeCheckFile filePath = do
  typeCheckOutcome <- try $ readProcess "ghc" ["-fno-code", "-v0", filePath] ""
  case typeCheckOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Type check error: " <> T.pack (show ex)
    Right "" -> return $ Right "✓ Type check passed"
    Right output -> return $ Left $ T.pack output

-- Get Module Imports
getModuleImports :: FilePath -> IO (Either Text [Text])
getModuleImports filePath = do
  moduleReadOutcome <- try $ T.readFile filePath
  case moduleReadOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Error reading file: " <> T.pack (show ex)
    Right fileContent -> do
      let imports = filter (T.isPrefixOf "import ") (T.lines fileContent)
      return $ Right imports

-- Get Module Exports
getModuleExports :: FilePath -> IO (Either Text [Text])
getModuleExports filePath = do
  exportsOutcome <- try $ T.readFile filePath
  case exportsOutcome of
    Left (ex :: SomeException) -> return $ Left $ "Error reading file: " <> T.pack (show ex)
    Right fileContent -> do
      let moduleLines = filter (T.isPrefixOf "module ") (T.lines fileContent)
      let exports = concatMap extractExports moduleLines
      return $ Right exports
  where
    extractExports line = 
      case T.breakOn "(" line of
        (_, "") -> []
        (_, rest) -> case T.breakOn ")" rest of
          (exports, _) -> [T.strip $ T.drop 1 exports]

-- Get Diagnostics from HLS via LSP
getDiagnosticsFromHLS :: FilePath -> IO ToolResult
getDiagnosticsFromHLS filePath = do
  -- For now, we don't have direct access to the LSP client from the global state
  -- This would require refactoring to maintain an LSP client connection
  -- Fall back to GHC for now but with a note about HLS being available
  ghcDiagnostics <- getDiagnosticsFromGHC filePath
  case ghcDiagnostics of
    ToolResult toolContent toolIsError -> return $ ToolResult 
      (ToolContent "text" (Just "Note: Using GHC diagnostics (HLS LSP integration planned)\n") : toolContent)
      toolIsError

-- Get Diagnostics from GHC
getDiagnosticsFromGHC :: FilePath -> IO ToolResult
getDiagnosticsFromGHC filePath = do
  ghcOutcome <- try $ do
    output <- readProcess "ghc" ["-fno-code", "-v0", filePath] ""
    return output
  
  case ghcOutcome of
    Left (ex :: SomeException) -> return $ ToolResult
      [ ToolContent "text" (Just $ "Error getting diagnostics: " <> T.pack (show ex)) ]
      (Just True)
    Right output -> return $ ToolResult
      [ ToolContent "text" (Just $ "Diagnostics for " <> T.pack filePath <> ":\n" <> T.pack output) ]
      Nothing
