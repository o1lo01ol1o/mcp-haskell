{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Tools.Documentation
  ( handleDocumentationTool
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Process
import qualified MCP.Types as Types

-- Handle Documentation Tool Calls
handleDocumentationTool :: Text -> Maybe Value -> IO Types.ToolResult
handleDocumentationTool toolName maybeArgs = case toolName of
  "show_documentation" -> showDocumentationTool maybeArgs
  "search_haddock" -> searchHaddockTool maybeArgs
  "generate_docs" -> generateDocsTool maybeArgs
  "browse_module_docs" -> browseModuleDocsTool maybeArgs
  "get_symbol_info" -> getSymbolInfoTool maybeArgs
  _ -> return $ Types.ToolResult
    [ Types.ToolContent "text" (Just $ "Unknown documentation tool: " <> toolName) ]
    (Just True)

-- Show Documentation Tool
showDocumentationTool :: Maybe Value -> IO Types.ToolResult
showDocumentationTool maybeArgs = do
  case parseModuleName maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: moduleName") ]
      (Just True)
    Just moduleName -> do
      result <- try $ findModuleDocumentation moduleName
      case result of
        Left (ex :: SomeException) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error finding documentation: " <> T.pack (show ex)) ]
          (Just True)
        Right (Right docPath) -> (case docPath of
            Just path -> do
              docContent <- try $ T.readFile path
              case docContent of
                Left (ex :: SomeException) -> return $ Types.ToolResult
                  [ Types.ToolContent "text" (Just $ "Error reading documentation: " <> T.pack (show ex)) ]
                  (Just True)
                Right content -> return $ Types.ToolResult
                  [ Types.ToolContent "text" (Just $ "Documentation for " <> moduleName <> ":\n" <> content) ]
                  Nothing
            Nothing -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "No documentation found for module: " <> moduleName) ]
              (Just True))
        Right (Left err) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Documentation lookup error: " <> err) ]
          (Just True)

-- Search Haddock Tool
searchHaddockTool :: Maybe Value -> IO Types.ToolResult
searchHaddockTool maybeArgs = do
  case parseSearchQuery maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: query") ]
      (Just True)
    Just query -> do
      result <- try $ searchInHaddockDocs query
      case result of
        Left (ex :: SomeException) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error searching documentation: " <> T.pack (show ex)) ]
          (Just True)
        Right matches -> do
          case matches of
            Left err -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Search error: " <> err) ]
              (Just True)
            Right matchList -> do
              let matchText = if null matchList
                    then "No matches found for: " <> query
                    else "Search results for '" <> query <> "':\n" <> T.intercalate "\n" matchList
              return $ Types.ToolResult
                [ Types.ToolContent "text" (Just matchText) ]
                Nothing

-- Generate Documentation Tool
generateDocsTool :: Maybe Value -> IO Types.ToolResult
generateDocsTool maybeArgs = do
  case parseProjectPath maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: projectPath") ]
      (Just True)
    Just projectPath -> do
      result <- try $ generateHaddockDocs projectPath
      case result of
        Left (ex :: SomeException) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error generating documentation: " <> T.pack (show ex)) ]
          (Just True)
        Right output -> 
          case output of
            Left err -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Documentation generation error: " <> err) ]
              (Just True)
            Right docs -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Documentation generated successfully:\n" <> docs) ]
              Nothing

-- Browse Module Documentation Tool
browseModuleDocsTool :: Maybe Value -> IO Types.ToolResult
browseModuleDocsTool maybeArgs = do
  case parseModuleName maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameter: moduleName") ]
      (Just True)
    Just moduleName -> do
      result <- try $ getModuleSymbols moduleName
      case result of
        Left (ex :: SomeException) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error browsing module: " <> T.pack (show ex)) ]
          (Just True)
        Right symbols -> do
          case symbols of
            Left err -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Error getting symbols: " <> err) ]
              (Just True)
            Right symbolList -> do
              let symbolsList = T.intercalate "\n" symbolList
              return $ Types.ToolResult
                [ Types.ToolContent "text" (Just $ "Symbols in " <> moduleName <> ":\n" <> symbolsList) ]
                Nothing

-- Get Symbol Information Tool
getSymbolInfoTool :: Maybe Value -> IO Types.ToolResult
getSymbolInfoTool maybeArgs = do
  case parseSymbolQuery maybeArgs of
    Nothing -> return $ Types.ToolResult
      [ Types.ToolContent "text" (Just "Missing required parameters: symbol and optionally moduleName") ]
      (Just True)
    Just (symbol, maybeModule) -> do
      result <- try $ getSymbolDocumentation symbol maybeModule
      case result of
        Left (ex :: SomeException) -> return $ Types.ToolResult
          [ Types.ToolContent "text" (Just $ "Error getting symbol info: " <> T.pack (show ex)) ]
          (Just True)
        Right info -> 
          case info of
            Left err -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Error getting symbol info: " <> err) ]
              (Just True)
            Right infoText -> return $ Types.ToolResult
              [ Types.ToolContent "text" (Just $ "Information for " <> symbol <> ":\n" <> infoText) ]
              Nothing

-- Find Module Documentation
findModuleDocumentation :: Text -> IO (Either Text (Maybe FilePath))
findModuleDocumentation moduleName = do
  let possiblePaths = 
        [ "/usr/local/share/doc/ghc/html/libraries/" ++ T.unpack moduleName ++ "/" ++ T.unpack moduleName ++ ".html"
        , "~/.cabal/share/doc/*/html/" ++ T.unpack moduleName ++ "/" ++ T.unpack moduleName ++ ".html"
        , "./dist-newstyle/doc/html/*/" ++ T.unpack moduleName ++ "/" ++ T.unpack moduleName ++ ".html"
        ]
  
  foundPaths <- mapM doesFileExist possiblePaths
  case filter snd (zip possiblePaths foundPaths) of
    ((path, _):_) -> return $ Right $ Just path
    [] -> return $ Right Nothing

-- Search in Haddock Documentation
searchInHaddockDocs :: Text -> IO (Either Text [Text])
searchInHaddockDocs query = do
  result <- try $ do
    -- Use hoogle if available, otherwise search in local docs
    output <- readProcess "hoogle" [T.unpack query] ""
    return $ take 10 $ T.lines $ T.pack output
  
  case result of
    Left (_ :: SomeException) -> 
      -- Fallback to searching in local documentation
      searchInLocalDocs query
    Right matches -> return $ Right matches

-- Search in Local Documentation
searchInLocalDocs :: Text -> IO (Either Text [Text])
searchInLocalDocs query = do
  result <- try $ do
    -- Search for .hs files containing the query
    output <- readProcess "grep" ["-r", "--include=*.hs", T.unpack query, "."] ""
    return $ take 20 $ T.lines $ T.pack output
  
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Search error: " <> T.pack (show ex)
    Right matches -> return $ Right $ filter (not . T.null) matches

-- Generate Haddock Documentation
generateHaddockDocs :: FilePath -> IO (Either Text Text)
generateHaddockDocs projectPath = do
  result <- try $ do
    setCurrentDirectory projectPath
    -- Try cabal first, then stack
    cabalExists <- doesFileExist "*.cabal"
    if cabalExists
      then readProcess "cabal" ["haddock", "--all"] ""
      else do
        stackExists <- doesFileExist "stack.yaml"
        if stackExists
          then readProcess "stack" ["haddock"] ""
          else return "No cabal file or stack.yaml found"
  
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Documentation generation failed: " <> T.pack (show ex)
    Right output -> return $ Right $ T.pack output

-- Get Module Symbols
getModuleSymbols :: Text -> IO (Either Text [Text])
getModuleSymbols moduleName = do
  result <- try $ do
    -- Use GHCi to get module info
    output <- readProcess "ghci" ["-e", ":browse " ++ T.unpack moduleName] ""
    return $ T.lines $ T.pack output
  
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error browsing module: " <> T.pack (show ex)
    Right symbols -> return $ Right $ filter (not . T.null) symbols

-- Get Symbol Documentation
getSymbolDocumentation :: Text -> Maybe Text -> IO (Either Text Text)
getSymbolDocumentation symbol maybeModule = do
  result <- try $ do
    let query = case maybeModule of
          Just modName -> T.unpack modName ++ "." ++ T.unpack symbol
          Nothing -> T.unpack symbol
    
    -- Try hoogle first
    hoogleOutput <- try @SomeException $ readProcess "hoogle" [query, "--info"] ""
    case hoogleOutput of
      Right output -> return output
      Left _ -> do
        -- Fallback to ghci
        ghciOutput <- readProcess "ghci" ["-e", ":info " ++ T.unpack symbol] ""
        return ghciOutput
  
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error getting symbol info: " <> T.pack (show ex)
    Right "" -> return $ Left $ "No information found for symbol: " <> symbol
    Right info -> return $ Right $ T.pack info

-- Parse Module Name from Arguments
parseModuleName :: Maybe Value -> Maybe Text
parseModuleName Nothing = Nothing
parseModuleName (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> case KM.lookup "moduleName" o of
      Just (String name) -> Just name
      _ -> Nothing
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing

-- Parse Search Query from Arguments
parseSearchQuery :: Maybe Value -> Maybe Text
parseSearchQuery Nothing = Nothing
parseSearchQuery (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> case KM.lookup "query" o of
      Just (String query) -> Just query
      _ -> Nothing
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing

-- Parse Project Path from Arguments
parseProjectPath :: Maybe Value -> Maybe FilePath
parseProjectPath Nothing = Nothing
parseProjectPath (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> case KM.lookup "projectPath" o of
      Just (String path) -> Just (T.unpack path)
      _ -> Nothing
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing

-- Parse Symbol Query from Arguments
parseSymbolQuery :: Maybe Value -> Maybe (Text, Maybe Text)
parseSymbolQuery Nothing = Nothing
parseSymbolQuery (Just args) = case fromJSON args of
  Success obj -> case obj of
    Object o -> do
      symbol <- case KM.lookup "symbol" o of
        Just (String s) -> Just s
        _ -> Nothing
      let maybeModule = case KM.lookup "moduleName" o of
            Just (String m) -> Just m
            _ -> Nothing
      return (symbol, maybeModule)
    _ -> Nothing
  Data.Aeson.Error _ -> Nothing
