{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Resources where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Aeson
import Data.ByteString (toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified HLS.Process
import MCP.Types
import System.Directory
import System.FilePath
import System.IO
import Utils.FileSystem
import Utils.Logging

-- Handle Resource Read Requests
handleResourceRead :: Text -> IO (Either Text ResourceContents)
handleResourceRead uri = case uri of
  "hls://logs" -> readHLSLogs
  "hls://config" -> readHLSConfig
  "hls://project-info" -> readProjectInfo
  "hls://workspace-symbols" -> readWorkspaceSymbols
  _ ->
    if T.isPrefixOf "hls://file/" uri
      then readFileResource (T.drop 11 uri) -- Remove "hls://file/" prefix
      else return $ Left $ "Unknown resource URI: " <> uri

-- Read HLS Logs
readHLSLogs :: IO (Either Text ResourceContents)
readHLSLogs = do
  result <- try @SomeException $ do
    -- Try to find HLS log files in common locations
    logPaths <- findHLSLogFiles
    case logPaths of
      [] -> return "No HLS log files found"
      (logPath : _) -> T.readFile logPath

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error reading HLS logs: " <> T.pack (show ex)
    Right content ->
      return $
        Right $
          ResourceContents
            [ResourceContent "hls://logs" (Just "text/plain") (Just content)]

-- Read HLS Configuration
readHLSConfig :: IO (Either Text ResourceContents)
readHLSConfig = do
  result <- try @SomeException $ do
    -- Read HLS configuration from common locations
    configPaths <- findHLSConfigFiles
    configs <- mapM readConfigFile configPaths
    return $ T.intercalate "\n---\n" configs

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error reading HLS config: " <> T.pack (show ex)
    Right content ->
      return $
        Right $
          ResourceContents
            [ResourceContent "hls://config" (Just "application/json") (Just content)]

-- Read Project Information
readProjectInfo :: IO (Either Text ResourceContents)
readProjectInfo = do
  result <- try @SomeException $ do
    currentDir <- getCurrentDirectory
    projectInfo <- getProjectInfo currentDir
    return $ encodeProjectInfo projectInfo

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error reading project info: " <> T.pack (show ex)
    Right content ->
      return $
        Right $
          ResourceContents
            [ResourceContent "hls://project-info" (Just "application/json") (Just content)]

-- Read Workspace Symbols
readWorkspaceSymbols :: IO (Either Text ResourceContents)
readWorkspaceSymbols = do
  result <- try @SomeException $ do
    currentDir <- getCurrentDirectory
    symbols <- getWorkspaceSymbols currentDir
    return $ T.intercalate "\n" symbols

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error reading workspace symbols: " <> T.pack (show ex)
    Right content ->
      return $
        Right $
          ResourceContents
            [ResourceContent "hls://workspace-symbols" (Just "text/plain") (Just content)]

-- Read File Resource
readFileResource :: Text -> IO (Either Text ResourceContents)
readFileResource filePath = do
  let path = T.unpack filePath
  result <- try @SomeException $ do
    content <- T.readFile path
    return content

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Error reading file: " <> T.pack (show ex)
    Right content ->
      let mimeType = getMimeType path
       in return $
            Right $
              ResourceContents
                [ResourceContent ("hls://file/" <> filePath) mimeType (Just content)]

-- Find HLS Log Files
findHLSLogFiles :: IO [FilePath]
findHLSLogFiles = do
  let possibleLogPaths =
        [ "~/.cache/hie-bios/hie.log",
          "~/.cache/haskell-language-server/haskell-language-server.log",
          "/tmp/hls.log",
          "./hls.log",
          "./.hie-bios/hie.log"
        ]

  existingPaths <- filterM doesFileExist possibleLogPaths
  return existingPaths

-- Find HLS Config Files
findHLSConfigFiles :: IO [FilePath]
findHLSConfigFiles = do
  let possibleConfigPaths =
        [ "hie.yaml",
          ".hie-bios",
          "hie-bios.yaml",
          ".hlint.yaml",
          "hlint.yaml"
        ]

  existingPaths <- filterM doesFileExist possibleConfigPaths
  return existingPaths

-- Read Configuration File
readConfigFile :: FilePath -> IO Text
readConfigFile path = do
  result <- try $ T.readFile path
  case result of
    Left (ex :: SomeException) -> return $ "Error reading " <> T.pack path <> ": " <> T.pack (show ex)
    Right content -> return $ "=== " <> T.pack path <> " ===\n" <> content

-- Encode Project Information
encodeProjectInfo :: ProjectInfo -> Text
encodeProjectInfo ProjectInfo {..} =
  let json =
        object
          [ "projectRoot" .= projectRoot,
            "cabalFiles" .= cabalFiles,
            "stackFiles" .= stackFiles,
            "haskellFiles" .= take 100 haskellFiles, -- Limit to first 100 files
            "haskellFileCount" .= length haskellFiles
          ]
   in T.decodeUtf8 $ toStrict $ encode json

-- Get Workspace Symbols
getWorkspaceSymbols :: FilePath -> IO [Text]
getWorkspaceSymbols _projectRoot = do
  result <- HLS.Process.getWorkspaceSymbols "" -- Empty query gets all symbols
  case result of
    Left err -> do
      logError $ "Error getting workspace symbols from HLS: " <> err
      return []
    Right symbols -> return symbols

-- Get MIME Type
getMimeType :: FilePath -> Maybe Text
getMimeType path = case takeExtension path of
  ".hs" -> Just "text/x-haskell"
  ".lhs" -> Just "text/x-literate-haskell"
  ".cabal" -> Just "text/x-cabal"
  ".yaml" -> Just "application/yaml"
  ".yml" -> Just "application/yaml"
  ".json" -> Just "application/json"
  ".md" -> Just "text/markdown"
  ".txt" -> Just "text/plain"
  ".log" -> Just "text/plain"
  _ -> Just "text/plain"

-- Helper function for concurrent map
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = do
  results <- mapM f xs
  return $ concat results

-- Get Available Resources
getAvailableResources :: IO [Resource]
getAvailableResources = do
  currentDir <- getCurrentDirectory
  haskellFiles <- take 50 <$> findHaskellFiles currentDir -- Limit to 50 files
  let baseResources =
        [ Resource "hls://logs" "HLS Logs" (Just "Haskell Language Server logs") (Just "text/plain"),
          Resource "hls://config" "HLS Configuration" (Just "HLS and HIE configuration files") (Just "application/json"),
          Resource "hls://project-info" "Project Information" (Just "Project structure and metadata") (Just "application/json"),
          Resource "hls://workspace-symbols" "Workspace Symbols" (Just "All symbols in the workspace") (Just "text/plain")
        ]

  let fileResources =
        map
          ( \file ->
              Resource
                ("hls://file/" <> T.pack file)
                (T.pack $ takeFileName file)
                (Just $ "Haskell source file: " <> T.pack file)
                (getMimeType file)
          )
          haskellFiles

  return $ baseResources ++ fileResources

-- Watch for Resource Changes (placeholder)
watchResourceChanges :: Text -> IO ()
watchResourceChanges uri = do
  logInfo $ "Watching for changes to resource: " <> uri
  -- This would implement file system watching
  -- For now, just log the request
  return ()

-- Validate Resource URI
validateResourceURI :: Text -> Bool
validateResourceURI uri =
  T.isPrefixOf "hls://" uri
    && uri `elem` ["hls://logs", "hls://config", "hls://project-info", "hls://workspace-symbols"]
    || T.isPrefixOf "hls://file/" uri

-- Get Resource Metadata
getResourceMetadata :: Text -> IO (Maybe Resource)
getResourceMetadata uri = case uri of
  "hls://logs" -> return $ Just $ Resource uri "HLS Logs" (Just "Server logs") (Just "text/plain")
  "hls://config" -> return $ Just $ Resource uri "Configuration" (Just "Server config") (Just "application/json")
  "hls://project-info" -> return $ Just $ Resource uri "Project Info" (Just "Project metadata") (Just "application/json")
  "hls://workspace-symbols" -> return $ Just $ Resource uri "Symbols" (Just "Workspace symbols") (Just "text/plain")
  _ ->
    if T.isPrefixOf "hls://file/" uri
      then do
        let filePath = T.drop 11 uri
        exists <- doesFileExist (T.unpack filePath)
        if exists
          then return $ Just $ Resource uri (T.pack $ takeFileName $ T.unpack filePath) (Just $ "File: " <> filePath) (getMimeType $ T.unpack filePath)
          else return Nothing
      else return Nothing
