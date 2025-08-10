{-# LANGUAGE OverloadedStrings #-}

module MCP.Tools.HLS where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Types
import HLS.Process

-- Handle HLS Tool Calls
handleHLSTool :: Text -> Maybe Value -> IO ToolResult
handleHLSTool toolName maybeArgs = case toolName of
  "restart_hls_server" -> restartHLSTool
  "start_hls_server" -> startHLSTool maybeArgs
  "stop_hls_server" -> stopHLSTool
  "get_hls_status" -> getHLSStatusTool
  "show_versions" -> showVersionsTool
  _ -> return $ ToolResult
    [ ToolContent "text" (Just $ "Unknown HLS tool: " <> toolName) ]
    (Just True)

-- Restart HLS Server Tool
restartHLSTool :: IO ToolResult
restartHLSTool = do
  result <- restartHLSServer
  case result of
    Left err -> return $ ToolResult
      [ ToolContent "text" (Just $ "Error restarting HLS: " <> err) ]
      (Just True)
    Right status -> return $ ToolResult
      [ ToolContent "text" (Just $ "HLS server restarted successfully. Status: " <> T.pack (show status)) ]
      Nothing

-- Start HLS Server Tool
startHLSTool :: Maybe Value -> IO ToolResult
startHLSTool maybeArgs = do
  let workDir = parseWorkingDir maybeArgs
  result <- startHLSServer workDir
  case result of
    Left err -> return $ ToolResult
      [ ToolContent "text" (Just $ "Error starting HLS: " <> err) ]
      (Just True)
    Right status -> return $ ToolResult
      [ ToolContent "text" (Just $ "HLS server started successfully. Status: " <> T.pack (show status)) ]
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
stopHLSTool :: IO ToolResult
stopHLSTool = do
  result <- stopHLSServer
  case result of
    Left err -> return $ ToolResult
      [ ToolContent "text" (Just $ "Error stopping HLS: " <> err) ]
      (Just True)
    Right status -> return $ ToolResult
      [ ToolContent "text" (Just $ "HLS server stopped successfully. Status: " <> T.pack (show status)) ]
      Nothing

-- Get HLS Status Tool
getHLSStatusTool :: IO ToolResult
getHLSStatusTool = do
  status <- getHLSStatus
  let statusText = case status of
        Running -> "HLS server is running"
        Stopped -> "HLS server is stopped"
        MCP.Types.Error txt -> "HLS server error: " <> txt
  
  return $ ToolResult
    [ ToolContent "text" (Just statusText) ]
    Nothing

-- Show Versions Tool
showVersionsTool :: IO ToolResult
showVersionsTool = do
  hlsVersionResult <- getHLSVersion
  let hlsVersion = case hlsVersionResult of
        Left err -> "Error getting HLS version: " <> err
        Right version -> "HLS Version: " <> version
  
  let mcpVersion = "MCP-HLS Version: 0.1.0.0"
  let protocolVersion = "MCP Protocol Version: " <> MCP.Types.mcpVersion
  
  return $ ToolResult
    [ ToolContent "text" (Just $ T.unlines [mcpVersion, protocolVersion, hlsVersion]) ]
    Nothing