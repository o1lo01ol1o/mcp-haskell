{-# LANGUAGE OverloadedStrings #-}

module MCP.Server
  ( runMCPServer
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import MCP.Protocol
import MCP.Types hiding (result)
import System.IO (hPutStrLn, stderr)

-- Main MCP Server Loop
runMCPServer :: IO ()
runMCPServer = do
  hPutStrLn stderr "MCP-HLS Server starting..."
  serverLoop

serverLoop :: IO ()
serverLoop = do
  serverLoop'
  where
    serverLoop' = do
      messageResult <- readMessage
      case messageResult of
        Left err -> do
          hPutStrLn stderr $ "JSON parse error: " ++ err
          let errorResp = createErrorResponse parseError "Parse error" Nothing Nothing
          sendMessage errorResp
          serverLoop'  -- Continue the loop
        Right Nothing -> do
          -- EOF encountered, wait and check again
          hPutStrLn stderr "EOF encountered, waiting for reconnection..."
          threadDelay 100000  -- Wait 100ms
          serverLoop'
        Right (Just req) -> do
          hPutStrLn stderr $ "Received request: " ++ show (method req)
          response <- safeProcessRequest req
          sendMessage response
          serverLoop'  -- Continue the loop

-- Safe request processing with error handling
safeProcessRequest :: JsonRpcRequest -> IO JsonRpcResponse
safeProcessRequest req = catch (processRequest req) handleException
  where
    handleException :: SomeException -> IO JsonRpcResponse
    handleException ex = do
      hPutStrLn stderr $ "Internal error: " ++ show ex
      return $ createErrorResponse internalError "Internal server error" Nothing Nothing
