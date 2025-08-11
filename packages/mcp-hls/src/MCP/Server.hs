{-# LANGUAGE OverloadedStrings #-}

module MCP.Server where

import Control.Exception (catch, SomeException)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 as L8
import System.IO (stderr, hPutStrLn)
import MCP.Protocol
import MCP.Types

-- Main MCP Server Loop
runMCPServer :: IO ()
runMCPServer = do
  System.IO.hPutStrLn stderr "MCP-HLS Server starting..."
  serverLoop

serverLoop :: IO ()
serverLoop = forever $ do
  result <- readMessage
  case result of
    Left err -> do
      System.IO.hPutStrLn stderr $ "JSON parse error: " ++ err
      let errorResp = createErrorResponse parseError "Parse error" Nothing Nothing
      sendMessage errorResp
    Right req -> do
      System.IO.hPutStrLn stderr $ "Received request: " ++ show (method req)
      response <- safeProcessRequest req
      sendMessage response

-- Safe request processing with error handling
safeProcessRequest :: JsonRpcRequest -> IO JsonRpcResponse
safeProcessRequest req = do
  result <- catch (processRequest req) handleException
  return result
  where
    handleException :: SomeException -> IO JsonRpcResponse
    handleException ex = do
      System.IO.hPutStrLn stderr $ "Internal error: " ++ show ex
      return $ createErrorResponse internalError "Internal server error" Nothing Nothing