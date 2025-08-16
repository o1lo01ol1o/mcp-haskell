{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.SDK.Transport.Stdio
  ( StdioTransport,
    newStdioTransport,
    newStdioTransportWithHandles,
    createStdioTransport,
  )
where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception (IOException, catch, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MCP.SDK.Error
import MCP.SDK.Protocol
import MCP.SDK.Transport
import System.IO (Handle, hFlush, hGetLine, hIsEOF, stderr, stdin, stdout)

-- | Stdio transport for MCP communication
data StdioTransport = StdioTransport
  { stInput :: Handle,
    stOutput :: Handle,
    stConnected :: TVar Bool,
    stMessageQueue :: TBQueue JSONRPCMessage,
    stReaderThread :: TVar (Maybe (IO ()))
  }

instance Eq StdioTransport where
  (StdioTransport _ _ conn1 _ _) == (StdioTransport _ _ conn2 _ _) = conn1 == conn2

-- | Create a new stdio transport with default handles (stdin/stdout)
newStdioTransport :: IO StdioTransport
newStdioTransport = newStdioTransportWithHandles stdin stdout

-- | Alias for compatibility with server examples
createStdioTransport :: Handle -> Handle -> IO StdioTransport
createStdioTransport = newStdioTransportWithHandles

-- | Default transport configuration
defaultTransportConfig :: TransportConfig
defaultTransportConfig =
  TransportConfig
    { transportTimeout = 30,
      transportBufferSize = 8192,
      transportRetries = 3
    }

-- | Create a stdio transport with custom handles
newStdioTransportWithHandles :: Handle -> Handle -> IO StdioTransport
newStdioTransportWithHandles input output = do
  connected <- newTVarIO True
  messageQueue <- newTBQueueIO 100 -- Buffer up to 100 messages
  readerThread <- newTVarIO Nothing

  let transport =
        StdioTransport
          { stInput = input,
            stOutput = output,
            stConnected = connected,
            stMessageQueue = messageQueue,
            stReaderThread = readerThread
          }

  -- Start the message reader thread
  startMessageReader transport
  return transport

-- | Start the background message reader thread
startMessageReader :: StdioTransport -> IO ()
startMessageReader transport@StdioTransport {..} = do
  readerAction <- async $ forever $ do
    connected <- readTVarIO stConnected
    when connected $ do
      result <- readMessage stInput
      case result of
        Left err -> do
          -- Log error but continue (could implement proper logging)
          TIO.hPutStrLn stderr $ "Transport read error: " <> T.pack (show err)
        Right msg -> do
          -- Queue the message for processing
          atomically $ writeTBQueue stMessageQueue msg

  atomically $ writeTVar stReaderThread (Just (cancel readerAction))

-- | Read a single JSON-RPC message from a handle
readMessage :: Handle -> IO (Either MCPError JSONRPCMessage)
readMessage handle = do
  result <- try $ do
    eof <- hIsEOF handle
    if eof
      then return $ Left ConnectionClosed
      else do
        line <- hGetLine handle
        let lineBytes = L8.pack line
        case decode lineBytes of
          Nothing -> return $ Left $ ParseError "Invalid JSON in message"
          Just msg -> return $ Right msg

  case result of
    Left (ex :: IOException) -> return $ Left $ TransportError $ T.pack $ show ex
    Right res -> return res

-- | Write a JSON-RPC message to a handle
writeMessage :: Handle -> JSONRPCMessage -> IO (Either MCPError ())
writeMessage handle msg = do
  result <- try $ do
    let jsonBytes = encode msg
    let jsonString = L8.unpack jsonBytes
    TIO.hPutStrLn handle (T.pack jsonString)
    hFlush handle

  case result of
    Left (ex :: IOException) -> return $ Left $ TransportError $ T.pack $ show ex
    Right _ -> return $ Right ()

instance Transport StdioTransport where
  sendMessage transport@StdioTransport {..} msg = liftIO $ do
    connected <- readTVarIO stConnected
    if connected
      then writeMessage stOutput msg
      else return $ Left $ TransportError "Transport not connected"

  receiveMessage transport@StdioTransport {..} = liftIO $ do
    connected <- readTVarIO stConnected
    if connected
      then do
        result <- atomically $ readTBQueue stMessageQueue
        return $ Right result
      else return $ Left $ TransportError "Transport not connected"

  closeTransport transport@StdioTransport {..} = liftIO $ do
    -- Mark as disconnected
    atomically $ writeTVar stConnected False

    -- Cancel reader thread if it exists
    maybeCancel <- readTVarIO stReaderThread
    case maybeCancel of
      Nothing -> return ()
      Just cancelAction -> cancelAction

    -- Clear message queue
    atomically $ do
      isEmpty <- isEmptyTBQueue stMessageQueue
      if isEmpty
        then return ()
        else do
          _ <- readTBQueue stMessageQueue
          return ()

  isConnected transport@StdioTransport {..} = liftIO $ readTVarIO stConnected
