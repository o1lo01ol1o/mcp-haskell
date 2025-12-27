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
import Control.Exception (IOException, try)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import MCP.SDK.Error
import MCP.SDK.Logging (logDebug, logError)
import MCP.SDK.Protocol
import MCP.SDK.Transport
import System.IO (Handle, hFlush, hGetLine, hIsEOF, hSetBinaryMode, stdin, stdout)
import Text.Read (readMaybe)

data StdioFraming
  = StdioFramingUnknown
  | StdioFramingContentLength
  | StdioFramingNewlineJson
  deriving (Eq, Show)

-- | Stdio transport for MCP communication
data StdioTransport = StdioTransport
  { stInput :: Handle,
    stOutput :: Handle,
    stConnected :: TVar Bool,
    stMessageQueue :: TBQueue JSONRPCMessage,
    stReaderThread :: TVar (Maybe (IO ())),
    stFraming :: TVar StdioFraming
  }

instance Eq StdioTransport where
  (StdioTransport _ _ conn1 _ _ _) == (StdioTransport _ _ conn2 _ _ _) = conn1 == conn2

-- | Create a new stdio transport with default handles (stdin/stdout)
newStdioTransport :: IO StdioTransport
newStdioTransport = newStdioTransportWithHandles stdin stdout

-- | Alias for compatibility with server examples
createStdioTransport :: Handle -> Handle -> IO StdioTransport
createStdioTransport = newStdioTransportWithHandles

-- | Create a stdio transport with custom handles
newStdioTransportWithHandles :: Handle -> Handle -> IO StdioTransport
newStdioTransportWithHandles input output = do
  hSetBinaryMode input True
  hSetBinaryMode output True
  connected <- newTVarIO True
  messageQueue <- newTBQueueIO 100 -- Buffer up to 100 messages
  readerThread <- newTVarIO Nothing
  framing <- newTVarIO StdioFramingUnknown

  let transport =
        StdioTransport
          { stInput = input,
            stOutput = output,
            stConnected = connected,
            stMessageQueue = messageQueue,
            stReaderThread = readerThread,
            stFraming = framing
          }

  -- Start the message reader thread
  startMessageReader transport
  return transport

-- | Start the background message reader thread
startMessageReader :: StdioTransport -> IO ()
startMessageReader StdioTransport {..} = do
  logDebug "Transport starting stdio reader thread"
  readerAction <- async $ forever $ do
    connected <- readTVarIO stConnected
    when connected $ do
      result <- readMessage stInput stFraming
      case result of
        Left err -> do
          logError $ "Transport read error: " <> T.pack (show err)
        Right msg -> do
          -- Queue the message for processing
          atomically $ writeTBQueue stMessageQueue msg

  atomically $ writeTVar stReaderThread (Just (cancel readerAction))

-- | Read a single JSON-RPC message from a handle
readMessage :: Handle -> TVar StdioFraming -> IO (Either MCPError JSONRPCMessage)
readMessage handle framingVar = do
  let loop = do
        eof <- hIsEOF handle
        if eof
          then return $ Left ConnectionClosed
          else do
            msg <- readNextMessage
            case msg of
              Right m -> pure (Right m)
              Left _err -> loop

  result <- try loop

  case result of
    Left (ex :: IOException) -> return $ Left $ TransportError $ T.pack $ show ex
    Right res -> return res
  where
    readNextMessage :: IO (Either MCPError JSONRPCMessage)
    readNextMessage = do
      logDebug "Transport awaiting header line"
      rawLine <- hGetLine handle
      logDebug $ "Transport header line: " <> T.pack (take 200 rawLine)
      let line = dropTrailingCR rawLine

      -- Codex currently sends newline-delimited JSON (no Content-Length framing).
      -- Autodetect it so we can interop with both framing styles.
      case detectNewlineJsonMessage line of
        Right (Just msg) -> do
          logDebug "Transport detected newline-delimited JSON framing"
          atomically $ writeTVar framingVar StdioFramingNewlineJson
          pure (Right msg)
        Right Nothing -> do
          -- Not JSON; if this doesn't look like a header line, ignore it and keep scanning.
          if ':' `elem` line
            then do
              logDebug "Transport detected Content-Length framing"
              atomically $ writeTVar framingVar StdioFramingContentLength
              mLen <- readHeadersFromFirstLine line Nothing
              case mLen of
                Nothing -> do
                  logDebug "Transport did not receive Content-Length header; ignoring message"
                  readNextMessage
                Just len -> do
                  logDebug $ "Transport received Content-Length: " <> T.pack (show len)
                  body <- BS.hGet handle len
                  let bodyLazy = LBS.fromStrict body
                  case eitherDecode bodyLazy of
                    Left errMsg -> do
                      logDebug $ "Transport decode error: " <> T.pack errMsg
                      readNextMessage
                    Right msg -> pure (Right msg)
            else do
              logDebug "Transport ignoring non-header line"
              readNextMessage
        Left err -> do
          logDebug $ "Transport decode error: " <> err
          readNextMessage

    detectNewlineJsonMessage :: String -> Either T.Text (Maybe JSONRPCMessage)
    detectNewlineJsonMessage s =
      case dropWhile (== ' ') s of
        '{' : _ ->
          case eitherDecode (LBS.fromStrict (BS8.pack s)) of
            Right msg -> Right (Just msg)
            Left errMsg -> Left (T.pack errMsg)
        _ -> Right Nothing

    readHeadersFromFirstLine :: String -> Maybe Int -> IO (Maybe Int)
    readHeadersFromFirstLine firstLine contentLen
      | null firstLine = pure contentLen
      | otherwise = do
          next <- parseHeaderLine firstLine contentLen
          readHeaders next

    readHeaders :: Maybe Int -> IO (Maybe Int)
    readHeaders contentLen = do
      rawLine <- hGetLine handle
      let line = dropTrailingCR rawLine
      if null line
        then pure contentLen
        else do
          next <- parseHeaderLine line contentLen
          readHeaders next

    parseHeaderLine :: String -> Maybe Int -> IO (Maybe Int)
    parseHeaderLine line contentLen =
      let (headerName, rest) = break (== ':') line
       in if null rest
            then pure contentLen
            else
              let value = dropWhile (== ' ') (drop 1 rest)
               in if map toLower headerName == "content-length"
                    then case readMaybe value of
                      Just n | n >= 0 -> pure (Just n)
                      _ -> do
                        logDebug $ "Transport invalid Content-Length value: " <> T.pack value
                        pure contentLen
                    else pure contentLen

    dropTrailingCR :: String -> String
    dropTrailingCR str =
      case reverse str of
        '\r' : xs -> reverse xs
        _ -> str

    toLower :: Char -> Char
    toLower c =
      case c of
        'A' -> 'a'
        'B' -> 'b'
        'C' -> 'c'
        'D' -> 'd'
        'E' -> 'e'
        'F' -> 'f'
        'G' -> 'g'
        'H' -> 'h'
        'I' -> 'i'
        'J' -> 'j'
        'K' -> 'k'
        'L' -> 'l'
        'M' -> 'm'
        'N' -> 'n'
        'O' -> 'o'
        'P' -> 'p'
        'Q' -> 'q'
        'R' -> 'r'
        'S' -> 's'
        'T' -> 't'
        'U' -> 'u'
        'V' -> 'v'
        'W' -> 'w'
        'X' -> 'x'
        'Y' -> 'y'
        'Z' -> 'z'
        other -> other

-- | Write a JSON-RPC message to a handle
writeMessage :: Handle -> StdioFraming -> JSONRPCMessage -> IO (Either MCPError ())
writeMessage handle framing msg = do
  result <- try $ do
    let jsonBytes = LBS.toStrict $ encode msg
    logDebug $
      "Transport sending message (" <> T.pack (show framing) <> "): " <> T.pack (take 200 (BS8.unpack jsonBytes))
    case framing of
      StdioFramingNewlineJson -> do
        BS.hPut handle jsonBytes
        BS.hPut handle (BS8.pack "\n")
      _ -> do
        let header = "Content-Length: " <> BS8.pack (show (BS.length jsonBytes)) <> "\r\n\r\n"
        BS.hPut handle header
        BS.hPut handle jsonBytes
    hFlush handle

  case result of
    Left (ex :: IOException) -> return $ Left $ TransportError $ T.pack $ show ex
    Right _ -> return $ Right ()

instance Transport StdioTransport where
  sendMessage StdioTransport {..} msg = liftIO $ do
    connected <- readTVarIO stConnected
    if connected
      then do
        framing <- readTVarIO stFraming
        writeMessage stOutput framing msg
      else return $ Left $ TransportError "Transport not connected"

  receiveMessage StdioTransport {..} = liftIO $ do
    connected <- readTVarIO stConnected
    if connected
      then do
        result <- atomically $ readTBQueue stMessageQueue
        return $ Right result
      else return $ Left $ TransportError "Transport not connected"

  closeTransport StdioTransport {..} = liftIO $ do
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

  isConnected StdioTransport {..} = liftIO $ readTVarIO stConnected
