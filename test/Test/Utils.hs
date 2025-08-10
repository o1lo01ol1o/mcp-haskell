{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Utils where

import Control.Concurrent.Async
import Control.Concurrent.STM  
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException, bracket, catch)
import System.Timeout (timeout)
import Control.Monad (void, forever)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified StmContainers.Map as STMMap
import qualified StmContainers.Multimap as STMMultimap
import qualified StmContainers.Set as STMSet
import qualified ListT
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import System.Process
import System.IO
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Debug.Trace

-- | LSP message types
data LSPMessage = LSPMessage
  { method :: Text
  , msgId :: Maybe Int
  , params :: Maybe Value
  } deriving (Show)

instance ToJSON LSPMessage where
  toJSON (LSPMessage m mid p) = object $ catMaybes
    [ Just ("jsonrpc" .= ("2.0" :: Text))
    , Just ("method" .= m)
    , ("id" .=) <$> mid
    , ("params" .=) <$> p
    ]
    where
      catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []

-- | HLS process handle with STM-based message routing using stm-containers
data HLSHandle = HLSHandle
  { hlsStdin :: Handle
  , hlsStdout :: Handle  
  , hlsProcess :: ProcessHandle
  , requestCounter :: TVar Int
  , responseQueue :: STMMap.Map Int Value              -- Responses by request ID
  , notificationQueue :: STMMultimap.Multimap Text Value -- Notifications by method name (allows multiple)
  , readerThread :: Async ()                          -- Background reader thread
  }

-- | Start HLS process for testing
startHLS :: FilePath -> IO HLSHandle
startHLS workDir = do
  trace ("Starting HLS in directory: " ++ workDir) $ return ()
  (Just stdin_h, Just stdout_h, Just stderr_h, proc_h) <- 
    createProcess (proc "haskell-language-server-wrapper" ["--lsp"])
      { cwd = Just workDir
      , std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  
  -- Store stderr handle for potential debugging
  hSetBuffering stderr_h LineBuffering
  
  trace "HLS process created, setting up handles..." $ return ()
  
  -- Set binary mode and appropriate buffering for LSP communication
  hSetBinaryMode stdin_h True
  hSetBinaryMode stdout_h True  
  hSetBinaryMode stderr_h False  -- Keep stderr in text mode for error messages
  hSetBuffering stdin_h NoBuffering
  hSetBuffering stdout_h NoBuffering  -- Back to NoBuffering for LSP
  hSetBuffering stderr_h LineBuffering
  
  -- Check if process started successfully
  procStatus <- getProcessExitCode proc_h
  case procStatus of
    Just exitCode -> do
      trace ("HLS process exited immediately with code: " ++ show exitCode) $ return ()
      error $ "HLS process failed to start with exit code: " ++ show exitCode
    Nothing -> trace "HLS process started successfully" $ return ()
  
  -- Give HLS a moment to start up
  threadDelay 500000  -- 0.5 seconds
  
  -- Check again after delay
  procStatus2 <- getProcessExitCode proc_h
  case procStatus2 of
    Just exitCode -> do
      trace ("HLS process exited after startup delay with code: " ++ show exitCode) $ return ()
      error $ "HLS process failed during startup with exit code: " ++ show exitCode
    Nothing -> trace "HLS process still running after startup delay" $ return ()
  
  counter <- newTVarIO 0
  responseQueue <- STMMap.newIO
  notificationQueue <- STMMultimap.newIO
  
  -- Start background reader thread that continuously reads from HLS
  reader <- async $ forever $ do
    result <- readLSPResponseRaw stdout_h
    case result of
      Left err -> do
        trace ("Background reader error: " ++ err) $ return ()
        threadDelay 100000  -- Brief pause before retry
      Right val -> do
        trace ("Background reader got message: " ++ show val) $ return ()
        routeMessage val responseQueue notificationQueue
    `catch` (\(_ :: SomeException) -> threadDelay 100000)
  
  let handle = HLSHandle stdin_h stdout_h proc_h counter responseQueue notificationQueue reader
  
  trace "HLS handle created successfully with STM-based background reader" $ return handle

-- | Route LSP message to appropriate STM container based on LSP spec
routeMessage :: Value -> STMMap.Map Int Value -> STMMultimap.Multimap Text Value -> IO ()
routeMessage val responseQueue notificationQueue = atomically $ do
  case val of
    Object obj -> 
      case KM.lookup "id" obj of
        Just (Number n) -> do
          -- Has ID - this is a response to a request (LSP spec)
          let reqId = floor n
          STMMap.insert val reqId responseQueue
          trace ("Routed response for request ID: " ++ show reqId) $ return ()
        _ -> 
          -- No ID - check if it's a notification with method field (LSP spec)
          case KM.lookup "method" obj of
            Just (String methodName) -> do
              STMMultimap.insert val methodName notificationQueue
              trace ("Routed notification: " ++ T.unpack methodName) $ return ()
            _ -> do
              -- Malformed message - store under "unknown" method
              STMMultimap.insert val "unknown" notificationQueue
              trace ("Routed malformed message as unknown notification") $ return ()
    _ -> do
      -- Non-object message - treat as unknown notification
      STMMultimap.insert val "malformed" notificationQueue
      trace ("Routed non-object message as malformed notification") $ return ()

-- | Read LSP response directly from handle (for background reader)
readLSPResponseRaw :: Handle -> IO (Either String Value)
readLSPResponseRaw handle = do
  result <- try @SomeException $ do
    -- Read Content-Length header
    headerLine <- readLine handle
    case parseContentLength headerLine of
      Nothing -> error $ "Invalid Content-Length header: " ++ headerLine
      Just len -> do
        -- Read empty line
        void $ readLine handle
        -- Read JSON content
        jsonStr <- LBS.hGet handle len
        case decode jsonStr of
          Nothing -> error $ "Invalid JSON response: " ++ show jsonStr
          Just val -> return val
  case result of
    Left ex -> return $ Left $ show ex
    Right val -> return $ Right val

-- | Stop HLS process  
stopHLS :: HLSHandle -> IO ()
stopHLS handle = do
  cancel (readerThread handle)  -- Stop background reader
  hClose (hlsStdin handle)
  hClose (hlsStdout handle)
  terminateProcess (hlsProcess handle)
  void $ waitForProcess (hlsProcess handle)

-- | Send LSP message with process status check
sendLSPMessage :: HLSHandle -> LSPMessage -> IO ()
sendLSPMessage handle msg = do
  -- Check if process is still running before sending
  procStatus <- getProcessExitCode (hlsProcess handle)
  case procStatus of
    Just exitCode -> do
      trace ("HLS process died before sending message, exit code: " ++ show exitCode) $ return ()
      error $ "HLS process terminated with exit code: " ++ show exitCode
    Nothing -> return ()
  
  let jsonBytes = encode msg
  let contentLength = LBS.length jsonBytes
  let header = "Content-Length: " <> show contentLength <> "\r\n\r\n"
  let headerBytes = L8.pack header
  
  trace ("Sending LSP message: " ++ T.unpack (method msg) ++ " (length: " ++ show contentLength ++ ")") $ return ()
  trace ("Message JSON: " ++ show jsonBytes) $ return ()
  
  LBS.hPutStr (hlsStdin handle) headerBytes
  LBS.hPutStr (hlsStdin handle) jsonBytes
  hFlush (hlsStdin handle)
  
  trace "LSP message sent successfully" $ return ()
  
  -- Small delay to let HLS process the message
  threadDelay 100000  -- 0.1 seconds
  
  -- Check if process is still running after sending
  procStatus2 <- getProcessExitCode (hlsProcess handle)
  case procStatus2 of
    Just exitCode -> trace ("HLS process died after sending message, exit code: " ++ show exitCode) $ return ()
    Nothing -> trace "HLS process still alive after message" $ return ()

-- | Read LSP response with timeout and better error handling
readLSPResponse :: HLSHandle -> IO (Either String Value)
readLSPResponse handle = do
  trace "Reading LSP response..." $ return ()
  
  -- First check if process is still running
  procStatus <- getProcessExitCode (hlsProcess handle)
  case procStatus of
    Just exitCode -> do
      trace ("HLS process terminated with exit code: " ++ show exitCode) $ return ()
      return $ Left $ "HLS process terminated with: " ++ show exitCode
    Nothing -> do
      trace "HLS process still running, reading response..." $ return ()
      result <- try @SomeException $ do
        -- Read Content-Length header byte by byte until \r\n
        trace "Reading Content-Length header..." $ return ()
        headerLine <- readLine (hlsStdout handle)
        trace ("Content-Length header: " ++ headerLine) $ return ()
        case parseContentLength headerLine of
          Nothing -> error $ "Invalid Content-Length header: " ++ headerLine
          Just len -> do
            trace ("Content length: " ++ show len) $ return ()
            -- Read empty line (\r\n)  
            trace "Reading empty line..." $ return ()
            void $ readLine (hlsStdout handle)
            -- Read JSON content
            trace ("Reading " ++ show len ++ " bytes of JSON content...") $ return ()
            jsonStr <- LBS.hGet (hlsStdout handle) len
            trace ("Raw JSON response: " ++ show jsonStr) $ return ()
            case decode jsonStr of
              Nothing -> error $ "Invalid JSON response: " ++ show jsonStr
              Just val -> do
                trace ("Parsed JSON successfully: " ++ show val) $ return ()
                return val
              
      case result of
        Left ex -> do
          trace ("Error reading LSP response: " ++ show ex) $ return ()
          return $ Left $ show ex
        Right val -> do
          trace "LSP response read successfully" $ return ()
          return $ Right val

-- Helper function to read a line from handle in binary mode
readLine :: Handle -> IO String
readLine handle = do
  chars <- readUntilCRLF []
  return $ reverse chars
  where
    readUntilCRLF acc = do
      char <- hGetChar handle
      case char of
        '\r' -> do
          nextChar <- hGetChar handle  -- Should be '\n'
          if nextChar == '\n'
            then return acc
            else readUntilCRLF (nextChar : '\r' : acc)
        _ -> readUntilCRLF (char : acc)

-- | Parse Content-Length from header
parseContentLength :: String -> Maybe Int
parseContentLength str
  | "Content-Length: " `isPrefixOf` str = 
      case reads (drop 16 str) of
        [(len, _)] -> Just len
        _ -> Nothing
  | otherwise = Nothing
  where
    isPrefixOf prefix s = take (length prefix) s == prefix

-- | Generate next request ID
nextRequestId :: HLSHandle -> IO Int
nextRequestId handle = atomically $ do
  current <- readTVar (requestCounter handle)
  writeTVar (requestCounter handle) (current + 1)
  return (current + 1)

-- | Wait for a specific response using STM containers
waitForResponse :: HLSHandle -> Int -> IO (Either String Value)
waitForResponse handle reqId = do
  trace ("Waiting for response with ID " ++ show reqId) $ return ()
  result <- timeout 10000000 $ atomically $ do  -- 10 second timeout
    maybeResponse <- STMMap.lookup reqId (responseQueue handle)
    case maybeResponse of
      Just response -> do
        -- Remove the response from the queue and return it
        STMMap.delete reqId (responseQueue handle)
        return response
      Nothing -> retry  -- STM retry until response arrives
  
  case result of
    Just response -> do
      trace ("Found response for ID " ++ show reqId) $ return ()
      return $ Right response
    Nothing -> do
      trace ("Timeout waiting for response ID " ++ show reqId) $ return ()
      return $ Left "Timeout waiting for response"

-- | Get all notifications for a specific method
getNotifications :: HLSHandle -> Text -> IO [Value]
getNotifications handle methodName = atomically $ do
  maybeSet <- STMMultimap.lookupByKey methodName (notificationQueue handle)
  case maybeSet of
    Nothing -> return []
    Just valueSet -> ListT.toList $ STMSet.listT valueSet

-- | Wait for at least one notification of a specific type
waitForNotification :: HLSHandle -> Text -> IO (Maybe Value)
waitForNotification handle methodName = do
  result <- timeout 5000000 $ atomically $ do  -- 5 second timeout
    maybeSet <- STMMultimap.lookupByKey methodName (notificationQueue handle)
    case maybeSet of
      Nothing -> retry  -- No notifications yet
      Just valueSet -> do
        isEmpty <- STMSet.null valueSet  
        if isEmpty
          then retry
          else do
            -- Get one value from set (simplified approach)
            values <- ListT.toList $ STMSet.listT valueSet
            case values of
              [] -> retry
              (first:_) -> return first
  return result

-- | Initialize HLS with basic capabilities
initializeHLS :: HLSHandle -> FilePath -> IO (Either String Value)
initializeHLS handle rootPath = do
  trace ("Initializing HLS with root path: " ++ rootPath) $ return ()
  
  -- Give HLS some time to fully start up
  threadDelay 1000000  -- 1 second
  trace "HLS startup delay complete, sending initialize request..." $ return ()
  
  reqId <- nextRequestId handle
  let initMsg = LSPMessage
        { method = "initialize"
        , msgId = Just reqId
        , params = Just $ object
            [ "processId" .= (1234 :: Int)
            , "rootPath" .= rootPath
            , "rootUri" .= ("file://" <> rootPath)
            , "capabilities" .= object
                [ "textDocument" .= object
                    [ "hover" .= object ["contentFormat" .= ["markdown" :: Text, "plaintext"]]
                    , "publishDiagnostics" .= object []
                    ]
                ]
            ]
        }
  
  sendLSPMessage handle initMsg
  trace "Initialize request sent, waiting for response..." $ return ()
  response <- waitForResponse handle reqId
  
  case response of
    Right val -> do
      trace ("Initialize response received successfully: " ++ show val) $ return ()
      -- Send initialized notification
      let initializedMsg = LSPMessage
            { method = "initialized"
            , msgId = Nothing
            , params = Just $ object []
            }
      trace "Sending initialized notification..." $ return ()
      sendLSPMessage handle initializedMsg
      -- Small delay after initialization  
      threadDelay 500000  -- 0.5 seconds
      
      -- Let the background reader handle any post-initialization notifications
      trace "Letting background reader handle any post-initialization notifications..." $ return ()
      threadDelay 1000000  -- Give it time to receive any immediate notifications
      
      -- Check what notifications we received (for debugging)
      diagnostics <- getNotifications handle "textDocument/publishDiagnostics"
      windowLogs <- getNotifications handle "window/logMessage"
      trace ("Received " ++ show (length diagnostics) ++ " diagnostic notifications") $ return ()
      trace ("Received " ++ show (length windowLogs) ++ " window log messages") $ return ()
      
      trace "HLS initialization complete" $ return ()
      return response
    Left err -> do
      trace ("Initialize request failed: " ++ err) $ return ()
      return $ Left err

-- | Test helper to run HLS operation with proper setup/teardown
withHLS :: FilePath -> (HLSHandle -> IO a) -> IO a
withHLS workDir action = bracket
  (startHLS workDir)
  stopHLS
  $ \handle -> do
    initResult <- initializeHLS handle workDir
    case initResult of
      Left err -> error $ "Failed to initialize HLS: " <> err
      Right _ -> action handle

-- | Helper to create a temporary test workspace
withTestWorkspace :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a  
withTestWorkspace files action = withSystemTempDirectory "hls-test" $ \tmpDir -> do
  -- Create test files
  mapM_ (createTestFile tmpDir) files
  action tmpDir
  where
    createTestFile dir (path, content) = do
      let fullPath = dir </> path
      writeFile fullPath content