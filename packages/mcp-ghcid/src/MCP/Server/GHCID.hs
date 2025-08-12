{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.GHCID
  ( runGHCIDServer
  , createGHCIDServer
  , GHCIDServer(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch, SomeException, try)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stderr, stdout, stdin, hFlush, hIsEOF, hReady, Handle, openFile, IOMode(..), hClose)
import qualified System.IO
import System.IO.Error (isEOFError)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))

-- MCP imports
import MCP.Types
import MCP.Protocol
import Utils.Logging

-- GHCID imports
import MCP.Tools.GHCID
import GHCID.Config

-- | Server capabilities for MCP protocol
data GHCIDServerCapabilities = GHCIDServerCapabilities
  { capServerName :: Text
  , capServerVersion :: Text
  , capProtocolVersion :: Text
  , capTools :: [Tool]
  , capResources :: [Value]
  } deriving (Show)

-- | GHCID MCP Server state
data GHCIDServer = GHCIDServer
  { ghcidRegistry :: GHCIDRegistry
  , serverConfig :: GHCIDServerConfig
  , serverCapabilities :: GHCIDServerCapabilities
  , logHandle :: Maybe Handle  -- Handle for message logging
  }

-- | Create GHCID MCP server  
createGHCIDServer :: GHCIDServerConfig -> IO GHCIDServer
createGHCIDServer config = do
  registry <- registerGHCIDTools
  
  -- Open log file for message logging
  logHandle <- openMessageLogFile
  
  let capabilities = GHCIDServerCapabilities
        { capServerName = serverName config
        , capServerVersion = serverVersion config
        , capProtocolVersion = mcpVersion
        , capTools = ghcidTools
        , capResources = []  -- Could add project discovery resources
        }
  
  return GHCIDServer
    { ghcidRegistry = registry
    , serverConfig = config  
    , serverCapabilities = capabilities
    , logHandle = logHandle
    }

-- | Open log file for message logging in a writable location
openMessageLogFile :: IO (Maybe Handle)
openMessageLogFile = do
  result <- try $ do
    -- Get user's home directory and create logs subdirectory
    homeDir <- getHomeDirectory
    let logsDir = homeDir </> ".mcp-ghcid"
        logFile = logsDir </> "messages.log"
    
    -- Create directory if it doesn't exist
    createDirectoryIfMissing True logsDir
    
    -- Open log file
    openFile logFile AppendMode
  
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to open message log file: " <> T.pack (show ex)
      return Nothing
    Right handle -> do
      homeDir <- getHomeDirectory
      let logPath = homeDir </> ".mcp-ghcid" </> "messages.log"
      logInfo $ "Opened message log file: " <> T.pack logPath
      return (Just handle)

-- | Log a JSON-RPC message to the message log file with timestamp
logJsonRpcMessage :: GHCIDServer -> Text -> Text -> IO ()
logJsonRpcMessage server direction content = do
  case logHandle server of
    Nothing -> return ()  -- No logging if file couldn't be opened
    Just handle -> do
      timestamp <- getCurrentTime
      let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" timestamp
          logEntry = T.concat [timeStr, " [", direction, "] ", content, "\n"]
      result <- try $ do
        T.hPutStr handle logEntry
        hFlush handle
      case result of
        Left (ex :: SomeException) -> 
          logError $ "Failed to write to message log: " <> T.pack (show ex)
        Right _ -> return ()

-- | Log an incoming JSON-RPC request
logIncomingMessage :: GHCIDServer -> JsonRpcRequest -> IO ()
logIncomingMessage server request = do
  let jsonStr = T.decodeUtf8 $ L8.toStrict $ encode request
  logJsonRpcMessage server "RECV" jsonStr

-- | Log an outgoing JSON-RPC response  
logOutgoingMessage :: GHCIDServer -> JsonRpcResponse -> IO ()
logOutgoingMessage server response = do
  let jsonStr = T.decodeUtf8 $ L8.toStrict $ encode response
  logJsonRpcMessage server "SEND" jsonStr

-- | Run the GHCID MCP server
runGHCIDServer :: GHCIDServerConfig -> IO ()
runGHCIDServer config = do
  server <- createGHCIDServer config
  
  logInfo $ "Starting MCP-GHCID Server v" <> serverVersion config
  -- STDIO protocol compliance: no stderr output during normal operation
  
  -- Log startup message to message log
  logJsonRpcMessage server "INFO" "=== MCP-GHCID Server Started ==="
  
  serverLoop server `catch` \(ex :: SomeException) -> do
    logError $ "Server error: " <> T.pack (show ex)
    -- Only log critical errors to stderr in exceptional cases
    System.IO.hPutStrLn stderr $ "Critical server error: " ++ show ex
  
  -- Cleanup: close log file handle
  case logHandle server of
    Nothing -> return ()
    Just handle -> do
      logInfo "Closing message log file"
      hClose handle

-- | Main server loop
serverLoop :: GHCIDServer -> IO ()
serverLoop server = do
  logInfo "Entering server loop"
  serverLoop' server
  where
    serverLoop' srv = do
      result <- MCP.Server.GHCID.readMessage
      case result of
        Left err -> do
          logError $ "Failed to read message: " <> err
          -- Continue the loop instead of crashing
          serverLoop' srv
        Right Nothing -> do
          -- EOF encountered - client has disconnected
          logJsonRpcMessage srv "INFO" "Client disconnected - EOF received"
          logInfo "Client disconnected, shutting down server"
          return ()
        Right (Just request) -> do
          -- Log incoming message
          logIncomingMessage srv request
          
          response <- handleRequest srv request
          case response of
            Left "No response needed for notification" -> do
              -- Notification processed successfully, no response to send
              logInfo "Notification processed successfully"
            Left err -> do
              let errorResponse = createErrorResponse internalError err Nothing (getRequestId request)
              sendResponse srv errorResponse
            Right resp -> sendResponse srv resp
          -- Continue the loop
          serverLoop' srv

-- | Read a JSON-RPC message from stdin with EOF handling
readMessage :: IO (Either Text (Maybe JsonRpcRequest))
readMessage = do
  result <- try MCP.Protocol.readLineWithEOF
  case result of
    Left ex | isEOFError ex -> do
      -- EOF is normal when client disconnects, return Nothing to indicate this
      return $ Right Nothing
    Left ex -> do
      -- Other IO errors
      return $ Left $ "IO error reading from stdin: " <> T.pack (show ex)
    Right Nothing -> do
      -- EOF detected by hIsEOF check
      return $ Right Nothing
    Right (Just line) -> do
      -- Successfully read a line, try to parse it
      case eitherDecodeStrict $ T.encodeUtf8 $ T.pack line of
        Left err -> return $ Left $ "Failed to decode JSON: " <> T.pack err
        Right request -> return $ Right $ Just request


-- | Send a JSON-RPC response to stdout
sendResponse :: GHCIDServer -> JsonRpcResponse -> IO ()
sendResponse server response = do
  -- Log outgoing message
  logOutgoingMessage server response
  
  -- Send to stdout
  L8.putStrLn $ encode response
  hFlush stdout

-- | Get request ID from request
getRequestId :: JsonRpcRequest -> Maybe Value
getRequestId (JsonRpcRequest _ _ _ reqId) = reqId

-- | Handle incoming MCP request
handleRequest :: GHCIDServer -> JsonRpcRequest -> IO (Either Text JsonRpcResponse)
handleRequest server request = do
  case validateRequest request of
    Left err -> return $ Right $ createErrorResponse (code err) (message err) (errorData err) (getRequestId request)
    Right _ -> do
      let methodName = method request
      logInfo $ "Handling request: " <> methodName
      
      result <- case methodName of
        "initialize" -> handleInitializeRequest server request
        "initialized" -> handleInitializedNotification server request
        "tools/call" -> handleToolCallRequest server request
        "tools/list" -> handleToolsListRequest server request
        "resources/list" -> handleResourcesListRequest server request
        _ -> return $ Left $ "Method not found: " <> methodName
      
      case result of
        Left err -> return $ Right $ createErrorResponse methodNotFound err Nothing (getRequestId request)
        Right value -> 
          -- Check if this is a notification (no id field) - don't send response
          case getRequestId request of
            Nothing -> return $ Left "No response needed for notification"
            Just _ -> return $ Right $ createResponse (Just value) Nothing (getRequestId request)

-- | Handle initialize request
handleInitializeRequest :: GHCIDServer -> JsonRpcRequest -> IO (Either Text Value)
handleInitializeRequest server _request = do
  logInfo "Processing initialize request"
  return $ Right $ object
    [ "protocolVersion" .= capProtocolVersion (serverCapabilities server)
    , "capabilities" .= object
        [ "tools" .= object
            [ "listChanged" .= False  -- Tools don't change dynamically
            ]
        , "resources" .= object
            [ "subscribe" .= False    -- No resource subscriptions yet
            , "listChanged" .= False
            ]
        ]
    , "serverInfo" .= object
        [ "name" .= capServerName (serverCapabilities server)
        , "version" .= serverVersion (serverConfig server)
        ]
    ]

-- | Handle initialized notification (no response needed)
handleInitializedNotification :: GHCIDServer -> JsonRpcRequest -> IO (Either Text Value)
handleInitializedNotification _server _request = do
  logInfo "Processing initialized notification - server is ready"
  -- For notifications, we return success but no response body is sent
  return $ Right $ object []

-- | Handle tool call request
handleToolCallRequest :: GHCIDServer -> JsonRpcRequest -> IO (Either Text Value)
handleToolCallRequest server request = do
  case params request of
    Nothing -> return $ Left "Missing tool call parameters"
    Just paramValue -> do
      case fromJSON paramValue of
        Data.Aeson.Error err -> return $ Left $ "Invalid tool call parameters: " <> T.pack err
        Success toolCall -> do
          let toolName = toolCallName toolCall
              toolParams = toolCallArguments toolCall
          
          logInfo $ "Calling tool: " <> toolName
          result <- handleGHCIDTool (ghcidRegistry server) toolName toolParams
          
          case result of
            Left err -> return $ Left err
            Right value -> return $ Right $ object
              [ "content" .= [object 
                  [ "type" .= ("text" :: Text)
                  , "text" .= value
                  ]
                ]
              ]

-- | Handle tools list request  
handleToolsListRequest :: GHCIDServer -> JsonRpcRequest -> IO (Either Text Value)
handleToolsListRequest server _request = do
  logInfo "Processing tools/list request"
  let tools = capTools (serverCapabilities server)
  return $ Right $ object
    [ "tools" .= Prelude.map toolToJSON tools
    ]
  where
    toolToJSON (Tool toolName toolDesc toolInputSchema) = object
      [ "name" .= toolName
      , "description" .= toolDesc  
      , "inputSchema" .= toolInputSchema
      ]

-- | Handle resources list request
handleResourcesListRequest :: GHCIDServer -> JsonRpcRequest -> IO (Either Text Value)
handleResourcesListRequest _server _request = do
  logInfo "Processing resources/list request"
  -- Could add project discovery resources here
  return $ Right $ object
    [ "resources" .= ([] :: [Value])
    ]

-- Helper data types

data ServerToolCall = ServerToolCall
  { toolCallName :: Text
  , toolCallArguments :: Value
  } deriving (Show)

instance FromJSON ServerToolCall where
  parseJSON = withObject "ToolCall" $ \o -> ServerToolCall
    <$> o .: "name"
    <*> o .: "arguments"