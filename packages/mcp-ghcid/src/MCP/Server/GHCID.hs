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
import System.IO (stderr, stdout, stdin, hFlush, hIsEOF, hReady)
import qualified System.IO
import System.IO.Error (isEOFError)

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
  }

-- | Create GHCID MCP server  
createGHCIDServer :: GHCIDServerConfig -> IO GHCIDServer
createGHCIDServer config = do
  registry <- registerGHCIDTools
  
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
    }

-- | Run the GHCID MCP server
runGHCIDServer :: GHCIDServerConfig -> IO ()
runGHCIDServer config = do
  server <- createGHCIDServer config
  
  logInfo $ "Starting MCP-GHCID Server v" <> serverVersion config
  System.IO.hPutStrLn stderr "MCP-GHCID Server starting..."
  
  serverLoop server `catch` \(ex :: SomeException) -> do
    logError $ "Server error: " <> T.pack (show ex)
    System.IO.hPutStrLn stderr $ "Server error: " ++ show ex

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
          -- EOF encountered, wait briefly and check again
          logInfo "EOF encountered, waiting for reconnection..."
          threadDelay 100000  -- Wait 100ms
          serverLoop' srv
        Right (Just request) -> do
          response <- handleRequest srv request
          case response of
            Left err -> do
              let errorResponse = createErrorResponse internalError err Nothing (getRequestId request)
              sendResponse errorResponse
            Right resp -> sendResponse resp
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
sendResponse :: JsonRpcResponse -> IO ()
sendResponse response = do
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
        "tools/call" -> handleToolCallRequest server request
        "tools/list" -> handleToolsListRequest server request
        "resources/list" -> handleResourcesListRequest server request
        _ -> return $ Left $ "Method not found: " <> methodName
      
      case result of
        Left err -> return $ Right $ createErrorResponse methodNotFound err Nothing (getRequestId request)
        Right value -> return $ Right $ createResponse (Just value) Nothing (getRequestId request)

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