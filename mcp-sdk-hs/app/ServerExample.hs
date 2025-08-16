{-# LANGUAGE OverloadedStrings #-}

module ServerExample where

import MCP.SDK.Server
import MCP.SDK.Types
import MCP.SDK.Transport
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A

-- Example server that demonstrates the monad transformer stack
main :: IO ()
main = do
  putStrLn "MCP Server with Monad Transformer Stack Example"
  putStrLn "================================================"
  
  -- Create a transport (this is simplified - in real usage you'd create stdio transport)
  transport <- createStdioTransport stdin stdout
  
  -- Configure the server
  let config = defaultServerConfig
        { serverName = "example-mcp-server"
        , serverVersion = "1.0.0"
        , serverLogLevel = Debug
        , serverMaxConnections = 10
        , serverRequestTimeout = 60
        }
  
  -- Create server handlers that run in ServerM monad
  let handlers = ServerHandlers
        { handleToolsList = exampleToolsListHandler
        , handleToolsCall = exampleToolsCallHandler
        , handleResourcesList = exampleResourcesListHandler
        , handleResourcesRead = exampleResourcesReadHandler
        , handlePromptsList = examplePromptsListHandler
        , handlePromptsGet = examplePromptsGetHandler
        , handlePing = examplePingHandler
        }
  
  -- Create capabilities
  let capabilities = Capabilities
        { roots = Just [Root "file:///" (Just "Example Root")]
        , sampling = Nothing
        , experimental = Nothing
        }
  
  -- Build and run the server
  result <- finalizeServer $ buildServer
    `withServerTransport` transport
    `withServerInfo` "example-server" "1.0.0"
    `withServerCapabilities` capabilities
    `withServerHandlers` handlers
    `withServerConfig` config
  
  case result of
    Left err -> putStrLn $ "Failed to create server: " ++ show err
    Right serverEnv -> do
      putStrLn "Server created successfully!"
      putStrLn "Running server (this demonstrates the monad transformer stack)..."
      
      -- Run the server - this demonstrates the ReaderT + LoggingT + IO stack
      serverResult <- runServer serverEnv
      case serverResult of
        Left err -> putStrLn $ "Server error: " ++ show err
        Right () -> putStrLn "Server completed successfully"

-- Example handlers running in ServerM monad
exampleToolsListHandler :: Maybe Text -> ServerM (Either MCPError ToolsListResponse)
exampleToolsListHandler _cursor = do
  -- Demonstrate logging in ServerM
  logInfoS "Handling tools/list request"
  logDebugS "Creating example tool list"
  
  -- Demonstrate accessing server config from ReaderT
  config <- asks serverConfig
  logDebugS $ "Server configured with timeout: " <> T.pack (show $ serverRequestTimeout config)
  
  -- Create example tools
  let tools = [ Tool "echo" (Just "Echo tool that returns input") 
                     (A.object ["type" A..= ("object" :: Text), 
                               "properties" A..= A.object ["text" A..= A.object ["type" A..= ("string" :: Text)]]])
              , Tool "uppercase" (Just "Convert text to uppercase")
                     (A.object ["type" A..= ("object" :: Text),
                               "properties" A..= A.object ["text" A..= A.object ["type" A..= ("string" :: Text)]]])
              ]
  
  logInfoS $ "Returning " <> T.pack (show $ length tools) <> " tools"
  return $ Right $ ToolsListResp tools

exampleToolsCallHandler :: Text -> Maybe A.Object -> ServerM (Either MCPError ToolsCallResponse)
exampleToolsCallHandler toolName args = do
  logInfoS $ "Handling tools/call request for: " <> toolName
  
  case toolName of
    "echo" -> do
      logDebugS "Executing echo tool"
      let result = case args >>= A.parseMaybe (A..: "text") of
            Just text -> [TextContent text]
            Nothing -> [TextContent "No text provided"]
      return $ Right $ ToolsCallResp [ToolCallResult result Nothing]
    
    "uppercase" -> do
      logDebugS "Executing uppercase tool"
      let result = case args >>= A.parseMaybe (A..: "text") of
            Just text -> [TextContent (T.toUpper text)]
            Nothing -> [TextContent "NO TEXT PROVIDED"]
      return $ Right $ ToolsCallResp [ToolCallResult result Nothing]
    
    _ -> do
      logWarnS $ "Unknown tool requested: " <> toolName
      return $ Left $ ToolExecutionError $ "Unknown tool: " <> toolName

exampleResourcesListHandler :: Maybe Text -> ServerM (Either MCPError ResourcesListResponse)
exampleResourcesListHandler _cursor = do
  logInfoS "Handling resources/list request"
  
  let resources = [ Resource "file:///example.txt" (Just "Example File") 
                             (Just "An example text file") (Just "text/plain")
                  , Resource "file:///data.json" (Just "Data File")
                             (Just "JSON data file") (Just "application/json")
                  ]
  
  logDebugS $ "Returning " <> T.pack (show $ length resources) <> " resources"
  return $ Right $ ResourcesListResp resources

exampleResourcesReadHandler :: Text -> ServerM (Either MCPError ResourcesReadResponse)
exampleResourcesReadHandler uri = do
  logInfoS $ "Handling resources/read request for: " <> uri
  
  -- Demonstrate error handling in ServerM
  case uri of
    "file:///example.txt" -> do
      let content = ResourceContent uri (Just "text/plain") (Just "This is example content") Nothing
      return $ Right $ ResourcesReadResp [content]
    
    "file:///data.json" -> do
      let jsonData = "{\"key\": \"value\", \"number\": 42}"
      let content = ResourceContent uri (Just "application/json") (Just jsonData) Nothing
      return $ Right $ ResourcesReadResp [content]
    
    _ -> do
      logWarnS $ "Resource not found: " <> uri
      return $ Left $ ResourceNotFound uri

examplePromptsListHandler :: Maybe Text -> ServerM (Either MCPError PromptsListResponse)
examplePromptsListHandler _cursor = do
  logInfoS "Handling prompts/list request"
  
  let prompts = [ Prompt "greeting" (Just "Generate a greeting message")
                         (Just [PromptArgument "name" (Just "Name of person to greet") (Just True)])
                , Prompt "summary" (Just "Summarize text")
                         (Just [PromptArgument "text" (Just "Text to summarize") (Just True),
                               PromptArgument "max_length" (Just "Maximum summary length") (Just False)])
                ]
  
  return $ Right $ PromptsListResp prompts

examplePromptsGetHandler :: Text -> Maybe A.Object -> ServerM (Either MCPError PromptsGetResponse)
examplePromptsGetHandler promptName args = do
  logInfoS $ "Handling prompts/get request for: " <> promptName
  
  case promptName of
    "greeting" -> do
      let name = case args >>= A.parseMaybe (A..: "name") of
            Just n -> n
            Nothing -> "World"
      let message = PromptMessage "user" (TextContent $ "Hello, " <> name <> "!")
      return $ Right $ PromptsGetResp [message]
    
    "summary" -> do
      let text = case args >>= A.parseMaybe (A..: "text") of
            Just t -> t
            Nothing -> ""
      let maxLen = case args >>= A.parseMaybe (A..: "max_length") of
            Just len -> len
            Nothing -> 100
      let summary = T.take maxLen text <> if T.length text > maxLen then "..." else ""
      let message = PromptMessage "assistant" (TextContent $ "Summary: " <> summary)
      return $ Right $ PromptsGetResp [message]
    
    _ -> do
      logWarnS $ "Unknown prompt requested: " <> promptName
      return $ Left $ ResourceNotFound promptName

examplePingHandler :: ServerM (Either MCPError PingResponse)
examplePingHandler = do
  logDebugS "Handling ping request"
  
  -- Demonstrate accessing server environment
  env <- ask
  let serverName = serverName $ serverConfig env
  logInfoS $ "Ping from server: " <> serverName
  
  return $ Right PingResp