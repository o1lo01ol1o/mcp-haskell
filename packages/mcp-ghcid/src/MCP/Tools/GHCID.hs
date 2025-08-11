{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Tools.GHCID
  ( -- * MCP Tools
    ghcidTools
  , handleGHCIDTool
  
    -- * Individual tools
  , startGHCIDTool
  , stopGHCIDTool  
  , restartGHCIDTool
  , getStatusTool
  , getMessagesTool
  , listProcessesTool
  
    -- * Tool registry
  , registerGHCIDTools
  , GHCIDRegistry
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.Map.Strict as Map

-- MCP imports
import MCP.Types
import MCP.Protocol
import Utils.Logging

-- GHCID imports  
import GHCID.Client
import GHCID.Config
import GHCID.Filter (FilterRequest(..), applyShellFilter)
import GHCID.Output (formatCompilerMessage)

-- | Global registry of active GHCID processes
type GHCIDRegistry = TVar (Map.Map Text GHCIDClient)

-- | All GHCID MCP tools
ghcidTools :: [Tool]
ghcidTools = 
  [ startGHCIDTool
  , stopGHCIDTool
  , restartGHCIDTool
  , getStatusTool
  , getMessagesTool      -- NEW: Now supports filtering
  , listProcessesTool
  ]
-- NOTE: clearMessagesTool removed - we always operate on current output

-- | Handle GHCID tool requests
handleGHCIDTool :: GHCIDRegistry -> Text -> Value -> IO (Either Text Value)
handleGHCIDTool registry toolName params = do
  case toolName of
    "ghcid.start" -> handleStartGHCID registry params
    "ghcid.stop" -> handleStopGHCID registry params
    "ghcid.restart" -> handleRestartGHCID registry params
    "ghcid.status" -> handleGetStatus registry params
    "ghcid.messages" -> handleGetMessages registry params
    "ghcid.list" -> handleListProcesses registry params
    _ -> return $ Left $ "Unknown GHCID tool: " <> toolName

-- | Start GHCID tool definition
startGHCIDTool :: Tool
startGHCIDTool = Tool
  { name = "ghcid.start"
  , description = Just "Start a GHCID process for a Haskell project"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Unique name for this GHCID process" :: Text)
              ]
          , "workingDir" .= object  
              [ "type" .= ("string" :: Text)
              , "description" .= ("Working directory (defaults to current)" :: Text)
              ]
          , "targetFiles" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "description" .= ("Target Haskell files to load" :: Text)
              ]
          , "cabalFile" .= object
              [ "type" .= ("string" :: Text)  
              , "description" .= ("Path to cabal file (optional)" :: Text)
              ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

-- | Stop GHCID tool definition
stopGHCIDTool :: Tool  
stopGHCIDTool = Tool
  { name = "ghcid.stop"
  , description = Just "Stop a running GHCID process"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the GHCID process to stop" :: Text)
              ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

-- | Restart GHCID tool definition
restartGHCIDTool :: Tool
restartGHCIDTool = Tool
  { name = "ghcid.restart"
  , description = Just "Restart a GHCID process" 
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the GHCID process to restart" :: Text)
              ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

-- | Get status tool definition
getStatusTool :: Tool
getStatusTool = Tool
  { name = "ghcid.status"
  , description = Just "Get status of a GHCID process"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the GHCID process" :: Text)
              ]
          ]
      , "required" .= (["name"] :: [Text])
      ]
  }

-- | Get messages tool definition with mutually exclusive filtering
getMessagesTool :: Tool
getMessagesTool = Tool
  { name = "ghcid.messages"  
  , description = Just "Get current GHCID output with optional filtering (use ONE of: grep, head, tail, or lines)"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Name of the GHCID process" :: Text)
              ]
          , "grep" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Grep pattern to filter output lines" :: Text)
              ]
          , "head" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Show first N lines of output" :: Text)
              ]
          , "tail" .= object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Show last N lines of output" :: Text)
              ]
          , "lines" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Line range (e.g., '10-20') to extract" :: Text)
              ]
          ]
      , "required" .= (["name"] :: [Text])
      , "additionalProperties" .= False
      , "oneOf" .= 
          [ object ["required" .= (["name"] :: [Text])]  -- No filter
          , object ["required" .= (["name", "grep"] :: [Text])]
          , object ["required" .= (["name", "head"] :: [Text])]  
          , object ["required" .= (["name", "tail"] :: [Text])]
          , object ["required" .= (["name", "lines"] :: [Text])]
          ]
      ]
  }

-- NOTE: clearMessagesTool removed - we always operate on current ghcid output

-- | List processes tool definition
listProcessesTool :: Tool
listProcessesTool = Tool
  { name = "ghcid.list"
  , description = Just "List all active GHCID processes"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object []
      ]
  }

-- Tool handlers

-- | Handle start GHCID request
handleStartGHCID :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleStartGHCID registry params = do
  case fromJSON params of
    Data.Aeson.Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
    Success req -> do
      let name = startName req
          workDir = maybe "." Prelude.id (startWorkingDir req)
          
      -- Detect project configuration
      config <- detectProjectConfig workDir
      let finalConfig = config 
            { targetFiles = maybe (targetFiles config) Prelude.id (startTargetFiles req)
            , cabalFile = startCabalFile req <|> cabalFile config
            }
      
      -- Create and start GHCID client
      result <- try @SomeException $ do
        client <- createGHCIDClient finalConfig
        startResult <- startGHCID client
        case startResult of
          Left err -> return $ Left err
          Right _ -> do
            -- Register the client
            atomically $ modifyTVar registry (Map.insert name client)
            return $ Right $ object 
              [ "success" .= True
              , "message" .= ("GHCID process started successfully" :: Text)
              , "name" .= name
              , "workingDir" .= workDir
              ]
              
      case result of
        Left ex -> return $ Left $ "Failed to start GHCID: " <> T.pack (show ex)
        Right res -> return res

-- | Handle stop GHCID request  
handleStopGHCID :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleStopGHCID registry params = do
  case fromJSON params of
    Data.Aeson.Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
    Success req -> do
      let name = stopName req
      
      clients <- readTVarIO registry
      case Map.lookup name clients of
        Nothing -> return $ Left $ "GHCID process not found: " <> name
        Just client -> do
          result <- stopGHCID client
          case result of
            Left err -> return $ Left err
            Right _ -> do
              -- Unregister the client
              atomically $ modifyTVar registry (Map.delete name)
              return $ Right $ object
                [ "success" .= True
                , "message" .= ("GHCID process stopped successfully" :: Text)
                , "name" .= name
                ]

-- | Handle restart GHCID request
handleRestartGHCID :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleRestartGHCID registry params = do
  case fromJSON params of
    Data.Aeson.Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
    Success req -> do
      let name = restartName req
      
      clients <- readTVarIO registry
      case Map.lookup name clients of
        Nothing -> return $ Left $ "GHCID process not found: " <> name
        Just client -> do
          result <- restartGHCID client
          case result of
            Left err -> return $ Left err
            Right _ -> return $ Right $ object
              [ "success" .= True
              , "message" .= ("GHCID process restarted successfully" :: Text)
              , "name" .= name
              ]

-- | Handle get status request
handleGetStatus :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleGetStatus registry params = do
  case fromJSON params of
    Data.Aeson.Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
    Success req -> do
      let name = statusName req
      
      clients <- readTVarIO registry
      case Map.lookup name clients of
        Nothing -> return $ Left $ "GHCID process not found: " <> name
        Just client -> do
          status <- getGHCIDStatus client
          return $ Right $ object
            [ "name" .= name
            , "status" .= T.pack (show status)
            ]

-- | Handle get messages request with filtering support
handleGetMessages :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleGetMessages registry params = do
  case fromJSON params of
    Data.Aeson.Error err -> return $ Left $ "Invalid parameters: " <> T.pack err
    Success req -> do
      let name = messagesName req
          filterReq = extractFilterRequest req
      
      clients <- readTVarIO registry
      case Map.lookup name clients of
        Nothing -> return $ Left $ "GHCID process not found: " <> name
        Just client -> do
          -- Get current output from buffer
          rawOutput <- getCurrentOutput client
          
          -- Apply shell filter if specified
          filteredResult <- applyShellFilter rawOutput filterReq
          case filteredResult of
            Left filterErr -> return $ Left $ "Filter error: " <> filterErr
            Right filteredOutput -> 
              return $ Right $ object
                [ "name" .= name
                , "output" .= filteredOutput
                , "filtered" .= (filterReq /= NoFilterRequest)
                , "totalLines" .= length (T.lines rawOutput)
                , "filteredLines" .= length (T.lines filteredOutput)
                ]

-- NOTE: handleClearMessages removed - we always operate on current output

-- | Handle list processes request
handleListProcesses :: GHCIDRegistry -> Value -> IO (Either Text Value)
handleListProcesses registry _params = do
  clients <- readTVarIO registry
  processInfo <- mapM getProcessInfo (Map.toList clients)
  return $ Right $ object
    [ "processes" .= processInfo
    , "count" .= Map.size clients
    ]
  where
    getProcessInfo (name, client) = do
      status <- getGHCIDStatus client
      messages <- getCompilerMessages client
      return $ object
        [ "name" .= name
        , "status" .= T.pack (show status)
        , "messageCount" .= length messages
        ]

-- Helper functions and data types

data StartGHCIDRequest = StartGHCIDRequest
  { startName :: Text
  , startWorkingDir :: Maybe FilePath
  , startTargetFiles :: Maybe [FilePath] 
  , startCabalFile :: Maybe FilePath
  } deriving (Show)

instance FromJSON StartGHCIDRequest where
  parseJSON = withObject "StartGHCIDRequest" $ \o -> StartGHCIDRequest
    <$> o .: "name"
    <*> o .:? "workingDir"
    <*> o .:? "targetFiles"
    <*> o .:? "cabalFile"

data StopGHCIDRequest = StopGHCIDRequest { stopName :: Text } deriving (Show)
instance FromJSON StopGHCIDRequest where
  parseJSON = withObject "StopGHCIDRequest" $ \o -> StopGHCIDRequest <$> o .: "name"

data RestartGHCIDRequest = RestartGHCIDRequest { restartName :: Text } deriving (Show)  
instance FromJSON RestartGHCIDRequest where
  parseJSON = withObject "RestartGHCIDRequest" $ \o -> RestartGHCIDRequest <$> o .: "name"

data StatusRequest = StatusRequest { statusName :: Text } deriving (Show)
instance FromJSON StatusRequest where
  parseJSON = withObject "StatusRequest" $ \o -> StatusRequest <$> o .: "name"

data MessagesRequest = MessagesRequest 
  { messagesName :: Text
  , messagesFilter :: FilterRequest
  } deriving (Show)
  
instance FromJSON MessagesRequest where
  parseJSON = withObject "MessagesRequest" $ \o -> do
    name <- o .: "name"
    filterReq <- parseJSON (Object o)  -- Reuse FilterRequest FromJSON
    return $ MessagesRequest name filterReq

-- Extract filter request from messages request
extractFilterRequest :: MessagesRequest -> FilterRequest
extractFilterRequest = messagesFilter

-- | Format compiler message for MCP response
formatMessageForMCP :: CompilerMessage -> Value
formatMessageForMCP msg = object
  [ "severity" .= T.pack (show (msgSeverity msg))
  , "file" .= msgFile msg
  , "line" .= msgLine msg
  , "column" .= msgColumn msg
  , "message" .= msgText msg
  , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (msgTimestamp msg)
  , "formatted" .= formatCompilerMessage msg
  ]

-- | Register GHCID tools with MCP server
registerGHCIDTools :: IO GHCIDRegistry
registerGHCIDTools = do
  registry <- newTVarIO Map.empty
  logInfo "GHCID MCP tools registered"
  return registry

-- Helper for Maybe alternative
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> b = b
a <|> _ = a