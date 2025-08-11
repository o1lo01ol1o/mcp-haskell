{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Router.GHCID
  ( -- * MCP Router
    GHCIDRouter(..)
  , createGHCIDRouter
  , routeGHCIDRequest
  , shutdownGHCIDRouter
  
    -- * Handler Implementation
  , createRequestHandlers
  , GHCIDRouterConfig(..)
  , defaultRouterConfig
  
    -- * Request Processing
  , processJSONRequest
  , processTypedRequest
  
    -- * Error Handling
  , RouterError(..)
  , handleRouterError
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (when, unless)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Time
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath (takeDirectory)

-- Internal imports
import GHCID.ProcessRegistry
import GHCID.ResourceManager
import GHCID.Filter (applyShellFilter)
import MCP.Types.GHCID
import Utils.Logging

-- | Router configuration
data GHCIDRouterConfig = GHCIDRouterConfig
  { routerEnableHealthChecks :: Bool
  , routerMaxProcesses :: Int
  , routerDefaultTimeout :: Int  -- seconds
  , routerLogRequests :: Bool
  } deriving (Show, Eq)

-- | Default router configuration  
defaultRouterConfig :: GHCIDRouterConfig
defaultRouterConfig = GHCIDRouterConfig
  { routerEnableHealthChecks = True
  , routerMaxProcesses = 10
  , routerDefaultTimeout = 30
  , routerLogRequests = True
  }

-- | Router error types
data RouterError
  = ProcessNotFound CabalURI
  | ProcessAlreadyExists CabalURI
  | ProcessStartFailed CabalURI Text
  | ProcessStopFailed CabalURI Text
  | InvalidRequest Text
  | RegistryShutdown
  | ResourceExhausted Text
  | TimeoutError Text
  deriving (Show, Eq)

-- | GHCID MCP router
data GHCIDRouter = GHCIDRouter
  { routerRegistry :: ProcessRegistry
  , routerConfig :: GHCIDRouterConfig
  , routerRequestCount :: TVar Int
  , routerIsShutdown :: TVar Bool
  }

-- | Create a new GHCID router
createGHCIDRouter :: GHCIDRouterConfig -> IO GHCIDRouter
createGHCIDRouter config = do
  logInfo "Creating GHCID MCP router"
  registry <- createProcessRegistry
  requestCount <- newTVarIO 0
  shutdown <- newTVarIO False
  
  let router = GHCIDRouter
        { routerRegistry = registry
        , routerConfig = config
        , routerRequestCount = requestCount
        , routerIsShutdown = shutdown
        }
  
  logInfo "GHCID MCP router created successfully"
  return router

-- | Shutdown the router
shutdownGHCIDRouter :: GHCIDRouter -> IO ()
shutdownGHCIDRouter GHCIDRouter{..} = do
  logInfo "Shutting down GHCID MCP router"
  atomically $ writeTVar routerIsShutdown True
  shutdownProcessRegistry routerRegistry
  logInfo "GHCID MCP router shutdown complete"

-- | Route a GHCID request through the router
routeGHCIDRequest :: GHCIDRouter -> RequestId -> SomeGHCIDRequest -> IO Value
routeGHCIDRequest router@GHCIDRouter{..} reqId request = do
  -- Check if router is shutdown
  isShutdown <- readTVarIO routerIsShutdown
  when isShutdown $ throwIO $ userError "Router is shutdown"
  
  -- Increment request counter
  atomically $ modifyTVar routerRequestCount (+1)
  
  -- Log request if enabled
  when (routerLogRequests routerConfig) $
    logInfo $ "Processing GHCID request: " <> getRequestId reqId
  
  -- Create handlers and process request
  handlers <- createRequestHandlers router
  result <- try @SomeException $ handleGHCIDRequest handlers reqId request
  
  case result of
    Left ex -> do
      logError $ "Error processing request " <> getRequestId reqId <> ": " <> T.pack (show ex)
      return $ encodeGHCIDResponse $ ErrorResponse reqId $ T.pack $ show ex
    Right response -> return response

-- | Create request handlers for the router
createRequestHandlers :: GHCIDRouter -> IO RequestHandler
createRequestHandlers router@GHCIDRouter{..} = do
  return RequestHandler
    { handleStart = handleStartRequest router
    , handleStop = handleStopRequest router
    , handleMessages = handleMessagesRequest router
    , handleList = handleListRequest router
    , handleStatus = handleStatusRequest router
    , handleRestart = handleRestartRequest router
    }

-- | Handle start GHCID request
handleStartRequest :: GHCIDRouter -> StartGHCIDData -> IO StartGHCIDResult
handleStartRequest GHCIDRouter{..} StartGHCIDData{..} = do
  logInfo $ "Starting GHCID process for " <> getCabalURI startCabalURI
  
  -- Check process limits
  activeCount <- length <$> listActiveProcesses routerRegistry
  when (activeCount >= routerMaxProcesses routerConfig) $
    throwIO $ userError "Maximum process limit reached"
  
  -- Start the process
  result <- startGHCIDProcess routerRegistry startCabalURI startWorkDir
  case result of
    Left err -> do
      logError $ "Failed to start GHCID process: " <> err
      return StartGHCIDResult
        { startSuccess = False
        , startMessage = err
        , startProcessId = Nothing
        }
    Right handle -> do
      logInfo $ "GHCID process started successfully for " <> getCabalURI startCabalURI
      return StartGHCIDResult
        { startSuccess = True
        , startMessage = "GHCID process started successfully"
        , startProcessId = Just $ getCabalURI startCabalURI
        }

-- | Handle stop GHCID request
handleStopRequest :: GHCIDRouter -> StopGHCIDData -> IO StopGHCIDResult
handleStopRequest GHCIDRouter{..} StopGHCIDData{..} = do
  logInfo $ "Stopping GHCID process for " <> getCabalURI stopCabalURI
  
  result <- if stopForce
    then stopGHCIDProcessWithTimeout routerRegistry 5 stopCabalURI
    else stopGHCIDProcess routerRegistry stopCabalURI
  
  case result of
    Left err -> do
      logError $ "Failed to stop GHCID process: " <> err
      return StopGHCIDResult
        { stopSuccess = False
        , stopMessage = err
        }
    Right _ -> do
      logInfo $ "GHCID process stopped successfully for " <> getCabalURI stopCabalURI
      return StopGHCIDResult
        { stopSuccess = True
        , stopMessage = "GHCID process stopped successfully"
        }

-- | Handle get messages request
handleMessagesRequest :: GHCIDRouter -> GetMessagesData -> IO MessagesResult
handleMessagesRequest GHCIDRouter{..} GetMessagesData{..} = do
  logInfo $ "Getting messages for " <> getCabalURI messagesCabalURI
  
  maybeHandle <- getGHCIDProcess routerRegistry messagesCabalURI
  case maybeHandle of
    Nothing -> throwIO $ userError $ "GHCID process not found: " ++ T.unpack (getCabalURI messagesCabalURI)
    Just handle -> do
      -- Get buffered output
      output <- getBufferedOutput handle
      let outputLines = T.lines output
      
      -- Apply filtering if requested
      filteredLines <- case messagesFilter of
        Nothing -> return outputLines
        Just filterReq -> do
          let inputText = T.unlines outputLines
          result <- applyShellFilter inputText filterReq
          case result of
            Left err -> return outputLines  -- Fallback to unfiltered on error
            Right filteredText -> return $ T.lines filteredText
      
      -- Apply count limit if requested
      let limitedLines = case messagesCount of
            Nothing -> filteredLines
            Just count -> take count filteredLines
      
      timestamp <- getCurrentTime
      return MessagesResult
        { messagesOutput = T.unlines limitedLines
        , messagesLines = limitedLines
        , messagesTimestamp = timestamp
        }

-- | Handle list processes request
handleListRequest :: GHCIDRouter -> ListProcessesData -> IO ProcessListResult
handleListRequest GHCIDRouter{..} ListProcessesData{..} = do
  logInfo "Listing GHCID processes"
  
  activeURIs <- listActiveProcesses routerRegistry
  
  if listIncludeStatus
    then do
      statuses <- mapM getProcessWithStatus activeURIs
      return ProcessListResult
        { processURIs = activeURIs
        , processStatuses = Just statuses
        }
    else return ProcessListResult
        { processURIs = activeURIs
        , processStatuses = Nothing
        }
  where
    getProcessWithStatus uri = do
      maybeHandle <- getGHCIDProcess routerRegistry uri
      case maybeHandle of
        Nothing -> return (uri, GHCIDStopped)
        Just handle -> do
          status <- getProcessStatus handle
          return (uri, status)

-- | Handle process status request
handleStatusRequest :: GHCIDRouter -> ProcessStatusData -> IO ProcessStatusResult
handleStatusRequest GHCIDRouter{..} ProcessStatusData{..} = do
  logInfo $ "Getting status for " <> getCabalURI statusCabalURI
  
  maybeHandle <- getGHCIDProcess routerRegistry statusCabalURI
  case maybeHandle of
    Nothing -> return ProcessStatusResult
      { processStatus = Nothing
      , processUptime = Nothing
      }
    Just handle -> do
      status <- getProcessStatus handle
      now <- getCurrentTime
      let uptime = T.pack $ show $ diffUTCTime now (getGHCIDStartTime handle)
      return ProcessStatusResult
        { processStatus = Just status
        , processUptime = Just uptime
        }
  where
    diffUTCTime = Data.Time.diffUTCTime

-- | Handle restart process request
handleRestartRequest :: GHCIDRouter -> RestartProcessData -> IO RestartProcessResult
handleRestartRequest router@GHCIDRouter{..} RestartProcessData{..} = do
  logInfo $ "Restarting GHCID process for " <> getCabalURI restartCabalURI
  
  -- Get current process info
  maybeHandle <- getGHCIDProcess routerRegistry restartCabalURI
  let workDir = case maybeHandle of
        Nothing -> fromMaybe "." restartWorkDir
        Just handle -> fromMaybe (getGHCIDWorkDir handle) restartWorkDir
  
  -- Stop existing process
  stopResult <- stopGHCIDProcess routerRegistry restartCabalURI
  let oldProcessId = case stopResult of
        Right _ -> Just $ getCabalURI restartCabalURI
        Left _ -> Nothing
  
  -- Start new process
  startResult <- startGHCIDProcess routerRegistry restartCabalURI workDir
  case startResult of
    Left err -> return RestartProcessResult
      { restartSuccess = False
      , restartMessage = "Restart failed: " <> err
      , restartOldProcessId = oldProcessId
      , restartNewProcessId = Nothing
      }
    Right _ -> return RestartProcessResult
      { restartSuccess = True
      , restartMessage = "Process restarted successfully"
      , restartOldProcessId = oldProcessId
      , restartNewProcessId = Just $ getCabalURI restartCabalURI
      }

-- | Process a JSON request
processJSONRequest :: GHCIDRouter -> Value -> IO Value
processJSONRequest router jsonRequest = do
  case parseMaybe decodeGHCIDRequest jsonRequest of
    Nothing -> do
      logError "Failed to parse GHCID request"
      return $ object 
        [ "error" .= object 
          [ "message" .= ("Invalid request format" :: Text)
          ]
        ]
    Just (SomeGHCIDRequest reqId request) -> 
      routeGHCIDRequest router reqId (SomeGHCIDRequest reqId request)

-- | Process a typed request (for internal use)
processTypedRequest :: GHCIDRouter -> RequestId -> GHCIDRequest a -> IO (GHCIDResponse a)
processTypedRequest router reqId request = do
  handlers <- createRequestHandlers router
  result <- case request of
    StartGHCID dat -> do
      res <- handleStart handlers dat
      return $ StartGHCIDResponse reqId res
    StopGHCID dat -> do
      res <- handleStop handlers dat
      return $ StopGHCIDResponse reqId res
    GetMessages dat -> do
      res <- handleMessages handlers dat
      return $ GetMessagesResponse reqId res
    ListProcesses dat -> do
      res <- handleList handlers dat
      return $ ListProcessesResponse reqId res
    ProcessStatus dat -> do
      res <- handleStatus handlers dat
      return $ ProcessStatusResponse reqId res
    RestartProcess dat -> do
      res <- handleRestart handlers dat
      return $ RestartProcessResponse reqId res
  
  return result

-- | Handle router errors consistently
handleRouterError :: RouterError -> GHCIDResponse a
handleRouterError err = case err of
  ProcessNotFound uri -> 
    ErrorResponse (RequestId "unknown") $ "Process not found: " <> getCabalURI uri
  ProcessAlreadyExists uri -> 
    ErrorResponse (RequestId "unknown") $ "Process already exists: " <> getCabalURI uri
  ProcessStartFailed uri msg -> 
    ErrorResponse (RequestId "unknown") $ "Failed to start process " <> getCabalURI uri <> ": " <> msg
  ProcessStopFailed uri msg -> 
    ErrorResponse (RequestId "unknown") $ "Failed to stop process " <> getCabalURI uri <> ": " <> msg
  InvalidRequest msg -> 
    ErrorResponse (RequestId "unknown") $ "Invalid request: " <> msg
  RegistryShutdown -> 
    ErrorResponse (RequestId "unknown") "Registry is shutting down"
  ResourceExhausted msg -> 
    ErrorResponse (RequestId "unknown") $ "Resource exhausted: " <> msg
  TimeoutError msg -> 
    ErrorResponse (RequestId "unknown") $ "Timeout: " <> msg

