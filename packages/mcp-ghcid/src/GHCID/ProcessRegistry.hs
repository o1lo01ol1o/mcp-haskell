{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.ProcessRegistry
  ( -- * Process Registry Types
    ProcessRegistry,
    GHCIDHandle,
    GHCIDStatus (..),
    CabalURI (..),

    -- * Registry Management
    createProcessRegistry,
    shutdownProcessRegistry,

    -- * Process Operations
    startGHCIDProcess,
    stopGHCIDProcess,
    stopGHCIDProcessWithTimeout,
    getGHCIDProcess,
    listActiveProcesses,
    getProcessStatus,

    -- * Process Communication
    sendToGHCID,
    readFromGHCID,
    getBufferedOutput,

    -- * Health Monitoring
    checkProcessHealth,
    restartUnhealthyProcesses,

    -- * Process Handle Accessors
    getGHCIDStartTime,
    getGHCIDWorkDir,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Function ((&))
-- STM containers

import Data.Hashable (Hashable (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import qualified ListT
-- Common imports
import Process.Signals (SignalInfo)
import qualified StmContainers.Bimap as StmBimap
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush)
import System.Process.Typed
import System.Timeout (timeout)
import Utils.Logging

-- | URI identifier for cabal projects
newtype CabalURI = CabalURI {getCabalURI :: Text}
  deriving (Show, Eq, Ord)

instance Hashable CabalURI where
  hashWithSalt salt (CabalURI uri) = hashWithSalt salt uri

instance ToJSON CabalURI where
  toJSON (CabalURI uri) = toJSON uri

instance FromJSON CabalURI where
  parseJSON = fmap CabalURI . parseJSON

-- | GHCID process status
data GHCIDStatus
  = GHCIDStarting
  | GHCIDRunning Int -- Module count
  | GHCIDCompiling
  | GHCIDError Text
  | GHCIDTerminated SignalInfo
  | GHCIDStopped
  deriving (Show, Eq, Ord)

instance ToJSON GHCIDStatus where
  toJSON GHCIDStarting = object ["status" .= ("starting" :: Text)]
  toJSON (GHCIDRunning count) = object ["status" .= ("running" :: Text), "moduleCount" .= count]
  toJSON GHCIDCompiling = object ["status" .= ("compiling" :: Text)]
  toJSON (GHCIDError err) = object ["status" .= ("error" :: Text), "error" .= err]
  toJSON (GHCIDTerminated info) = object ["status" .= ("terminated" :: Text), "signal" .= show info]
  toJSON GHCIDStopped = object ["status" .= ("stopped" :: Text)]

instance FromJSON GHCIDStatus where
  parseJSON = withObject "GHCIDStatus" $ \o -> do
    status <- o .: "status"
    case (status :: Text) of
      "starting" -> return GHCIDStarting
      "running" -> GHCIDRunning <$> o .: "moduleCount"
      "compiling" -> return GHCIDCompiling
      "error" -> GHCIDError <$> o .: "error"
      "terminated" -> return GHCIDStopped -- Simplified for now
      "stopped" -> return GHCIDStopped
      _ -> return GHCIDStopped

-- | Individual GHCID process handle with STM state
data GHCIDHandle = GHCIDHandle
  { ghcidProcess :: Process Handle Handle Handle,
    ghcidStatus :: TVar GHCIDStatus,
    ghcidOutput :: TBQueue Text, -- Buffered output lines
    ghcidErrors :: TBQueue Text, -- Error messages
    ghcidStartTime :: UTCTime,
    ghcidCabalURI :: CabalURI,
    ghcidWorkDir :: FilePath,
    ghcidOutputReader :: TVar (Maybe (Async ())),
    ghcidErrorReader :: TVar (Maybe (Async ())),
    ghcidHealthCheck :: TVar UTCTime -- Last health check
  }

instance Show GHCIDHandle where
  show GHCIDHandle {..} =
    "GHCIDHandle{cabalURI="
      ++ show ghcidCabalURI
      ++ ", workDir="
      ++ show ghcidWorkDir
      ++ ", startTime="
      ++ show ghcidStartTime
      ++ "}"

instance Eq GHCIDHandle where
  h1 == h2 =
    ghcidCabalURI h1 == ghcidCabalURI h2
      && ghcidWorkDir h1 == ghcidWorkDir h2
      && ghcidStartTime h1 == ghcidStartTime h2

-- | STM-based process registry
data ProcessRegistry = ProcessRegistry
  { processMap :: StmMap.Map CabalURI GHCIDHandle,
    activeProcesses :: StmSet.Set CabalURI,
    processCount :: TVar Int,
    registryShutdown :: TVar Bool,
    healthMonitor :: TVar (Maybe (Async ()))
  }

-- | Create a new process registry
createProcessRegistry :: IO ProcessRegistry
createProcessRegistry = do
  logInfo "Creating GHCID process registry"
  registry <- atomically $ do
    pMap <- StmMap.new
    active <- StmSet.new
    count <- newTVar 0
    shutdown <- newTVar False
    monitor <- newTVar Nothing
    return
      ProcessRegistry
        { processMap = pMap,
          activeProcesses = active,
          processCount = count,
          registryShutdown = shutdown,
          healthMonitor = monitor
        }

  -- Start health monitoring
  monitorAsync <- async $ healthMonitorLoop registry
  atomically $ writeTVar (healthMonitor registry) (Just monitorAsync)

  logInfo "Process registry created successfully"
  return registry

-- | Shutdown the entire process registry
shutdownProcessRegistry :: ProcessRegistry -> IO ()
shutdownProcessRegistry registry@ProcessRegistry {..} = do
  logInfo "Shutting down GHCID process registry"

  -- Mark as shutting down
  atomically $ writeTVar registryShutdown True

  -- Stop health monitor
  maybeMonitor <- readTVarIO healthMonitor
  case maybeMonitor of
    Just monitor -> cancel monitor
    Nothing -> return ()

  -- Get all active processes
  activeURIs <- atomically $ ListT.toList $ StmSet.listT activeProcesses

  -- Stop all processes with timeout
  stopResults <- mapM (stopGHCIDProcessWithTimeout registry 10) activeURIs

  let failures = filter (either (const True) (const False)) stopResults
  when (not $ null failures) $
    logWarn $
      "Failed to stop " <> T.pack (show $ length failures) <> " processes during shutdown"

  logInfo "Process registry shutdown complete"

-- | Start a new GHCID process for the given cabal URI
startGHCIDProcess :: ProcessRegistry -> CabalURI -> FilePath -> IO (Either Text GHCIDHandle)
startGHCIDProcess registry@ProcessRegistry {..} cabalURI workDir = do
  isShuttingDown <- readTVarIO registryShutdown
  if isShuttingDown
    then return $ Left "Registry is shutting down"
    else do
      logInfo $ "Starting GHCID process for " <> getCabalURI cabalURI

      -- Check if process already exists
      existingProcess <- atomically $ StmMap.lookup cabalURI processMap
      case existingProcess of
        Just handle -> do
          status <- readTVarIO (ghcidStatus handle)
          case status of
            GHCIDStopped -> startNewProcess
            GHCIDError _ -> startNewProcess
            _ -> return $ Left "GHCID process already running for this URI"
        Nothing -> startNewProcess
  where
    startNewProcess = do
      result <- try @SomeException $ createGHCIDHandle cabalURI workDir
      case result of
        Left ex -> do
          logError $ "Failed to start GHCID process: " <> T.pack (show ex)
          return $ Left $ "Failed to start GHCID: " <> T.pack (show ex)
        Right handle -> do
          -- Register in STM map
          atomically $ do
            StmMap.insert handle cabalURI processMap
            StmSet.insert cabalURI activeProcesses
            modifyTVar processCount (+ 1)

          logInfo $ "GHCID process started successfully for " <> getCabalURI cabalURI
          return $ Right handle

-- | Create a new GHCID handle with proper resource management
createGHCIDHandle :: CabalURI -> FilePath -> IO GHCIDHandle
createGHCIDHandle cabalURI workDir = do
  startTime <- getCurrentTime

  -- Configure GHCID process
  let processConfig =
        proc "ghcid" ["--command", "cabal repl"]
          & setWorkingDir workDir
          & setStdin createPipe
          & setStdout createPipe
          & setStderr createPipe

  -- Start the process
  process <- startProcess processConfig

  -- Create STM variables
  (status, outputQueue, errorQueue, outputReader, errorReader, healthVar) <- atomically $ do
    s <- newTVar GHCIDStarting
    oq <- newTBQueue 10000 -- 10k line buffer
    eq <- newTBQueue 1000 -- 1k error buffer
    or <- newTVar Nothing
    er <- newTVar Nothing
    hv <- newTVar startTime
    return (s, oq, eq, or, er, hv)

  let handle =
        GHCIDHandle
          { ghcidProcess = process,
            ghcidStatus = status,
            ghcidOutput = outputQueue,
            ghcidErrors = errorQueue,
            ghcidStartTime = startTime,
            ghcidCabalURI = cabalURI,
            ghcidWorkDir = workDir,
            ghcidOutputReader = outputReader,
            ghcidErrorReader = errorReader,
            ghcidHealthCheck = healthVar
          }

  -- Start output readers with proper error handling
  bracket
    (return handle)
    (\h -> cleanupGHCIDHandle h)
    ( \h -> do
        outReader <- async $ outputReaderLoop h (getStdout process)
        errReader <- async $ errorReaderLoop h (getStderr process)

        atomically $ do
          writeTVar (ghcidOutputReader h) (Just outReader)
          writeTVar (ghcidErrorReader h) (Just errReader)
          writeTVar (ghcidStatus h) (GHCIDRunning 0)

        return h
    )

-- | Output reader loop with error handling
outputReaderLoop :: GHCIDHandle -> Handle -> IO ()
outputReaderLoop GHCIDHandle {..} handle = do
  result <- try @SomeException $ do
    contents <- T.hGetContents handle
    let outputLines = T.lines contents
    atomically $ mapM_ (writeTBQueue ghcidOutput) outputLines

  case result of
    Left ex -> do
      logError $ "GHCID output reader error: " <> T.pack (show ex)
      atomically $ writeTVar ghcidStatus (GHCIDError $ T.pack $ show ex)
    Right _ ->
      logInfo $ "GHCID output reader finished for " <> getCabalURI ghcidCabalURI

-- | Error reader loop
errorReaderLoop :: GHCIDHandle -> Handle -> IO ()
errorReaderLoop GHCIDHandle {..} handle = do
  result <- try @SomeException $ do
    contents <- T.hGetContents handle
    let errorLines = T.lines contents
    atomically $ mapM_ (writeTBQueue ghcidErrors) errorLines

  case result of
    Left ex ->
      logError $ "GHCID error reader error: " <> T.pack (show ex)
    Right _ ->
      logInfo $ "GHCID error reader finished for " <> getCabalURI ghcidCabalURI

-- | Stop a GHCID process with timeout
stopGHCIDProcess :: ProcessRegistry -> CabalURI -> IO (Either Text ())
stopGHCIDProcess registry cabalURI = stopGHCIDProcessWithTimeout registry 30 cabalURI

-- | Stop a GHCID process with specified timeout
stopGHCIDProcessWithTimeout :: ProcessRegistry -> Int -> CabalURI -> IO (Either Text ())
stopGHCIDProcessWithTimeout ProcessRegistry {..} timeoutSecs cabalURI = do
  logInfo $ "Stopping GHCID process for " <> getCabalURI cabalURI

  maybeHandle <- atomically $ StmMap.lookup cabalURI processMap
  case maybeHandle of
    Nothing -> return $ Left "GHCID process not found"
    Just handle -> do
      result <- timeout (timeoutSecs * 1000000) $ cleanupGHCIDHandle handle
      case result of
        Nothing -> do
          logError $ "Timeout stopping GHCID process for " <> getCabalURI cabalURI
          -- Force cleanup
          void $ try @SomeException $ stopProcess (ghcidProcess handle)
          removeFromRegistry
          return $ Left "Timeout stopping GHCID process"
        Just _ -> do
          removeFromRegistry
          logInfo $ "GHCID process stopped for " <> getCabalURI cabalURI
          return $ Right ()
  where
    removeFromRegistry = atomically $ do
      StmMap.delete cabalURI processMap
      StmSet.delete cabalURI activeProcesses
      modifyTVar processCount (\x -> max 0 (x - 1))

-- | Clean up a GHCID handle
cleanupGHCIDHandle :: GHCIDHandle -> IO ()
cleanupGHCIDHandle GHCIDHandle {..} = do
  logInfo $ "Cleaning up GHCID handle for " <> getCabalURI ghcidCabalURI

  -- Cancel async readers
  maybeOutReader <- readTVarIO ghcidOutputReader
  maybeErrReader <- readTVarIO ghcidErrorReader

  case maybeOutReader of
    Just reader -> cancel reader
    Nothing -> return ()

  case maybeErrReader of
    Just reader -> cancel reader
    Nothing -> return ()

  -- Stop the process
  result <- try @SomeException $ stopProcess ghcidProcess
  case result of
    Left ex -> logWarn $ "Error stopping GHCID process: " <> T.pack (show ex)
    Right _ -> return ()

  -- Update status
  atomically $ writeTVar ghcidStatus GHCIDStopped

-- | Get a GHCID process handle
getGHCIDProcess :: ProcessRegistry -> CabalURI -> IO (Maybe GHCIDHandle)
getGHCIDProcess ProcessRegistry {..} cabalURI =
  atomically $ StmMap.lookup cabalURI processMap

-- | List all active process URIs
listActiveProcesses :: ProcessRegistry -> IO [CabalURI]
listActiveProcesses ProcessRegistry {..} =
  atomically $ ListT.toList $ StmSet.listT activeProcesses

-- | Get process status
getProcessStatus :: GHCIDHandle -> IO GHCIDStatus
getProcessStatus GHCIDHandle {..} = readTVarIO ghcidStatus

-- | Send a message to GHCID (not typically needed, but for completeness)
sendToGHCID :: GHCIDHandle -> Text -> IO (Either Text ())
sendToGHCID _ _ = return $ Left "GHCID does not accept input messages"

-- | Read from GHCID output queue
readFromGHCID :: GHCIDHandle -> IO (Either Text Text)
readFromGHCID GHCIDHandle {..} = do
  result <- atomically $ readTBQueue ghcidOutput
  return $ Right result

-- | Get buffered output as text
getBufferedOutput :: GHCIDHandle -> IO Text
getBufferedOutput GHCIDHandle {..} = do
  lines <- atomically $ flushTBQueueCustom ghcidOutput
  return $ T.unlines lines

-- | Flush all items from a TBQueue (custom implementation to avoid naming conflict)
flushTBQueueCustom :: TBQueue a -> STM [a]
flushTBQueueCustom queue = do
  isEmpty <- isEmptyTBQueue queue
  if isEmpty
    then return []
    else do
      item <- readTBQueue queue
      rest <- flushTBQueueCustom queue
      return (item : rest)

-- | Health monitoring loop
healthMonitorLoop :: ProcessRegistry -> IO ()
healthMonitorLoop registry@ProcessRegistry {..} = do
  isShutdown <- readTVarIO registryShutdown
  if isShutdown
    then logInfo "Health monitor shutting down"
    else do
      -- Check all processes
      activeURIs <- atomically $ ListT.toList $ StmSet.listT activeProcesses
      mapM_ (checkSingleProcessHealth registry) activeURIs

      -- Wait before next check
      threadDelay 30000000 -- 30 seconds
      healthMonitorLoop registry
  where
    threadDelay = threadDelay

-- | Check health of a single process
checkProcessHealth :: ProcessRegistry -> CabalURI -> IO (Either Text GHCIDStatus)
checkProcessHealth registry cabalURI = do
  maybeHandle <- getGHCIDProcess registry cabalURI
  case maybeHandle of
    Nothing -> return $ Left "Process not found"
    Just handle -> do
      status <- getProcessStatus handle
      now <- getCurrentTime
      atomically $ writeTVar (ghcidHealthCheck handle) now
      return $ Right status

-- | Internal health check for a single process
checkSingleProcessHealth :: ProcessRegistry -> CabalURI -> IO ()
checkSingleProcessHealth registry cabalURI = do
  result <- checkProcessHealth registry cabalURI
  case result of
    Left err -> logWarn $ "Health check failed for " <> getCabalURI cabalURI <> ": " <> err
    Right status -> case status of
      GHCIDError _ -> do
        logWarn $ "Unhealthy GHCID process detected: " <> getCabalURI cabalURI
        -- Could implement auto-restart here
        return ()
      _ -> return ()

-- | Restart all unhealthy processes
restartUnhealthyProcesses :: ProcessRegistry -> IO [(CabalURI, Either Text GHCIDHandle)]
restartUnhealthyProcesses registry = do
  activeURIs <- listActiveProcesses registry
  results <- mapM (restartIfUnhealthy registry) activeURIs
  return $ concat results
  where
    restartIfUnhealthy :: ProcessRegistry -> CabalURI -> IO [(CabalURI, Either Text GHCIDHandle)]
    restartIfUnhealthy reg uri = do
      maybeHandle <- getGHCIDProcess reg uri
      case maybeHandle of
        Nothing -> return []
        Just handle -> do
          status <- getProcessStatus handle
          case status of
            GHCIDError _ -> do
              logInfo $ "Restarting unhealthy process: " <> getCabalURI uri
              stopResult <- stopGHCIDProcess reg uri
              case stopResult of
                Left err -> return [(uri, Left $ "Failed to stop: " <> err)]
                Right _ -> do
                  startResult <- startGHCIDProcess reg uri (ghcidWorkDir handle)
                  return [(uri, startResult)]
            _ -> return []

-- | Accessor functions for GHCIDHandle fields
getGHCIDStartTime :: GHCIDHandle -> UTCTime
getGHCIDStartTime GHCIDHandle {..} = ghcidStartTime

getGHCIDWorkDir :: GHCIDHandle -> FilePath
getGHCIDWorkDir GHCIDHandle {..} = ghcidWorkDir
