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
    getProcessLastMessage,

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

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException, onException, throwIO, try)
import Control.Monad (void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Function ((&))
-- STM containers

import Data.Hashable (Hashable (..))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Text.Read (readMaybe)
import qualified ListT
-- Common imports
import Process.Signals (SignalInfo)
import qualified StmContainers.Bimap as StmBimap
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (isEOFError)
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
    ghcidHealthCheck :: TVar UTCTime, -- Last health check
    ghcidLastMessage :: TVar (Maybe Text)
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
startGHCIDProcess :: ProcessRegistry -> CabalURI -> FilePath -> Maybe Text -> [String] -> IO (Either Text GHCIDHandle)
startGHCIDProcess registry@ProcessRegistry {..} cabalURI workDir commandOverride extraArgs = do
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
      result <- try @SomeException $ createGHCIDHandle cabalURI workDir commandOverride extraArgs
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
createGHCIDHandle :: CabalURI -> FilePath -> Maybe Text -> [String] -> IO GHCIDHandle
createGHCIDHandle cabalURI workDir commandOverride extraArgs = do
  startTime <- getCurrentTime

  let ghcidExecutable = "ghcid"
      commandValue = maybe "cabal repl" T.unpack commandOverride
      ghcidArgs = ["--command", commandValue] ++ extraArgs
      commandDisplay = renderCommand ghcidExecutable ghcidArgs
      launchMessage =
        "Launching ghcid for "
          <> getCabalURI cabalURI
          <> " in "
          <> T.pack workDir
          <> " using: "
          <> commandDisplay
          <> " (awaiting first output)"

  logInfo $
    "Launching ghcid for "
      <> getCabalURI cabalURI
      <> " with command: "
      <> T.pack ghcidExecutable
      <> " "
      <> T.unwords (map T.pack ghcidArgs)
  logInfo launchMessage

  -- Configure GHCID process
  let processConfig =
        proc ghcidExecutable ghcidArgs
          & setWorkingDir workDir
          & setStdin createPipe
          & setStdout createPipe
          & setStderr createPipe

  -- Start the process
  process <- startProcess processConfig

  -- Create STM variables
  (status, outputQueue, errorQueue, outputReader, errorReader, healthVar, lastMsgVar) <- atomically $ do
    s <- newTVar GHCIDStarting
    oq <- newTBQueue 10000 -- 10k line buffer
    eq <- newTBQueue 1000 -- 1k error buffer
    or <- newTVar Nothing
    er <- newTVar Nothing
    hv <- newTVar startTime
    lm <- newTVar (Just launchMessage)
    return (s, oq, eq, or, er, hv, lm)

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
            ghcidHealthCheck = healthVar,
            ghcidLastMessage = lastMsgVar
          }

  let setup = do
        outReader <- async $ outputReaderLoop handle (getStdout process)
        errReader <- async $ errorReaderLoop handle (getStderr process)

        atomically $ do
          writeTVar (ghcidOutputReader handle) (Just outReader)
          writeTVar (ghcidErrorReader handle) (Just errReader)
          writeTVar (ghcidStatus handle) GHCIDStarting

        return handle

      cleanup = cleanupGHCIDHandle handle

  handle <- setup `onException` cleanup

  exitNow <- getExitCode process
  case exitNow of
    Nothing -> return handle
    Just code -> do
      (outLines, errLines) <- atomically $ drainQueues handle
      let decoratedErrs = map ("[stderr] " <>) errLines
          combinedLines = outLines ++ decoratedErrs
          combinedLog = T.unlines combinedLines
          exitMsg = "ghcid exited immediately with " <> T.pack (show code)
          fullMsg = if T.null combinedLog then exitMsg else exitMsg <> "\n" <> combinedLog
      atomically $ writeTVar (ghcidStatus handle) (GHCIDError fullMsg)
      logError $ "ghcid failed to start for " <> getCabalURI cabalURI <> ": " <> fullMsg
      cleanup
      throwIO (userError (T.unpack fullMsg))
  where
    renderCommand :: String -> [String] -> Text
    renderCommand exe args = T.unwords (map renderArg (exe : args))

    renderArg :: String -> Text
    renderArg arg
      | any isSpace arg = T.pack $ "\"" ++ concatMap escapeChar arg ++ "\""
      | otherwise = T.pack arg

    escapeChar :: Char -> String
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

-- | Output reader loop with error handling
outputReaderLoop :: GHCIDHandle -> Handle -> IO ()
outputReaderLoop ghcidHandle@GHCIDHandle {..} handle = go
  where
    go = do
      lineResult <- try @SomeException (T.hGetLine handle)
      case lineResult of
        Left ex
          | Just ioEx <- fromException ex, isEOFError ioEx ->
              logInfo $ "GHCID stdout closed for " <> getCabalURI ghcidCabalURI
          | otherwise -> do
              let errTxt = T.pack (show ex)
              logError $ "GHCID output reader error: " <> errTxt
              atomically $ writeTVar ghcidStatus (GHCIDError errTxt)
        Right line -> do
          processStdoutLine ghcidHandle line
          go

-- | Error reader loop
errorReaderLoop :: GHCIDHandle -> Handle -> IO ()
errorReaderLoop ghcidHandle@GHCIDHandle {..} handle = go
  where
    go = do
      lineResult <- try @SomeException (T.hGetLine handle)
      case lineResult of
        Left ex
          | Just ioEx <- fromException ex, isEOFError ioEx ->
              logInfo $ "GHCID stderr closed for " <> getCabalURI ghcidCabalURI
          | otherwise ->
              logError $ "GHCID error reader error: " <> T.pack (show ex)
        Right line -> do
          processStderrLine ghcidHandle line
          go

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
  atomically $ do
    current <- readTVar ghcidStatus
    let nextStatus = case current of
          GHCIDError _ -> current
          _ -> GHCIDStopped
    writeTVar ghcidStatus nextStatus

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

-- | Get the last message observed from stdout/stderr for a process.
getProcessLastMessage :: GHCIDHandle -> IO (Maybe Text)
getProcessLastMessage GHCIDHandle {..} = readTVarIO ghcidLastMessage

-- | Consume and process a stdout line from ghcid.
processStdoutLine :: GHCIDHandle -> Text -> IO ()
processStdoutLine handle@GHCIDHandle {..} rawLine = do
  let line = sanitizeLine rawLine
  atomically $ writeTBQueue ghcidOutput line
  when (not $ T.null line) $ do
    logDebug $ "[ghcid stdout] " <> line
    atomically $ writeTVar ghcidLastMessage (Just line)
  updateStatusFromStdout handle line

-- | Consume and process a stderr line from ghcid.
processStderrLine :: GHCIDHandle -> Text -> IO ()
processStderrLine handle@GHCIDHandle {..} rawLine = do
  let line = sanitizeLine rawLine
  atomically $ writeTBQueue ghcidErrors line
  when (not $ T.null line) $ do
    logDebug $ "[ghcid stderr] " <> line
    atomically $ writeTVar ghcidLastMessage (Just ("stderr: " <> line))
  updateStatusFromStderr handle line

-- | Remove common ANSI / control characters so downstream parsing is stable.
sanitizeLine :: Text -> Text
sanitizeLine = T.stripEnd . T.filter (\c -> c == '\t' || c >= ' ')

updateStatusFromStdout :: GHCIDHandle -> Text -> IO ()
updateStatusFromStdout handle@GHCIDHandle {..} line =
  when (not $ T.null line) $
    atomically $ do
      prev <- readTVar ghcidStatus
      case interpretStdoutLine prev line of
        Nothing -> return ()
        Just newStatus -> writeTVar ghcidStatus newStatus

updateStatusFromStderr :: GHCIDHandle -> Text -> IO ()
updateStatusFromStderr GHCIDHandle {..} line =
  when (not $ T.null line) $
    atomically $ do
      prev <- readTVar ghcidStatus
      case interpretStderrLine prev line of
        Nothing -> return ()
        Just newStatus -> writeTVar ghcidStatus newStatus

interpretStdoutLine :: GHCIDStatus -> Text -> Maybe GHCIDStatus
interpretStdoutLine prev line
  | isAllGood = Just (GHCIDRunning moduleCount')
  | mentionsModulesLoaded = Just (GHCIDRunning moduleCount')
  | isCompilingLine = Just GHCIDCompiling
  | isExitFailure = Just (GHCIDError (T.strip line))
  | otherwise = Nothing
  where
    lower = T.toLower (T.strip line)
    moduleCount' =
      case extractModuleCount line <|> previousModuleCount prev of
        Just count -> count
        Nothing -> 0

    isAllGood = "all good" `T.isInfixOf` lower
    mentionsModulesLoaded = "module loaded" `T.isInfixOf` lower || "modules loaded" `T.isInfixOf` lower
    isCompilingLine =
      any (`T.isPrefixOf` lower) ["compiling", "reloading", "loading", "ghci> :reload"]
        || "ghci> :l" `T.isPrefixOf` lower
    isExitFailure =
      "exited unexpectedly" `T.isInfixOf` lower
        || "exit failure" `T.isInfixOf` lower
        || (containsErrorKeyword lower && not ("warning" `T.isInfixOf` lower))

interpretStderrLine :: GHCIDStatus -> Text -> Maybe GHCIDStatus
interpretStderrLine _ line
  | T.null trimmed = Nothing
  | containsErrorKeyword lower && not ("warning" `T.isInfixOf` lower) = Just (GHCIDError trimmed)
  | otherwise = Nothing
  where
    trimmed = T.strip line
    lower = T.toLower trimmed

containsErrorKeyword :: Text -> Bool
containsErrorKeyword lower =
  or
    [ "error:" `T.isInfixOf` lower
    , "fatal" `T.isInfixOf` lower
    , "exception" `T.isInfixOf` lower
    , "unknown target" `T.isInfixOf` lower
    , "failed to" `T.isInfixOf` lower
    , "panic" `T.isInfixOf` lower
    ]

extractModuleCount :: Text -> Maybe Int
extractModuleCount txt =
  let lower = T.toLower txt
      (prefix, rest) = T.breakOn "module" lower
   in if T.null rest
        then Nothing
        else
          let digits = T.takeWhileEnd isDigit prefix
           in if not (T.null digits)
                then readMaybe (T.unpack digits)
                else do
                  let cleanedWords = map (T.dropAround (\c -> not (isAlpha c))) (T.words prefix)
                  word <- safeLast (filter (not . T.null) cleanedWords)
                  lookup word spelledNumbers
  where
    spelledNumbers =
      [ ("one", 1)
      , ("two", 2)
      , ("three", 3)
      , ("four", 4)
      , ("five", 5)
      , ("six", 6)
      , ("seven", 7)
      , ("eight", 8)
      , ("nine", 9)
      , ("ten", 10)
      ]

    safeLast [] = Nothing
    safeLast xs = Just (last xs)

previousModuleCount :: GHCIDStatus -> Maybe Int
previousModuleCount (GHCIDRunning count) = Just count
previousModuleCount _ = Nothing

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
getBufferedOutput handle = do
  (outLines, errLines) <- atomically $ drainQueues handle
  let decoratedErrs = map ("[stderr] " <>) errLines
  return $ T.unlines (outLines ++ decoratedErrs)

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

drainQueues :: GHCIDHandle -> STM ([Text], [Text])
drainQueues GHCIDHandle {..} = do
  outs <- flushTBQueueCustom ghcidOutput
  errs <- flushTBQueueCustom ghcidErrors
  return (outs, errs)

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
                  startResult <- startGHCIDProcess reg uri (ghcidWorkDir handle) Nothing []
                  return [(uri, startResult)]
            _ -> return []

-- | Accessor functions for GHCIDHandle fields
getGHCIDStartTime :: GHCIDHandle -> UTCTime
getGHCIDStartTime GHCIDHandle {..} = ghcidStartTime

getGHCIDWorkDir :: GHCIDHandle -> FilePath
getGHCIDWorkDir GHCIDHandle {..} = ghcidWorkDir
