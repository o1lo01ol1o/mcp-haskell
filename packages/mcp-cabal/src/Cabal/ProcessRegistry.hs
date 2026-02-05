{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cabal.ProcessRegistry
  ( -- * Process Registry Types
    ProcessRegistry
  , CabalTestHandle
  , CabalTestStatus(..)
  , CabalURI(..)

    -- * Registry Management
  , createProcessRegistry
  , shutdownProcessRegistry

    -- * Process Operations
  , startCabalTestProcess
  , stopCabalTestProcess
  , stopCabalTestProcessWithTimeout
  , getCabalTestProcess
  , listKnownProcesses
  , getProcessStatus
  , getProcessLastMessage

    -- * Process Communication
  , getBufferedOutput

    -- * Process Handle Accessors
  , getCabalTestStartTime
  , getCabalTestWorkDir
  ) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException, onException, try)
import Control.Monad (void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Hashable (Hashable (..))
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified ListT
import Process.Signals (SignalInfo)
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Set as StmSet
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getExecutablePath)
import System.IO (Handle)
import System.IO.Error (isEOFError)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (CPid (..), ProcessGroupID)
import System.Process.Typed
import System.Timeout (timeout)
import Utils.Logging

stdoutBufferCapacity :: Int
stdoutBufferCapacity = 10000

stderrBufferCapacity :: Int
stderrBufferCapacity = 2000

-- | URI identifier for cabal projects.
newtype CabalURI = CabalURI {getCabalURI :: Text}
  deriving (Show, Eq, Ord)

instance Hashable CabalURI where
  hashWithSalt salt (CabalURI uri) = hashWithSalt salt uri

instance ToJSON CabalURI where
  toJSON (CabalURI uri) = toJSON uri

instance FromJSON CabalURI where
  parseJSON = fmap CabalURI . parseJSON

-- | Cabal test process status.
data CabalTestStatus
  = CabalTestStarting
  | CabalTestRunning
  | CabalTestSuccess
  | CabalTestFailed Text
  | CabalTestTerminated SignalInfo
  | CabalTestStopped
  deriving (Show, Eq, Ord)

instance ToJSON CabalTestStatus where
  toJSON CabalTestStarting = object ["status" .= ("starting" :: Text)]
  toJSON CabalTestRunning = object ["status" .= ("running" :: Text)]
  toJSON CabalTestSuccess = object ["status" .= ("success" :: Text)]
  toJSON (CabalTestFailed err) = object ["status" .= ("failed" :: Text), "error" .= err]
  toJSON (CabalTestTerminated info) = object ["status" .= ("terminated" :: Text), "signal" .= show info]
  toJSON CabalTestStopped = object ["status" .= ("stopped" :: Text)]

instance FromJSON CabalTestStatus where
  parseJSON = withObject "CabalTestStatus" $ \o -> do
    status <- o .: "status"
    case (status :: Text) of
      "starting" -> return CabalTestStarting
      "running" -> return CabalTestRunning
      "success" -> return CabalTestSuccess
      "failed" -> CabalTestFailed <$> o .: "error"
      "terminated" -> return CabalTestStopped
      "stopped" -> return CabalTestStopped
      _ -> return CabalTestStopped

-- | Individual Cabal test process handle with STM state.
data CabalTestHandle = CabalTestHandle
  { cabalProcess :: Process Handle Handle Handle
  , cabalPid :: Maybe CPid
  , cabalStatus :: TVar CabalTestStatus
  , cabalOutput :: TBQueue Text
  , cabalErrors :: TBQueue Text
  , cabalOutputBuffer :: TVar (Seq Text)
  , cabalErrorBuffer :: TVar (Seq Text)
  , cabalStartTime :: UTCTime
  , cabalURI :: CabalURI
  , cabalWorkDir :: FilePath
  , cabalRegistryRef :: ProcessRegistry
  , cabalOutputReader :: TVar (Maybe (Async ()))
  , cabalErrorReader :: TVar (Maybe (Async ()))
  , cabalExitWatcher :: TVar (Maybe (Async ()))
  , cabalLastMessage :: TVar (Maybe Text)
  }

instance Show CabalTestHandle where
  show CabalTestHandle {..} =
    "CabalTestHandle{cabalURI="
      ++ show cabalURI
      ++ ", workDir="
      ++ show cabalWorkDir
      ++ ", startTime="
      ++ show cabalStartTime
      ++ "}"

instance Eq CabalTestHandle where
  h1 == h2 =
    cabalURI h1 == cabalURI h2
      && cabalWorkDir h1 == cabalWorkDir h2
      && cabalStartTime h1 == cabalStartTime h2

-- | STM-based process registry.
data ProcessRegistry = ProcessRegistry
  { processMap :: StmMap.Map CabalURI CabalTestHandle
  , activeProcesses :: StmSet.Set CabalURI
  , processCount :: TVar Int
  , registryShutdown :: TVar Bool
  }

data StartDecision
  = Denied Text
  | FreshStart
  | RestartPrev CabalTestHandle

-- | Create a new process registry.
createProcessRegistry :: IO ProcessRegistry
createProcessRegistry = do
  logInfo "Creating Cabal test process registry"
  registry <- atomically $ do
    pMap <- StmMap.new
    active <- StmSet.new
    count <- newTVar 0
    shutdown <- newTVar False
    return
      ProcessRegistry
        { processMap = pMap
        , activeProcesses = active
        , processCount = count
        , registryShutdown = shutdown
        }
  logInfo "Process registry created successfully"
  return registry

-- | Shutdown the entire process registry.
shutdownProcessRegistry :: ProcessRegistry -> IO ()
shutdownProcessRegistry registry@ProcessRegistry {..} = do
  logInfo "Shutting down Cabal test process registry"
  atomically $ writeTVar registryShutdown True
  activeURIs <- atomically $ ListT.toList $ StmSet.listT activeProcesses
  stopResults <- mapM (stopCabalTestProcessWithTimeout registry 10) activeURIs
  let failures = filter (either (const True) (const False)) stopResults
  when (not $ null failures) $
    logWarn $
      "Failed to stop " <> T.pack (show $ length failures) <> " processes during shutdown"
  logInfo "Process registry shutdown complete"

-- | Start a new Cabal test process for the given cabal URI.
startCabalTestProcess :: ProcessRegistry -> CabalURI -> FilePath -> [String] -> IO (Either Text CabalTestHandle)
startCabalTestProcess registry@ProcessRegistry {..} cabalURI workDir commandParts = do
  decision <- atomically $ do
    shuttingDown <- readTVar registryShutdown
    if shuttingDown
      then pure $ Denied "Registry is shutting down"
      else do
        existingHandle <- StmMap.lookup cabalURI processMap
        case existingHandle of
          Just handle -> do
            status <- readTVar (cabalStatus handle)
            case status of
              CabalTestRunning -> pure $ Denied "Cabal test already running for this URI"
              CabalTestStarting -> pure $ Denied "Cabal test already running for this URI"
              _ -> prepareRestart handle
          Nothing -> do
            alreadyActive <- StmSet.lookup cabalURI activeProcesses
            if alreadyActive
              then pure $ Denied "Cabal test already running for this URI"
              else do
                StmSet.insert cabalURI activeProcesses
                pure FreshStart
  case decision of
    Denied err -> return $ Left err
    RestartPrev prevHandle -> doRestart (Just prevHandle)
    FreshStart -> doRestart Nothing
  where
    prepareRestart handle = do
      StmMap.delete cabalURI processMap
      StmSet.insert cabalURI activeProcesses
      modifyTVar processCount (\x -> max 0 (x - 1))
      pure $ RestartPrev handle

    doRestart maybeOldHandle = do
      validation <- validateWorkDir
      case validation of
        Left err -> do
          cleanupPrevious maybeOldHandle
          atomically $ StmSet.delete cabalURI activeProcesses
          logError err
          return $ Left err
        Right () -> do
          cleanupPrevious maybeOldHandle
          logInfo $ "Starting Cabal test for " <> getCabalURI cabalURI
          result <- try @SomeException $ createCabalTestHandle registry cabalURI workDir commandParts
          case result of
            Left ex -> do
              atomically $ StmSet.delete cabalURI activeProcesses
              logError $ "Failed to start Cabal test: " <> T.pack (show ex)
              return $ Left $ "Failed to start cabal test: " <> T.pack (show ex)
            Right handle -> do
              atomically $ do
                StmMap.insert handle cabalURI processMap
                modifyTVar processCount (+ 1)
              logInfo $ "Cabal test started successfully for " <> getCabalURI cabalURI
              return $ Right handle

    cleanupPrevious = maybe (pure ()) cleanupSilently

    cleanupSilently handle = do
      _ <- try @SomeException $ cleanupCabalTestHandle handle
      pure ()

    validateWorkDir = do
      dirExists <- doesDirectoryExist workDir
      if not dirExists
        then pure $ Left $ "Working directory does not exist: " <> T.pack workDir
        else do
          contentsResult <- try @SomeException (listDirectory workDir)
          case contentsResult of
            Left ex -> pure $ Left $ "Unable to inspect working directory: " <> T.pack (show ex)
            Right entries -> do
              let hasCabal = any (".cabal" `isSuffixOf`) entries
                  hasProject = "cabal.project" `elem` entries || "cabal.project.local" `elem` entries
              when (not (hasCabal || hasProject)) $
                logWarn $ "No .cabal or cabal.project file found in " <> T.pack workDir <> "; cabal may fail"
              pure (Right ())

-- | Create a new Cabal test handle with proper resource management.
createCabalTestHandle :: ProcessRegistry -> CabalURI -> FilePath -> [String] -> IO CabalTestHandle
createCabalTestHandle registry cabalURI workDir commandParts = do
  startTime <- getCurrentTime
  let commandDisplay = renderCommandParts commandParts
      launchMessage =
        "Launching cabal test for "
          <> getCabalURI cabalURI
          <> " in "
          <> T.pack workDir
          <> " using: "
          <> commandDisplay
          <> " (awaiting output)"

  logInfo $
    "Launching cabal test for "
      <> getCabalURI cabalURI
      <> " with command: "
      <> commandDisplay

  selfExecutable <- getExecutablePath
  CPid parentRaw <- getProcessID
  let wrapperArgs =
        ["--internal-cabal-wrapper", "--parent-pid", show (fromIntegral parentRaw :: Int), "--"]
          ++ commandParts
      processConfig =
        proc selfExecutable wrapperArgs
          & setWorkingDir workDir
          & setStdin createPipe
          & setStdout createPipe
          & setStderr createPipe
          & setCreateGroup True

  process <- startProcess processConfig
  rawPid <- getPid process
  let pid = fmap (CPid . fromIntegral) rawPid

  ( statusVar
    , outputQueue
    , errorQueue
    , outputSnapshotVar
    , errorSnapshotVar
    , outputReaderVar
    , errorReaderVar
    , exitWatcherVar
    , lastMsgVar
    ) <-
    atomically $ do
      s <- newTVar CabalTestStarting
      oq <- newTBQueue (fromIntegral stdoutBufferCapacity)
      eq <- newTBQueue (fromIntegral stderrBufferCapacity)
      os <- newTVar Seq.empty
      es <- newTVar Seq.empty
      outReaderVar <- newTVar Nothing
      errReaderVar <- newTVar Nothing
      ew <- newTVar Nothing
      lm <- newTVar (Just launchMessage)
      return (s, oq, eq, os, es, outReaderVar, errReaderVar, ew, lm)

  let cabalHandle =
        CabalTestHandle
          { cabalProcess = process
          , cabalPid = pid
          , cabalStatus = statusVar
          , cabalOutput = outputQueue
          , cabalErrors = errorQueue
          , cabalOutputBuffer = outputSnapshotVar
          , cabalErrorBuffer = errorSnapshotVar
          , cabalStartTime = startTime
          , cabalURI = cabalURI
          , cabalWorkDir = workDir
          , cabalRegistryRef = registry
          , cabalOutputReader = outputReaderVar
          , cabalErrorReader = errorReaderVar
          , cabalExitWatcher = exitWatcherVar
          , cabalLastMessage = lastMsgVar
          }

  let setup = do
        outReader <- async $ outputReaderLoop cabalHandle (getStdout process)
        errReader <- async $ errorReaderLoop cabalHandle (getStderr process)
        exitWatcher <- async $ exitWatcherLoop cabalHandle
        atomically $ do
          writeTVar (cabalOutputReader cabalHandle) (Just outReader)
          writeTVar (cabalErrorReader cabalHandle) (Just errReader)
          writeTVar (cabalExitWatcher cabalHandle) (Just exitWatcher)
          writeTVar (cabalStatus cabalHandle) CabalTestRunning
        return cabalHandle

      cleanup = cleanupCabalTestHandle cabalHandle

  cabalHandle' <- setup `onException` cleanup
  return cabalHandle'
  where
    renderCommandParts :: [String] -> Text
    renderCommandParts parts = T.unwords (map renderArg parts)

    renderArg :: String -> Text
    renderArg arg
      | any isSpace arg = T.pack $ "\"" ++ concatMap escapeChar arg ++ "\""
      | otherwise = T.pack arg

    escapeChar :: Char -> String
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

-- | Stop a Cabal test process with timeout.
stopCabalTestProcess :: ProcessRegistry -> CabalURI -> IO (Either Text ())
stopCabalTestProcess registry cabalURI = stopCabalTestProcessWithTimeout registry 30 cabalURI

-- | Stop a Cabal test process with specified timeout.
stopCabalTestProcessWithTimeout :: ProcessRegistry -> Int -> CabalURI -> IO (Either Text ())
stopCabalTestProcessWithTimeout ProcessRegistry {..} timeoutSecs cabalURI = do
  logInfo $ "Stopping Cabal test for " <> getCabalURI cabalURI
  maybeHandle <- atomically $ StmMap.lookup cabalURI processMap
  case maybeHandle of
    Nothing -> return $ Left "Cabal test process not found"
    Just handle -> do
      result <- timeout (timeoutSecs * 1000000) $ cleanupCabalTestHandle handle
      case result of
        Nothing -> do
          logError $ "Timeout stopping Cabal test for " <> getCabalURI cabalURI
          void $ try @SomeException $ stopProcess (cabalProcess handle)
          removeFromActive
          return $ Left "Timeout stopping cabal test process"
        Just _ -> do
          removeFromActive
          logInfo $ "Cabal test stopped for " <> getCabalURI cabalURI
          return $ Right ()
  where
    removeFromActive = atomically $ StmSet.delete cabalURI activeProcesses

-- | Clean up a Cabal test handle.
cleanupCabalTestHandle :: CabalTestHandle -> IO ()
cleanupCabalTestHandle handle@CabalTestHandle {..} = do
  logInfo $ "Cleaning up Cabal test handle for " <> getCabalURI cabalURI
  currentThread <- myThreadId
  let cancelAsync maybeAsync = case maybeAsync of
        Nothing -> return ()
        Just worker -> do
          let workerTid = asyncThreadId worker
          when (workerTid /= currentThread) $ cancel worker

  maybeOutReader <- readTVarIO cabalOutputReader
  maybeErrReader <- readTVarIO cabalErrorReader
  maybeExitWatcher <- readTVarIO cabalExitWatcher

  cancelAsync maybeOutReader
  cancelAsync maybeErrReader
  cancelAsync maybeExitWatcher

  atomically $ do
    writeTVar cabalOutputReader Nothing
    writeTVar cabalErrorReader Nothing
    writeTVar cabalExitWatcher Nothing

  terminateProcessTree handle
  result <- try @SomeException $ stopProcess cabalProcess
  case result of
    Left ex -> logWarn $ "Error stopping Cabal test process: " <> T.pack (show ex)
    Right _ -> return ()

  atomically $ do
    current <- readTVar cabalStatus
    let nextStatus = case current of
          CabalTestSuccess -> current
          CabalTestFailed _ -> current
          _ -> CabalTestStopped
    writeTVar cabalStatus nextStatus

-- | Attempt to terminate the entire process tree.
terminateProcessTree :: CabalTestHandle -> IO ()
terminateProcessTree CabalTestHandle {..} =
  case cabalPid of
    Nothing -> return ()
    Just (CPid rawPid) -> do
      let pgid :: ProcessGroupID
          pgid = fromIntegral rawPid
      let send sig =
            void $
              try @SomeException $
                signalProcessGroup sig pgid
      send sigTERM
      threadDelay 500000
      exited <- getExitCode cabalProcess
      case exited of
        Just _ -> return ()
        Nothing -> do
          send sigKILL
          threadDelay 200000

-- | Output reader loop with error handling.
outputReaderLoop :: CabalTestHandle -> Handle -> IO ()
outputReaderLoop cabalHandle@CabalTestHandle {..} handle = go
  where
    go = do
      lineResult <- try @SomeException (TIO.hGetLine handle)
      case lineResult of
        Left ex
          | Just ioEx <- fromException ex, isEOFError ioEx ->
              logInfo $ "Cabal test stdout closed for " <> getCabalURI cabalURI
          | otherwise -> do
              let errTxt = T.pack (show ex)
              logError $ "Cabal test output reader error: " <> errTxt
              atomically $ writeTVar cabalStatus (CabalTestFailed errTxt)
        Right line -> do
          processStdoutLine cabalHandle line
          go

-- | Error reader loop.
errorReaderLoop :: CabalTestHandle -> Handle -> IO ()
errorReaderLoop cabalHandle@CabalTestHandle {..} handle = go
  where
    go = do
      lineResult <- try @SomeException (TIO.hGetLine handle)
      case lineResult of
        Left ex
          | Just ioEx <- fromException ex, isEOFError ioEx ->
              logInfo $ "Cabal test stderr closed for " <> getCabalURI cabalURI
          | otherwise ->
              logError $ "Cabal test error reader error: " <> T.pack (show ex)
        Right line -> do
          processStderrLine cabalHandle line
          go

-- | Exit watcher loop to update status when cabal test completes.
exitWatcherLoop :: CabalTestHandle -> IO ()
exitWatcherLoop CabalTestHandle {..} = do
  exitCode <- waitExitCode cabalProcess
  let (nextStatus, msg) =
        case exitCode of
          ExitSuccess -> (CabalTestSuccess, "cabal test completed successfully")
          ExitFailure code ->
            ( CabalTestFailed ("cabal test failed with exit code " <> T.pack (show code))
            , "cabal test failed (exit code " <> T.pack (show code) <> ")"
            )
  atomically $ do
    writeTVar cabalStatus nextStatus
    writeTVar cabalLastMessage (Just msg)
    StmSet.delete cabalURI (activeProcesses cabalRegistryRef)
  logInfo $ "Cabal test finished for " <> getCabalURI cabalURI

-- | Get a Cabal test process handle.
getCabalTestProcess :: ProcessRegistry -> CabalURI -> IO (Maybe CabalTestHandle)
getCabalTestProcess ProcessRegistry {..} cabalURI =
  atomically $ StmMap.lookup cabalURI processMap

-- | List all known process URIs.
listKnownProcesses :: ProcessRegistry -> IO [CabalURI]
listKnownProcesses ProcessRegistry {..} = do
  entries <- atomically $ ListT.toList $ StmMap.listT processMap
  return (map fst entries)

-- | Get process status.
getProcessStatus :: CabalTestHandle -> IO CabalTestStatus
getProcessStatus CabalTestHandle {..} = readTVarIO cabalStatus

-- | Get the last message observed from stdout/stderr for a process.
getProcessLastMessage :: CabalTestHandle -> IO (Maybe Text)
getProcessLastMessage CabalTestHandle {..} = readTVarIO cabalLastMessage

-- | Consume and process a stdout line from cabal test.
processStdoutLine :: CabalTestHandle -> Text -> IO ()
processStdoutLine CabalTestHandle {..} rawLine = do
  let line = sanitizeLine rawLine
  shouldLog <- atomically $ do
    writeTBQueue cabalOutput line
    modifyTVar' cabalOutputBuffer (appendBounded stdoutBufferCapacity line)
    when (not $ T.null line) $
      writeTVar cabalLastMessage (Just line)
    pure (not $ T.null line)
  when shouldLog $
    logDebug $ "[cabal stdout] " <> line

-- | Consume and process a stderr line from cabal test.
processStderrLine :: CabalTestHandle -> Text -> IO ()
processStderrLine CabalTestHandle {..} rawLine = do
  let line = sanitizeLine rawLine
  atomically $ do
    writeTBQueue cabalErrors line
    modifyTVar' cabalErrorBuffer (appendBounded stderrBufferCapacity line)
  when (not $ T.null line) $ do
    logDebug $ "[cabal stderr] " <> line
    atomically $ writeTVar cabalLastMessage (Just ("stderr: " <> line))

appendBounded :: Int -> Text -> Seq Text -> Seq Text
appendBounded capacity line seq
  | capacity <= 0 = Seq.singleton line
  | Seq.length seq >= capacity =
      let trimmed = Seq.drop 1 seq
       in trimmed Seq.|> line
  | otherwise = seq Seq.|> line

-- | Remove common ANSI / control characters so downstream parsing is stable.
sanitizeLine :: Text -> Text
sanitizeLine = T.stripEnd . T.filter (\c -> c == '\t' || c >= ' ')

-- | Get buffered output as text.
getBufferedOutput :: CabalTestHandle -> IO Text
getBufferedOutput CabalTestHandle {..} = do
  outLines <- toList <$> readTVarIO cabalOutputBuffer
  errLines <- toList <$> readTVarIO cabalErrorBuffer
  let decoratedErrs = map ("[stderr] " <>) errLines
  return $ T.unlines (outLines ++ decoratedErrs)

-- | Accessors for start time and work dir.
getCabalTestStartTime :: CabalTestHandle -> UTCTime
getCabalTestStartTime CabalTestHandle {..} = cabalStartTime

getCabalTestWorkDir :: CabalTestHandle -> FilePath
getCabalTestWorkDir CabalTestHandle {..} = cabalWorkDir
