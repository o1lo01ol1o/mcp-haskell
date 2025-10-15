{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Process
  ( -- * HLS Process Management
    HLSProcess (..),
    HLSConfig (..),
    HLSStatus (..),

    -- * Process Lifecycle
    startHLSProcess,
    stopHLSProcess,
    restartHLSProcess,
    getHLSStatus,

    -- * Configuration
    defaultHLSConfig,
    detectHLSExecutable,

    -- * Communication
    sendHLSRequest,
    readHLSResponse,

    -- * Health Monitoring
    checkHLSHealth,
    isHLSRunning
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, try)
import Control.Monad (void, when)
import Data.Aeson (Value)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Foreign.C.Types (CInt (..))
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.IO (Handle)
import qualified System.IO as IO
import System.IO.Error (isEOFError)
import System.Posix.Signals (Signal)
import System.Process.Typed
  ( Process,
    createPipe,
    getStderr,
    getStdin,
    getStdout,
    proc,
    setStderr,
    setStdin,
    setStdout,
    setWorkingDir,
    startProcess,
    stopProcess,
    waitExitCode
  )

import Process.Manager
import Process.Signals (SignalInfo (..), getSignalName)
import qualified Utils.Logging as Log

-- | HLS process status.
data HLSStatus
  = HLSStopped
  | HLSStarting
  | HLSRunning
  | HLSError Text
  | HLSTerminated SignalInfo
  deriving (Show, Eq)

-- | Configuration for the HLS process.
data HLSConfig = HLSConfig
  { hlsCommand :: String,
    hlsArgs :: [String],
    hlsWorkDir :: FilePath,
    hlsLogLevel :: Text,
    hlsEnableLogging :: Bool
  }
  deriving (Show, Eq)

-- | Default configuration used when launching HLS.
defaultHLSConfig :: FilePath -> HLSConfig
defaultHLSConfig workDir =
  HLSConfig
    { hlsCommand = "haskell-language-server",
      hlsArgs = ["--lsp"],
      hlsWorkDir = workDir,
      hlsLogLevel = "info",
      hlsEnableLogging = True
    }

-- | Runtime handle for a managed HLS process.
data HLSProcess = HLSProcess
  { hlsProcessManager :: ProcessManager HLSProcessData,
    hlsConfig :: HLSConfig,
    hlsStatus :: TVar HLSStatus,
    hlsStartTime :: UTCTime,
    hlsLastHealth :: TVar UTCTime
  }

-- | Internal data retained for lifecycle management.
data HLSProcessData = HLSProcessData
  { hlsProcessHandle :: Process Handle Handle Handle,
    hlsInputHandle :: Handle,
    hlsOutputHandle :: Handle,
    hlsErrorHandle :: Handle,
    hlsOutputReader :: Async (),
    hlsErrorReader :: Async (),
    hlsWatchdog :: Async ()
  }

instance Show HLSProcessData where
  show _ = "HLSProcessData{...}"

-- | Launch the HLS process and attach watchdogs/readers.
startHLSProcess :: HLSConfig -> IO (Either Text HLSProcess)
startHLSProcess config = do
  Log.logInfo "Starting HLS process"
  resolved <- resolveExecutable (hlsCommand config)
  case resolved of
    Left err -> pure $ Left err
    Right executablePath -> do
      when (executablePath /= hlsCommand config) $
        Log.logInfo $ "Resolved HLS executable: " <> T.pack executablePath

      startTime <- getCurrentTime
      processManager <- createProcessManager "hls"
      statusVar <- newTVarIO HLSStarting
      healthVar <- newTVarIO startTime

      let effectiveConfig =
            config
              { hlsCommand = executablePath
              }

          hlsProcess =
            HLSProcess
              { hlsProcessManager = processManager,
                hlsConfig = effectiveConfig,
                hlsStatus = statusVar,
                hlsStartTime = startTime,
                hlsLastHealth = healthVar
              }

      result <-
        Process.Manager.startProcess
          processManager
          (hlsWorkDir effectiveConfig)
          (buildCommandLine effectiveConfig)
          (initializeHLS hlsProcess)

      case result of
        Left err -> do
          atomically $ writeTVar statusVar (HLSError err)
          pure $ Left err
        Right _handle -> do
          atomically $ writeTVar statusVar HLSRunning
          Log.logInfo "HLS process started successfully"
          pure $ Right hlsProcess

-- | Stop the managed HLS process.
stopHLSProcess :: HLSProcess -> IO (Either Text ())
stopHLSProcess hlsProcess@HLSProcess {..} = do
  Log.logInfo "Stopping HLS process"
  result <- Process.Manager.stopProcess hlsProcessManager (cleanupHLS hlsProcess)
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      atomically $ writeTVar hlsStatus HLSStopped
      Log.logInfo "HLS process stopped successfully"
      pure $ Right ()

-- | Restart the HLS process with the same configuration.
restartHLSProcess :: HLSProcess -> IO (Either Text ())
restartHLSProcess hlsProcess@HLSProcess {..} = do
  Log.logInfo "Restarting HLS process"
  result <-
    Process.Manager.restartProcess
      hlsProcessManager
      (hlsWorkDir hlsConfig)
      (buildCommandLine hlsConfig)
      (initializeHLS hlsProcess)
      (cleanupHLS hlsProcess)
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      atomically $ writeTVar hlsStatus HLSRunning
      Log.logInfo "HLS process restarted successfully"
      pure $ Right ()

-- | Current status of the managed process.
getHLSStatus :: HLSProcess -> IO HLSStatus
getHLSStatus HLSProcess {..} = readTVarIO hlsStatus

-- | Send an LSP request (not yet implemented).
sendHLSRequest :: HLSProcess -> Value -> IO (Either Text ())
sendHLSRequest _ _ =
  pure $ Left "HLS request sending not implemented"

-- | Read an LSP response (not yet implemented).
readHLSResponse :: HLSProcess -> IO (Either Text Value)
readHLSResponse _ =
  pure $ Left "HLS response reading not implemented"

-- | Update health status and report the latest process state.
checkHLSHealth :: HLSProcess -> IO (Either Text HLSStatus)
checkHLSHealth hlsProcess@HLSProcess {..} = do
  status <- getHLSStatus hlsProcess
  touchHealth hlsLastHealth
  pure $ Right status

-- | Whether the process is currently running.
isHLSRunning :: HLSProcess -> IO Bool
isHLSRunning hlsProcess = do
  status <- getHLSStatus hlsProcess
  pure $ case status of
    HLSRunning -> True
    _ -> False

-- Internal -------------------------------------------------------------------

buildCommandLine :: HLSConfig -> [String]
buildCommandLine HLSConfig {..} = hlsCommand : hlsArgs

initializeHLS :: HLSProcess -> IO (Either Text HLSProcessData)
initializeHLS hlsProcess@HLSProcess {..} = do
  let HLSConfig {..} = hlsConfig
      processConfig =
        proc hlsCommand hlsArgs
          & setStdin createPipe
          & setStdout createPipe
          & setStderr createPipe
          & setWorkingDir hlsWorkDir

  result <- try @SomeException $ startProcess processConfig
  case result of
    Left ex -> pure $ Left $ "Failed to start HLS: " <> T.pack (show ex)
    Right process -> do
      let inputHandle = getStdin process
          outputHandle = getStdout process
          errorHandle = getStderr process

      IO.hSetBuffering outputHandle IO.LineBuffering
      IO.hSetBuffering errorHandle IO.LineBuffering

      outputReader <- async $ readHLSOutput hlsProcess outputHandle
      errorReader <- async $ readHLSError hlsProcess errorHandle
      watchdog <- async $ monitorHLSProcess hlsProcess process

      pure $
        Right
          HLSProcessData
            { hlsProcessHandle = process,
              hlsInputHandle = inputHandle,
              hlsOutputHandle = outputHandle,
              hlsErrorHandle = errorHandle,
              hlsOutputReader = outputReader,
              hlsErrorReader = errorReader,
              hlsWatchdog = watchdog
            }

cleanupHLS :: HLSProcess -> HLSProcessData -> IO ()
cleanupHLS HLSProcess {..} HLSProcessData {..} = do
  let cancelAsyncSafe worker =
        void $
          try @SomeException $ do
            cancel worker
            void (waitCatch worker)

  cancelAsyncSafe hlsOutputReader
  cancelAsyncSafe hlsErrorReader
  cancelAsyncSafe hlsWatchdog

  void $ try @SomeException (IO.hClose hlsInputHandle)
  void $ try @SomeException (IO.hClose hlsOutputHandle)
  void $ try @SomeException (IO.hClose hlsErrorHandle)
  void $ try @SomeException (stopProcess hlsProcessHandle)

  touchHealth hlsLastHealth
  atomically $ writeTVar hlsStatus HLSStopped
  Log.logInfo "HLS process resources cleaned up"

readHLSOutput :: HLSProcess -> Handle -> IO ()
readHLSOutput process handle =
  streamHandle process "HLS stdout" handle (\line -> Log.logDebug $ "[HLS stdout] " <> line)

readHLSError :: HLSProcess -> Handle -> IO ()
readHLSError process handle =
  streamHandle process "HLS stderr" handle (\line -> Log.logWarn $ "[HLS stderr] " <> line)

monitorHLSProcess :: HLSProcess -> Process Handle Handle Handle -> IO ()
monitorHLSProcess HLSProcess {..} process = do
  exitCode <- waitExitCode process
  touchHealth hlsLastHealth
  case exitCode of
    ExitSuccess -> do
      Log.logInfo "HLS process exited normally"
      atomically $ writeTVar hlsStatus HLSStopped
    ExitFailure code
      | code >= 128 -> do
          now <- getCurrentTime
          let signalNumber = code - 128
              signal :: Signal
              signal = fromIntegral signalNumber
              info =
                SignalInfo
                  { signalNumber = fromIntegral signalNumber :: CInt,
                    signalName = getSignalName signal,
                    receivedAt = now
                  }
          Log.logWarn $ "HLS process terminated by signal " <> signalName info
          atomically $ writeTVar hlsStatus (HLSTerminated info)
      | otherwise -> do
          Log.logError $ "HLS process exited with failure code " <> T.pack (show code)
          atomically $ writeTVar hlsStatus (HLSError $ "Exited with code " <> T.pack (show code))

streamHandle :: HLSProcess -> Text -> Handle -> (Text -> IO ()) -> IO ()
streamHandle HLSProcess {..} label handle logLine = do
  Log.logInfo $ label <> " reader started"
  let loop = do
        chunkResult <- try @IOException (BS.hGetSome handle 4096)
        case chunkResult of
          Left e ->
            if isEOFError e
              then Log.logInfo $ label <> " stream closed"
              else Log.logWarn $ label <> " reader error: " <> T.pack (show e)
          Right chunk ->
            if BS.null chunk
              then Log.logInfo $ label <> " stream closed"
              else do
                let textChunk = TE.decodeUtf8With TE.lenientDecode chunk
                logLine textChunk
                touchHealth hlsLastHealth
                loop
  loop

touchHealth :: TVar UTCTime -> IO ()
touchHealth healthVar = do
  now <- getCurrentTime
  atomically $ writeTVar healthVar now

-- Executable discovery -------------------------------------------------------

detectHLSExecutable :: String -> IO (Maybe FilePath)
detectHLSExecutable command = do
  found <- Dir.findExecutable command
  case found of
    Just path -> pure (Just path)
    Nothing -> search alternatives
  where
    alternatives =
      filter (/= command)
        [ "haskell-language-server-wrapper",
          "haskell-language-server",
          "hls"
        ]

    search :: [String] -> IO (Maybe FilePath)
    search [] = pure Nothing
    search (candidate : rest) = do
      resolution <- Dir.findExecutable candidate
      maybe (search rest) (pure . Just) resolution

resolveExecutable :: String -> IO (Either Text String)
resolveExecutable command = do
  detected <- detectHLSExecutable command
  case detected of
    Just path -> pure (Right path)
    Nothing ->
      pure $
        Left $
          "HLS executable not found. Tried "
            <> T.pack command
            <> " and common alternatives like haskell-language-server-wrapper."
