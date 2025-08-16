{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.Signals
  ( -- * Signal Handling
    GHCIDSignalHandler (..),
    installGHCIDSignalHandlers,
    waitForShutdownSignal,
    gracefulShutdown,
    forceShutdown,

    -- * Shutdown Management
    ShutdownReason (..),
    ShutdownConfig (..),
    defaultShutdownConfig,
    withGracefulShutdown,

    -- * Signal-Safe Operations
    signalSafeLog,
    signalSafeCleanup,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
-- Internal imports
import GHCID.ProcessRegistry (ProcessRegistry, shutdownProcessRegistry)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals
import qualified System.Timeout
import Utils.Logging

-- | Reasons for shutdown
data ShutdownReason
  = SignalShutdown Signal
  | ExceptionShutdown SomeException
  | ManualShutdown
  | TimeoutShutdown
  deriving (Show)

-- | Shutdown configuration
data ShutdownConfig = ShutdownConfig
  { shutdownGracePeriod :: Int, -- seconds to allow for graceful shutdown
    shutdownForceDelay :: Int, -- additional seconds before forced termination
    shutdownLogOutput :: Bool, -- whether to log shutdown progress
    shutdownExitCode :: ExitCode -- exit code to use
  }
  deriving (Show, Eq)

-- | Default shutdown configuration
defaultShutdownConfig :: ShutdownConfig
defaultShutdownConfig =
  ShutdownConfig
    { shutdownGracePeriod = 30,
      shutdownForceDelay = 10,
      shutdownLogOutput = True,
      shutdownExitCode = ExitSuccess
    }

-- | Signal handler for GHCID applications
data GHCIDSignalHandler = GHCIDSignalHandler
  { signalShutdownVar :: TVar Bool,
    signalReasonVar :: TVar (Maybe ShutdownReason),
    signalReceivedTime :: TVar (Maybe UTCTime),
    signalRegistry :: Maybe ProcessRegistry,
    signalConfig :: ShutdownConfig
  }

-- | Install signal handlers for GHCID application
installGHCIDSignalHandlers ::
  Maybe ProcessRegistry ->
  ShutdownConfig ->
  IO GHCIDSignalHandler
installGHCIDSignalHandlers registry config = do
  shutdownVar <- newTVarIO False
  reasonVar <- newTVarIO Nothing
  timeVar <- newTVarIO Nothing

  let handler =
        GHCIDSignalHandler
          { signalShutdownVar = shutdownVar,
            signalReasonVar = reasonVar,
            signalReceivedTime = timeVar,
            signalRegistry = registry,
            signalConfig = config
          }

  -- Install signal handlers
  void $ installHandler sigTERM (Catch $ handleShutdownSignal handler sigTERM) Nothing
  void $ installHandler sigINT (Catch $ handleShutdownSignal handler sigINT) Nothing
  void $ installHandler sigHUP (Catch $ handleShutdownSignal handler sigHUP) Nothing
  void $ installHandler sigQUIT (Catch $ handleShutdownSignal handler sigQUIT) Nothing

  when (shutdownLogOutput config) $
    logInfo "GHCID signal handlers installed (SIGTERM, SIGINT, SIGHUP, SIGQUIT)"

  return handler

-- | Handle shutdown signals
handleShutdownSignal :: GHCIDSignalHandler -> Signal -> IO ()
handleShutdownSignal GHCIDSignalHandler {..} sig = do
  now <- getCurrentTime

  -- Record the shutdown signal
  atomically $ do
    writeTVar signalShutdownVar True
    writeTVar signalReasonVar (Just $ SignalShutdown sig)
    writeTVar signalReceivedTime (Just now)

  -- Log the signal (using signal-safe logging)
  signalSafeLog $ "Received signal: " <> getSignalName sig

  -- Start graceful shutdown process
  void $ forkIO $ gracefulShutdown signalRegistry signalConfig (SignalShutdown sig)

-- | Get human-readable signal name
getSignalName :: Signal -> Text
getSignalName sig
  | sig == sigTERM = "SIGTERM"
  | sig == sigINT = "SIGINT"
  | sig == sigHUP = "SIGHUP"
  | sig == sigQUIT = "SIGQUIT"
  | sig == sigUSR1 = "SIGUSR1"
  | sig == sigUSR2 = "SIGUSR2"
  | otherwise = "UNKNOWN"

-- | Wait for a shutdown signal
waitForShutdownSignal :: GHCIDSignalHandler -> IO ShutdownReason
waitForShutdownSignal GHCIDSignalHandler {..} = do
  atomically $ do
    shutdown <- readTVar signalShutdownVar
    unless shutdown retry
    reason <- readTVar signalReasonVar
    case reason of
      Nothing -> retry
      Just r -> return r

-- | Perform graceful shutdown
gracefulShutdown ::
  Maybe ProcessRegistry ->
  ShutdownConfig ->
  ShutdownReason ->
  IO ()
gracefulShutdown registry config@ShutdownConfig {..} reason = do
  when shutdownLogOutput $
    logInfo $
      "Starting graceful shutdown due to: " <> T.pack (show reason)

  startTime <- getCurrentTime

  -- Start shutdown with timeout
  shutdownResult <- timeout (shutdownGracePeriod * 1000000) $ do
    performGracefulShutdown registry config

  endTime <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime endTime startTime) :: Double

  case shutdownResult of
    Nothing -> do
      when shutdownLogOutput $
        logWarn $
          "Graceful shutdown timed out after " <> T.pack (show shutdownGracePeriod) <> " seconds"
      forceShutdown registry config reason
    Just _ -> do
      when shutdownLogOutput $
        logInfo $
          "Graceful shutdown completed in " <> T.pack (show elapsed) <> " seconds"
      exitWith shutdownExitCode
  where
    -- diffUTCTime already imported
    timeout = System.Timeout.timeout

-- | Perform the actual graceful shutdown steps
performGracefulShutdown :: Maybe ProcessRegistry -> ShutdownConfig -> IO ()
performGracefulShutdown registry ShutdownConfig {..} = do
  -- Step 1: Stop all GHCID processes (shutdown registry)
  case registry of
    Nothing -> return ()
    Just reg -> do
      when shutdownLogOutput $ logInfo "Shutting down process registry"
      shutdownProcessRegistry reg

  -- Step 3: Final cleanup
  when shutdownLogOutput $ logInfo "Performing final cleanup"
  performFinalCleanup

-- | Perform final cleanup
performFinalCleanup :: IO ()
performFinalCleanup = do
  -- Flush all log outputs
  void $ try @SomeException $ do
    hFlush stdout
    hFlush stderr

  -- Brief delay to ensure cleanup completion
  threadDelay 100000 -- 100ms

-- | Force shutdown when graceful shutdown fails
forceShutdown ::
  Maybe ProcessRegistry ->
  ShutdownConfig ->
  ShutdownReason ->
  IO ()
forceShutdown registry ShutdownConfig {..} reason = do
  when shutdownLogOutput $
    logWarn $
      "Forcing shutdown due to: " <> T.pack (show reason)

  -- Give a brief moment for forced cleanup
  void $ timeout (shutdownForceDelay * 1000000) $ do
    signalSafeCleanup registry

  when shutdownLogOutput $
    logError "Forced shutdown complete"

  exitWith (ExitFailure 130) -- 128 + SIGINT
  where
    timeout = System.Timeout.timeout

-- | Signal-safe logging (writes directly to stderr)
signalSafeLog :: Text -> IO ()
signalSafeLog msg = do
  now <- getCurrentTime
  let timeStr = take 19 $ show now -- Take YYYY-MM-DD HH:MM:SS
  void $ try @SomeException $ do
    hPutStrLn stderr $ timeStr ++ " [SIGNAL] " ++ T.unpack msg
    hFlush stderr

-- | Signal-safe cleanup operations
signalSafeCleanup :: Maybe ProcessRegistry -> IO ()
signalSafeCleanup registry = do
  void $ try @SomeException $ do
    case registry of
      Nothing -> return ()
      Just reg -> shutdownProcessRegistry reg

-- | Run an action with graceful shutdown handling
withGracefulShutdown ::
  ShutdownConfig ->
  Maybe ProcessRegistry ->
  IO a ->
  IO a
withGracefulShutdown config registry action = do
  handler <- installGHCIDSignalHandlers registry config

  -- Fork a thread to monitor for shutdown signals
  shutdownMonitor <- async $ do
    reason <- waitForShutdownSignal handler
    gracefulShutdown registry config reason

  -- Run the main action
  result <- action `finally` cancel shutdownMonitor

  -- Cancel the shutdown monitor if we complete normally
  cancel shutdownMonitor
  return result
  where
    async = Control.Concurrent.Async.async
    cancel = Control.Concurrent.Async.cancel
