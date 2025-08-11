{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Signals
  ( -- * Signal Handling for HLS
    HLSSignalHandler(..)
  , installHLSSignalHandlers
  , waitForShutdownSignal
  , gracefulShutdown
  , forceShutdown
  
    -- * Shutdown Management
  , ShutdownReason(..)
  , ShutdownConfig(..)
  , defaultShutdownConfig
  , withGracefulShutdown
  
    -- * Signal-Safe Operations
  , signalSafeLog
  , signalSafeCleanup
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, try, finally)
import Control.Monad (void, when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Posix.Signals
import System.IO (stderr, stdout, hPutStrLn, hFlush)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import qualified System.Timeout
import qualified Control.Concurrent.Async

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
  { shutdownGracePeriod :: Int      -- seconds to allow for graceful shutdown
  , shutdownForceDelay :: Int       -- additional seconds before forced termination
  , shutdownLogOutput :: Bool       -- whether to log shutdown progress
  , shutdownExitCode :: ExitCode    -- exit code to use
  } deriving (Show, Eq)

-- | Default shutdown configuration
defaultShutdownConfig :: ShutdownConfig
defaultShutdownConfig = ShutdownConfig
  { shutdownGracePeriod = 30
  , shutdownForceDelay = 10
  , shutdownLogOutput = True
  , shutdownExitCode = ExitSuccess
  }

-- | Signal handler for HLS applications
data HLSSignalHandler = HLSSignalHandler
  { signalShutdownVar :: TVar Bool
  , signalReasonVar :: TVar (Maybe ShutdownReason)
  , signalReceivedTime :: TVar (Maybe UTCTime)
  , signalConfig :: ShutdownConfig
  }

-- | Install signal handlers for HLS application
installHLSSignalHandlers :: ShutdownConfig -> IO HLSSignalHandler
installHLSSignalHandlers config = do
  shutdownVar <- newTVarIO False
  reasonVar <- newTVarIO Nothing
  timeVar <- newTVarIO Nothing
  
  let handler = HLSSignalHandler
        { signalShutdownVar = shutdownVar
        , signalReasonVar = reasonVar
        , signalReceivedTime = timeVar
        , signalConfig = config
        }
  
  -- Install signal handlers
  void $ installHandler sigTERM (Catch $ handleShutdownSignal handler sigTERM) Nothing
  void $ installHandler sigINT (Catch $ handleShutdownSignal handler sigINT) Nothing
  void $ installHandler sigHUP (Catch $ handleShutdownSignal handler sigHUP) Nothing
  void $ installHandler sigQUIT (Catch $ handleShutdownSignal handler sigQUIT) Nothing
  
  when (shutdownLogOutput config) $
    logInfo "HLS signal handlers installed (SIGTERM, SIGINT, SIGHUP, SIGQUIT)"
  
  return handler

-- | Handle shutdown signals
handleShutdownSignal :: HLSSignalHandler -> Signal -> IO ()
handleShutdownSignal HLSSignalHandler{..} sig = do
  now <- getCurrentTime
  
  -- Record the shutdown signal
  atomically $ do
    writeTVar signalShutdownVar True
    writeTVar signalReasonVar (Just $ SignalShutdown sig)
    writeTVar signalReceivedTime (Just now)
  
  -- Log the signal (using signal-safe logging)
  signalSafeLog $ "Received signal: " <> getSignalName sig
  
  -- Start graceful shutdown process
  void $ forkIO $ gracefulShutdown signalConfig (SignalShutdown sig)

-- | Get human-readable signal name
getSignalName :: Signal -> Text
getSignalName sig
  | sig == sigTERM = "SIGTERM"
  | sig == sigINT  = "SIGINT"
  | sig == sigHUP  = "SIGHUP"
  | sig == sigQUIT = "SIGQUIT"
  | sig == sigUSR1 = "SIGUSR1"
  | sig == sigUSR2 = "SIGUSR2"
  | otherwise = "UNKNOWN"

-- | Wait for a shutdown signal
waitForShutdownSignal :: HLSSignalHandler -> IO ShutdownReason
waitForShutdownSignal HLSSignalHandler{..} = do
  atomically $ do
    shutdown <- readTVar signalShutdownVar
    unless shutdown retry
    reason <- readTVar signalReasonVar
    case reason of
      Nothing -> retry
      Just r -> return r

-- | Perform graceful shutdown
gracefulShutdown :: ShutdownConfig -> ShutdownReason -> IO ()
gracefulShutdown config@ShutdownConfig{..} reason = do
  when shutdownLogOutput $
    logInfo $ "Starting graceful shutdown due to: " <> T.pack (show reason)
  
  startTime <- getCurrentTime
  
  -- Start shutdown with timeout
  shutdownResult <- System.Timeout.timeout (shutdownGracePeriod * 1000000) $ do
    performGracefulShutdown config
  
  endTime <- getCurrentTime
  let elapsed = realToFrac $ diffUTCTime endTime startTime
  
  case shutdownResult of
    Nothing -> do
      when shutdownLogOutput $
        logWarn $ "Graceful shutdown timed out after " <> T.pack (show shutdownGracePeriod) <> " seconds"
      forceShutdown config reason
    Just _ -> do
      when shutdownLogOutput $
        logInfo $ "Graceful shutdown completed in " <> T.pack (show (elapsed :: Double)) <> " seconds"
      exitWith shutdownExitCode

-- | Perform the actual graceful shutdown steps
performGracefulShutdown :: ShutdownConfig -> IO ()
performGracefulShutdown ShutdownConfig{..} = do
  when shutdownLogOutput $ logInfo "Performing HLS shutdown cleanup"
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
forceShutdown :: ShutdownConfig -> ShutdownReason -> IO ()
forceShutdown ShutdownConfig{..} reason = do
  when shutdownLogOutput $
    logWarn $ "Forcing shutdown due to: " <> T.pack (show reason)
  
  -- Give a brief moment for forced cleanup
  void $ System.Timeout.timeout (shutdownForceDelay * 1000000) $ do
    signalSafeCleanup
  
  when shutdownLogOutput $
    logError "Forced shutdown complete"
  
  exitWith (ExitFailure 130) -- 128 + SIGINT

-- | Signal-safe logging (writes directly to stderr)
signalSafeLog :: Text -> IO ()
signalSafeLog msg = do
  now <- getCurrentTime
  let timeStr = take 19 $ show now -- Take YYYY-MM-DD HH:MM:SS
  void $ try @SomeException $ do
    hPutStrLn stderr $ timeStr ++ " [SIGNAL] " ++ T.unpack msg
    hFlush stderr

-- | Signal-safe cleanup operations
signalSafeCleanup :: IO ()
signalSafeCleanup = do
  -- Attempt emergency cleanup without error handling
  void $ try @SomeException $ do
    logInfo "Emergency cleanup completed"

-- | Run an action with graceful shutdown handling
withGracefulShutdown :: ShutdownConfig -> IO a -> IO a
withGracefulShutdown config action = do
  handler <- installHLSSignalHandlers config
  
  -- Fork a thread to monitor for shutdown signals
  shutdownMonitor <- Control.Concurrent.Async.async $ do
    reason <- waitForShutdownSignal handler
    gracefulShutdown config reason
  
  -- Run the main action
  result <- action `finally` Control.Concurrent.Async.cancel shutdownMonitor
  
  -- Cancel the shutdown monitor if we complete normally
  Control.Concurrent.Async.cancel shutdownMonitor
  return result