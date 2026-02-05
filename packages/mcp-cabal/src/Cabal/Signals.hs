{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cabal.Signals
  ( -- * Signal Handling
    CabalSignalHandler(..)
  , installCabalSignalHandlers
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
import qualified Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Cabal.ProcessRegistry (ProcessRegistry, shutdownProcessRegistry)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals
import qualified System.Timeout
import Utils.Logging

-- | Reasons for shutdown.
data ShutdownReason
  = SignalShutdown Signal
  | ExceptionShutdown SomeException
  | ManualShutdown
  | TimeoutShutdown
  deriving (Show)

-- | Shutdown configuration.
data ShutdownConfig = ShutdownConfig
  { shutdownGracePeriod :: Int
  , shutdownForceDelay :: Int
  , shutdownLogOutput :: Bool
  , shutdownExitCode :: ExitCode
  } deriving (Show, Eq)

-- | Default shutdown configuration.
defaultShutdownConfig :: ShutdownConfig
defaultShutdownConfig =
  ShutdownConfig
    { shutdownGracePeriod = 30
    , shutdownForceDelay = 10
    , shutdownLogOutput = True
    , shutdownExitCode = ExitSuccess
    }

-- | Signal handler for Cabal applications.
data CabalSignalHandler = CabalSignalHandler
  { signalShutdownVar :: TVar Bool
  , signalReasonVar :: TVar (Maybe ShutdownReason)
  , signalReceivedTime :: TVar (Maybe UTCTime)
  , signalRegistry :: Maybe ProcessRegistry
  , signalConfig :: ShutdownConfig
  }

-- | Install signal handlers for Cabal application.
installCabalSignalHandlers ::
  Maybe ProcessRegistry ->
  ShutdownConfig ->
  IO CabalSignalHandler
installCabalSignalHandlers registry config = do
  shutdownVar <- newTVarIO False
  reasonVar <- newTVarIO Nothing
  timeVar <- newTVarIO Nothing

  let handler =
        CabalSignalHandler
          { signalShutdownVar = shutdownVar
          , signalReasonVar = reasonVar
          , signalReceivedTime = timeVar
          , signalRegistry = registry
          , signalConfig = config
          }

  void $ installHandler sigTERM (Catch $ handleShutdownSignal handler sigTERM) Nothing
  void $ installHandler sigINT (Catch $ handleShutdownSignal handler sigINT) Nothing
  void $ installHandler sigHUP (Catch $ handleShutdownSignal handler sigHUP) Nothing
  void $ installHandler sigQUIT (Catch $ handleShutdownSignal handler sigQUIT) Nothing

  when (shutdownLogOutput config) $
    logInfo "Cabal signal handlers installed (SIGTERM, SIGINT, SIGHUP, SIGQUIT)"

  return handler

handleShutdownSignal :: CabalSignalHandler -> Signal -> IO ()
handleShutdownSignal CabalSignalHandler {..} sig = do
  now <- getCurrentTime
  atomically $ do
    writeTVar signalShutdownVar True
    writeTVar signalReasonVar (Just $ SignalShutdown sig)
    writeTVar signalReceivedTime (Just now)
  signalSafeLog $ "Received signal: " <> getSignalName sig
  void $ forkIO $ gracefulShutdown signalRegistry signalConfig (SignalShutdown sig)

getSignalName :: Signal -> Text
getSignalName sig
  | sig == sigTERM = "SIGTERM"
  | sig == sigINT = "SIGINT"
  | sig == sigHUP = "SIGHUP"
  | sig == sigQUIT = "SIGQUIT"
  | sig == sigUSR1 = "SIGUSR1"
  | sig == sigUSR2 = "SIGUSR2"
  | otherwise = "UNKNOWN"

waitForShutdownSignal :: CabalSignalHandler -> IO ShutdownReason
waitForShutdownSignal CabalSignalHandler {..} = do
  atomically $ do
    shutdown <- readTVar signalShutdownVar
    unless shutdown retry
    reason <- readTVar signalReasonVar
    case reason of
      Nothing -> retry
      Just r -> return r

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
    timeout = System.Timeout.timeout

performGracefulShutdown :: Maybe ProcessRegistry -> ShutdownConfig -> IO ()
performGracefulShutdown registry ShutdownConfig {..} = do
  case registry of
    Nothing -> return ()
    Just reg -> do
      when shutdownLogOutput $ logInfo "Shutting down process registry"
      shutdownProcessRegistry reg
  when shutdownLogOutput $ logInfo "Performing final cleanup"
  performFinalCleanup

performFinalCleanup :: IO ()
performFinalCleanup = do
  void $ try @SomeException $ do
    hFlush stdout
    hFlush stderr
  threadDelay 100000

forceShutdown ::
  Maybe ProcessRegistry ->
  ShutdownConfig ->
  ShutdownReason ->
  IO ()
forceShutdown registry ShutdownConfig {..} reason = do
  when shutdownLogOutput $
    logWarn $
      "Forcing shutdown due to: " <> T.pack (show reason)

  void $ timeout (shutdownForceDelay * 1000000) $ do
    signalSafeCleanup registry

  when shutdownLogOutput $
    logError "Forced shutdown complete"

  exitWith (ExitFailure 130)
  where
    timeout = System.Timeout.timeout

signalSafeLog :: Text -> IO ()
signalSafeLog msg = do
  now <- getCurrentTime
  let timeStr = take 19 $ show now
  void $ try @SomeException $ do
    hPutStrLn stderr $ timeStr ++ " [SIGNAL] " ++ T.unpack msg
    hFlush stderr

signalSafeCleanup :: Maybe ProcessRegistry -> IO ()
signalSafeCleanup registry = do
  case registry of
    Nothing -> return ()
    Just reg -> do
      void $ try @SomeException $ shutdownProcessRegistry reg

-- | Run an action with graceful shutdown handling.
withGracefulShutdown ::
  ShutdownConfig ->
  Maybe ProcessRegistry ->
  IO a ->
  IO a
withGracefulShutdown config registry action = do
  handler <- installCabalSignalHandlers registry config

  shutdownMonitor <- async $ do
    reason <- waitForShutdownSignal handler
    gracefulShutdown registry config reason

  result <- action `finally` cancel shutdownMonitor
  cancel shutdownMonitor
  return result
  where
    async = Control.Concurrent.Async.async
    cancel = Control.Concurrent.Async.cancel
