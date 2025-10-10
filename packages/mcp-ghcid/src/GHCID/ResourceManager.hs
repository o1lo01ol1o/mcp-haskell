{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.ResourceManager
  ( -- * Resource Management
    withProcessRegistry
  , withGHCIDProcess
  , withTimeout
  , safeAsyncWithCleanup
  
    -- * Resource Cleanup
  , ResourceCleanup(..)
  , cleanupResource
  , registerCleanup
  
    -- * Exception-Safe Operations
  , bracketGHCIDProcess
  , finallyGHCIDProcess
  , onExceptionGHCIDProcess
  
    -- * Timeout Management
  , TimeoutConfig(..)
  , defaultTimeoutConfig
  , withTimeoutConfig
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, bracket_, finally, onException, try)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

-- Internal imports
import GHCID.ProcessRegistry
import Utils.Logging

-- | Resource cleanup action
data ResourceCleanup = ResourceCleanup
  { cleanupName :: Text
  , cleanupAction :: IO ()
  , cleanupTimeout :: Int  -- seconds
  , cleanupPriority :: Int -- lower numbers = higher priority
  }

-- | Timeout configuration for various operations
data TimeoutConfig = TimeoutConfig
  { processStartTimeout :: Int    -- seconds
  , processStopTimeout :: Int     -- seconds
  , healthCheckTimeout :: Int     -- seconds
  , outputReadTimeout :: Int      -- seconds
  , registryShutdownTimeout :: Int -- seconds
  } deriving (Show, Eq)

-- | Default timeout configuration
defaultTimeoutConfig :: TimeoutConfig
defaultTimeoutConfig = TimeoutConfig
  { processStartTimeout = 30
  , processStopTimeout = 10
  , healthCheckTimeout = 5
  , outputReadTimeout = 2
  , registryShutdownTimeout = 60
  }

-- | Exception-safe process registry management
withProcessRegistry :: (ProcessRegistry -> IO a) -> IO a
withProcessRegistry action = bracket
  createProcessRegistry
  shutdownProcessRegistry
  action

-- | Exception-safe GHCID process management
withGHCIDProcess :: ProcessRegistry 
                 -> CabalURI 
                 -> FilePath 
                 -> (GHCIDHandle -> IO a) 
                 -> IO (Either Text a)
withGHCIDProcess registry cabalURI workDir action = do
  logInfo $ "Starting GHCID process with resource management for " <> getCabalURI cabalURI
  
  result <- try @SomeException $ bracket
    (startGHCIDProcess registry cabalURI workDir Nothing [])
    (\handleResult -> case handleResult of
      Left _ -> return () -- Nothing to cleanup if start failed
      Right _ -> void $ stopGHCIDProcess registry cabalURI)
    (\handleResult -> case handleResult of
      Left err -> return $ Left err
      Right handle -> fmap Right $ action handle)
  
  case result of
    Left ex -> do
      logError $ "Exception in withGHCIDProcess: " <> T.pack (show ex)
      return $ Left $ "Resource management error: " <> T.pack (show ex)
    Right res -> return res

-- | Bracket pattern specifically for GHCID processes
bracketGHCIDProcess :: ProcessRegistry
                    -> CabalURI
                    -> FilePath
                    -> (GHCIDHandle -> IO a)
                    -> (GHCIDHandle -> IO b)
                    -> (GHCIDHandle -> IO c)
                    -> IO (Either Text c)
bracketGHCIDProcess registry cabalURI workDir acquire cleanup action = do
  startResult <- startGHCIDProcess registry cabalURI workDir Nothing []
  case startResult of
    Left err -> return $ Left err
    Right handle -> do
      result <- try @SomeException $ bracket_
        (acquire handle)
        (cleanup handle)
        (action handle)
      
      -- Always try to stop the process
      void $ stopGHCIDProcess registry cabalURI
      
      case result of
        Left ex -> return $ Left $ "Bracket error: " <> T.pack (show ex)
        Right res -> return $ Right res

-- | Finally pattern for GHCID processes
finallyGHCIDProcess :: ProcessRegistry
                    -> CabalURI
                    -> FilePath
                    -> (GHCIDHandle -> IO a)
                    -> (GHCIDHandle -> IO b)
                    -> IO (Either Text a)
finallyGHCIDProcess registry cabalURI workDir action finalAction = do
  startResult <- startGHCIDProcess registry cabalURI workDir Nothing []
  case startResult of
    Left err -> return $ Left err
    Right handle -> do
      result <- try @SomeException $ 
        action handle `finally` finalAction handle
      
      -- Always try to stop the process
      void $ stopGHCIDProcess registry cabalURI
      
      case result of
        Left ex -> return $ Left $ "Finally error: " <> T.pack (show ex)
        Right res -> return $ Right res

-- | OnException pattern for GHCID processes
onExceptionGHCIDProcess :: ProcessRegistry
                        -> CabalURI
                        -> FilePath
                        -> (GHCIDHandle -> IO a)
                        -> (GHCIDHandle -> IO b)
                        -> IO (Either Text a)
onExceptionGHCIDProcess registry cabalURI workDir action exceptionAction = do
  startResult <- startGHCIDProcess registry cabalURI workDir Nothing []
  case startResult of
    Left err -> return $ Left err
    Right handle -> do
      result <- try @SomeException $ 
        action handle `onException` exceptionAction handle
      
      -- Always try to stop the process
      void $ stopGHCIDProcess registry cabalURI
      
      case result of
        Left ex -> return $ Left $ "OnException error: " <> T.pack (show ex)
        Right res -> return $ Right res

-- | Timeout wrapper with configurable timeout
withTimeout :: Int -> IO a -> IO (Either Text a)
withTimeout seconds action = do
  result <- timeout (seconds * 1000000) action
  case result of
    Nothing -> return $ Left $ "Operation timed out after " <> T.pack (show seconds) <> " seconds"
    Just res -> return $ Right res

-- | Timeout wrapper with timeout configuration
withTimeoutConfig :: TimeoutConfig -> (TimeoutConfig -> Int) -> IO a -> IO (Either Text a)
withTimeoutConfig config selector action = 
  withTimeout (selector config) action

-- | Safe async operation with automatic cleanup
safeAsyncWithCleanup :: IO a 
                     -> (Async a -> IO b)
                     -> IO b
safeAsyncWithCleanup action cleanup = bracket
  (async action)
  cancel
  cleanup

-- | Clean up a resource with timeout and error handling
cleanupResource :: ResourceCleanup -> IO (Either Text ())
cleanupResource ResourceCleanup{..} = do
  logInfo $ "Cleaning up resource: " <> cleanupName
  result <- withTimeout cleanupTimeout cleanupAction
  case result of
    Left timeoutErr -> do
      logError $ "Cleanup timeout for " <> cleanupName <> ": " <> timeoutErr
      return $ Left timeoutErr
    Right _ -> do
      logInfo $ "Successfully cleaned up resource: " <> cleanupName
      return $ Right ()

registerCleanup :: TVar [ResourceCleanup] -> ResourceCleanup -> STM ()
registerCleanup cleanupVar cleanup = do
  cleanups <- readTVar cleanupVar
  writeTVar cleanupVar (cleanup : cleanups)

-- Helper for sorting by priority
