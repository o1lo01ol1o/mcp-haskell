{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Process
  ( -- * HLS Process Management
    HLSProcess(..)
  , HLSConfig(..)
  , HLSStatus(..)
  
    -- * Process Lifecycle
  , startHLSProcess
  , stopHLSProcess
  , restartHLSProcess
  , getHLSStatus
  
    -- * Configuration
  , defaultHLSConfig
  , detectHLSExecutable
  
    -- * Communication
  , sendHLSRequest
  , readHLSResponse
  
    -- * Health Monitoring
  , checkHLSHealth
  , isHLSRunning
  ) where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (SomeException, try, bracket)
import Control.Monad (void, when)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.FilePath ((</>))
import System.Process.Typed
import System.Directory (findExecutable)
import System.IO (Handle)
import Data.Function ((&))

import MCP.Types hiding (HLSStatus)
import Process.Manager
import Process.Signals (SignalInfo)
import Utils.Logging

-- | HLS process status
data HLSStatus
  = HLSStopped
  | HLSStarting
  | HLSRunning
  | HLSError Text
  | HLSTerminated SignalInfo
  deriving (Show, Eq)

-- | Configuration for HLS process
data HLSConfig = HLSConfig
  { hlsCommand :: String           -- Command to run (usually "haskell-language-server")
  , hlsArgs :: [String]           -- Additional arguments
  , hlsWorkDir :: FilePath        -- Working directory
  , hlsLogLevel :: Text           -- Log level (debug, info, warn, error)
  , hlsEnableLogging :: Bool      -- Whether to enable file logging
  } deriving (Show, Eq)

-- | Default HLS configuration
defaultHLSConfig :: FilePath -> HLSConfig
defaultHLSConfig workDir = HLSConfig
  { hlsCommand = "haskell-language-server"
  , hlsArgs = ["--lsp"]
  , hlsWorkDir = workDir
  , hlsLogLevel = "info"
  , hlsEnableLogging = True
  }

-- | HLS process handle
data HLSProcess = HLSProcess
  { hlsProcessManager :: ProcessManager HLSProcessData
  , hlsConfig :: HLSConfig
  , hlsStatus :: TVar HLSStatus
  , hlsStartTime :: UTCTime
  , hlsLastHealth :: TVar UTCTime
  }

-- Internal process data
data HLSProcessData = HLSProcessData
  { hlsProcessHandle :: Process Handle Handle Handle
  , hlsInputHandle :: Handle
  , hlsOutputHandle :: Handle
  , hlsErrorHandle :: Handle
  , hlsOutputReader :: Async ()
  , hlsErrorReader :: Async ()
  }

instance Show HLSProcessData where
  show _ = "HLSProcessData{...}"

-- | Start HLS process
startHLSProcess :: HLSConfig -> IO (Either Text HLSProcess)
startHLSProcess config = do
  logInfo "Starting HLS process"
  
  -- Check if HLS executable exists
  hlsPath <- detectHLSExecutable (hlsCommand config)
  case hlsPath of
    Nothing -> return $ Left $ "HLS executable not found: " <> T.pack (hlsCommand config)
    Just _ -> do
      startTime <- getCurrentTime
      processManager <- createProcessManager "hls"
      statusVar <- newTVarIO HLSStarting
      healthVar <- newTVarIO startTime
      
      let hlsProcess = HLSProcess
            { hlsProcessManager = processManager
            , hlsConfig = config
            , hlsStatus = statusVar
            , hlsStartTime = startTime
            , hlsLastHealth = healthVar
            }
      
      result <- Process.Manager.startProcess 
        processManager
        (hlsWorkDir config)
        (buildCommandLine config)
        (initializeHLS hlsProcess)
        
      case result of
        Left err -> do
          atomically $ writeTVar statusVar (HLSError err)
          return $ Left err
        Right _ -> do
          atomically $ writeTVar statusVar HLSRunning
          logInfo "HLS process started successfully"
          return $ Right hlsProcess

-- | Build command line for HLS
buildCommandLine :: HLSConfig -> [String]
buildCommandLine HLSConfig{..} = hlsCommand : hlsArgs

-- | Initialize HLS process data
initializeHLS :: HLSProcess -> IO (Either Text HLSProcessData)
initializeHLS HLSProcess{..} = do
  let config = hlsConfig
      processConfig = proc (hlsCommand config) (hlsArgs config)
                    & setStdin createPipe  
                    & setStdout createPipe
                    & setStderr createPipe
                    & setWorkingDir (hlsWorkDir config)
  
  result <- try @SomeException $ System.Process.Typed.startProcess processConfig
  case result of
    Left ex -> return $ Left $ "Failed to start HLS: " <> T.pack (show ex)
    Right process -> do
      let inputHandle = getStdin process
          outputHandle = getStdout process
          errorHandle = getStderr process
      
      -- Start output readers
      outputReader <- async $ readHLSOutput outputHandle
      errorReader <- async $ readHLSError errorHandle
      
      return $ Right $ HLSProcessData
        { hlsProcessHandle = process
        , hlsInputHandle = inputHandle
        , hlsOutputHandle = outputHandle
        , hlsErrorHandle = errorHandle
        , hlsOutputReader = outputReader
        , hlsErrorReader = errorReader
        }

-- | Read HLS stdout
readHLSOutput :: Handle -> IO ()
readHLSOutput handle = do
  result <- try @SomeException $ do
    logInfo "HLS output reader started"
    -- This would normally read LSP messages
    -- For now, just log that we're reading
  case result of
    Left ex -> logError $ "HLS output reader error: " <> T.pack (show ex)
    Right _ -> logInfo "HLS output reader finished"

-- | Read HLS stderr
readHLSError :: Handle -> IO ()
readHLSError handle = do
  result <- try @SomeException $ do
    logInfo "HLS error reader started" 
    -- This would normally read error messages
  case result of
    Left ex -> logError $ "HLS error reader error: " <> T.pack (show ex)
    Right _ -> logInfo "HLS error reader finished"

-- | Stop HLS process
stopHLSProcess :: HLSProcess -> IO (Either Text ())
stopHLSProcess HLSProcess{..} = do
  logInfo "Stopping HLS process"
  result <- Process.Manager.stopProcess hlsProcessManager cleanupHLS
  case result of
    Left err -> return $ Left err
    Right _ -> do
      atomically $ writeTVar hlsStatus HLSStopped
      logInfo "HLS process stopped successfully"
      return $ Right ()

-- | Cleanup HLS process resources
cleanupHLS :: HLSProcessData -> IO ()
cleanupHLS HLSProcessData{..} = do
  -- Cancel async readers
  cancel hlsOutputReader
  cancel hlsErrorReader
  
  -- Stop the process
  result <- try @SomeException $ System.Process.Typed.stopProcess hlsProcessHandle
  case result of
    Left ex -> logWarn $ "Error stopping HLS process: " <> T.pack (show ex)
    Right _ -> logInfo "HLS process stopped successfully"

-- | Restart HLS process
restartHLSProcess :: HLSProcess -> IO (Either Text ())
restartHLSProcess hlsProcess@HLSProcess{..} = do
  logInfo "Restarting HLS process"
  result <- Process.Manager.restartProcess 
    hlsProcessManager
    (hlsWorkDir hlsConfig)
    (buildCommandLine hlsConfig)
    (initializeHLS hlsProcess)
    cleanupHLS
    
  case result of
    Left err -> return $ Left err
    Right _ -> do
      atomically $ writeTVar hlsStatus HLSRunning
      logInfo "HLS process restarted successfully"
      return $ Right ()

-- | Get HLS status
getHLSStatus :: HLSProcess -> IO HLSStatus
getHLSStatus HLSProcess{..} = readTVarIO hlsStatus

-- | Detect HLS executable
detectHLSExecutable :: String -> IO (Maybe FilePath)
detectHLSExecutable command = do
  result <- findExecutable command
  case result of
    Nothing -> do
      -- Try common locations
      let alternatives = ["haskell-language-server-wrapper", "hls"]
      results <- mapM findExecutable alternatives
      return $ head $ filter (\x -> x /= Nothing) results
    Just path -> return $ Just path

-- | Send request to HLS
sendHLSRequest :: HLSProcess -> Value -> IO (Either Text ())
sendHLSRequest _process _request = do
  -- This would send LSP requests to HLS
  return $ Left "HLS request sending not implemented"

-- | Read response from HLS  
readHLSResponse :: HLSProcess -> IO (Either Text Value)
readHLSResponse _process = do
  -- This would read LSP responses from HLS
  return $ Left "HLS response reading not implemented"

-- | Check HLS health
checkHLSHealth :: HLSProcess -> IO (Either Text HLSStatus)
checkHLSHealth hlsProcess@HLSProcess{..} = do
  status <- getHLSStatus hlsProcess
  now <- getCurrentTime
  atomically $ writeTVar hlsLastHealth now
  return $ Right status

-- | Check if HLS is running
isHLSRunning :: HLSProcess -> IO Bool
isHLSRunning hlsProcess = do
  status <- getHLSStatus hlsProcess
  return $ case status of
    HLSRunning -> True
    _ -> False