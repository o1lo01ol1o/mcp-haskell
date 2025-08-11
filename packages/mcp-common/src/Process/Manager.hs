{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Process.Manager
  ( -- * Process Handle Types
    ProcessHandle(..)
  , ProcessState(..)
  , ProcessManager(..)
  
    -- * Process Lifecycle
  , createProcessManager
  , startProcess
  , stopProcess  
  , restartProcess
  , getProcessStatus
  
    -- * Process Communication
  , sendMessage
  , readMessage
  
    -- * Process Health
  , isProcessRunning
  , waitForProcess
  , killProcess
  
    -- * Utilities
  , getProcessVersion
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO, writeTVar, atomically)
import Control.Exception (SomeException, try, bracket)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed hiding (startProcess, stopProcess)
import qualified System.Process.Typed as PT
import System.Timeout (timeout)
import Process.Signals (SignalInfo)
import Utils.Logging

-- | Generic process handle for any external process
data ProcessHandle a = ProcessHandle
  { processClient :: a                    -- Client-specific handle (LSPClient, GHCIDClient, etc)
  , workingDirectory :: FilePath          -- Working directory
  , processCommandLine :: [String]        -- Original command line for restart
  }

-- | Process state tracking
data ProcessState
  = ProcessStopped
  | ProcessStarting  
  | ProcessRunning
  | ProcessUnhealthy Text
  | ProcessDead ExitCode
  | ProcessTerminated SignalInfo
  deriving (Show, Eq)

-- | Generic process manager for any type of external process
data ProcessManager a = ProcessManager
  { processVar :: MVar (Maybe (ProcessHandle a))
  , statusVar :: TVar ProcessState
  , processName :: Text
  }

-- | Create a new process manager
createProcessManager :: Text -> IO (ProcessManager a)
createProcessManager name = do
  pVar <- newMVar Nothing
  sVar <- newTVarIO ProcessStopped
  return ProcessManager
    { processVar = pVar
    , statusVar = sVar
    , processName = name
    }

-- | Start a process using a custom initializer function
startProcess :: forall a. ProcessManager a 
             -> FilePath                      -- Working directory
             -> [String]                      -- Command line arguments  
             -> IO (Either Text a)            -- Process initializer
             -> IO (Either Text (ProcessHandle a))
startProcess manager workDir cmdArgs initializer = do
  currentProcess <- readMVar (processVar manager)
  case currentProcess of
    Just _ -> return $ Left $ processName manager <> " is already running"
    Nothing -> do
      atomically $ writeTVar (statusVar manager) ProcessStarting
      result <- try @SomeException startProcessInternal
      case result of
        Left ex -> do
          atomically $ writeTVar (statusVar manager) (ProcessUnhealthy $ T.pack $ show ex)
          return $ Left $ "Failed to start " <> processName manager <> ": " <> T.pack (show ex)
        Right handle -> do
          modifyMVar_ (processVar manager) (const $ return $ Just handle)
          atomically $ writeTVar (statusVar manager) ProcessRunning
          return $ Right handle
  where
    startProcessInternal :: IO (ProcessHandle a)
    startProcessInternal = do
      logInfo $ "Starting " <> processName manager <> " in " <> T.pack workDir
      
      -- Initialize the client-specific process
      clientResult <- initializer
      case clientResult of
        Left err -> error $ "Failed to initialize client: " ++ T.unpack err
        Right client -> do
          return ProcessHandle 
            { processClient = client
            , workingDirectory = workDir  
            , processCommandLine = cmdArgs
            }

-- | Stop a running process with a custom cleanup function
stopProcess :: ProcessManager a 
            -> (a -> IO ())                  -- Client cleanup function
            -> IO (Either Text ProcessState)
stopProcess manager cleanup = do
  currentProcess <- takeMVar (processVar manager)
  case currentProcess of
    Nothing -> do
      putMVar (processVar manager) Nothing
      return $ Left $ processName manager <> " is not running"
    Just handle -> do
      logInfo $ "Stopping " <> processName manager
      
      -- Cleanup client resources
      result <- try @SomeException $ cleanup (processClient handle)
      case result of
        Left ex -> logWarn $ "Cleanup failed for " <> processName manager <> ": " <> T.pack (show ex)
        Right _ -> logInfo $ processName manager <> " cleanup completed"
      
      putMVar (processVar manager) Nothing
      atomically $ writeTVar (statusVar manager) ProcessStopped
      return $ Right ProcessStopped

-- | Restart a process  
restartProcess :: ProcessManager a 
               -> FilePath                      -- Working directory
               -> [String]                      -- Command line arguments
               -> IO (Either Text a)            -- Process initializer  
               -> (a -> IO ())                  -- Client cleanup function
               -> IO (Either Text (ProcessHandle a))
restartProcess manager workDir cmdArgs initializer cleanup = do
  logInfo $ "Restarting " <> processName manager
  
  stopResult <- Process.Manager.stopProcess manager cleanup
  case stopResult of
    Left err -> return $ Left err
    Right _ -> do
      -- Give the process time to clean up
      threadDelay 1000000 -- 1 second
      startProcess manager workDir cmdArgs initializer

-- | Get current process status  
getProcessStatus :: ProcessManager a -> IO ProcessState
getProcessStatus manager = readTVarIO (statusVar manager)

-- | Check if process is running
isProcessRunning :: ProcessManager a -> IO Bool
isProcessRunning manager = do
  status <- getProcessStatus manager
  case status of
    ProcessRunning -> return True
    _ -> return False

-- | Send a message to the process (generic text-based)
sendMessage :: ProcessManager a 
            -> (a -> Text -> IO (Either Text ()))  -- Client-specific send function
            -> Text 
            -> IO (Either Text ())
sendMessage manager sendFn msg = do
  currentProcess <- readMVar (processVar manager)
  case currentProcess of
    Nothing -> return $ Left $ processName manager <> " is not running"
    Just handle -> do
      result <- try @SomeException $ sendFn (processClient handle) msg
      case result of
        Left ex -> return $ Left $ "Failed to send message: " <> T.pack (show ex)
        Right sendResult -> return sendResult

-- | Read a message from the process (generic text-based)
readMessage :: ProcessManager a 
            -> (a -> IO (Either Text Text))    -- Client-specific read function
            -> IO (Either Text Text)
readMessage manager readFn = do
  currentProcess <- readMVar (processVar manager)
  case currentProcess of
    Nothing -> return $ Left $ processName manager <> " is not running"
    Just handle -> do
      result <- try @SomeException $ readFn (processClient handle)
      case result of
        Left ex -> return $ Left $ "Failed to read message: " <> T.pack (show ex)
        Right readResult -> return readResult

-- | Execute an action with a running process
withProcess :: ProcessManager a 
            -> (ProcessHandle a -> IO b) 
            -> IO (Either Text b)
withProcess manager action = do
  currentProcess <- readMVar (processVar manager)
  case currentProcess of
    Nothing -> return $ Left $ processName manager <> " is not running"
    Just handle -> do
      result <- try @SomeException $ action handle
      case result of
        Left ex -> return $ Left $ "Process action failed: " <> T.pack (show ex)
        Right val -> return $ Right val

-- | Wait for a process to exit (with timeout)
waitForProcess :: ProcessManager a 
               -> Int                           -- Timeout in seconds
               -> IO (Either Text ExitCode)
waitForProcess manager timeoutSecs = do
  result <- timeout (timeoutSecs * 1000000) waitLoop
  case result of
    Nothing -> return $ Left "Timeout waiting for process"
    Just exitCode -> return $ Right exitCode
  where
    waitLoop :: IO ExitCode
    waitLoop = do
      status <- getProcessStatus manager
      case status of
        ProcessDead exitCode -> return exitCode
        ProcessStopped -> return ExitSuccess
        _ -> do
          threadDelay 100000 -- 100ms
          waitLoop

-- | Kill a process forcefully (implementation depends on process type)
killProcess :: ProcessManager a 
            -> (a -> IO ())                  -- Client-specific kill function
            -> IO (Either Text ())
killProcess manager killFn = do
  currentProcess <- readMVar (processVar manager)
  case currentProcess of
    Nothing -> return $ Left $ processName manager <> " is not running"
    Just handle -> do
      result <- try @SomeException $ killFn (processClient handle)
      case result of
        Left ex -> return $ Left $ "Failed to kill process: " <> T.pack (show ex)
        Right _ -> do
          atomically $ writeTVar (statusVar manager) ProcessStopped
          return $ Right ()

-- | Find an executable in PATH
findExecutable :: String -> IO (Either Text FilePath)
findExecutable name = do
  result <- try @SomeException $ Dir.findExecutable name
  case result of
    Left ex -> return $ Left $ "Failed to find executable: " <> T.pack (show ex)
    Right Nothing -> return $ Left $ "Executable not found in PATH: " <> T.pack name
    Right (Just path) -> return $ Right path

-- | Get version of an executable
getProcessVersion :: [String]                    -- Command line to get version
                  -> IO (Either Text Text)
getProcessVersion cmdLine = do
  result <- timeout 5000000 $ try @SomeException $ do  -- 5 second timeout
    let processConfig = setStdout byteStringOutput $ proc (head cmdLine) (tail cmdLine)
    (exitCode, output, _) <- readProcess processConfig
    case exitCode of
      ExitSuccess -> return $ T.strip $ T.decodeUtf8 $ LBS.toStrict output
      ExitFailure code -> error $ "Process failed with exit code: " ++ show code
  case result of
    Nothing -> return $ Left "Timeout getting process version"
    Just (Left ex) -> return $ Left $ "Failed to get process version: " <> T.pack (show ex)
    Just (Right output) -> return $ Right output