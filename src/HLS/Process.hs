{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.Process where

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import System.Process.Typed
import System.Directory (findExecutable)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import MCP.Types

-- HLS Process Handle
data HLSHandle = HLSHandle
  { processHandle :: Process Handle Handle Handle
  , workingDirectory :: FilePath
  }

-- Global HLS Process State
hlsProcessVar :: MVar (Maybe HLSHandle)
hlsProcessVar = unsafePerformIO $ newMVar Nothing
{-# NOINLINE hlsProcessVar #-}

-- Start HLS Server
startHLSServer :: Maybe FilePath -> IO (Either Text HLSStatus)
startHLSServer maybeWorkDir = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Just _ -> return $ Left "HLS server is already running"
    Nothing -> do
      result <- try @SomeException startProcess
      case result of
        Left ex -> return $ Left $ "Failed to start HLS: " <> T.pack (show ex)
        Right handle -> do
          modifyMVar_ hlsProcessVar (const $ return $ Just handle)
          return $ Right Running
  where
    workDir = maybe "." Prelude.id maybeWorkDir
    
    startProcess :: IO HLSHandle
    startProcess = do
      let processConfig = proc "haskell-language-server-wrapper" ["--lsp"]
                        & setWorkingDir workDir
                        & setStdin createPipe
                        & setStdout createPipe
                        & setStderr createPipe
      
      process <- System.Process.Typed.startProcess processConfig
      
      -- Give the process a moment to start up
      threadDelay 500000  -- 0.5 seconds
      
      -- Check if process started successfully
      exitCode <- getExitCode process
      case exitCode of
        Just code -> do
          -- Process already exited, stop it and throw error
          stopProcess process
          Prelude.error $ "HLS process exited immediately with code: " ++ show code
        Nothing -> 
          -- Process is running
          return HLSHandle
            { processHandle = process
            , workingDirectory = workDir
            }

-- Stop HLS Server
stopHLSServer :: IO (Either Text HLSStatus)
stopHLSServer = do
  currentProcess <- takeMVar hlsProcessVar
  case currentProcess of
    Nothing -> do
      putMVar hlsProcessVar Nothing
      return $ Left "HLS server is not running"
    Just handle -> do
      result <- try @SomeException $ stopProcess (processHandle handle)
      putMVar hlsProcessVar Nothing
      case result of
        Left ex -> return $ Left $ "Failed to stop HLS: " <> T.pack (show ex)
        Right _ -> return $ Right Stopped

-- Restart HLS Server
restartHLSServer :: IO (Either Text HLSStatus)
restartHLSServer = do
  stopResult <- stopHLSServer
  case stopResult of
    Left err -> return $ Left err
    Right _ -> do
      -- Give the process time to clean up
      threadDelay 1000000  -- 1 second
      startHLSServer Nothing

-- Get HLS Status
getHLSStatus :: IO HLSStatus
getHLSStatus = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return Stopped
    Just handle -> do
      result <- try @SomeException $ checkExitCode (processHandle handle)
      case result of
        Left ex -> return $ Error $ T.pack $ show ex
        Right _ -> return Running

-- Send LSP Message to HLS
sendLSPMessage :: Text -> IO (Either Text ())
sendLSPMessage msg = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      result <- try @SomeException $ do
        T.hPutStrLn (getStdin (processHandle handle)) msg
      case result of
        Left ex -> return $ Left $ "Failed to send message: " <> T.pack (show ex)
        Right _ -> return $ Right ()

-- Read LSP Response from HLS
readLSPResponse :: IO (Either Text Text)
readLSPResponse = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      result <- try @SomeException $ T.hGetLine (getStdout (processHandle handle))
      case result of
        Left ex -> return $ Left $ "Failed to read response: " <> T.pack (show ex)
        Right line -> return $ Right line

-- Find HLS Executable
findHLSExecutable :: IO (Either Text FilePath)
findHLSExecutable = do
  result <- try @SomeException $ findExecutable "haskell-language-server-wrapper"
  case result of
    Left ex -> return $ Left $ "Failed to find HLS: " <> T.pack (show ex)
    Right Nothing -> return $ Left "HLS not found in PATH"
    Right (Just path) -> return $ Right path

-- Get HLS Version
getHLSVersion :: IO (Either Text Text)
getHLSVersion = do
  result <- try @SomeException $ do
    let processConfig = proc "haskell-language-server-wrapper" ["--version"]
                      & setStdout byteStringOutput
    (exitCode, output, _) <- readProcess processConfig
    case exitCode of
      ExitSuccess -> return $ T.strip $ T.decodeUtf8 $ LBS.toStrict output
      _ -> Prelude.error "Process failed"
  case result of
    Left ex -> return $ Left $ "Failed to get HLS version: " <> T.pack (show ex)
    Right output -> return $ Right output