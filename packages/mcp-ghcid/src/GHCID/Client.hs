{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.Client
  ( GHCIDClient
  , GHCIDConfig(..)
  , GHCIDStatus(..)
  , CompilerMessage(..)
  , MessageSeverity(..)
  
    -- * Client lifecycle
  , createGHCIDClient
  , startGHCID  
  , stopGHCID
  , restartGHCID
  , getGHCIDStatus
  
    -- * Output monitoring  
  , getCurrentOutput
  , getCompilerMessages
  , subscribeToUpdates
  
    -- * Configuration
  , defaultGHCIDConfig
  , detectCabalProject
  ) where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import System.FilePath ((</>))
import System.Process.Typed
import qualified System.Process as SP
import System.IO (Handle)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Function ((&))
import Process.Manager
import Process.Signals (SignalInfo)
import qualified Utils.Logging as Logging

-- | Configuration for GHCID process
data GHCIDConfig = GHCIDConfig
  { ghcidCommand :: String           -- Command to run (usually "ghcid")
  , ghcidArgs :: [String]           -- Additional arguments
  , targetFiles :: [FilePath]       -- Files to load (e.g., ["src/Main.hs"])
  , cabalFile :: Maybe FilePath     -- Path to .cabal file
  , workingDir :: FilePath          -- Working directory
  , reloadOnChange :: Bool          -- Auto-reload on file changes
  , testCommand :: Maybe String     -- Optional test command
  } deriving (Show, Eq)

-- | Default GHCID configuration
defaultGHCIDConfig :: FilePath -> GHCIDConfig
defaultGHCIDConfig workDir = GHCIDConfig
  { ghcidCommand = "ghcid"
  , ghcidArgs = []
  , targetFiles = []
  , cabalFile = Nothing
  , workingDir = workDir
  , reloadOnChange = True
  , testCommand = Nothing
  }

-- | GHCID process status
data GHCIDStatus
  = GHCIDStopped
  | GHCIDStarting
  | GHCIDRunning Int                -- Number of loaded modules
  | GHCIDCompiling
  | GHCIDError Text
  | GHCIDTerminated SignalInfo
  deriving (Show, Eq)

-- | Compiler message severity
data MessageSeverity = Error | Warning | Info | Hint
  deriving (Show, Eq)

-- | Custom Ord instance for MessageSeverity to order by severity (Error > Warning > Info > Hint)
instance Ord MessageSeverity where
  Error `compare` Error = EQ
  Error `compare` _ = GT
  Warning `compare` Error = LT
  Warning `compare` Warning = EQ
  Warning `compare` _ = GT
  Info `compare` Error = LT
  Info `compare` Warning = LT
  Info `compare` Info = EQ
  Info `compare` Hint = GT
  Hint `compare` Hint = EQ
  Hint `compare` _ = LT

-- | Compiler message from GHCID
data CompilerMessage = CompilerMessage
  { msgSeverity :: MessageSeverity
  , msgFile :: Maybe FilePath
  , msgLine :: Maybe Int
  , msgColumn :: Maybe Int
  , msgText :: Text
  , msgTimestamp :: UTCTime
  } deriving (Show, Eq)

-- | Circular buffer for ghcid output
data OutputBuffer = OutputBuffer
  { bufferLines :: Seq Text        -- Recent output lines
  , bufferMaxSize :: Int          -- Maximum lines to keep
  , bufferTimestamp :: UTCTime    -- Last update time
  } deriving (Show, Eq)

-- | Create new output buffer
newOutputBuffer :: Int -> IO (TVar OutputBuffer)
newOutputBuffer maxSize = do
  now <- getCurrentTime
  newTVarIO $ OutputBuffer Seq.empty maxSize now

-- | Add line to circular buffer
addToBuffer :: TVar OutputBuffer -> Text -> IO ()
addToBuffer bufferVar line = do
  now <- getCurrentTime
  atomically $ do
    buffer <- readTVar bufferVar
    let newLines = bufferLines buffer |> line
        trimmedLines = if Seq.length newLines > bufferMaxSize buffer
                      then Seq.drop 1 newLines
                      else newLines
    writeTVar bufferVar $ buffer 
      { bufferLines = trimmedLines
      , bufferTimestamp = now
      }

-- | Get all buffered output as text
getBufferedOutput :: TVar OutputBuffer -> IO Text
getBufferedOutput bufferVar = do
  buffer <- readTVarIO bufferVar
  return $ T.unlines $ toList $ bufferLines buffer

-- Helper for Seq append
(|>) :: Seq a -> a -> Seq a
s |> x = s Seq.|> x

-- Helper to convert Seq to list (no longer needed as we import it from Data.Foldable)
-- toList :: Seq a -> [a] 
-- toList = Data.Foldable.toList

-- | GHCID client handle
data GHCIDClient = GHCIDClient
  { ghcidProcess :: ProcessManager GHCIDProcess
  , ghcidConfig :: GHCIDConfig
  , outputBuffer :: TVar OutputBuffer           -- NEW: Streaming output buffer
  , statusVar :: TVar GHCIDStatus
  , subscribersVar :: TVar [CompilerMessage -> IO ()]
  }

-- Internal process handle
data GHCIDProcess = GHCIDProcess
  { process :: SP.ProcessHandle
  , outputReader :: Async ()
  , errorReader :: Async ()
  } 

-- Manual Show instance since Async doesn't have Show
instance Show GHCIDProcess where
  show (GHCIDProcess _ _ _) = "GHCIDProcess{...}"

-- | Create a new GHCID client
createGHCIDClient :: GHCIDConfig -> IO GHCIDClient  
createGHCIDClient config = do
  processManager <- createProcessManager "ghcid"
  buffer <- newOutputBuffer 10000  -- Keep last 10k lines
  statusVar <- newTVarIO GHCIDStopped
  subscribersVar <- newTVarIO []
  
  return GHCIDClient
    { ghcidProcess = processManager
    , ghcidConfig = config
    , outputBuffer = buffer           -- NEW: Use output buffer
    , statusVar = statusVar
    , subscribersVar = subscribersVar
    }

-- | Start GHCID process
startGHCID :: GHCIDClient -> IO (Either Text ())
startGHCID client@GHCIDClient{..} = do
  Logging.logInfo "Starting GHCID process"
  atomically $ writeTVar statusVar GHCIDStarting
  
  result <- Process.Manager.startProcess 
    ghcidProcess
    (workingDir ghcidConfig)
    (buildCommandLine ghcidConfig)
    (initializeGHCID client)
    
  case result of
    Left err -> do
      atomically $ writeTVar statusVar (GHCIDError err)
      return $ Left err
    Right _handle -> do
      atomically $ writeTVar statusVar GHCIDCompiling
      return $ Right ()

-- Build command line arguments for GHCID
buildCommandLine :: GHCIDConfig -> [String]
buildCommandLine GHCIDConfig{..} = 
  [ghcidCommand] ++ ghcidArgs ++ 
  case cabalFile of
    Nothing -> targetFiles
    Just _ -> ["--command", "cabal repl"] ++ targetFiles

-- Initialize GHCID process
initializeGHCID :: GHCIDClient -> IO (Either Text GHCIDProcess)
initializeGHCID client@GHCIDClient{..} = do
  let config = ghcidConfig
      processConfig = proc (ghcidCommand config) (ghcidArgs config ++ buildGHCIDArgs config)
                    & setStdin closed
                    & setStdout createPipe  
                    & setStderr createPipe
                    & setWorkingDir (workingDir config)
  
  result <- try @SomeException $ System.Process.Typed.startProcess processConfig
  case result of
    Left ex -> return $ Left $ "Failed to start GHCID: " <> T.pack (show ex)
    Right process -> do
      -- Start output readers
      outputReader <- async $ readGHCIDOutput client (getStdout process)
      errorReader <- async $ readGHCIDError client (getStderr process)
      
      return $ Right $ GHCIDProcess
        { process = unsafeProcessHandle process
        , outputReader = outputReader
        , errorReader = errorReader
        }

-- Build GHCID-specific arguments
buildGHCIDArgs :: GHCIDConfig -> [String]
buildGHCIDArgs GHCIDConfig{..} = 
  concat
    [ case cabalFile of
        Nothing -> targetFiles
        Just _ -> ["--command", "cabal repl " ++ unwords targetFiles]
    , if reloadOnChange then [] else ["--no-reload"]
    , case testCommand of
        Nothing -> []
        Just cmd -> ["--test", cmd]
    ]

-- Read GHCID stdout output
readGHCIDOutput :: GHCIDClient -> Handle -> IO ()
readGHCIDOutput client handle = do
  result <- try @SomeException $ streamGHCIDOutput client handle
  case result of
    Left ex -> Logging.logError $ "Error reading GHCID output: " <> T.pack (show ex)
    Right _ -> return ()

-- Stream GHCID output line by line to buffer
streamGHCIDOutput :: GHCIDClient -> Handle -> IO ()
streamGHCIDOutput GHCIDClient{..} handle = do
  contents <- T.hGetContents handle
  let outputLines = T.lines contents
  mapM_ (addToBuffer outputBuffer) outputLines

-- Read GHCID stderr output  
readGHCIDError :: GHCIDClient -> Handle -> IO ()
readGHCIDError _ handle = do
  result <- try @SomeException $ T.hGetContents handle
  case result of
    Left ex -> Logging.logError $ "Error reading GHCID error: " <> T.pack (show ex)
    Right output -> Logging.logWarn $ "GHCID error output: " <> output

-- | Stop GHCID process  
stopGHCID :: GHCIDClient -> IO (Either Text ())
stopGHCID GHCIDClient{..} = do
  Logging.logInfo "Stopping GHCID process"
  result <- Process.Manager.stopProcess ghcidProcess cleanupGHCID
  case result of
    Left err -> return $ Left err
    Right _ -> do
      atomically $ writeTVar statusVar GHCIDStopped
      return $ Right ()

-- Cleanup GHCID process resources
cleanupGHCID :: GHCIDProcess -> IO ()
cleanupGHCID GHCIDProcess{..} = do
  -- Cancel async readers
  cancel outputReader
  cancel errorReader
  
  -- Stop the process
  result <- try @SomeException $ SP.terminateProcess process
  case result of
    Left ex -> Logging.logWarn $ "Error stopping GHCID process: " <> T.pack (show ex)
    Right _ -> Logging.logInfo "GHCID process stopped successfully"

-- | Restart GHCID process
restartGHCID :: GHCIDClient -> IO (Either Text ())
restartGHCID client@GHCIDClient{..} = do
  Logging.logInfo "Restarting GHCID process"
  result <- Process.Manager.restartProcess 
    ghcidProcess
    (workingDir ghcidConfig)
    (buildCommandLine ghcidConfig)
    (initializeGHCID client)
    cleanupGHCID
    
  case result of
    Left err -> return $ Left err
    Right _handle -> return $ Right ()

-- | Get current GHCID status
getGHCIDStatus :: GHCIDClient -> IO GHCIDStatus
getGHCIDStatus GHCIDClient{..} = readTVarIO statusVar

-- | Get current buffered output as text
getCurrentOutput :: GHCIDClient -> IO Text
getCurrentOutput GHCIDClient{..} = getBufferedOutput outputBuffer

-- | Get current compiler messages (legacy compatibility - parses from current output)
getCompilerMessages :: GHCIDClient -> IO [CompilerMessage]
getCompilerMessages client = do
  _ <- getCurrentOutput client
  -- For now, return empty list - parsing can be implemented later if needed
  return []

-- NOTE: clearMessages removed - we always operate on current output buffer

-- | Subscribe to compiler message updates
subscribeToUpdates :: GHCIDClient -> (CompilerMessage -> IO ()) -> IO ()
subscribeToUpdates GHCIDClient{..} callback = 
  atomically $ modifyTVar subscribersVar (callback :)

-- | Detect cabal project in directory
detectCabalProject :: FilePath -> IO (Maybe FilePath)
detectCabalProject dir = do
  result <- try @SomeException $ do
    let cabalFile = dir </> "*.cabal"  -- This would need proper glob matching
    return cabalFile
  case result of
    Left _ -> return Nothing
    Right path -> return $ Just path
