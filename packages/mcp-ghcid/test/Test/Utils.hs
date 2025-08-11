{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Utils where

import Control.Concurrent (threadDelay, ThreadId)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when, filterM)
import Test.Hspec (expectationFailure)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hPutStrLn, hSetBuffering)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), createProcess, getProcessExitCode, proc, terminateProcess)
import System.Timeout (timeout)

-- Production imports from GHCID library
import GHCID.Client (GHCIDClient, GHCIDConfig(..), defaultGHCIDConfig, createGHCIDClient, startGHCID, stopGHCID, getCurrentOutput)
import GHCID.Filter (FilterRequest(..), applyShellFilter)

-- Global process registry for cleanup
allGHCIDProcesses :: MVar [ProcessHandle]
allGHCIDProcesses = unsafePerformIO $ newMVar []
{-# NOINLINE allGHCIDProcesses #-}

-- | Cleanup all tracked processes
cleanupAllProcesses :: IO ()
cleanupAllProcesses = do
  pids <- takeMVar allGHCIDProcesses
  mapM_ cleanupProcess pids
  putMVar allGHCIDProcesses []
  threadDelay 500000 -- Wait 500ms for all processes to terminate
  where
    cleanupProcess pHandle = do
      exitCode <- getProcessExitCode pHandle
      when (isNothing exitCode) $ do
        terminateProcess pHandle
        threadDelay 200000 -- Wait 200ms for each process termination

-- | GHCID handle for testing
data GHCIDHandle = GHCIDHandle
  { ghcidClient :: GHCIDClient,
    ghcidWorkDir :: FilePath
  }

-- | Start GHCID process for testing
startTestGHCID :: FilePath -> IO (Either Text GHCIDHandle)
startTestGHCID workDir = do
  let config = defaultGHCIDConfig workDir
  result <- try @SomeException $ do
    client <- createGHCIDClient config
    startResult <- startGHCID client
    case startResult of
      Left err -> return $ Left err
      Right _ -> do
        let handle = GHCIDHandle client workDir
        -- Give GHCID a moment to initialize
        threadDelay 2000000 -- 2 second delay
        return $ Right handle
        
  case result of
    Left ex -> return $ Left $ "Failed to start GHCID: " <> T.pack (show ex)
    Right res -> return res

-- | Stop GHCID process
stopTestGHCID :: GHCIDHandle -> IO ()
stopTestGHCID handle = do
  _ <- stopGHCID (ghcidClient handle)
  return ()

-- | Test helper to run GHCID operation with proper setup/teardown
withTestGHCID :: FilePath -> (GHCIDHandle -> IO a) -> IO (Either Text a)
withTestGHCID workDir action = do
  result <- try @SomeException $ bracket
    (startTestGHCID workDir)
    (\eitherHandle -> case eitherHandle of
       Left _ -> return ()
       Right handle -> stopTestGHCID handle)
    (\eitherHandle -> case eitherHandle of
       Left err -> error $ T.unpack err
       Right handle -> action handle)
  case result of
    Left ex -> return $ Left $ T.pack $ show ex
    Right res -> return $ Right res

-- | Helper to create a temporary Haskell project
withTestHaskellProject :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTestHaskellProject files action = withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
  -- Create default cabal file if not provided
  let hasCAbalFile = any ((".cabal" `T.isSuffixOf`) . T.pack . fst) files
  let allFiles = if hasCAbalFile 
                 then files
                 else ("test-project.cabal", defaultCAbalFile) : files
  
  -- Create test files
  mapM_ (createTestFile tmpDir) allFiles
  action tmpDir
  where
    createTestFile dir (path, content) = do
      let fullPath = dir </> path
      writeFile fullPath content
    
    defaultCAbalFile = unlines
      [ "cabal-version: 3.0"
      , "name: test-project"
      , "version: 0.1.0.0"
      , "build-type: Simple"
      , ""
      , "library"
      , "  default-language: Haskell2010"
      , "  hs-source-dirs: src"
      , "  exposed-modules: Test.Module"
      , "  build-depends: base >= 4.16"
      ]

-- | MCP Server handle for testing GHCID tools
data MCPGHCIDServer = MCPGHCIDServer
  { mcpProcess :: ProcessHandle,
    mcpStdin :: Handle,
    mcpStdout :: Handle,
    mcpProjectPath :: FilePath
  }

instance Show MCPGHCIDServer where
  show (MCPGHCIDServer _ _ _ path) = "MCPGHCIDServer{mcpProjectPath=" ++ show path ++ "}"

-- | Discover MCP GHCID executable dynamically
findMCPGHCIDExecutable :: IO (Maybe FilePath)
findMCPGHCIDExecutable = do
  -- Try environment variable first
  envPath <- lookupEnv "MCP_GHCID_EXECUTABLE"
  case envPath of
    Just path -> return $ Just path
    Nothing -> do
      -- Try to find in PATH
      pathResult <- findExecutable "mcp-ghcid"
      case pathResult of
        Just path -> return $ Just path
        Nothing -> do
          -- Try common build locations
          currentDir <- getCurrentDirectory
          let commonPaths =
                [ currentDir </> "dist-newstyle/build/aarch64-osx/ghc-9.8.4/mcp-ghcid-0.1.0.0/x/mcp-ghcid/build/mcp-ghcid/mcp-ghcid"
                , currentDir </> "dist-newstyle/build/x86_64-linux/ghc-9.8.4/mcp-ghcid-0.1.0.0/x/mcp-ghcid/build/mcp-ghcid/mcp-ghcid"
                , currentDir </> "dist-newstyle/build/x86_64-osx/ghc-9.8.4/mcp-ghcid-0.1.0.0/x/mcp-ghcid/build/mcp-ghcid/mcp-ghcid"
                ]
          findFirstExisting commonPaths
  where
    findFirstExisting [] = return Nothing
    findFirstExisting (path:paths) = do
      exists <- findExecutable path
      case exists of
        Just _ -> return $ Just path
        Nothing -> findFirstExisting paths

-- | Start MCP GHCID server for testing
withMCPGHCIDServer :: FilePath -> (MCPGHCIDServer -> IO a) -> IO (Either Text a)
withMCPGHCIDServer projectPath action = do
  result <- try $ bracket
    (startMCPGHCIDServer projectPath)
    stopMCPGHCIDServer
    action
  case result of
    Left (ex :: SomeException) -> return $ Left $ T.pack $ show ex
    Right res -> return $ Right res

startMCPGHCIDServer :: FilePath -> IO MCPGHCIDServer
startMCPGHCIDServer projectPath = do
  mcpExecutable <- findMCPGHCIDExecutable
  case mcpExecutable of
    Nothing -> error "MCP GHCID executable not found. Set MCP_GHCID_EXECUTABLE or ensure mcp-ghcid is in PATH"
    Just execPath -> do
      (Just stdin_h, Just stdout_h, Nothing, proc_h) <-
        createProcess (proc execPath [])
          { cwd = Just projectPath,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = NoStream
          }

      -- Set buffering
      hSetBuffering stdin_h NoBuffering
      hSetBuffering stdout_h LineBuffering

      -- Give MCP server time to start
      threadDelay 1000000 -- 1 second
      return $ MCPGHCIDServer proc_h stdin_h stdout_h projectPath

stopMCPGHCIDServer :: MCPGHCIDServer -> IO ()
stopMCPGHCIDServer (MCPGHCIDServer _proc stdin_h stdout_h _) = do
  result <- try @SomeException $ do
    hClose stdin_h
    hClose stdout_h
  case result of
    Left _ -> return () -- Ignore cleanup errors
    Right _ -> return ()

-- | Test an MCP GHCID tool
testMCPGHCIDTool :: MCPGHCIDServer -> Text -> Maybe Value -> Int -> IO (Either Text Value)
testMCPGHCIDTool (MCPGHCIDServer _ stdin_h stdout_h _) toolName maybeArgs timeoutMicros = do
  result <- try $ do
    let request = object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "id" .= (1 :: Int)
          , "params" .= object
              [ "name" .= toolName
              , "arguments" .= fromMaybe (object []) maybeArgs
              ]
          ]

    -- Send request
    let requestStr = L8.unpack $ encode request
    hPutStrLn stdin_h requestStr
    hFlush stdin_h

    -- Read response with timeout
    responseResult <- timeout timeoutMicros $ hGetLine stdout_h
    case responseResult of
      Nothing -> error $ "Timeout waiting for MCP response after " ++ show (timeoutMicros `div` 1000000) ++ " seconds"
      Just responseStr -> do
        case eitherDecode (L8.pack responseStr) of
          Left parseErr -> error $ "Failed to parse MCP response: " ++ parseErr
          Right response -> return response

  case result of
    Left (ex :: SomeException) -> return $ Left $ T.pack $ show ex
    Right val -> return $ Right val

-- | Test filter functionality
testFilterFunction :: Text -> FilterRequest -> IO (Either Text Text)
testFilterFunction input filterReq = applyShellFilter input filterReq

-- | Sample test data
sampleGHCIDOutput :: Text
sampleGHCIDOutput = T.unlines
  [ "All good (1 module)"
  , "src/Test/Module.hs:5:1: warning: [-Wunused-imports]"
  , "    The import of 'Data.List' is redundant"
  , "      except perhaps to import instances from 'Data.List'"
  , "    To import instances alone, use: import Data.List()"
  , "  |"
  , "5 | import Data.List"
  , "  | ^^^^^^^^^^^^^^^^"
  , ""
  , "src/Test/Module.hs:10:1: error:"
  , "    Variable not in scope: unknownFunction"
  , "   |"
  , "10 | result = unknownFunction 42"
  , "   | ^^^^^^"
  , ""
  , "1 module loaded."
  ]

-- | Timeout wrapper for tests
withTestTimeout :: Int -> IO a -> IO a
withTestTimeout seconds action = do
  result <- timeout (seconds * 1000000) action
  case result of
    Nothing -> error $ "Test timed out after " ++ show seconds ++ " seconds"
    Just res -> return res

-- | Test expectation helpers
shouldContainAnyOf :: (Show a, Eq a) => [a] -> [a] -> IO ()
shouldContainAnyOf actual expected = 
  if any (`elem` actual) expected
  then return ()
  else expectationFailure $ 
    "Expected " ++ show actual ++ " to contain at least one of " ++ show expected

shouldNotContain :: (Show a, Eq a) => [a] -> [a] -> IO ()
shouldNotContain actual unwanted =
  if any (`elem` actual) unwanted
  then expectationFailure $
    "Expected " ++ show actual ++ " to not contain any of " ++ show unwanted
  else return ()

-- | Resource leak detection helpers
data ResourceTracker = ResourceTracker
  { trackedProcesses :: MVar [ProcessHandle]
  , trackedFiles :: MVar [Handle]
  , trackedThreads :: MVar [ThreadId]
  }

createResourceTracker :: IO ResourceTracker
createResourceTracker = ResourceTracker
  <$> newMVar []
  <*> newMVar []
  <*> newMVar []

checkResourceLeaks :: ResourceTracker -> IO Bool
checkResourceLeaks ResourceTracker{..} = do
  processes <- readMVar trackedProcesses
  files <- readMVar trackedFiles
  threads <- readMVar trackedThreads
  
  activeProcesses <- filterM (fmap isNothing . getProcessExitCode) processes
  -- Note: Can't easily check if handles/threads are active, so we assume they are
  
  return $ null activeProcesses && null files && null threads

