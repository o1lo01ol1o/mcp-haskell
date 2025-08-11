{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Utils where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hPutStrLn, hSetBuffering)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), createProcess, getProcessExitCode, proc, terminateProcess)
import System.Process.Typed (Process, unsafeProcessHandle)
import System.Timeout (timeout)
import Text.Regex.TDFA ((=~))

-- Production imports from main library
import qualified HLS.Client as HLS
import qualified MCP.Server as MCP

-- Global process registry for cleanup
allHLSProcesses :: MVar [ProcessHandle]
allHLSProcesses = unsafePerformIO $ newMVar []
{-# NOINLINE allHLSProcesses #-}

-- | Cleanup all tracked processes
cleanupAllProcesses :: IO ()
cleanupAllProcesses = do
  pids <- takeMVar allHLSProcesses
  mapM_ cleanupProcess pids
  putMVar allHLSProcesses []
  threadDelay 500000 -- Wait 500ms for all processes to terminate
  where
    cleanupProcess pHandle = do
      exitCode <- getProcessExitCode pHandle
      when (isNothing exitCode) $ do
        terminateProcess pHandle
        threadDelay 200000 -- Wait 200ms for each process termination

-- | HLS handle that uses production code
data HLSHandle = HLSHandle
  { hlsClient :: HLS.LSPClient,
    hlsWorkDir :: FilePath
  }

-- | Start HLS process using production code
startHLS :: FilePath -> IO HLSHandle
startHLS workDir = do
  -- Give HLS more time to start up
  result <- timeout 20000000 $ HLS.initializeLSPClient workDir -- 20 second timeout for initialization
  case result of
    Nothing -> error "HLS initialization timed out after 20 seconds"
    Just (Left err) -> error $ "Failed to initialize LSP client: " ++ T.unpack err
    Just (Right client) -> do
      let handle = HLSHandle client workDir
      -- Track the process handle for cleanup
      let pHandle = unsafeProcessHandle (HLS.processHandle client)
      modifyMVar_ allHLSProcesses (\pids -> return (pHandle : pids))
      -- Give HLS a moment to fully initialize
      threadDelay 2000000 -- 2 second delay after initialization
      return handle

-- | Stop HLS process using production cleanup
stopHLS :: HLSHandle -> IO ()
stopHLS handle = HLS.closeLSPClient (hlsClient handle)

-- | Extract process handle for compatibility
hlsProcess :: HLSHandle -> ProcessHandle
hlsProcess handle = unsafeProcessHandle (HLS.processHandle (hlsClient handle))

-- | Test helper to run HLS operation with proper setup/teardown
withHLS :: FilePath -> (HLSHandle -> IO a) -> IO a
withHLS workDir action = do
  result <- timeout 30000000 $ -- 30 second total timeout (more generous)
    bracket
      (startHLS workDir)
      stopHLS
      action
  case result of
    Nothing -> error "HLS operation timed out after 30 seconds"
    Just res -> return res

-- | Helper to create a temporary test workspace
withTestWorkspace :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withTestWorkspace files action = withSystemTempDirectory "hls-test" $ \tmpDir -> do
  -- Create test files
  mapM_ (createTestFile tmpDir) files
  action tmpDir
  where
    createTestFile dir (path, content) = do
      let fullPath = dir </> path
      writeFile fullPath content

-- Production LSP Client Helper Functions

-- | Send hover request using production client
sendHoverRequest :: HLSHandle -> Text -> Int -> Int -> IO (Either String Value)
sendHoverRequest handle uri line char = do
  result <- HLS.getHover (hlsClient handle) uri line char
  case result of
    Left err -> return $ Left $ T.unpack err
    Right content ->
      return $ Right $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "result" .= object
            [ "contents" .= object
                [ "kind" .= ("markdown" :: Text)
                , "value" .= content
                ]
            ]
        ]

-- | Send document symbols request using production client
sendDocumentSymbolsRequest :: HLSHandle -> Text -> IO (Either String Value)
sendDocumentSymbolsRequest handle uri = do
  result <- HLS.getDocumentSymbols (hlsClient handle) uri
  case result of
    Left err -> return $ Left $ T.unpack err
    Right symbols ->
      return $ Right $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "result" .= symbols
        ]

-- | Send workspace symbols request using production client
sendWorkspaceSymbolsRequest :: HLSHandle -> Text -> IO (Either String Value)
sendWorkspaceSymbolsRequest handle query = do
  result <- HLS.getWorkspaceSymbols (hlsClient handle) query
  case result of
    Left err -> return $ Left $ T.unpack err
    Right symbols ->
      return $ Right $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "result" .= symbols
        ]

-- | Send code actions request using production client
sendCodeActionsRequest :: HLSHandle -> Text -> Int -> Int -> Int -> Int -> IO (Either String Value)
sendCodeActionsRequest handle uri startLine startChar endLine endChar = do
  result <- HLS.getCodeActions (hlsClient handle) uri startLine startChar endLine endChar
  case result of
    Left err -> return $ Left $ T.unpack err
    Right actions ->
      return $ Right $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "result" .= actions
        ]

-- | Send execute command request using production client
sendExecuteCommandRequest :: HLSHandle -> Text -> [Value] -> IO (Either String Value)
sendExecuteCommandRequest handle command args = do
  result <- HLS.executeCommand (hlsClient handle) command args
  case result of
    Left err -> return $ Left $ T.unpack err
    Right response ->
      return $ Right $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (1 :: Int)
        , "result" .= response
        ]

-- | Get file diagnostics using production client
getFileDiagnosticsFromLSP :: HLSHandle -> Text -> Text -> IO (Either String [Value])
getFileDiagnosticsFromLSP handle uri content = do
  result <- HLS.getFileDiagnostics (hlsClient handle) uri content
  case result of
    Left err -> return $ Left $ T.unpack err
    Right diagnostics -> return $ Right diagnostics

-- | Send didOpen notification using production client
sendDidOpenNotificationToLSP :: HLSHandle -> Text -> Text -> IO (Either String ())
sendDidOpenNotificationToLSP handle uri content = do
  result <- HLS.sendDidOpenNotification (hlsClient handle) uri content
  case result of
    Left err -> return $ Left $ T.unpack err
    Right _ -> return $ Right ()

-- MCP Demo Server Utilities with Production Code

-- | MCP server handle for testing
data MCPDemoServer = MCPDemoServer
  { mcpProcess :: ProcessHandle,
    mcpStdin :: Handle,
    mcpStdout :: Handle,
    mcpProjectPath :: FilePath
  }

instance Show MCPDemoServer where
  show (MCPDemoServer _ _ _ path) = "MCPDemoServer{mcpProjectPath=" ++ show path ++ "}"

-- | Start MCP server with demo project using production server
withMCPDemoServer :: FilePath -> (MCPDemoServer -> IO a) -> IO (Either Text a)
withMCPDemoServer demoProjectPath action = do
  result <- try $ bracket
    (startMCPDemoServer demoProjectPath)
    stopMCPDemoServer
    action
  case result of
    Left (ex :: SomeException) -> return $ Left $ T.pack $ show ex
    Right res -> return $ Right res

-- | Discover MCP executable dynamically
findMCPExecutable :: IO (Maybe FilePath)
findMCPExecutable = do
  -- Try environment variable first
  envPath <- lookupEnv "MCP_HLS_EXECUTABLE"
  case envPath of
    Just path -> return $ Just path
    Nothing -> do
      -- Try to find in PATH
      pathResult <- findExecutable "mcp-hls"
      case pathResult of
        Just path -> return $ Just path
        Nothing -> do
          -- Try common build locations
          currentDir <- getCurrentDirectory
          let commonPaths =
                [ currentDir </> "dist-newstyle/build/aarch64-osx/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
                , currentDir </> "dist-newstyle/build/x86_64-linux/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
                , currentDir </> "dist-newstyle/build/x86_64-osx/ghc-9.8.4/mcp-hls-0.1.0.0/x/mcp-hls/build/mcp-hls/mcp-hls"
                ]
          findFirstExisting commonPaths
  where
    findFirstExisting [] = return Nothing
    findFirstExisting (path:paths) = do
      exists <- findExecutable path
      case exists of
        Just _ -> return $ Just path
        Nothing -> findFirstExisting paths

-- | Start the MCP server process with demo project using production code
startMCPDemoServer :: FilePath -> IO MCPDemoServer
startMCPDemoServer projectPath = do
  mcpExecutable <- findMCPExecutable
  case mcpExecutable of
    Nothing -> error "MCP executable not found. Set MCP_HLS_EXECUTABLE or ensure mcp-hls is in PATH"
    Just execPath -> do
      (Just stdin_h, Just stdout_h, Nothing, proc_h) <-
        createProcess (proc execPath [])
          { cwd = Just projectPath,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = NoStream -- Suppress stderr noise
          }

      -- Set buffering
      hSetBuffering stdin_h NoBuffering
      hSetBuffering stdout_h LineBuffering

      -- Check if process started successfully
      procStatus <- getProcessExitCode proc_h
      case procStatus of
        Just exitCode -> do
          error $ "MCP server failed to start with exit code: " ++ show exitCode
        Nothing -> return ()

      -- Give MCP server time to start
      threadDelay 1000000 -- 1 second
      return $ MCPDemoServer proc_h stdin_h stdout_h projectPath

-- | Stop the MCP server
stopMCPDemoServer :: MCPDemoServer -> IO ()
stopMCPDemoServer (MCPDemoServer _proc stdin_h stdout_h _) = do
  -- Graceful cleanup
  result <- try @SomeException $ do
    hClose stdin_h
    hClose stdout_h
  case result of
    Left _ -> return () -- Ignore cleanup errors
    Right _ -> return ()

-- | Test an MCP tool with configurable timeout
testMCPTool :: MCPDemoServer -> Text -> Maybe Value -> Int -> IO (Either Text Value)
testMCPTool (MCPDemoServer _ stdin_h stdout_h _) toolName maybeArgs timeoutMicros = do
  result <- try $ do
    -- Construct MCP tool call request
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

    -- Read response with configurable timeout
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

-- | Test expectation for MCP tools with configurable timeout
data MCPToolExpectation = MCPToolExpectation
  { mcpToolName :: Text,
    mcpToolArgs :: Maybe Value,
    mcpExpectedResult :: ExpectationType,
    mcpTimeout :: Int -- Timeout in microseconds
  }
  deriving (Show)

-- | Systematic MCP tool testing with expectations
testMCPToolWithExpectation :: MCPDemoServer -> MCPToolExpectation -> IO Bool
testMCPToolWithExpectation server expectation = do
  result <- testMCPTool server (mcpToolName expectation) (mcpToolArgs expectation) (mcpTimeout expectation)
  case result of
    Left errorMsg ->
      case mcpExpectedResult expectation of
        ShouldFail -> return True
        ShouldHaveErrorType expectedError -> return $ expectedError `T.isInfixOf` errorMsg
        _ -> return False
    Right response ->
      let responseText = T.pack $ show response
      in return $ verifyMCPExpectation (mcpExpectedResult expectation) responseText

-- | Verify MCP tool expectation
verifyMCPExpectation :: ExpectationType -> Text -> Bool
verifyMCPExpectation ShouldSucceed responseText =
  not $ "error" `T.isInfixOf` T.toLower responseText
verifyMCPExpectation ShouldFail responseText =
  "error" `T.isInfixOf` T.toLower responseText
verifyMCPExpectation (ShouldContainText expected) responseText =
  expected `T.isInfixOf` responseText
verifyMCPExpectation (ShouldMatchPattern pattern) responseText =
  responseText =~ T.unpack pattern
verifyMCPExpectation (ShouldHaveErrorType expectedError) responseText =
  expectedError `T.isInfixOf` responseText
verifyMCPExpectation _ _ = True -- Other expectations not applicable to MCP

-- | Run multiple MCP tool tests in sequence
runMCPToolTests :: MCPDemoServer -> [MCPToolExpectation] -> IO [(Text, Bool)]
runMCPToolTests server expectations = do
  results <- mapM (testMCPToolWithExpectation server) expectations
  return $ zip (map mcpToolName expectations) results

-- | MCP tool test result summary
data MCPTestSummary = MCPTestSummary
  { totalTests :: Int,
    passedTests :: Int,
    failedTests :: Int,
    testResults :: [(Text, Bool)]
  }
  deriving (Show)

-- | Generate test summary
generateMCPTestSummary :: [(Text, Bool)] -> MCPTestSummary
generateMCPTestSummary results =
  let total = length results
      passed = length $ filter snd results
      failed = total - passed
  in MCPTestSummary total passed failed results

-- | Data type for expectation verification
data ExpectationType
  = ShouldSucceed
  | ShouldFail
  | ShouldContainText Text
  | ShouldMatchPattern Text
  | ShouldHaveErrorType Text
  | ShouldHaveWarningCount Int
  | ShouldHaveCompletionCount Int
  | ShouldHaveTypeSignature Text
  | ShouldHaveImport Text
  deriving (Show, Eq)
