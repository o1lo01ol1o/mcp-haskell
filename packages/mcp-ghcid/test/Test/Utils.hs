{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Utils where

import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, mask, onException, try)
import Control.Monad (filterM, forM, unless, when)
import Test.Hspec (expectationFailure)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr)
import System.IO.Error (isEOFError, ioeGetErrorType, isResourceVanishedErrorType)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import qualified Control.Exception as E
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getProcessExitCode, proc, terminateProcess, waitForProcess)
import qualified System.Process.Typed as PT
import System.Exit (ExitCode (..))
import System.Timeout (timeout)

-- Production imports from GHCID library
import GHCID.Client (GHCIDClient, GHCIDConfig(..), defaultGHCIDConfig, createGHCIDClient, startGHCID, stopGHCID, getCurrentOutput)
import GHCID.Filter (FilterRequest(..), applyShellFilter)

-- Global process registry for cleanup
type TrackedProcess = (Int, ProcessHandle)

allGHCIDProcesses :: MVar [TrackedProcess]
allGHCIDProcesses = unsafePerformIO $ newMVar []
{-# NOINLINE allGHCIDProcesses #-}

nextProcessId :: MVar Int
nextProcessId = unsafePerformIO $ newMVar 0
{-# NOINLINE nextProcessId #-}

-- | Cleanup all tracked processes
cleanupAllProcesses :: IO ()
cleanupAllProcesses = do
  tracked <- takeMVar allGHCIDProcesses
  mapM_ (cleanupProcess . snd) tracked
  putMVar allGHCIDProcesses []
  threadDelay 500000 -- Wait 500ms for all processes to terminate
  where
    cleanupProcess pHandle = do
      exitCode <- getProcessExitCode pHandle
      when (isNothing exitCode) $ do
        _ <- try @SomeException $ do
          terminateProcess pHandle
          _ <- waitForProcess pHandle
          pure ()
        threadDelay 200000 -- Wait 200ms for each process termination

registerProcessHandle :: ProcessHandle -> IO Int
registerProcessHandle handle = do
  ident <- modifyMVar nextProcessId $ \n -> let n' = n + 1 in pure (n', n')
  modifyMVar_ allGHCIDProcesses $ \handles ->
    pure $ (ident, handle) : handles
  pure ident

unregisterProcessHandle :: Int -> IO ()
unregisterProcessHandle ident =
  modifyMVar_ allGHCIDProcesses $ \handles ->
    pure $ dropHandle handles
  where
    dropHandle [] = []
    dropHandle ((i,h):xs)
      | i == ident = xs
      | otherwise = (i,h) : dropHandle xs

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
        waitForOutput handle 40
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
    (\eitherHandle ->
       case eitherHandle of
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
      createDirectoryIfMissing True (takeDirectory fullPath)
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
    mcpProcessId :: Int,
    mcpStdin :: Handle,
    mcpStdout :: Handle,
    mcpProjectPath :: FilePath
  }

instance Show MCPGHCIDServer where
  show (MCPGHCIDServer _ ident _ _ path) =
    "MCPGHCIDServer{mcpProcessId=" ++ show ident ++ ", mcpProjectPath=" ++ show path ++ "}"

-- | Discover MCP GHCID executable dynamically
findMCPGHCIDExecutable :: IO (Maybe FilePath)
findMCPGHCIDExecutable = do
  envOverride <- lookupEnv "MCP_GHCID_EXECUTABLE"
  case envOverride of
    Just manual | not (null manual) -> do
      exists <- doesFileExist manual
      pure $ if exists then Just manual else Nothing
    _ -> do
      envVars <- getEnvironment
      packageDir <- getCurrentDirectory
      let repoRoot = takeDirectory $ takeDirectory packageDir
          cabalDir = repoRoot </> "dist-newstyle" </> "tmp"
          cabalEnv = ("CABAL_DIR", cabalDir) : filter ((/= "CABAL_DIR") . fst) envVars
          cabalProc args =
            PT.setEnv cabalEnv
              $ PT.setWorkingDir repoRoot
              $ PT.proc "cabal" args

      createDirectoryIfMissing True cabalDir

      buildResult <- try @SomeException $ PT.runProcess (cabalProc ["build", "exe:mcp-ghcid"])
      case buildResult of
        Left _ -> searchFallback repoRoot
        Right _ -> do
          attempt <- try @SomeException $ PT.readProcess (cabalProc ["list-bin", "exe:mcp-ghcid"])
          case attempt of
            Right (ExitSuccess, stdoutBs, _) -> do
              let textOut = T.pack (L8.unpack stdoutBs)
                  candidates = map T.unpack
                    $ reverse
                    $ filter (not . T.null)
                    $ map T.strip (T.lines textOut)
              firstExisting candidates >>= maybe (searchFallback repoRoot) (pure . Just)
            _ -> searchFallback repoRoot
  where
    firstExisting [] = pure Nothing
    firstExisting (path:rest) = do
      exists <- doesFileExist path
      if exists
        then pure (Just path)
        else firstExisting rest

    searchFallback root = do
      distPaths <- findBinaries (root </> "dist-newstyle")
      firstExisting distPaths

    findBinaries dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entries <- listDirectory dir
          fmap concat . forM entries $ \entry -> do
            let full = dir </> entry
            isDir <- doesDirectoryExist full
            if isDir
              then findBinaries full
              else pure [full | takeFileName full == "mcp-ghcid"]

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
startMCPGHCIDServer projectPath = mask $ \restore -> do
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

      ident <- registerProcessHandle proc_h

      -- Set buffering
      hSetBuffering stdin_h NoBuffering
      hSetBuffering stdout_h LineBuffering

      let server = MCPGHCIDServer proc_h ident stdin_h stdout_h projectPath
          startup = do
            threadDelay 1000000 -- 1 second
            pure server

      restore startup `onException` stopMCPGHCIDServer server

stopMCPGHCIDServer :: MCPGHCIDServer -> IO ()
stopMCPGHCIDServer (MCPGHCIDServer proc ident stdin_h stdout_h _) = do
  unregisterProcessHandle ident
  _ <- try @SomeException $ do
    hClose stdin_h
    hClose stdout_h
  _ <- try @SomeException $ do
    exitCode <- getProcessExitCode proc
    case exitCode of
      Nothing -> do
        terminateProcess proc
        _ <- waitForProcess proc
        pure ()
      Just _ -> pure ()
  pure ()

-- | Test an MCP GHCID tool
testMCPGHCIDTool :: MCPGHCIDServer -> Text -> Maybe Value -> Int -> IO (Either Text Value)
testMCPGHCIDTool (MCPGHCIDServer _ _ stdin_h stdout_h _) toolName maybeArgs timeoutMicros = do
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

-- Integration testing helpers for MCP servers
withMCPGhcidServer :: FilePath -> ((Handle, Handle) -> IO a) -> IO a
withMCPGhcidServer execPath action =
  mask $ \restore -> do
    (Just stdinHandle, Just stdoutHandle, _, procHandle) <-
      createProcess (proc execPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }

    ident <- registerProcessHandle procHandle

    let cleanup = do
          let shutdownMsg = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("notifications/shutdown" :: Text)
                ]
              exitMsg = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "method" .= ("notifications/exit" :: Text)
                ]

              sendQuiet value =
                try @SomeException $ do
                  L8.hPutStrLn stdinHandle (encode value)
                  hFlush stdinHandle
                  threadDelay 200000

          _ <- sendQuiet shutdownMsg
          _ <- sendQuiet exitMsg

          _ <- try @SomeException $ hClose stdinHandle
          _ <- try @SomeException $ hClose stdoutHandle

          exitResult <- timeout (5 * 1000000) (waitForProcess procHandle)
          case exitResult of
            Just _ -> pure ()
            Nothing -> do
              _ <- try @SomeException $ terminateProcess procHandle
              _ <- try @SomeException $ waitForProcess procHandle
              pure ()

          unregisterProcessHandle ident

    hSetBuffering stdinHandle LineBuffering
    hSetBuffering stdoutHandle LineBuffering

    restore (action (stdinHandle, stdoutHandle))
      `finally` cleanup

sendRequest :: Handle -> Value -> IO ()
sendRequest h value = do
  L8.hPutStrLn h (encode value)
  hFlush h

readResponse :: Handle -> Int -> IO (Either String Value)
readResponse h micros = loop
  where
    loop = do
      res <- timeout micros readJsonLine
      case res of
        Nothing -> pure $ Left "Timed out waiting for MCP response"
        Just decoded ->
          case decoded of
            Right (Object obj)
              | KM.member "id" obj -> pure decoded
              | Just (String method) <- KM.lookup "method" obj -> do
                  hPutStrLn stderr $ "Ignoring notification from server: " <> T.unpack method
                  loop
            _ -> pure decoded

    readJsonLine = do
      lineResult <- try @E.IOException (hGetLine h)
      case lineResult of
        Left ioEx
          | isEOFError ioEx -> pure $ Left "Connection closed"
          | isResourceVanishedErrorType (ioeGetErrorType ioEx) -> pure $ Left "Connection closed"
          | ioeGetErrorType ioEx == InvalidArgument -> pure $ Left "Connection closed"
          | otherwise -> pure $ Left ("Transport read error: " <> show ioEx)
        Right line -> do
          let trimmed = dropWhile isSpace line
          if not (null trimmed) && head trimmed == '{'
            then pure $ eitherDecode (L8.pack trimmed)
            else do
              hPutStrLn stderr $ "Ignoring non-JSON line from server: " <> line
              readJsonLine

pollForMessage :: (Handle, Handle) -> Text -> Text -> Int -> IO (Either String Text)
pollForMessage (hin, hout) cabalUri needle attempts = loop attempts Nothing
  where
    needleLower = T.toLower needle

    loop 0 lastMsg =
      pure $ Left ("Exceeded polling attempts. Last message: " <> maybe "<none>" T.unpack lastMsg)
    loop n lastMsg = do
      sendRequest hin $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (fromIntegral n :: Int)
        , "method" .= ("tools/call" :: Text)
        , "params" .= object
            [ "name" .= ("ghcid-messages" :: Text)
            , "arguments" .= object
                [ "cabalURI" .= cabalUri
                , "count" .= (80 :: Int)
                ]
            ]
        ]
      resp <- readResponse hout 10000000
      case resp of
        Left err ->
          if "Timed out" `T.isInfixOf` T.pack err
            then do
              threadDelay 500000
              loop (n - 1) lastMsg
            else pure $ Left err
        Right val -> case extractToolText val of
          Nothing -> pure $ Left "Malformed tool response"
          Just txt ->
            case decodeMessagePayload txt of
              Left parseErr -> pure $ Left ("Failed to parse message payload: " <> parseErr)
              Right (outputTxt, linesList) ->
                if any (\line -> needleLower `T.isInfixOf` T.toLower line) linesList
                  then pure $ Right outputTxt
                  else do
                    threadDelay 500000
                    loop (n - 1) (Just outputTxt)

pollForStatus :: (Handle, Handle) -> Text -> Int -> IO (Either String (Text, Maybe Int, Maybe Text, Maybe Text))
pollForStatus (hin, hout) cabalUri attempts = loop attempts Nothing
  where
    loop 0 lastSnapshot =
      pure $ Left ("Exceeded status polling attempts. Last status: " <> maybe "<none>" formatSnapshot lastSnapshot)
    loop n lastSnapshot = do
      sendRequest hin $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= (fromIntegral n :: Int)
        , "method" .= ("tools/call" :: Text)
        , "params" .= object
            [ "name" .= ("ghcid-status" :: Text)
            , "arguments" .= object
                [ "cabalURI" .= cabalUri
                ]
            ]
        ]
      resp <- readResponse hout 10000000
      case resp of
        Left err ->
          if "Timed out" `T.isInfixOf` T.pack err
            then do
              threadDelay 500000
              loop (n - 1) lastSnapshot
            else pure $ Left err
        Right val -> case extractToolText val of
          Nothing -> pure $ Left "Malformed status response"
          Just txt ->
            case decodeStatusPayload txt of
              Left parseErr -> pure $ Left ("Failed to parse status payload: " <> parseErr)
              Right snapshot@(stateTxt, _, errMsg, _) ->
                case T.toLower stateTxt of
                  "running" -> pure $ Right snapshot
                  "error" -> pure $ Left ("GHCID watch errored: " <> maybe (T.unpack stateTxt) T.unpack errMsg)
                  _ -> do
                    threadDelay 1000000
                    loop (n - 1) (Just snapshot)

    formatSnapshot (stateTxt, moduleCount, errMsg, latestMsg) =
      T.unpack stateTxt
        <> maybe "" (\mc -> " (modules: " <> show mc <> ")") moduleCount
        <> maybe "" (\err -> " (error: " <> T.unpack err <> ")") errMsg
        <> maybe "" (\msg -> " (latest: " <> T.unpack msg <> ")") latestMsg

decodeStatusPayload :: Text -> Either String (Text, Maybe Int, Maybe Text, Maybe Text)
decodeStatusPayload txt =
  case eitherDecode (L8.pack (T.unpack txt)) of
    Left err -> Left err
    Right val ->
      parseEither
        (withObject "statusPayload" $ \o -> do
          latestMsg <- o .:? "processLatestMessage"
          statusVal <- o .:? "processStatus"
          case statusVal of
            Nothing -> pure ("stopped", Nothing, Nothing, latestMsg)
            Just Null -> pure ("stopped", Nothing, Nothing, latestMsg)
            Just (Object statusObj) -> do
              statusTxt <- statusObj .: "status"
              moduleCount <- statusObj .:? "moduleCount"
              errTxt <- statusObj .:? "error"
              pure (statusTxt, moduleCount, errTxt, latestMsg)
            _ -> pure ("unknown", Nothing, Nothing, latestMsg)
        )
        val

decodeMessagePayload :: Text -> Either String (Text, [Text])
decodeMessagePayload txt =
  case eitherDecode (L8.pack (T.unpack txt)) of
    Left err -> Left err
    Right val ->
      parseEither
        (withObject "messagesPayload" $ \o -> do
          outputTxt <- o .:? "messagesOutput" .!= ""
          linesList <- o .:? "messagesLines" .!= []
          pure (outputTxt, linesList)
        )
        val

validateToolResponse :: Text -> Value -> IO ()
validateToolResponse name value = do
  let isValid = parseMaybe (withObject "resp" $ \o -> do
        resultVal <- o .:? "result"
        case resultVal of
          Just (Object resObj) -> do
            contentVal <- resObj .:? "content"
            errFlag <- resObj .:? "isError"
            case errFlag of
              Just True -> fail "Tool response reported error"
              _ -> case contentVal of
                Just (Array arr) | not (null arr) -> pure ()
                _ -> fail "Tool response missing content"
          _ -> fail "Missing result object"
        ) value
  case isValid of
    Nothing -> expectationFailure $ "Malformed response for " <> T.unpack name <> ": " <> show value
    Just _ -> pure ()

extractToolText :: Value -> Maybe Text
extractToolText = parseMaybe $ withObject "response" $ \o -> do
  resultVal <- o .:? "result"
  case resultVal of
    Nothing -> fail "Missing result"
    Just resObj -> withObject "result" (\r -> do
      content <- r .: "content"
      case content of
        Array arr -> case V.toList arr of
          (Object obj : _) -> obj .: "text"
          _ -> fail "Unexpected content structure"
        _ -> fail "Unexpected content type"
      ) resObj

assertAllGood :: Text -> Bool
assertAllGood msg = "all good" `T.isInfixOf` T.toLower msg
waitForOutput :: GHCIDHandle -> Int -> IO ()
waitForOutput handle attempts
  | attempts <= 0 = pure ()
  | otherwise = do
      output <- getCurrentOutput (ghcidClient handle)
      if any (not . isSpace) (T.unpack output)
        then pure ()
        else do
          threadDelay 200000
          waitForOutput handle (attempts - 1)
