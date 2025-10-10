{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Client
  ( LSPClient,
    ProcessStatus (..),
    processHandle,
    processStatus,
    initializeLSPClient,
    closeLSPClient,
    getHover,
    formatDocument,
    gotoDefinition,
    findReferences,
    getDocumentSymbols,
    getCompletions,
    getWorkspaceSymbols,
    getCodeLenses,
    getCodeActions,
    executeCommand,
    getFileDiagnostics,
    sendDidOpenNotification,
    organizeImportsCommand,
    insertImportCommand,
    removeUnusedImportsCommand,
    cleanupLSPClient,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever, replicateM)
import Data.Aeson (FromJSON (parseJSON), Result (..), Value (..), eitherDecode, encode, fromJSON, object, (.=), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (toList, traverse_)
import Data.Function ((&))
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Process.Signals (SignalInfo (..), setupSignalHandlers)
-- import Language.LSP.Protocol.Types (LspId, ResponseError(..))
import System.IO
import System.Process.Typed
import System.Timeout (timeout)
import Utils.Logging

-- Temporary LSP types definitions
type LspId = Int

data ResponseError = ResponseError
  { errorCode :: Int,
    errorMessage :: Text,
    errorData :: Maybe Value
  }
  deriving (Show, Eq)

instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" $ \o ->
    ResponseError
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

-- Process Status
data ProcessStatus
  = Healthy
  | Unhealthy Text
  | Dead ExitCode
  | Terminated SignalInfo -- NEW: Explicit signal termination
  deriving (Show, Eq)

-- LSP Client State
data LSPClient = LSPClient
  { processHandle :: Process Handle Handle Handle,
    requestId :: TVar Int,
    pendingRequests :: TVar [(LspId, TMVar (Either ResponseError Value))],
    diagnosticsStore :: TVar [(Text, [Value])], -- URI -> diagnostics
    processStatus :: TVar ProcessStatus,
    backgroundReaderAsync :: TVar (Maybe (Async ())),
    processWatchdogAsync :: TVar (Maybe (Async ())),
    signalInfo :: TVar (Maybe SignalInfo) -- NEW: Signal information tracking
  }

instance Show LSPClient where
  show _ = "LSPClient{...}"

-- Initialize LSP Client
initializeLSPClient :: FilePath -> IO (Either Text LSPClient)
initializeLSPClient workDir = do
  result <- try $ do
    -- Start HLS process
    let processConfig =
          proc "haskell-language-server-wrapper" ["--lsp"]
            & setWorkingDir workDir
            & setStdin createPipe
            & setStdout createPipe
            & setStderr createPipe

    process <- System.Process.Typed.startProcess processConfig

    -- Initialize client state
    reqId <- newTVarIO 0
    pending <- newTVarIO []
    diagnostics <- newTVarIO []
    status <- newTVarIO Healthy
    readerAsync <- newTVarIO Nothing
    watchdogAsync <- newTVarIO Nothing
    signalVar <- newTVarIO Nothing -- NEW: Signal tracking
    let client = LSPClient process reqId pending diagnostics status readerAsync watchdogAsync signalVar

    -- NEW: Setup signal handlers for HLS process
    setupSignalHandlers signalVar

    -- Start response handler
    readerAsyncHandle <- async $ responseHandler client (getStdout process)
    atomically $ writeTVar (backgroundReaderAsync client) (Just readerAsyncHandle)

    -- Start process watchdog
    watchdogAsyncHandle <- async $ processWatchdog client
    atomically $ writeTVar (processWatchdogAsync client) (Just watchdogAsyncHandle)

    -- Give HLS time to start up before sending initialize
    threadDelay 2000000 -- 2 seconds for HLS process to start

    -- Send initialize request
    initResult <- timeout 15000000 $ sendInitializeRequest client workDir -- 15 second timeout
    case initResult of
      Nothing -> return $ Left "Timeout during HLS initialization"
      Just (Left err) -> return $ Left err
      Just (Right _) -> do
        -- Send initialized notification
        notifyResult <- sendNotification client "initialized" (object [])
        case notifyResult of
          Left err -> return $ Left $ "Failed to send initialized notification: " <> err
          Right _ -> do
            -- Wait a bit more for HLS to be fully ready
            threadDelay 1000000 -- 1 second
            return $ Right client

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to initialize LSP client: " <> T.pack (show ex)
    Right res -> return res

-- Send Initialize Request
sendInitializeRequest :: LSPClient -> FilePath -> IO (Either Text Value)
sendInitializeRequest client workDir = do
  let initParams =
        object
          [ "processId" .= (Nothing :: Maybe Int),
            "rootUri" .= ("file://" <> T.pack workDir),
            "workspaceFolders"
              .= ( [ object
                       [ "uri" .= ("file://" <> T.pack workDir),
                         "name" .= T.takeWhileEnd (/= '/') (T.pack workDir)
                       ]
                   ] ::
                     [Value]
                 ),
            "capabilities"
              .= object
                [ "workspace"
                    .= object
                      [ "workspaceEdit" .= object ["documentChanges" .= True],
                        "symbol" .= object ["symbolKind" .= object []]
                      ],
                  "textDocument"
                    .= object
                      [ "hover" .= object ["contentFormat" .= ["markdown" :: Text, "plaintext"]],
                        "completion"
                          .= object
                            [ "completionItem"
                                .= object
                                  [ "snippetSupport" .= True,
                                    "documentationFormat" .= ["markdown" :: Text]
                                  ]
                            ],
                        "formatting" .= object [],
                        "diagnostics" .= object []
                      ]
                ]
          ]
  sendRequest client "initialize" initParams

-- Send LSP Request
sendRequest :: LSPClient -> Text -> Value -> IO (Either Text Value)
sendRequest LSPClient {..} method params = do
  -- Check process health first
  status <- readTVarIO processStatus
  case status of
    Dead exitCode -> return $ Left $ "HLS process is dead (exit code: " <> T.pack (show exitCode) <> ")"
    Unhealthy err -> return $ Left $ "HLS process is unhealthy: " <> err
    Terminated sigInfo -> return $ Left $ "HLS process terminated by signal: " <> signalName sigInfo
    Healthy -> do
      -- Get next request ID
      reqIdNum <- atomically $ do
        current <- readTVar requestId
        writeTVar requestId (current + 1)
        return current

      let reqId = reqIdNum

      -- Create response variable
      responseVar <- newEmptyTMVarIO

      -- Add to pending requests
      atomically $ modifyTVar pendingRequests ((reqId, responseVar) :)

      -- Send request
      let request =
            object
              [ "jsonrpc" .= ("2.0" :: Text),
                "method" .= method,
                "params" .= params,
                "id" .= reqIdNum
              ]

      result <- try $ do
        let requestText = T.decodeUtf8 $ toStrict $ encode request
        let message = "Content-Length: " <> T.pack (show (T.length requestText)) <> "\r\n\r\n" <> requestText
        T.hPutStr (getStdin processHandle) message
        hFlush (getStdin processHandle)

      case result of
        Left (ex :: SomeException) -> do
          -- Remove from pending requests
          atomically $ modifyTVar pendingRequests (filter ((/= reqId) . fst))
          return $ Left $ "Failed to send request: " <> T.pack (show ex)
        Right _ -> do
          -- Wait for response with timeout
          responseResult <- timeout 10000000 $ atomically $ takeTMVar responseVar -- 10 second timeout
          case responseResult of
            Nothing -> do
              -- Remove from pending requests on timeout
              atomically $ modifyTVar pendingRequests (filter ((/= reqId) . fst))
              return $ Left "Timeout waiting for LSP response"
            Just response -> case response of
              Left err -> return $ Left $ "LSP Error: " <> T.pack (show err)
              Right val -> return $ Right val

-- Send LSP Notification
sendNotification :: LSPClient -> Text -> Value -> IO (Either Text ())
sendNotification LSPClient {..} method params = do
  let notification =
        object
          [ "jsonrpc" .= ("2.0" :: Text),
            "method" .= method,
            "params" .= params
          ]

  result <- try $ do
    let notificationText = T.decodeUtf8 $ toStrict $ encode notification
    let message = "Content-Length: " <> T.pack (show (T.length notificationText)) <> "\r\n\r\n" <> notificationText
    T.hPutStr (getStdin processHandle) message
    hFlush (getStdin processHandle)

  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to send notification: " <> T.pack (show ex)
    Right _ -> return $ Right ()

-- Response Handler
responseHandler :: LSPClient -> Handle -> IO ()
responseHandler client@LSPClient {..} handle = do
  result <- try @SomeException $ forever $ do
    -- Check if process is still alive
    status <- readTVarIO processStatus
    case status of
      Dead _ -> throwIO (userError "Process died")
      Unhealthy _ -> throwIO (userError "Process unhealthy")
      Terminated _ -> throwIO (userError "Process terminated by signal")
      Healthy -> do
        -- Read message with timeout
        messageResult <- timeout 5000000 $ do
          -- 5 second timeout
          contentLengthLine <- T.hGetLine handle
          let contentLength = read $ T.unpack $ T.drop 16 contentLengthLine
          _ <- T.hGetLine handle -- empty line
          message <- replicateM contentLength (hGetChar handle)
          return $ T.pack message

        case messageResult of
          Nothing -> do
            -- Don't log timeout messages, just silently continue
            return ()
          Just messageText -> do
            case eitherDecode (fromStrict $ T.encodeUtf8 messageText) of
              Left _ -> return () -- Silently ignore parse errors
              Right response -> handleResponse client response

  case result of
    Left ex -> do
      -- Silently handle background reader errors
      atomically $ writeTVar processStatus (Unhealthy $ T.pack $ show ex)
    Right _ -> pure ()

-- Handle Response
handleResponse :: LSPClient -> Value -> IO ()
handleResponse LSPClient {..} response = do
  case response of
    Object obj -> do
      case KM.lookup "id" obj of
        Just (Number n) -> do
          let reqId = round n
          -- Find pending request
          maybePending <- atomically $ do
            pending <- readTVar pendingRequests
            case lookup reqId pending of
              Nothing -> return Nothing
              Just responseVar -> do
                writeTVar pendingRequests (filter ((/= reqId) . fst) pending)
                return $ Just responseVar

          case maybePending of
            Nothing -> logWarn $ "Received response for unknown request ID: " <> T.pack (show n)
            Just responseVar -> do
              case KM.lookup "error" obj of
                Just errorVal ->
                  case fromJSON errorVal of
                    Success err -> atomically $ putTMVar responseVar (Left err)
                    Aeson.Error _ -> atomically $ putTMVar responseVar (Left $ ResponseError (-1) "Parse error" Nothing)
                Nothing ->
                  case KM.lookup "result" obj of
                    Just result -> atomically $ putTMVar responseVar (Right result)
                    Nothing -> atomically $ putTMVar responseVar (Left $ ResponseError (-1) "No result" Nothing)
        Nothing -> do
          -- Handle notifications (no id field)
          case KM.lookup "method" obj of
            Just (String "textDocument/publishDiagnostics") -> do
              case KM.lookup "params" obj of
                Just (Object params) -> do
                  case (KM.lookup "uri" params, KM.lookup "diagnostics" params) of
                    (Just (String uri), Just (Array diagnostics)) -> do
                      -- Store diagnostics for this URI
                      atomically $ modifyTVar diagnosticsStore $ \store ->
                        (uri, toList diagnostics) : filter ((/= uri) . fst) store
                    _ -> logWarn "Invalid diagnostics notification format"
                _ -> logWarn "Missing params in diagnostics notification"
            Just (String method) -> logWarn $ "Unhandled notification method: " <> method
            Just method -> logWarn $ "Unhandled notification method: " <> T.pack (show method)
            _ -> logWarn "Received notification without method"
        _ -> return () -- Malformed response
    _ -> logError "Received non-object response"

-- Process Watchdog
processWatchdog :: LSPClient -> IO ()
processWatchdog client@LSPClient {..} = do
  result <- try @SomeException $ forever $ do
    threadDelay 2000000 -- Check every 2 seconds

    -- Check both exit codes AND signal status
    exitResult <- try @SomeException $ checkExitCode processHandle
    maybeSignal <- readTVarIO signalInfo

    case (exitResult, maybeSignal) of
      -- Signal was received - handle signal termination
      (_, Just sigInfo) -> do
        logError $ "HLS process terminated by signal: " <> T.pack (show sigInfo)
        atomically $ writeTVar processStatus (Terminated sigInfo)
        handleSignalTermination client sigInfo
        throwIO (userError "Process terminated by signal")

      -- Process exited without signal
      (Left ex, Nothing) -> do
        -- Process has exited, try to get the actual exit code
        exitCodeResult <- try @SomeException $ waitExitCode processHandle
        case exitCodeResult of
          Left _ -> do
            logError $ "Process died: " <> T.pack (show ex)
            atomically $ writeTVar processStatus (Dead (ExitFailure (-1)))
          Right exitCode -> do
            logError $ "HLS process died with exit code: " <> T.pack (show exitCode)
            atomically $ writeTVar processStatus (Dead exitCode)
        -- Cancel background reader
        maybeReader <- readTVarIO backgroundReaderAsync
        case maybeReader of
          Just readerAsync -> cancel readerAsync
          Nothing -> return ()
        throwIO (userError "Process died")

      -- Process still running, no signals
      (Right _, Nothing) -> return () -- Continue monitoring
  case result of
    Left ex -> logError $ "Watchdog error: " <> T.pack (show ex)
    Right _ -> pure ()

-- Handle Signal Termination
handleSignalTermination :: LSPClient -> SignalInfo -> IO ()
handleSignalTermination client sigInfo = do
  logWarn $ "Handling signal termination: " <> signalName sigInfo
  cleanupForSignal client sigInfo

-- Signal-specific cleanup procedures
cleanupForSignal :: LSPClient -> SignalInfo -> IO ()
cleanupForSignal LSPClient {..} sigInfo = do
  let sigName = signalName sigInfo

  logInfo $ "Starting cleanup for signal: " <> sigName

  case sigName of
    "SIGTERM" -> do
      -- Graceful termination - allow some time for cleanup
      logInfo "SIGTERM: Performing graceful shutdown"
      gracefulCleanup
    "SIGINT" -> do
      -- Interrupt - immediate but clean shutdown
      logInfo "SIGINT: Performing interrupt cleanup"
      gracefulCleanup
    "SIGHUP" -> do
      -- Terminal disconnect - cleanup communication
      logInfo "SIGHUP: Cleaning up after terminal disconnect"
      communicationCleanup
    "SIGPIPE" -> do
      -- Broken pipe - LSP communication failed
      logError "SIGPIPE: LSP communication pipe broken"
      communicationCleanup
    "SIGABRT" -> do
      -- Process abort - emergency cleanup
      logError "SIGABRT: Emergency cleanup after process abort"
      emergencyCleanup
    "SIGQUIT" -> do
      -- Quit signal - clean shutdown with core dump
      logInfo "SIGQUIT: Performing quit cleanup"
      gracefulCleanup
    "SIGUSR1" -> do
      -- User signal 1 - custom handling
      logInfo "SIGUSR1: Custom signal handling"
      gracefulCleanup
    "SIGUSR2" -> do
      -- User signal 2 - custom handling
      logInfo "SIGUSR2: Custom signal handling"
      gracefulCleanup
    _ -> do
      logWarn $ "Unknown signal: " <> sigName <> ", performing default cleanup"
      gracefulCleanup

  logInfo $ "Cleanup completed for signal: " <> sigName
  where
    gracefulCleanup = do
      -- Cancel background threads gracefully
      maybeReader <- readTVarIO backgroundReaderAsync
      maybeWatchdog <- readTVarIO processWatchdogAsync
      traverse_ cancel maybeReader
      traverse_ cancel maybeWatchdog

      -- Close handles safely
      _ <- try @SomeException $ hClose (getStdin processHandle)
      _ <- try @SomeException $ hClose (getStdout processHandle)
      _ <- try @SomeException $ hClose (getStderr processHandle)

      -- Wait briefly for process cleanup
      threadDelay 1000000 -- 1 second
    communicationCleanup = do
      -- Focus on cleaning up communication channels
      _ <- try @SomeException $ hClose (getStdin processHandle)
      _ <- try @SomeException $ hClose (getStdout processHandle)
      gracefulCleanup

    emergencyCleanup = do
      -- Immediate cleanup without delays
      maybeReader <- readTVarIO backgroundReaderAsync
      maybeWatchdog <- readTVarIO processWatchdogAsync
      traverse_ cancel maybeReader
      traverse_ cancel maybeWatchdog

      -- Force close handles
      _ <- try @SomeException $ hClose (getStdin processHandle)
      _ <- try @SomeException $ hClose (getStdout processHandle)
      _ <- try @SomeException $ hClose (getStderr processHandle)
      return ()

-- Cleanup LSP Client Resources
cleanupLSPClient :: LSPClient -> IO ()
cleanupLSPClient LSPClient {..} = do
  -- Cancel background threads
  maybeReader <- readTVarIO backgroundReaderAsync
  maybeWatchdogHandle <- readTVarIO processWatchdogAsync

  traverse_ cancel maybeReader
  traverse_ cancel maybeWatchdogHandle

  -- Close handles
  _ <- try @SomeException $ hClose (getStdin processHandle)
  _ <- try @SomeException $ hClose (getStdout processHandle)
  _ <- try @SomeException $ hClose (getStderr processHandle)

  -- Stop process
  _ <- try @SomeException $ stopProcess processHandle

  -- Clear state
  atomically $ do
    writeTVar processStatus (Dead (ExitFailure (-1)))
    writeTVar backgroundReaderAsync Nothing
    writeTVar processWatchdogAsync Nothing

-- Close LSP Client
closeLSPClient :: LSPClient -> IO ()
closeLSPClient client = cleanupLSPClient client

-- Format Document Request
formatDocument :: LSPClient -> Text -> Text -> IO (Either Text Text)
formatDocument client uri content = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "options"
              .= object
                [ "tabSize" .= (2 :: Int),
                  "insertSpaces" .= True
                ]
          ]

  result <- sendRequest client "textDocument/formatting" params
  case result of
    Left err -> return $ Left err
    Right (Array edits) -> do
      -- Apply text edits to content
      appliedResult <- applyTextEdits content (toList edits)
      case appliedResult of
        Left applyErr -> return $ Left applyErr
        Right newContent -> return $ Right newContent
    Right _ -> return $ Left "Invalid formatting response"

-- Get Hover Information
getHover :: LSPClient -> Text -> Int -> Int -> IO (Either Text Text)
getHover client uri line char = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "position"
              .= object
                [ "line" .= line,
                  "character" .= char
                ]
          ]

  result <- sendRequest client "textDocument/hover" params
  case result of
    Left err -> return $ Left err
    Right (Object obj) ->
      case KM.lookup "contents" obj of
        Just (String txt) -> return $ Right txt
        Just (Object contents) ->
          case KM.lookup "value" contents of
            Just (String txt) -> return $ Right txt
            _ -> return $ Left "No hover content"
        _ -> return $ Left "No hover content"
    Right _ -> return $ Left "Invalid hover response"

-- Go to Definition
gotoDefinition :: LSPClient -> Text -> Int -> Int -> IO (Either Text [Value])
gotoDefinition client uri line char = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "position"
              .= object
                [ "line" .= line,
                  "character" .= char
                ]
          ]

  result <- sendRequest client "textDocument/definition" params
  case result of
    Left err -> return $ Left err
    Right (Array locations) -> return $ Right $ toList locations
    Right (Object obj) ->
      -- Single location object
      return $ Right [Object obj]
    Right Null ->
      -- No definition found
      return $ Right []
    Right _ -> return $ Left "Invalid definition response"

-- Find References
findReferences :: LSPClient -> Text -> Int -> Int -> Bool -> IO (Either Text [Value])
findReferences client uri line char includeDeclaration = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "position"
              .= object
                [ "line" .= line,
                  "character" .= char
                ],
            "context"
              .= object
                [ "includeDeclaration" .= includeDeclaration
                ]
          ]

  result <- sendRequest client "textDocument/references" params
  case result of
    Left err -> return $ Left err
    Right (Array references) -> return $ Right $ toList references
    Right Null ->
      -- No references found
      return $ Right []
    Right _ -> return $ Left "Invalid references response"

-- Get Document Symbols
getDocumentSymbols :: LSPClient -> Text -> IO (Either Text [Value])
getDocumentSymbols client uri = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri]
          ]

  result <- sendRequest client "textDocument/documentSymbol" params
  case result of
    Left err -> return $ Left err
    Right (Array symbols) -> return $ Right $ toList symbols
    Right Null ->
      -- No symbols found
      return $ Right []
    Right _ -> return $ Left "Invalid document symbols response"

-- Get Completions
getCompletions :: LSPClient -> Text -> Int -> Int -> IO (Either Text [Value])
getCompletions client uri line char = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "position"
              .= object
                [ "line" .= line,
                  "character" .= char
                ],
            "context"
              .= object
                [ "triggerKind" .= (1 :: Int) -- Invoked
                ]
          ]

  result <- sendRequest client "textDocument/completion" params
  case result of
    Left err -> return $ Left err
    Right (Object obj) ->
      -- CompletionList format
      case KM.lookup "items" obj of
        Just (Array items) -> return $ Right $ toList items
        _ -> return $ Right []
    Right (Array items) ->
      -- Direct array of completion items
      return $ Right $ toList items
    Right Null ->
      -- No completions found
      return $ Right []
    Right _ -> return $ Left "Invalid completion response"

-- Get Workspace Symbols
getWorkspaceSymbols :: LSPClient -> Text -> IO (Either Text [Value])
getWorkspaceSymbols client query = do
  let params =
        object
          [ "query" .= query
          ]

  result <- sendRequest client "workspace/symbol" params
  case result of
    Left err -> return $ Left err
    Right (Array symbols) -> return $ Right $ toList symbols
    Right Null ->
      -- No symbols found
      return $ Right []
    Right _ -> return $ Left "Invalid workspace symbols response"

-- Get Code Lenses
getCodeLenses :: LSPClient -> Text -> IO (Either Text [Value])
getCodeLenses client uri = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri]
          ]

  result <- sendRequest client "textDocument/codeLens" params
  case result of
    Left err -> return $ Left err
    Right (Array lenses) -> return $ Right $ toList lenses
    Right Null ->
      -- No code lenses found
      return $ Right []
    Right _ -> return $ Left "Invalid code lenses response"

-- Get Code Actions
getCodeActions :: LSPClient -> Text -> Int -> Int -> Int -> Int -> IO (Either Text [Value])
getCodeActions client uri startLine startChar endLine endChar = do
  let params =
        object
          [ "textDocument" .= object ["uri" .= uri],
            "range"
              .= object
                [ "start"
                    .= object
                      [ "line" .= startLine,
                        "character" .= startChar
                      ],
                  "end"
                    .= object
                      [ "line" .= endLine,
                        "character" .= endChar
                      ]
                ],
            "context"
              .= object
                [ "diagnostics" .= ([] :: [Value]) -- Empty diagnostics array
                ]
          ]

  result <- sendRequest client "textDocument/codeAction" params
  case result of
    Left err -> return $ Left err
    Right (Array actions) -> return $ Right $ toList actions
    Right Null ->
      -- No code actions found
      return $ Right []
    Right _ -> return $ Left "Invalid code actions response"

-- Execute Command
executeCommand :: LSPClient -> Text -> [Value] -> IO (Either Text Value)
executeCommand client command args = do
  let params =
        object
          [ "command" .= command,
            "arguments" .= args
          ]

  result <- sendRequest client "workspace/executeCommand" params
  case result of
    Left err -> return $ Left err
    Right response -> return $ Right response

-- Send didOpen notification to trigger diagnostics
sendDidOpenNotification :: LSPClient -> Text -> Text -> IO (Either Text ())
sendDidOpenNotification client uri content = do
  let params =
        object
          [ "textDocument"
              .= object
                [ "uri" .= uri,
                  "languageId" .= ("haskell" :: Text),
                  "version" .= (1 :: Int),
                  "text" .= content
                ]
          ]
  sendNotification client "textDocument/didOpen" params

-- Get File Diagnostics
getFileDiagnostics :: LSPClient -> Text -> Text -> IO (Either Text [Value])
getFileDiagnostics client uri content = do
  -- First send didOpen notification to trigger diagnostics
  openResult <- sendDidOpenNotification client uri content
  case openResult of
    Left err -> return $ Left err
    Right _ -> do
      -- Wait a bit for diagnostics to be processed
      threadDelay 2000000 -- 2 seconds

      -- Retrieve stored diagnostics for this URI
      storedDiagnostics <- atomically $ do
        store <- readTVar (diagnosticsStore client)
        return $ maybe [] Prelude.id $ lookup uri store

      return $ Right storedDiagnostics

-- Organize Imports Command
organizeImportsCommand :: LSPClient -> Text -> IO (Either Text Value)
organizeImportsCommand client uri = do
  let params =
        object
          [ "command" .= ("ghcide-code-actions-imports:organizeImports" :: Text),
            "arguments" .= [object ["uri" .= uri]]
          ]

  result <- sendRequest client "workspace/executeCommand" params
  case result of
    Left err -> return $ Left err
    Right response -> return $ Right response

-- Insert Import Command
insertImportCommand :: LSPClient -> Text -> Text -> Maybe Text -> IO (Either Text Value)
insertImportCommand client uri moduleName maybeSymbol = do
  let importArgs = case maybeSymbol of
        Nothing ->
          object
            [ "uri" .= uri,
              "module" .= moduleName
            ]
        Just symbol ->
          object
            [ "uri" .= uri,
              "module" .= moduleName,
              "symbol" .= symbol
            ]
      params =
        object
          [ "command" .= ("ghcide-extend-import-action:extendImport" :: Text),
            "arguments" .= [importArgs]
          ]

  result <- sendRequest client "workspace/executeCommand" params
  case result of
    Left err -> return $ Left err
    Right response -> return $ Right response

-- Remove Unused Imports Command
removeUnusedImportsCommand :: LSPClient -> Text -> IO (Either Text Value)
removeUnusedImportsCommand client uri = do
  let params =
        object
          [ "command" .= ("ghcide-code-actions-unused-imports:removeAll" :: Text),
            "arguments" .= [object ["uri" .= uri]]
          ]

  result <- sendRequest client "workspace/executeCommand" params
  case result of
    Left err -> return $ Left err
    Right response -> return $ Right response

-- Apply Text Edits to Content
applyTextEdits :: Text -> [Value] -> IO (Either Text Text)
applyTextEdits content edits = do
  case parseTextEdits edits of
    Left err -> return $ Left err
    Right parsedEdits -> do
      -- Sort edits in reverse order (by position) to apply from end to start
      let sortedEdits = sortBy (comparing (Down . editStartPos)) parsedEdits
      return $ Right $ foldl applyEdit content sortedEdits
  where
    parseTextEdits :: [Value] -> Either Text [TextEdit]
    parseTextEdits = mapM parseTextEdit

    parseTextEdit :: Value -> Either Text TextEdit
    parseTextEdit val = case fromJSON val of
      Success edit -> Right edit
      Aeson.Error err -> Left $ "Failed to parse text edit: " <> T.pack err

    editStartPos :: TextEdit -> (Int, Int)
    editStartPos edit =
      let range = editRange edit
          start = rangeStart range
       in (positionLine start, positionCharacter start)

    applyEdit :: Text -> TextEdit -> Text
    applyEdit text edit =
      let range = editRange edit
          newText = editNewText edit
          start = rangeStart range
          end = rangeEnd range
       in replaceTextRange text start end newText

-- Text Edit Types (simplified LSP types)
data TextEdit = TextEdit
  { editRange :: Range,
    editNewText :: Text
  }
  deriving (Show, Eq)

data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }
  deriving (Show, Eq)

data Position = Position
  { positionLine :: Int,
    positionCharacter :: Int
  }
  deriving (Show, Eq)

instance FromJSON TextEdit where
  parseJSON = withObject "TextEdit" $ \o ->
    TextEdit
      <$> o .: "range"
      <*> o .: "newText"

instance FromJSON Range where
  parseJSON = withObject "Range" $ \o ->
    Range
      <$> o .: "start"
      <*> o .: "end"

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o ->
    Position
      <$> o .: "line"
      <*> o .: "character"

-- Replace text in a range
replaceTextRange :: Text -> Position -> Position -> Text -> Text
replaceTextRange text start end newText =
  let textLines = T.lines text
      startLine = positionLine start
      startChar = positionCharacter start
      endLine = positionLine end
      endChar = positionCharacter end
   in if startLine == endLine
        then -- Single line replacement
          let line = textLines !! startLine
              before = T.take startChar line
              after = T.drop endChar line
              newLine = before <> newText <> after
              newLines = take startLine textLines ++ [newLine] ++ drop (startLine + 1) textLines
         in T.unlines newLines
        else -- Multi-line replacement
          let startLineText = textLines !! startLine
              endLineText = textLines !! endLine
              before = T.take startChar startLineText
              after = T.drop endChar endLineText
              replacementLines = T.lines (before <> newText <> after)
              newLines = take startLine textLines ++ replacementLines ++ drop (endLine + 1) textLines
         in T.unlines newLines
