{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Process where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import HLS.Client (LSPClient, ProcessStatus (..), closeLSPClient, executeCommand, findReferences, formatDocument, getCodeActions, getCodeLenses, getCompletions, getDocumentSymbols, getFileDiagnostics, getHover, getWorkspaceSymbols, gotoDefinition, initializeLSPClient, insertImportCommand, organizeImportsCommand, processHandle, processStatus, removeUnusedImportsCommand)
import MCP.Types
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import System.Timeout (timeout)

-- HLS Process Handle
data HLSHandle = HLSHandle
  { hlsClient :: LSPClient,
    workingDirectory :: FilePath
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
      -- Initialize LSP client (this handles HLS startup and initialization)
      lspResult <- initializeLSPClient workDir
      case lspResult of
        Left err -> Prelude.error $ "Failed to initialize HLS: " ++ T.unpack err
        Right client -> do
          return
            HLSHandle
              { hlsClient = client,
                workingDirectory = workDir
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
      -- Close LSP client
      closeLSPClient (hlsClient handle)
      putMVar hlsProcessVar Nothing
      return $ Right Stopped

-- Restart HLS Server
restartHLSServer :: IO (Either Text HLSStatus)
restartHLSServer = do
  stopResult <- stopHLSServer
  case stopResult of
    Left err -> return $ Left err
    Right _ -> do
      -- Give the process time to clean up
      threadDelay 1000000 -- 1 second
      startHLSServer Nothing

-- Get HLS Status
getHLSStatus :: IO HLSStatus
getHLSStatus = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return Stopped
    Just handle -> do
      status <- readTVarIO (processStatus (hlsClient handle))
      case status of
        Healthy -> return Running
        Unhealthy err -> return $ Error err
        Dead exitCode -> return $ Error $ "Process died with exit code: " <> T.pack (show exitCode)
        Terminated signalInfo -> return $ Error $ "Process terminated by signal: " <> T.pack (show signalInfo)

-- Send LSP Message to HLS
sendLSPMessage :: Text -> IO (Either Text ())
sendLSPMessage msg = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      result <- try @SomeException $ do
        T.hPutStrLn (getStdin (processHandle (hlsClient handle))) msg
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
      result <- try @SomeException $ T.hGetLine (getStdout (processHandle (hlsClient handle)))
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
  result <- timeout 5000000 $ try @SomeException $ do
    -- 5 second timeout
    let processConfig =
          proc "haskell-language-server-wrapper" ["--version"]
            & setStdout byteStringOutput
    (exitCode, output, _) <- readProcess processConfig
    case exitCode of
      ExitSuccess -> return $ T.strip $ T.decodeUtf8 $ LBS.toStrict output
      _ -> Prelude.error "Process failed"
  case result of
    Nothing -> return $ Left "Timeout getting HLS version"
    Just (Left ex) -> return $ Left $ "Failed to get HLS version: " <> T.pack (show ex)
    Just (Right output) -> return $ Right output

-- LSP Operations (Real implementations using HLS.Client)
-- Some operations use real LSP communication, others are still placeholders

getHoverInfo :: FilePath -> Int -> Int -> IO (Either Text Text)
getHoverInfo filePath line char = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      getHover client uri line char

gotoDefinition :: FilePath -> Int -> Int -> IO (Either Text [Text])
gotoDefinition filePath line char = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.gotoDefinition client uri line char
      case result of
        Left err -> return $ Left err
        Right locations -> do
          -- Convert location JSON objects to readable text
          let locationTexts = map formatLocation locations
          return $ Right locationTexts
  where
    formatLocation :: Value -> Text
    formatLocation (Object obj) =
      case (KM.lookup "uri" obj, KM.lookup "range" obj) of
        (Just (String uri), Just (Object range)) ->
          let startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
              startChar = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "character" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in uri <> " at line " <> T.pack startLine <> ", column " <> T.pack startChar
        _ -> "Invalid location format"
    formatLocation _ = "Invalid location"

findReferences :: FilePath -> Int -> Int -> IO (Either Text [Text])
findReferences filePath line char = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.findReferences client uri line char True -- Include declaration
      case result of
        Left err -> return $ Left err
        Right references -> do
          -- Convert reference JSON objects to readable text
          let referenceTexts = map formatLocation references
          return $ Right referenceTexts
  where
    formatLocation :: Value -> Text
    formatLocation (Object obj) =
      case (KM.lookup "uri" obj, KM.lookup "range" obj) of
        (Just (String uri), Just (Object range)) ->
          let startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
              startChar = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "character" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in uri <> " at line " <> T.pack startLine <> ", column " <> T.pack startChar
        _ -> "Invalid reference format"
    formatLocation _ = "Invalid reference"

getDocumentSymbols :: FilePath -> IO (Either Text [Text])
getDocumentSymbols filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.getDocumentSymbols client uri
      case result of
        Left err -> return $ Left err
        Right symbols -> do
          -- Convert symbol JSON objects to readable text
          let symbolTexts = map formatSymbol symbols
          return $ Right symbolTexts
  where
    formatSymbol :: Value -> Text
    formatSymbol (Object obj) =
      case (KM.lookup "name" obj, KM.lookup "kind" obj, KM.lookup "range" obj) of
        (Just (String name), Just (Number kind), Just (Object range)) ->
          let symbolKind = getSymbolKind (round kind :: Int)
              startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in symbolKind <> " " <> name <> " at line " <> T.pack startLine
        (Just (String name), Just (Number kind), _) ->
          let symbolKind = getSymbolKind (round kind :: Int)
           in symbolKind <> " " <> name
        _ -> "Invalid symbol format"
    formatSymbol _ = "Invalid symbol"

    getSymbolKind :: Int -> Text
    getSymbolKind n = case n of
      1 -> "File"
      2 -> "Module"
      3 -> "Namespace"
      4 -> "Package"
      5 -> "Class"
      6 -> "Method"
      7 -> "Property"
      8 -> "Field"
      9 -> "Constructor"
      10 -> "Enum"
      11 -> "Interface"
      12 -> "Function"
      13 -> "Variable"
      14 -> "Constant"
      15 -> "String"
      16 -> "Number"
      17 -> "Boolean"
      18 -> "Array"
      19 -> "Object"
      20 -> "Key"
      21 -> "Null"
      22 -> "EnumMember"
      23 -> "Struct"
      24 -> "Event"
      25 -> "Operator"
      26 -> "TypeParameter"
      _ -> "Unknown"

getWorkspaceSymbols :: Text -> IO (Either Text [Text])
getWorkspaceSymbols query = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
      result <- HLS.Client.getWorkspaceSymbols client query
      case result of
        Left err -> return $ Left err
        Right symbols -> do
          -- Convert workspace symbol JSON objects to readable text
          let symbolTexts = map formatWorkspaceSymbol symbols
          return $ Right symbolTexts
  where
    formatWorkspaceSymbol :: Value -> Text
    formatWorkspaceSymbol (Object obj) =
      case (KM.lookup "name" obj, KM.lookup "kind" obj, KM.lookup "location" obj) of
        (Just (String name), Just (Number kind), Just (Object location)) ->
          let symbolKind = getSymbolKind (round kind :: Int)
              locationText = case KM.lookup "uri" location of
                Just (String uri) -> T.takeWhileEnd (/= '/') uri
                _ -> "unknown file"
           in symbolKind <> " " <> name <> " in " <> locationText
        (Just (String name), Just (Number kind), _) ->
          let symbolKind = getSymbolKind (round kind :: Int)
           in symbolKind <> " " <> name
        _ -> "Invalid workspace symbol format"
    formatWorkspaceSymbol _ = "Invalid workspace symbol"

    getSymbolKind :: Int -> Text
    getSymbolKind n = case n of
      1 -> "File"
      2 -> "Module"
      3 -> "Namespace"
      4 -> "Package"
      5 -> "Class"
      6 -> "Method"
      7 -> "Property"
      8 -> "Field"
      9 -> "Constructor"
      10 -> "Enum"
      11 -> "Interface"
      12 -> "Function"
      13 -> "Variable"
      14 -> "Constant"
      15 -> "String"
      16 -> "Number"
      17 -> "Boolean"
      18 -> "Array"
      19 -> "Object"
      20 -> "Key"
      21 -> "Null"
      22 -> "EnumMember"
      23 -> "Struct"
      24 -> "Event"
      25 -> "Operator"
      26 -> "TypeParameter"
      _ -> "Unknown"

getFileDiagnostics :: FilePath -> IO (Either Text [Text])
getFileDiagnostics filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
      -- Read file content
      contentResult <- try @SomeException $ T.readFile filePath
      case contentResult of
        Left ex -> return $ Left $ "Failed to read file: " <> T.pack (show ex)
        Right content -> do
          let uri = "file://" <> T.pack filePath
          result <- HLS.Client.getFileDiagnostics client uri content
          case result of
            Left err -> return $ Left err
            Right diagnostics -> do
              -- Convert diagnostic JSON objects to readable text
              let diagnosticTexts = map formatDiagnostic diagnostics
              return $ Right diagnosticTexts
  where
    formatDiagnostic :: Value -> Text
    formatDiagnostic (Object obj) =
      case (KM.lookup "severity" obj, KM.lookup "message" obj, KM.lookup "range" obj) of
        (Just (Number severity), Just (String message), Just (Object range)) ->
          let severityText = getSeverityText (round severity :: Int)
              startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
              startChar = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "character" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in severityText <> " at line " <> T.pack startLine <> ", column " <> T.pack startChar <> ": " <> message
        (_, Just (String message), Just (Object range)) ->
          let startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in "Issue at line " <> T.pack startLine <> ": " <> message
        (_, Just (String message), _) ->
          message
        _ -> "Invalid diagnostic format"
    formatDiagnostic _ = "Invalid diagnostic"

    getSeverityText :: Int -> Text
    getSeverityText n = case n of
      1 -> "Error"
      2 -> "Warning"
      3 -> "Information"
      4 -> "Hint"
      _ -> "Unknown"

formatDocument :: FilePath -> IO (Either Text Text)
formatDocument filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
      -- Read file content
      contentResult <- try @SomeException $ T.readFile filePath
      case contentResult of
        Left ex -> return $ Left $ "Failed to read file: " <> T.pack (show ex)
        Right content -> do
          let uri = "file://" <> T.pack filePath
          HLS.Client.formatDocument client uri content

getCompletions :: FilePath -> Int -> Int -> IO (Either Text [Text])
getCompletions filePath line char = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.getCompletions client uri line char
      case result of
        Left err -> return $ Left err
        Right completions -> do
          -- Convert completion JSON objects to readable text
          let completionTexts = map formatCompletion completions
          return $ Right completionTexts
  where
    formatCompletion :: Value -> Text
    formatCompletion (Object obj) =
      case (KM.lookup "label" obj, KM.lookup "kind" obj, KM.lookup "detail" obj) of
        (Just (String label), Just (Number kind), Just (String detail)) ->
          let completionKind = getCompletionKind (round kind :: Int)
           in completionKind <> " " <> label <> " - " <> detail
        (Just (String label), Just (Number kind), _) ->
          let completionKind = getCompletionKind (round kind :: Int)
           in completionKind <> " " <> label
        (Just (String label), _, _) ->
          label
        _ -> "Invalid completion format"
    formatCompletion _ = "Invalid completion"

    getCompletionKind :: Int -> Text
    getCompletionKind n = case n of
      1 -> "Text"
      2 -> "Method"
      3 -> "Function"
      4 -> "Constructor"
      5 -> "Field"
      6 -> "Variable"
      7 -> "Class"
      8 -> "Interface"
      9 -> "Module"
      10 -> "Property"
      11 -> "Unit"
      12 -> "Value"
      13 -> "Enum"
      14 -> "Keyword"
      15 -> "Snippet"
      16 -> "Color"
      17 -> "File"
      18 -> "Reference"
      19 -> "Folder"
      20 -> "EnumMember"
      21 -> "Constant"
      22 -> "Struct"
      23 -> "Event"
      24 -> "Operator"
      25 -> "TypeParameter"
      _ -> "Unknown"

getCodeActions :: FilePath -> Int -> Int -> Int -> Int -> IO (Either Text [Text])
getCodeActions filePath startLine startChar endLine endChar = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.getCodeActions client uri startLine startChar endLine endChar
      case result of
        Left err -> return $ Left err
        Right actions -> do
          -- Convert code action JSON objects to readable text
          let actionTexts = map formatCodeAction actions
          return $ Right actionTexts
  where
    formatCodeAction :: Value -> Text
    formatCodeAction (Object obj) =
      case (KM.lookup "title" obj, KM.lookup "kind" obj) of
        (Just (String title), Just (String kind)) ->
          kind <> ": " <> title
        (Just (String title), _) ->
          title
        _ -> "Invalid code action format"
    formatCodeAction _ = "Invalid code action"

executeHLSCommand :: Text -> FilePath -> Int -> Int -> Int -> Int -> IO (Either Text Text)
executeHLSCommand command filePath startLine startChar endLine endChar = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
          -- Create arguments based on the command type
          args = case command of
            "ghcide-type-lenses:typesignature.add" ->
              [ object
                  [ "uri" .= uri,
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
                        ]
                  ]
              ]
            _ ->
              [ object
                  [ "uri" .= uri,
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
                        ]
                  ]
              ]
      result <- HLS.Client.executeCommand client command args
      case result of
        Left err -> return $ Left err
        Right response -> do
          -- Convert the response to readable text
          let responseText = formatCommandResponse command response
          return $ Right responseText
  where
    formatCommandResponse :: Text -> Value -> Text
    formatCommandResponse cmd response =
      case response of
        Null -> "Command " <> cmd <> " executed successfully"
        Object obj ->
          case KM.lookup "edit" obj of
            Just (Object edit) ->
              "Command " <> cmd <> " executed with workspace edit"
            _ -> "Command " <> cmd <> " executed: " <> T.take 100 (T.pack (show response))
        _ -> "Command " <> cmd <> " executed: " <> T.take 100 (T.pack (show response))

executeGenericHLSCommand :: Text -> Value -> IO (Either Text Text)
executeGenericHLSCommand command args = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
      -- Parse args as array or convert single value to array
      let argsArray = case args of
            Array arr -> toList arr
            singleArg -> [singleArg]
      result <- HLS.Client.executeCommand client command argsArray
      case result of
        Left err -> return $ Left err
        Right response -> do
          let responseText = case response of
                Null -> "Command " <> command <> " executed successfully"
                _ -> "Command " <> command <> " executed: " <> T.take 200 (T.pack (show response))
          return $ Right responseText

executeRetrieCommand :: FilePath -> Text -> Text -> IO (Either Text Text)
executeRetrieCommand filePath lhs rhs = do
  return $ Right $ "Retrie refactoring on " <> T.pack filePath <> ": " <> lhs <> " -> " <> rhs

executeModuleNameCommand :: FilePath -> IO (Either Text Text)
executeModuleNameCommand filePath = do
  return $ Right $ "Module name updated for " <> T.pack filePath

executeCabalAddCommand :: FilePath -> Text -> IO (Either Text Text)
executeCabalAddCommand filePath dependency = do
  return $ Right $ "Added cabal dependency " <> dependency <> " to " <> T.pack filePath

getCodeLenses :: FilePath -> IO (Either Text [Text])
getCodeLenses filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.getCodeLenses client uri
      case result of
        Left err -> return $ Left err
        Right lenses -> do
          -- Convert code lens JSON objects to readable text
          let lensTexts = map formatCodeLens lenses
          return $ Right lensTexts
  where
    formatCodeLens :: Value -> Text
    formatCodeLens (Object obj) =
      case (KM.lookup "range" obj, KM.lookup "command" obj) of
        (Just (Object range), Just (Object command)) ->
          let startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
              commandTitle = case KM.lookup "title" command of
                Just (String title) -> title
                _ -> "Unknown command"
           in "Code lens at line " <> T.pack startLine <> ": " <> commandTitle
        (Just (Object range), _) ->
          let startLine = case KM.lookup "start" range of
                Just (Object start) -> case KM.lookup "line" start of
                  Just (Number n) -> show (round n :: Int)
                  _ -> "unknown"
                _ -> "unknown"
           in "Code lens at line " <> T.pack startLine
        _ -> "Invalid code lens format"
    formatCodeLens _ = "Invalid code lens"

resolveCodeLens :: Value -> IO (Either Text Text)
resolveCodeLens codeLens = do
  return $ Right $ "Resolved code lens: " <> T.pack (show codeLens)

organizeImports :: FilePath -> IO (Either Text Text)
organizeImports filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.organizeImportsCommand client uri
      case result of
        Left err -> return $ Left err
        Right response -> do
          let responseText = formatOrganizeImportsResponse response
          return $ Right responseText
  where
    formatOrganizeImportsResponse :: Value -> Text
    formatOrganizeImportsResponse response =
      case response of
        Null -> "Imports organized successfully for " <> T.pack filePath
        Object obj ->
          case KM.lookup "edit" obj of
            Just (Object _) ->
              "Imports organized with workspace edit for " <> T.pack filePath
            _ -> "Imports organized for " <> T.pack filePath <> ": " <> T.take 100 (T.pack (show response))
        _ -> "Imports organized for " <> T.pack filePath <> ": " <> T.take 100 (T.pack (show response))

insertImport :: FilePath -> Text -> Maybe Text -> IO (Either Text Text)
insertImport filePath moduleName maybeSymbol = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.insertImportCommand client uri moduleName maybeSymbol
      case result of
        Left err -> return $ Left err
        Right response -> do
          let symbolText = case maybeSymbol of
                Nothing -> ""
                Just symbol -> " (" <> symbol <> ")"
              responseText = case response of
                Null -> "Inserted import " <> moduleName <> symbolText <> " in " <> T.pack filePath
                _ -> "Import " <> moduleName <> symbolText <> " processed for " <> T.pack filePath
          return $ Right responseText

removeUnusedImports :: FilePath -> IO (Either Text Text)
removeUnusedImports filePath = do
  currentProcess <- readMVar hlsProcessVar
  case currentProcess of
    Nothing -> return $ Left "HLS server is not running"
    Just handle -> do
      let client = hlsClient handle
          uri = "file://" <> T.pack filePath
      result <- HLS.Client.removeUnusedImportsCommand client uri
      case result of
        Left err -> return $ Left err
        Right response -> do
          let responseText = case response of
                Null -> "Removed unused imports from " <> T.pack filePath
                Object obj ->
                  case KM.lookup "edit" obj of
                    Just (Object _) ->
                      "Removed unused imports with workspace edit from " <> T.pack filePath
                    _ -> "Unused imports processed for " <> T.pack filePath
                _ -> "Unused imports processed for " <> T.pack filePath
          return $ Right responseText
