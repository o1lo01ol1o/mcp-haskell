{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HLS.Client where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import Control.Monad (forever, void, replicateM)
import Data.Aeson
import Data.Aeson (withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
-- import Language.LSP.Protocol.Types (LspId, ResponseError(..))
import System.IO
import System.Process.Typed
import MCP.Types
import Utils.Logging

-- Temporary LSP types definitions  
type LspId = Int
data ResponseError = ResponseError 
  { errorCode :: Int
  , errorMessage :: Text  
  , errorData :: Maybe Value
  } deriving (Show, Eq)

instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" $ \o -> ResponseError
    <$> o .: "code"
    <*> o .: "message" 
    <*> o .:? "data"

-- LSP Client State
data LSPClient = LSPClient
  { processHandle :: Process Handle Handle Handle
  , requestId :: TVar Int
  , pendingRequests :: TVar [(LspId, TMVar (Either ResponseError Value))]
  }

instance Show LSPClient where
  show _ = "LSPClient{...}"

-- Initialize LSP Client
initializeLSPClient :: FilePath -> IO (Either Text LSPClient)
initializeLSPClient workDir = do
  result <- try $ do
    -- Start HLS process
    let processConfig = proc "haskell-language-server-wrapper" ["--lsp"]
                      & setWorkingDir workDir
                      & setStdin createPipe
                      & setStdout createPipe
                      & setStderr createPipe
    
    process <- System.Process.Typed.startProcess processConfig
    
    -- Initialize client state
    reqId <- newTVarIO 0
    pending <- newTVarIO []
    
    let client = LSPClient process reqId pending
    
    -- Start response handler
    void $ async $ responseHandler client (getStdout process)
    
    -- Send initialize request
    initResult <- sendInitializeRequest client workDir
    case initResult of
      Left err -> return $ Left err
      Right _ -> do
        -- Send initialized notification
        sendNotification client "initialized" (object [])
        return $ Right client
  
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to initialize LSP client: " <> T.pack (show ex)
    Right res -> return res

-- Send Initialize Request
sendInitializeRequest :: LSPClient -> FilePath -> IO (Either Text Value)
sendInitializeRequest client workDir = do
  let initParams = object
        [ "processId" .= (Nothing :: Maybe Int)
        , "rootUri" .= ("file://" <> T.pack workDir)
        , "workspaceFolders" .= ([object
            [ "uri" .= ("file://" <> T.pack workDir)
            , "name" .= T.takeWhileEnd (/= '/') (T.pack workDir)
            ]] :: [Value])
        , "capabilities" .= object
            [ "workspace" .= object
                [ "workspaceEdit" .= object ["documentChanges" .= True]
                , "symbol" .= object ["symbolKind" .= object []]
                ]
            , "textDocument" .= object
                [ "hover" .= object ["contentFormat" .= ["markdown" :: Text, "plaintext"]]
                , "completion" .= object 
                    [ "completionItem" .= object 
                        [ "snippetSupport" .= True
                        , "documentationFormat" .= ["markdown" :: Text]
                        ]
                    ]
                , "formatting" .= object []
                , "diagnostics" .= object []
                ]
            ]
        ]
  sendRequest client "initialize" initParams

-- Send LSP Request
sendRequest :: LSPClient -> Text -> Value -> IO (Either Text Value)
sendRequest LSPClient{..} method params = do
  -- Get next request ID
  reqIdNum <- atomically $ do
    current <- readTVar requestId
    writeTVar requestId (current + 1)
    return current
  
  let reqId = fromIntegral reqIdNum
  
  -- Create response variable
  responseVar <- newEmptyTMVarIO
  
  -- Add to pending requests
  atomically $ modifyTVar pendingRequests ((reqId, responseVar):)
  
  -- Send request
  let request = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= method
        , "params" .= params
        , "id" .= reqIdNum
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
      -- Wait for response (with timeout could be added here)
      response <- atomically $ takeTMVar responseVar
      case response of
        Left err -> return $ Left $ "LSP Error: " <> T.pack (show err)
        Right val -> return $ Right val

-- Send LSP Notification
sendNotification :: LSPClient -> Text -> Value -> IO (Either Text ())
sendNotification LSPClient{..} method params = do
  let notification = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= method
        , "params" .= params
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
responseHandler client@LSPClient{..} handle = forever $ do
  result <- try $ do
    -- Read Content-Length header
    contentLengthLine <- T.hGetLine handle
    let contentLength = read $ T.unpack $ T.drop 16 contentLengthLine -- "Content-Length: "
    
    -- Skip empty line
    _ <- T.hGetLine handle
    
    -- Read exact number of characters for the message
    message <- replicateM contentLength (hGetChar handle)
    return $ T.pack message
  
  case result of
    Left (ex :: SomeException) -> logError $ "Response handler error: " <> T.pack (show ex)
    Right messageText -> do
      case eitherDecode (fromStrict $ T.encodeUtf8 messageText) of
        Left parseErr -> logError $ "Failed to parse response: " <> T.pack parseErr
        Right response -> handleResponse client response

-- Handle Response
handleResponse :: LSPClient -> Value -> IO ()
handleResponse LSPClient{..} response = do
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
                    Data.Aeson.Error _ -> atomically $ putTMVar responseVar (Left $ ResponseError (-1) "Parse error" Nothing)
                Nothing -> 
                  case KM.lookup "result" obj of
                    Just result -> atomically $ putTMVar responseVar (Right result)
                    Nothing -> atomically $ putTMVar responseVar (Left $ ResponseError (-1) "No result" Nothing)
        _ -> return () -- Notification or malformed response
    _ -> logError "Received non-object response"

-- Close LSP Client
closeLSPClient :: LSPClient -> IO ()
closeLSPClient LSPClient{..} = do
  _ <- try @SomeException $ hClose (getStdin processHandle)
  _ <- try @SomeException $ stopProcess processHandle
  return ()

-- Format Document Request
formatDocument :: LSPClient -> Text -> Text -> IO (Either Text Text)
formatDocument client uri content = do
  let params = object
        [ "textDocument" .= object ["uri" .= uri]
        , "options" .= object
            [ "tabSize" .= (2 :: Int)
            , "insertSpaces" .= True
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
  let params = object
        [ "textDocument" .= object ["uri" .= uri]
        , "position" .= object
            [ "line" .= line
            , "character" .= char
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

-- Get Diagnostics
getDiagnostics :: LSPClient -> Text -> IO (Either Text [Value])
getDiagnostics client uri = do
  -- Diagnostics are typically sent as notifications, not responses
  -- For now, return empty diagnostics
  return $ Right []

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
      Data.Aeson.Error err -> Left $ "Failed to parse text edit: " <> T.pack err
    
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
  { editRange :: Range
  , editNewText :: Text
  } deriving (Show, Eq)

data Range = Range
  { rangeStart :: Position
  , rangeEnd :: Position
  } deriving (Show, Eq)

data Position = Position
  { positionLine :: Int
  , positionCharacter :: Int
  } deriving (Show, Eq)

instance FromJSON TextEdit where
  parseJSON = withObject "TextEdit" $ \o -> TextEdit
    <$> o .: "range"
    <*> o .: "newText"

instance FromJSON Range where
  parseJSON = withObject "Range" $ \o -> Range
    <$> o .: "start"
    <*> o .: "end"

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o -> Position
    <$> o .: "line"
    <*> o .: "character"

-- Replace text in a range
replaceTextRange :: Text -> Position -> Position -> Text -> Text
replaceTextRange text start end newText =
  let lines = T.lines text
      startLine = positionLine start
      startChar = positionCharacter start
      endLine = positionLine end
      endChar = positionCharacter end
  in if startLine == endLine
     then -- Single line replacement
       let line = lines !! startLine
           before = T.take startChar line
           after = T.drop endChar line
           newLine = before <> newText <> after
           newLines = take startLine lines ++ [newLine] ++ drop (startLine + 1) lines
       in T.unlines newLines
     else -- Multi-line replacement
       let startLineText = lines !! startLine
           endLineText = lines !! endLine
           before = T.take startChar startLineText
           after = T.drop endChar endLineText
           replacementLines = T.lines (before <> newText <> after)
           newLines = take startLine lines ++ replacementLines ++ drop (endLine + 1) lines
       in T.unlines newLines