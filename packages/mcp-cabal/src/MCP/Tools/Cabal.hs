{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Tools.Cabal
  ( -- * Tool implementations
    executeCabalTool
  , startCabalTestProcess
  , stopCabalTestProcess
  , getCabalTestStatus
  , getCabalTestMessages
  , listCabalTestProcesses
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Hashable (hash)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), isAbsolute)
import Cabal.Filter (applyShellFilter)
import Cabal.ProcessRegistry (CabalURI (..), ProcessRegistry)
import qualified Cabal.ProcessRegistry as PR
import MCP.SDK.Types (Content (TextContent), ToolCallResult (..), ToolsCallRequest (..), ToolsCallResponse (..))
import MCP.Types.Cabal
import Utils.Logging

-- | Execute a Cabal tool based on the tool name.
executeCabalTool :: ProcessRegistry -> ToolsCallRequest -> IO ToolsCallResponse
executeCabalTool registry req =
  case textToToolName (toolName req) of
    Nothing ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Unknown Cabal tool: " <> toolName req])
            (Just True)
    Just toolType -> case toolType of
      StartCabalTest -> startCabalTestProcess registry (toolArguments req)
      StopCabalTest -> stopCabalTestProcess registry (toolArguments req)
      ProcessStatus -> getCabalTestStatus registry (toolArguments req)
      GetMessages -> getCabalTestMessages registry (toolArguments req)
      ListProcesses -> listCabalTestProcesses registry (toolArguments req)

-- | Start a new cabal test process.
startCabalTestProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
startCabalTestProcess registry args = do
  logInfo "Starting cabal test process"
  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for cabal-test-start"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success startArgs -> do
          let cabalURI = startCabalURI startArgs
              workDir = startWorkDir startArgs
              target = startTarget startArgs
              extraArgsTexts = normalizeStartOptions (startOptions startArgs)

          commandPartsWithBuildDir <- buildTestCommand cabalURI target extraArgsTexts
          logDebug $
            "Launching cabal test for "
              <> getCabalURI cabalURI
              <> " with command: "
              <> renderCommandParts commandPartsWithBuildDir

          startResult <- PR.startCabalTestProcess registry cabalURI workDir commandPartsWithBuildDir
          case startResult of
            Left err -> return $ Left err
            Right _ -> do
              let resultData =
                    StartCabalTestResult
                      { startSuccess = True
                      , startMessage = "Cabal test started successfully"
                      , startProcessId = Just $ getCabalURI cabalURI
                      }
              return $ Right $ toJSON resultData
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to start cabal test: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error starting cabal test: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "Cabal test start failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to start cabal test: " <> err])
            (Just True)
    Right (Right response) -> do
      logInfo "Cabal test started successfully"
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Stop a running cabal test process.
stopCabalTestProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
stopCabalTestProcess registry args = do
  logInfo "Stopping cabal test process"
  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for cabal-test-stop"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success stopArgs -> do
          let cabalURI = stopCabalURI stopArgs
              _force = stopForce stopArgs

          stopResult <- PR.stopCabalTestProcess registry cabalURI
          case stopResult of
            Left err -> return $ Left err
            Right _ -> do
              let resultData =
                    StopCabalTestResult
                      { stopSuccess = True
                      , stopMessage = "Cabal test stopped successfully"
                      }
              return $ Right $ toJSON resultData
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to stop cabal test: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error stopping cabal test: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "Cabal test stop failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to stop cabal test: " <> err])
            (Just True)
    Right (Right response) -> do
      logInfo "Cabal test stopped successfully"
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Get status of a cabal test process.
getCabalTestStatus :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getCabalTestStatus registry args = do
  logInfo "Getting cabal test status"
  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for cabal-test-status"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success statusArgs -> do
          let cabalURI = statusCabalURI statusArgs
          existing <- PR.getCabalTestProcess registry cabalURI
          status <- case existing of
            Nothing -> return Nothing
            Just handle -> Just <$> PR.getProcessStatus handle
          lastMessage <- case existing of
            Nothing -> return Nothing
            Just handle -> PR.getProcessLastMessage handle
          uptime <- case existing of
            Nothing -> return Nothing
            Just handle -> do
              now <- getCurrentTime
              let seconds = round (diffUTCTime now (PR.getCabalTestStartTime handle)) :: Int
              return $ Just (T.pack (show seconds) <> "s")
          let resultData =
                ProcessStatusResult
                  { processStatus = status
                  , processUptime = uptime
                  , processLatestMessage = lastMessage
                  }
          return $ Right $ toJSON resultData
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to get cabal test status: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error getting cabal test status: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "Cabal test status check failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to get cabal test status: " <> err])
            (Just True)
    Right (Right response) ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Get messages from a cabal test process.
getCabalTestMessages :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getCabalTestMessages registry args = do
  logInfo "Getting cabal test messages"
  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for cabal-test-messages"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success messagesArgs -> do
          let cabalURI = messagesCabalURI messagesArgs
              filterReq = messagesFilter messagesArgs
              count = messagesCount messagesArgs

          maybeHandle <- PR.getCabalTestProcess registry cabalURI
          case maybeHandle of
            Nothing -> return $ Left "No cabal test process found for this project"
            Just handle -> do
              messages <- T.lines <$> PR.getBufferedOutput handle
              filteredMessages <- case filterReq of
                Nothing -> return messages
                Just filterSpec -> do
                  filtered <- applyShellFilter (T.unlines messages) filterSpec
                  case filtered of
                    Left _ -> return []
                    Right output -> return $ T.lines output
              let limitedMessages = case count of
                    Nothing -> filteredMessages
                    Just n -> takeLast n filteredMessages
              timestamp <- getCurrentTime
              let resultData =
                    MessagesResult
                      { messagesOutput = T.unlines limitedMessages
                      , messagesLines = limitedMessages
                      , messagesTimestamp = timestamp
                      }
              return $ Right $ toJSON resultData
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to get cabal test messages: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error getting cabal test messages: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "Cabal test messages retrieval failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to get cabal test messages: " <> err])
            (Just True)
    Right (Right response) ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

takeLast :: Int -> [a] -> [a]
takeLast n xs =
  let k = max 0 (length xs - n)
   in drop k xs

-- | List cabal test processes.
listCabalTestProcesses :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
listCabalTestProcesses registry args = do
  logInfo "Listing cabal test processes"
  result <- try $ do
    let parsedArgs = case args of
          Nothing -> Success (ListProcessesArgs False)
          Just argsObject -> fromJSON (Object argsObject)
    case parsedArgs of
      Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
      Success listArgs -> do
        let includeStatus = listIncludeStatus listArgs
        processes <- PR.listKnownProcesses registry
        statusInfo <-
          if includeStatus
            then do
              handles <- mapM (PR.getCabalTestProcess registry) processes
              statuses <-
                mapM
                  ( \mh -> case mh of
                      Nothing -> return Nothing
                      Just h -> Just <$> PR.getProcessStatus h
                  )
                  handles
              return $ Just $ zip processes (map (maybe CabalTestStopped id) statuses)
            else return Nothing
        let resultData =
              ProcessListResult
                { processURIs = processes
                , processStatuses = statusInfo
                }
        return $ Right $ toJSON resultData
  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to list cabal test processes: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error listing cabal test processes: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "Cabal test process listing failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to list cabal test processes: " <> err])
            (Just True)
    Right (Right response) ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

normalizeStartOptions :: Maybe StartCabalTestOptions -> [Text]
normalizeStartOptions Nothing = []
normalizeStartOptions (Just StartCabalTestOptions {..}) = startAdditionalArgs

buildTestCommand :: CabalURI -> Maybe Text -> [Text] -> IO [String]
buildTestCommand cabalURI target extraArgs = do
  let cleanedTarget =
        case fmap T.strip target of
          Just t | T.null t -> Nothing
          other -> other
  commandWithTarget <- pure $ ["cabal", "test"] ++ maybe [] (\t -> [T.unpack t]) cleanedTarget
  let extra = map T.unpack extraArgs
  addIsolatedBuildDir cabalURI (commandWithTarget ++ extra)

addIsolatedBuildDir :: CabalURI -> [String] -> IO [String]
addIsolatedBuildDir cabalURI parts = do
  cacheRoot <- resolveCacheRoot
  let buildDirName = "cabal-test-" <> show (abs (hash (getCabalURI cabalURI)))
      buildDir = cacheRoot </> "cabal-test" </> buildDirName
  createDirectoryIfMissing True buildDir
  pure $ injectBuildDirArg buildDir parts

resolveCacheRoot :: IO FilePath
resolveCacheRoot = do
  tmpBase <- getTemporaryDirectory
  override <- lookupEnv "MCP_CACHE_DIR"
  pure $ case override of
    Nothing -> tmpBase </> "mcp-cache"
    Just path
      | isAbsolute path -> path
      | otherwise -> tmpBase </> path

injectBuildDirArg :: FilePath -> [String] -> [String]
injectBuildDirArg buildDir parts
  | hasBuildDir parts = parts
  | otherwise =
      case parts of
        ("cabal" : "test" : rest) -> "cabal" : "test" : "--builddir" : buildDir : rest
        _ -> parts
  where
    hasBuildDir = any (\arg -> arg == "--builddir" || "--builddir=" `isPrefixOf` arg)

renderCommandParts :: [String] -> Text
renderCommandParts = T.unwords . map quotePart
  where
    quotePart part
      | needsQuote part = "\"" <> T.concatMap escapeChar (T.pack part) <> "\""
      | otherwise = T.pack part
    needsQuote txt = null txt || any isSpace txt
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

jsonToText :: Value -> Text
jsonToText = TE.decodeUtf8 . BL.toStrict . encode
