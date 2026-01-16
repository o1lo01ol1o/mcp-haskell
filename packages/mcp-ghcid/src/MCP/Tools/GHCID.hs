{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Tools.GHCID
  ( -- * Tool implementations
    executeGHCIDTool,
    startGHCIDProcess,
    stopGHCIDProcess,
    restartGHCIDProcess,
    getGHCIDStatus,
    getGHCIDMessages,
    listGHCIDProcesses,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (find)
import Data.Hashable (hash)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropTrailingPathSeparator, isAbsolute, takeDirectory, takeExtension, takeFileName, (</>))
import GHCID.Filter (applyShellFilter)
import GHCID.ProcessRegistry (CabalURI (..), GHCIDStatus (..), ProcessRegistry)
import qualified GHCID.ProcessRegistry as PR
import MCP.SDK.Types (Content (TextContent), ToolCallResult (..), ToolsCallRequest (..), ToolsCallResponse (..))
import MCP.Types.GHCID
import Utils.Logging

-- | Execute a GHCID tool based on the tool name
executeGHCIDTool :: ProcessRegistry -> ToolsCallRequest -> IO ToolsCallResponse
executeGHCIDTool registry req = do
  case textToToolName (toolName req) of
    Nothing ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Unknown GHCID tool: " <> toolName req])
            (Just True)
    Just toolType -> case toolType of
      StartGHCID -> startGHCIDProcess registry (toolArguments req)
      StopGHCID -> stopGHCIDProcess registry (toolArguments req)
      RestartProcess -> restartGHCIDProcess registry (toolArguments req)
      ProcessStatus -> getGHCIDStatus registry (toolArguments req)
      GetMessages -> getGHCIDMessages registry (toolArguments req)
      ListProcesses -> listGHCIDProcesses registry (toolArguments req)

-- | Start a new GHCID process
startGHCIDProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
startGHCIDProcess registry args = do
  logInfo "Starting GHCID process"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid-start"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success startArgs -> do
          let cabalURI = startCabalURI startArgs
              workDir = startWorkDir startArgs
              requestedComponent = startComponent startArgs
              (commandSpecOverride, extraArgsTexts) = normalizeStartOptions (startOptions startArgs)
          resolvedSpec <- liftIO $ resolveCommandSpec cabalURI workDir commandSpecOverride requestedComponent
          resolvedSpecWithBuildDir <-
            case commandSpecOverride of
              Just _ -> pure resolvedSpec
              Nothing -> liftIO $ addIsolatedBuildDir workDir cabalURI resolvedSpec
          let commandOverride = renderCommandSpec resolvedSpecWithBuildDir
              extraArgs = map T.unpack extraArgsTexts
          logDebug $
            "Launching ghcid for "
              <> getCabalURI cabalURI
              <> " with command: "
              <> commandOverride
              <> (if null extraArgsTexts then "" else " " <> T.unwords extraArgsTexts)

          -- Check if process already exists
          existing <- PR.getGHCIDProcess registry cabalURI
          case existing of
            Just _ -> return $ Left "GHCID process already running for this project"
            Nothing -> do
              -- Start the process using ProcessRegistry
              startResult <- PR.startGHCIDProcess registry cabalURI workDir (Just commandOverride) extraArgs

              case startResult of
                Left err -> return $ Left err
                Right _ -> do
                  let resultData =
                        StartGHCIDResult
                          { startSuccess = True,
                            startMessage = "GHCID process started successfully",
                            startProcessId = Just $ getCabalURI cabalURI
                          }

                  return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to start GHCID process: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error starting GHCID: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID start failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to start GHCID: " <> err])
            (Just True)
    Right (Right response) -> do
      logInfo "GHCID process started successfully"
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Stop a running GHCID process
stopGHCIDProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
stopGHCIDProcess registry args = do
  logInfo "Stopping GHCID process"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid-stop"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success stopArgs -> do
          let cabalURI = stopCabalURI stopArgs
              _force = stopForce stopArgs

          -- Look up the process
          existing <- PR.getGHCIDProcess registry cabalURI
          case existing of
            Nothing -> return $ Left "No GHCID process running for this project"
            Just _ -> do
              -- Stop the process
              stopResult <- PR.stopGHCIDProcess registry cabalURI

              case stopResult of
                Left err -> return $ Left err
                Right _ -> do
                  let resultData =
                        StopGHCIDResult
                          { stopSuccess = True,
                            stopMessage = "GHCID process stopped successfully"
                          }

                  return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to stop GHCID process: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error stopping GHCID: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID stop failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to stop GHCID: " <> err])
            (Just True)
    Right (Right response) -> do
      logInfo "GHCID process stopped successfully"
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Restart a GHCID process
restartGHCIDProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
restartGHCIDProcess registry args = do
  logInfo "Restarting GHCID process"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid-restart"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success restartArgs -> do
          let cabalURI = restartCabalURI restartArgs
              newWorkDir = restartWorkDir restartArgs
              requestedComponent = restartComponent restartArgs

          -- Look up the existing process before we tear it down so we can reuse its settings.
          existing <- PR.getGHCIDProcess registry cabalURI
          let previousWorkDir = fmap PR.getGHCIDWorkDir existing
              cabalPath = T.unpack (getCabalURI cabalURI)
              defaultWorkDir =
                let dir = takeDirectory cabalPath
                 in if null dir then "." else dir
              workDir = fromMaybe defaultWorkDir (newWorkDir <|> previousWorkDir)

          oldProcessId <- case existing of
            Nothing -> return Nothing
            Just _ -> do
              -- Stop the existing process
              stopResult <- PR.stopGHCIDProcess registry cabalURI
              case stopResult of
                Left _ -> return Nothing -- Continue with restart anyway
                Right _ -> return $ Just "old_process"

          resolvedSpec <- liftIO $ resolveCommandSpec cabalURI workDir Nothing requestedComponent
          resolvedSpecWithBuildDir <- liftIO $ addIsolatedBuildDir workDir cabalURI resolvedSpec
          let commandOverride = renderCommandSpec resolvedSpecWithBuildDir
          logDebug $
            "Restart launching ghcid for "
              <> getCabalURI cabalURI
              <> " with command: "
              <> commandOverride
          startResult <- PR.startGHCIDProcess registry cabalURI workDir (Just commandOverride) []

          case startResult of
            Left err -> return $ Left err
            Right _ -> do
              let resultData =
                    RestartProcessResult
                      { restartSuccess = True,
                        restartMessage = "GHCID process restarted successfully",
                        restartOldProcessId = oldProcessId,
                        restartNewProcessId = Just $ getCabalURI cabalURI
                      }

              return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to restart GHCID process: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error restarting GHCID: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID restart failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to restart GHCID: " <> err])
            (Just True)
    Right (Right response) -> do
      logInfo "GHCID process restarted successfully"
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Get status of a GHCID process
getGHCIDStatus :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getGHCIDStatus registry args = do
  logInfo "Getting GHCID process status"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid-status"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success statusArgs -> do
          let cabalURI = statusCabalURI statusArgs

          -- Look up the process
          existing <- PR.getGHCIDProcess registry cabalURI
          status <- case existing of
            Nothing -> return Nothing
            Just handle -> Just <$> PR.getProcessStatus handle
          lastMessage <- case existing of
            Nothing -> return Nothing
            Just handle -> PR.getProcessLastMessage handle
          let latestMessage =
                case (status, lastMessage) of
                  (Just GHCIDStarting, Nothing) -> Just "ghcid is starting; no output yet"
                  _ -> lastMessage

          let resultData =
                ProcessStatusResult
                  { processStatus = status,
                    processUptime = case existing of
                      Nothing -> Nothing
                      Just _ -> Just "unknown", -- Could implement uptime tracking
                    processLatestMessage = latestMessage
                  }

          return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to get GHCID status: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error getting GHCID status: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID status check failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to get GHCID status: " <> err])
            (Just True)
    Right (Right response) -> do
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

-- | Get messages from a GHCID process
getGHCIDMessages :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getGHCIDMessages registry args = do
  logInfo "Getting GHCID messages"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid-messages"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success messagesArgs -> do
          let cabalURI = messagesCabalURI messagesArgs
              filterReq = messagesFilter messagesArgs
              count = messagesCount messagesArgs

          -- Look up the process
          maybeHandle <- PR.getGHCIDProcess registry cabalURI
          case maybeHandle of
            Nothing -> return $ Left "No GHCID process running for this project"
            Just handle -> do
              -- Get messages from the client
              messages <- T.lines <$> PR.getBufferedOutput handle

              -- Apply filtering if requested
              filteredMessages <- case filterReq of
                Nothing -> return messages
                Just filterSpec -> do
                  filtered <- applyShellFilter (T.unlines messages) filterSpec
                  case filtered of
                    Left _ -> return []
                    Right output -> return $ T.lines output

              -- Limit count if requested
              let limitedMessages = case count of
                    Nothing -> filteredMessages
                    Just n -> takeLast n filteredMessages

              timestamp <- getCurrentTime
              let resultData =
                    MessagesResult
                      { messagesOutput = T.unlines limitedMessages,
                        messagesLines = limitedMessages,
                        messagesTimestamp = timestamp
                      }

              return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to get GHCID messages: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error getting GHCID messages: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID messages retrieval failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to get GHCID messages: " <> err])
            (Just True)
    Right (Right response) -> do
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

takeLast :: Int -> [a] -> [a]
takeLast n xs =
  let k = max 0 (length xs - n)
   in drop k xs

-- | List all GHCID processes
listGHCIDProcesses :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
listGHCIDProcesses registry args = do
  logInfo "Listing GHCID processes"

  result <- try $ do
    let parsedArgs = case args of
          Nothing -> Success (ListProcessesArgs False)
          Just argsObject -> fromJSON (Object argsObject)

    case parsedArgs of
      Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
      Success listArgs -> do
        let includeStatus = listIncludeStatus listArgs

        -- Get all registered processes
        processes <- PR.listActiveProcesses registry

        statusInfo <-
          if includeStatus
            then do
              handles <- mapM (PR.getGHCIDProcess registry) processes
              statuses <-
                mapM
                  ( \mh -> case mh of
                      Nothing -> return Nothing
                      Just h -> Just <$> PR.getProcessStatus h
                  )
                  handles
              return $ Just $ zip processes (map (maybe GHCIDStopped id) statuses)
            else return Nothing

        let resultData =
              ProcessListResult
                { processURIs = processes,
                  processStatuses = statusInfo
                }

        return $ Right $ toJSON resultData

  case result of
    Left (ex :: SomeException) -> do
      logError $ "Failed to list GHCID processes: " <> T.pack (show ex)
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Error listing GHCID processes: " <> T.pack (show ex)])
            (Just True)
    Right (Left err) -> do
      logWarn $ "GHCID process listing failed: " <> err
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent $ "Failed to list GHCID processes: " <> err])
            (Just True)
    Right (Right response) ->
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent (jsonToText response)])
            Nothing

normalizeStartOptions :: Maybe StartGHCIDOptions -> (Maybe CommandSpec, [Text])
normalizeStartOptions Nothing = (Nothing, [])
normalizeStartOptions (Just StartGHCIDOptions {..}) = (startCommandSpec, startAdditionalArgs)

-- | When multiple ghcid/cabal repl sessions run concurrently in a monorepo, sharing
-- a single `dist-newstyle/` can lead to builddir races/corruption. To avoid this,
-- default invocations isolate Cabal's build directory per project (cabalURI).
--
-- This only applies when the caller did not explicitly override the command.
addIsolatedBuildDir :: FilePath -> CabalURI -> CommandSpec -> IO CommandSpec
addIsolatedBuildDir _workDir cabalURI spec = do
  cacheRoot <- resolveCacheRoot
  let buildDirName = "cabal-build-" <> show (abs (hash (getCabalURI cabalURI)))
      buildDir = cacheRoot </> "cabal-build" </> buildDirName
  createDirectoryIfMissing True buildDir
  pure $ injectBuildDirArg buildDir spec

resolveCacheRoot :: IO FilePath
resolveCacheRoot = do
  tmpBase <- getTemporaryDirectory
  override <- lookupEnv "MCP_CACHE_DIR"
  pure $ case override of
    Nothing -> tmpBase </> "mcp-cache"
    Just path
      | isAbsolute path -> path
      | otherwise -> tmpBase </> path

injectBuildDirArg :: FilePath -> CommandSpec -> CommandSpec
injectBuildDirArg buildDir spec =
  case spec of
    CommandList parts -> CommandList (injectIntoParts parts)
    CommandString txt -> CommandString (injectIntoString txt)
  where
    buildDirText = T.pack buildDir

    injectIntoParts :: [Text] -> [Text]
    injectIntoParts parts =
      case parts of
        ("cabal" : "repl" : _) -> ensureBuildDir parts
        ("cabal" : "v2-repl" : _) -> ensureBuildDir parts
        _ -> parts

    ensureBuildDir :: [Text] -> [Text]
    ensureBuildDir parts
      | any (== "--builddir") parts = parts
      | otherwise =
          case parts of
            ("cabal" : cmd : rest) -> "cabal" : cmd : "--builddir" : buildDirText : rest
            _ -> parts

    injectIntoString :: Text -> Text
    injectIntoString txt
      | "--builddir" `T.isInfixOf` txt = txt
      | "cabal repl" `T.isPrefixOf` T.strip txt = T.strip txt <> " --builddir " <> quoteIfNeeded buildDirText
      | "cabal v2-repl" `T.isPrefixOf` T.strip txt = T.strip txt <> " --builddir " <> quoteIfNeeded buildDirText
      | otherwise = txt

    quoteIfNeeded :: Text -> Text
    quoteIfNeeded t
      | T.any isSpace t = "\"" <> T.concatMap escapeChar t <> "\""
      | otherwise = t

    escapeChar '"' = T.pack "\\\""
    escapeChar '\\' = T.pack "\\\\"
    escapeChar c = T.singleton c

renderCommandSpec :: CommandSpec -> Text
renderCommandSpec (CommandString t) = t
renderCommandSpec (CommandList parts) = renderCommandParts parts

renderCommandParts :: [Text] -> Text
renderCommandParts = T.unwords . map quotePart
  where
    quotePart part
      | needsQuote part = "\"" <> T.concatMap escapeChar part <> "\""
      | otherwise = part

    needsQuote txt = T.null txt || T.any isSpace txt || T.any (`elem` ['"', '\\']) txt

    escapeChar '"' = T.pack "\\\""
    escapeChar '\\' = T.pack "\\\\"
    escapeChar c = T.singleton c

jsonToText :: Value -> Text
jsonToText = TE.decodeUtf8 . BL.toStrict . encode

resolveCommandSpec :: CabalURI -> FilePath -> Maybe CommandSpec -> Maybe Text -> IO CommandSpec
resolveCommandSpec cabalURI workDir overrideSpec componentOverride =
  case overrideSpec of
    Just spec -> pure spec
    Nothing -> do
      result <- deriveDefaultCommandSpec cabalURI workDir componentOverride
      case result of
        Left err -> ioError (userError (T.unpack err))
        Right spec -> pure spec

deriveDefaultCommandSpec :: CabalURI -> FilePath -> Maybe Text -> IO (Either Text CommandSpec)
deriveDefaultCommandSpec cabalURI workDir componentOverride = do
  let rawPath = T.unpack (getCabalURI cabalURI)
  resolved <- resolveCabalFilePath workDir rawPath
  case resolved of
    Nothing -> do
      let err = "Unable to locate cabal file for URI " <> getCabalURI cabalURI
      logWarn $ err <> "; start request will fail"
      pure (Left err)
    Just cabalFile -> do
      contents <- TIO.readFile cabalFile
      let cabalLines = T.lines contents
          pkgName = extractPackageName cabalLines
          hasLib = hasLibraryStanza cabalLines
      case fmap T.strip componentOverride of
        Just comp | not (T.null comp) -> do
          let normalizedComp = normalizeComponentName comp
          logDebug $ "Using requested component '" <> comp <> "' (normalized to '" <> normalizedComp <> "') for " <> T.pack cabalFile
          pure $ Right (CommandList (map T.pack ["cabal", "repl"] ++ [normalizedComp]))
        _ ->
          case pkgName of
            Nothing -> do
              logWarn $ "Could not determine package name from cabal file " <> T.pack cabalFile <> "; defaulting to 'cabal repl'"
              pure $ Right (CommandString "cabal repl")
            Just name -> do
              let target = if hasLib then "lib:" <> name else name
              logDebug $ "Detected package name '" <> name <> "' from " <> T.pack cabalFile <> "; using target '" <> target <> "'"
              pure $ Right (CommandList ((map T.pack ["cabal", "repl"]) ++ [target]))

resolveCabalFilePath :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveCabalFilePath workDir rawPath = do
  let candidate = if isAbsolute rawPath then rawPath else workDir </> rawPath
  fileExists <- doesFileExist candidate
  if fileExists
    then pure (Just candidate)
    else do
      dirExists <- doesDirectoryExist candidate
      if not dirExists
        then pure Nothing
        else do
          entries <- listDirectory candidate
          let cabalFiles = filter ((== ".cabal") . takeExtension) entries
              expected = takeFileName (dropTrailingPathSeparator rawPath) <> ".cabal"
              preferred = find (== expected) cabalFiles <|> listToMaybe cabalFiles
          pure $ (candidate </>) <$> preferred

extractPackageName :: [Text] -> Maybe Text
extractPackageName = go
  where
    go [] = Nothing
    go (line:rest) =
      let stripped = T.strip line
          lower = T.toLower stripped
      in if T.isPrefixOf "name" lower
           then
             let (prefix, suffix) = T.breakOn ":" stripped
             in if T.strip (T.toLower prefix) == "name"
                  then
                    let rawValue = T.strip $ T.takeWhile (/= '#') (T.drop 1 suffix)
                        tokens = T.words rawValue
                    in case tokens of
                         (pkg:_) ->
                           let cleaned = T.dropAround (== '"') pkg
                           in if T.null cleaned then go rest else Just cleaned
                         _ -> go rest
                  else go rest
           else go rest

hasLibraryStanza :: [Text] -> Bool
hasLibraryStanza = any isLibrary
  where
    isLibrary line =
      let stripped = T.strip line
      in T.isPrefixOf "library" (T.toLower stripped)

normalizeComponentName :: Text -> Text
normalizeComponentName rawComp
  | T.null trimmed = trimmed
  | hasKnownPrefix trimmed = trimmed
  | otherwise =
      case T.splitOn ":" trimmed of
        (pkg:kind:rest)
          | let kindTag = T.toLower kind
          , kindTag `elem` knownKinds ->
              let suffix = if null rest then [pkg] else rest
                  normalizedPrefix = case kindTag of
                    "lib" -> "lib"
                    "exe" -> "exe"
                    "test" -> "test"
                    "bench" -> "bench"
                    other -> other
               in T.intercalate ":" (normalizedPrefix : suffix)
        _ -> trimmed
  where
    trimmed = T.strip rawComp
    knownKinds = ["lib", "exe", "test", "bench"]
    hasKnownPrefix txt =
      let lower = T.toLower txt
      in any (\prefix -> prefix `T.isPrefixOf` lower) (map (<> ":") knownKinds)
