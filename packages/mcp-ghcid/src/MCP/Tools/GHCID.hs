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

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
-- MCP SDK imports

-- Internal imports

import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.Vector as V
import GHCID.Client (GHCIDClient, createGHCIDClient, getCurrentOutput, startGHCID, stopGHCID)
import GHCID.Config (GHCIDConfig (..), defaultGHCIDConfig)
import GHCID.Filter (FilterRequest (..), applyShellFilter)
import GHCID.Output (formatCompilerMessage)
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
      Nothing -> return $ Left "Missing arguments for ghcid.start"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success startArgs -> do
          let cabalURI = startCabalURI startArgs
              workDir = startWorkDir startArgs
              options = startOptions startArgs

          -- Check if process already exists
          existing <- PR.getGHCIDProcess registry cabalURI
          case existing of
            Just _ -> return $ Left "GHCID process already running for this project"
            Nothing -> do
              -- Start the process using ProcessRegistry
              startResult <- PR.startGHCIDProcess registry cabalURI workDir

              case startResult of
                Left err -> return $ Left err
                Right _ -> do
                  let resultData =
                        StartGHCIDResult
                          { startSuccess = True,
                            startMessage = "GHCID process started successfully",
                            startProcessId = Just $ getCabalURI cabalURI
                          }

                  return $ Right $ T.pack $ show $ toJSON resultData

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
            (V.fromList [TextContent response])
            Nothing

-- | Stop a running GHCID process
stopGHCIDProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
stopGHCIDProcess registry args = do
  logInfo "Stopping GHCID process"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid.stop"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success stopArgs -> do
          let cabalURI = stopCabalURI stopArgs
              force = stopForce stopArgs

          -- Look up the process
          existing <- PR.getGHCIDProcess registry cabalURI
          case existing of
            Nothing -> return $ Left "No GHCID process running for this project"
            Just ghcidClient -> do
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

                  return $ Right $ T.pack $ show $ toJSON resultData

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
            (V.fromList [TextContent response])
            Nothing

-- | Restart a GHCID process
restartGHCIDProcess :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
restartGHCIDProcess registry args = do
  logInfo "Restarting GHCID process"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid.restart"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success restartArgs -> do
          let cabalURI = restartCabalURI restartArgs
              newWorkDir = restartWorkDir restartArgs

          -- Look up the existing process
          existing <- PR.getGHCIDProcess registry cabalURI
          oldProcessId <- case existing of
            Nothing -> return Nothing
            Just ghcidClient -> do
              -- Stop the existing process
              stopResult <- PR.stopGHCIDProcess registry cabalURI
              case stopResult of
                Left _ -> return Nothing -- Continue with restart anyway
                Right _ -> return $ Just "old_process"

          -- Start new process
          let workDir = maybe (T.unpack $ getCabalURI cabalURI) id newWorkDir

          startResult <- PR.startGHCIDProcess registry cabalURI workDir

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

              return $ Right $ T.pack $ show $ toJSON resultData

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
            (V.fromList [TextContent response])
            Nothing

-- | Get status of a GHCID process
getGHCIDStatus :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getGHCIDStatus registry args = do
  logInfo "Getting GHCID process status"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid.status"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success statusArgs -> do
          let cabalURI = statusCabalURI statusArgs

          -- Look up the process
          existing <- PR.getGHCIDProcess registry cabalURI
          status <- case existing of
            Nothing -> return Nothing
            Just handle -> Just <$> PR.getProcessStatus handle

          let resultData =
                ProcessStatusResult
                  { processStatus = status,
                    processUptime = case existing of
                      Nothing -> Nothing
                      Just _ -> Just "unknown" -- Could implement uptime tracking
                  }

          return $ Right $ T.pack $ show $ toJSON resultData

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
            (V.fromList [TextContent response])
            Nothing

-- | Get messages from a GHCID process
getGHCIDMessages :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
getGHCIDMessages registry args = do
  logInfo "Getting GHCID messages"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid.messages"
      Just argsObject -> case fromJSON (Object argsObject) of
        Data.Aeson.Error err -> return $ Left $ T.pack $ "Invalid arguments: " <> err
        Success messagesArgs -> do
          let cabalURI = messagesCabalURI messagesArgs
              filterReq = messagesFilter messagesArgs
              count = messagesCount messagesArgs

          -- Look up the process
          existing <- PR.getGHCIDProcess registry cabalURI
          case existing of
            Nothing -> return $ Left "No GHCID process running for this project"
            Just ghcidClient -> do
              -- Get messages from the client
              messages <- case existing of
                Nothing -> return []
                Just handle -> T.lines <$> PR.getBufferedOutput handle

              -- Apply filtering if requested
              filteredMessages <- case filterReq of
                Nothing -> return messages
                Just filter -> do
                  filtered <- applyShellFilter (T.unlines messages) filter
                  case filtered of
                    Left err -> return []
                    Right output -> return $ T.lines output

              -- Limit count if requested
              let limitedMessages = case count of
                    Nothing -> filteredMessages
                    Just n -> take n filteredMessages

              timestamp <- getCurrentTime
              let resultData =
                    MessagesResult
                      { messagesOutput = T.unlines limitedMessages,
                        messagesLines = limitedMessages,
                        messagesTimestamp = timestamp
                      }

              return $ Right $ T.pack $ show $ toJSON resultData

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
            (V.fromList [TextContent response])
            Nothing

-- | List all GHCID processes
listGHCIDProcesses :: ProcessRegistry -> Maybe Data.Aeson.Object -> IO ToolsCallResponse
listGHCIDProcesses registry args = do
  logInfo "Listing GHCID processes"

  result <- try $ do
    case args of
      Nothing -> return $ Left "Missing arguments for ghcid.list"
      Just argsObject -> case fromJSON (Object argsObject) of
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

          return $ Right $ T.pack $ show $ toJSON resultData

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
    Right (Right response) -> do
      return $
        ToolsCallResponse $
          ToolCallResult
            (V.fromList [TextContent response])
            Nothing
