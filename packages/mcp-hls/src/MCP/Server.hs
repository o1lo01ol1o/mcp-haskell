{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MCP.Server
  ( runMCPServer,
    runHLSServer,
    HLSServerState,
    initializeHLSServerState,
    withHLSServerState
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (bracket, throw, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    fromJSON,
    Object,
    Result (Error, Success),
    Value (Object),
    encode,
    object,
    withObject,
    (.=),
    (.:?)
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import HLS.Config (HLSServerConfig (..), defaultServerConfig)
import qualified HLS.Process as HLS
import HLS.Signals (ShutdownConfig (..), defaultShutdownConfig, withGracefulShutdown)
import MCP.SDK.Capabilities (ServerCapabilityBuilder (..), ServerCapabilities, ToolsCapability (..), buildServerCapabilities)
import MCP.SDK.Error (MCPError (..))
import qualified MCP.SDK.Server as Server
import MCP.SDK.Server.API (registerTool)
import MCP.SDK.Server.Monad (ServerConfig (..), ServerM, runServerM)
import MCP.SDK.Transport (wrapTransport)
import MCP.SDK.Transport.Stdio (createStdioTransport)
import MCP.SDK.Types
  ( Content (TextContent),
    Tool (..),
    ToolCallResult (..),
    ToolDefinition (..),
    ToolHandlerContext,
    ToolsCallResponse (..)
  )
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (stdin, stdout)
import System.Posix.Process (exitImmediately, getParentProcessID)
import System.Posix.Types (CPid (..))
import qualified Utils.Logging as Log

-- | Interval used to monitor the parent process (microseconds).
parentCheckIntervalMicros :: Int
parentCheckIntervalMicros = 1000000

-- | Shared server state for managing the HLS process lifecycle.
data HLSServerState = HLSServerState
  { hlsProcessVar :: TVar (Maybe HLS.HLSProcess),
    serverConfig :: HLSServerConfig
  }

-- | Initialise server state.
initializeHLSServerState :: HLSServerConfig -> IO HLSServerState
initializeHLSServerState cfg = do
  processVar <- newTVarIO Nothing
  pure
    HLSServerState
      { hlsProcessVar = processVar,
        serverConfig = cfg
      }

-- | Ensure the HLS process is cleaned up when leaving the bracket.
withHLSServerState :: HLSServerConfig -> (HLSServerState -> IO a) -> IO a
withHLSServerState cfg =
  bracket (initializeHLSServerState cfg) (\state -> stopActiveProcess state)

-- | Entry point used by the executable. Derives the workspace from the current directory.
runMCPServer :: IO ()
runMCPServer = do
  cwd <- getCurrentDirectory
  let cfg = defaultServerConfig {defaultWorkspace = cwd}
  runHLSServer cfg

-- | Run the MCP server with an explicit configuration.
runHLSServer :: HLSServerConfig -> IO ()
runHLSServer cfg = do
  Log.logInfo "Starting MCP-HLS server with SDK integration"

  let shutdownConfig =
        defaultShutdownConfig
          { shutdownGracePeriod = 30,
            shutdownForceDelay = 10,
            shutdownLogOutput = True,
            shutdownExitCode = ExitSuccess
          }

  result <-
    try @MCPError $
      withHLSServerState cfg $ \state ->
        withParentDeathWatcher state $
          withGracefulShutdown shutdownConfig $ do
            transport <- createStdioTransport stdin stdout

            let sdkConfig =
                  Server.defaultServerConfig
                    { serverInstructions = Just (instructionsMessage (serverConfig state))
                    }

            serverResult <-
              Server.finalizeServer $
                Server.withServerConfig sdkConfig $
                  Server.withServerCapabilities createServerCapabilities $
                    Server.withServerInfo (serverName (serverConfig state)) (serverVersion (serverConfig state)) $
                      Server.withServerTransport (wrapTransport transport) $
                        Server.buildServer

            case serverResult of
              Left err -> throw err
              Right serverEnv -> do
                runServerM serverEnv $ registerHLSTools state
                Server.runServer serverEnv

  case result of
    Left err -> do
      Log.logError $ "HLS MCP server failed: " <> T.pack (show err)
    Right _ ->
      Log.logInfo "HLS MCP server terminated cleanly"

-- | Capabilities exposed by the HLS server.
createServerCapabilities :: ServerCapabilities
createServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability Nothing,
        scbResources = Nothing,
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Register HLS-specific tools with the MCP SDK server.
registerHLSTools :: HLSServerState -> ServerM ()
registerHLSTools state = do
  liftIO $ Log.logInfo "Registering HLS MCP tools"

  registerTool
    "hls-start"
    ToolDefinition
      { tdTool =
          Tool
            { toolNameField = "hls-start",
              toolDescription = Just "Start the Haskell Language Server process",
              toolInputSchema =
                case
                    object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "work_dir"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("Workspace directory to launch HLS in" :: Text)
                                  ],
                              "executable"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("HLS executable to launch (defaults to haskell-language-server-wrapper)" :: Text)
                                  ],
                              "args"
                                .= object
                                  [ "type" .= ("array" :: Text),
                                    "items" .= object ["type" .= ("string" :: Text)],
                                    "description" .= ("Additional arguments for the HLS process" :: Text)
                                  ],
                              "log_level"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("Log level for HLS (debug|info|warn|error)" :: Text)
                                  ],
                              "enable_logging"
                                .= object
                                  [ "type" .= ("boolean" :: Text),
                                    "description" .= ("Enable file logging for HLS" :: Text)
                                  ]
                            ]
                      ]
                  of
                    Object obj -> obj
                    _ -> KM.empty
            },
        tdHandler = Just $ handleStartHLSTool state
      }

  registerTool
    "hls-stop"
    ToolDefinition
      { tdTool =
          Tool
            { toolNameField = "hls-stop",
              toolDescription = Just "Stop the running Haskell Language Server process",
              toolInputSchema =
                case
                    object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "force"
                                .= object
                                  [ "type" .= ("boolean" :: Text),
                                    "description" .= ("Attempt a forceful stop if graceful shutdown fails (currently informational)" :: Text)
                                  ]
                            ]
                      ]
                  of
                    Object obj -> obj
                    _ -> KM.empty
            },
        tdHandler = Just $ handleStopHLSTool state
      }

  registerTool
    "hls-restart"
    ToolDefinition
      { tdTool =
          Tool
            { toolNameField = "hls-restart",
              toolDescription = Just "Restart the Haskell Language Server process",
              toolInputSchema =
                case
                    object
                      [ "type" .= ("object" :: Text),
                        "properties"
                          .= object
                            [ "work_dir"
                                .= object
                                  [ "type" .= ("string" :: Text),
                                    "description" .= ("Optional workspace directory for the restarted process" :: Text)
                                  ]
                            ]
                      ]
                  of
                    Object obj -> obj
                    _ -> KM.empty
            },
        tdHandler = Just $ handleRestartHLSTool state
      }

  registerTool
    "hls-status"
    ToolDefinition
      { tdTool =
          Tool
            { toolNameField = "hls-status",
              toolDescription = Just "Retrieve the status of the managed HLS process",
              toolInputSchema =
                case
                    object
                      [ "type" .= ("object" :: Text)
                      ]
                  of
                    Object obj -> obj
                    _ -> KM.empty
            },
        tdHandler = Just $ handleStatusHLSTool state
      }

  liftIO $ Log.logInfo "HLS MCP tools registered successfully"

-- Tool argument types ---------------------------------------------------------

data StartHLSArgs = StartHLSArgs
  { startWorkDir :: Maybe FilePath,
    startExecutable :: Maybe String,
    startArguments :: Maybe [String],
    startLogLevel :: Maybe Text,
    startEnableLogging :: Maybe Bool
  }

instance FromJSON StartHLSArgs where
  parseJSON =
    withObject "StartHLSArgs" $ \o ->
      StartHLSArgs
        <$> o .:? "work_dir"
        <*> o .:? "executable"
        <*> o .:? "args"
        <*> o .:? "log_level"
        <*> o .:? "enable_logging"

data StopHLSArgs = StopHLSArgs
  { stopForce :: Maybe Bool
  }

instance FromJSON StopHLSArgs where
  parseJSON =
    withObject "StopHLSArgs" $ \o ->
      StopHLSArgs
        <$> o .:? "force"

data RestartHLSArgs = RestartHLSArgs
  { restartWorkDir :: Maybe FilePath
  }

instance FromJSON RestartHLSArgs where
  parseJSON =
    withObject "RestartHLSArgs" $ \o ->
      RestartHLSArgs
        <$> o .:? "work_dir"

defaultStartArgs :: StartHLSArgs
defaultStartArgs =
  StartHLSArgs
    { startWorkDir = Nothing,
      startExecutable = Nothing,
      startArguments = Nothing,
      startLogLevel = Nothing,
      startEnableLogging = Nothing
    }

defaultStopArgs :: StopHLSArgs
defaultStopArgs = StopHLSArgs {stopForce = Nothing}

defaultRestartArgs :: RestartHLSArgs
defaultRestartArgs = RestartHLSArgs {restartWorkDir = Nothing}

parseStartArgs :: Maybe Object -> Either MCPError StartHLSArgs
parseStartArgs Nothing = Right defaultStartArgs
parseStartArgs (Just obj) =
  case fromJSON (Object obj) of
    Success parsed -> Right parsed
    Error err -> Left $ ValidationError $ "Invalid arguments for hls-start: " <> T.pack err

parseStopArgs :: Maybe Object -> Either MCPError StopHLSArgs
parseStopArgs Nothing = Right defaultStopArgs
parseStopArgs (Just obj) =
  case fromJSON (Object obj) of
    Success parsed -> Right parsed
    Error err -> Left $ ValidationError $ "Invalid arguments for hls-stop: " <> T.pack err

parseRestartArgs :: Maybe Object -> Either MCPError RestartHLSArgs
parseRestartArgs Nothing = Right defaultRestartArgs
parseRestartArgs (Just obj) =
  case fromJSON (Object obj) of
    Success parsed -> Right parsed
    Error err -> Left $ ValidationError $ "Invalid arguments for hls-restart: " <> T.pack err

-- Tool handlers ---------------------------------------------------------------

handleStartHLSTool :: HLSServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStartHLSTool state _ args =
  case parseStartArgs args of
    Left err -> pure $ Left err
    Right parsed -> do
      result <- liftIO $ startHLSWithArgs state parsed
      pure $ fmap jsonResponse result

handleStopHLSTool :: HLSServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStopHLSTool state _ args =
  case parseStopArgs args of
    Left err -> pure $ Left err
    Right StopHLSArgs {..} -> do
      when (fromMaybe False stopForce) $
        liftIO $ Log.logWarn "Forceful stop requested; attempting graceful shutdown first"
      result <- liftIO $ stopHLSProcessAction state
      pure $ fmap jsonResponse result

handleRestartHLSTool :: HLSServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleRestartHLSTool state _ args =
  case parseRestartArgs args of
    Left err -> pure $ Left err
    Right parsed -> do
      result <- liftIO $ restartHLSWithArgs state parsed
      pure $ fmap jsonResponse result

handleStatusHLSTool :: HLSServerState -> ToolHandlerContext ServerM -> Maybe Object -> ServerM (Either MCPError ToolsCallResponse)
handleStatusHLSTool state _ _ = do
  payload <- liftIO $ statusPayload state
  pure $ Right (jsonResponse payload)

-- Core process management -----------------------------------------------------

startHLSWithArgs :: HLSServerState -> StartHLSArgs -> IO (Either MCPError Value)
startHLSWithArgs HLSServerState {..} StartHLSArgs {..} = do
  existing <- readTVarIO hlsProcessVar
  case existing of
    Just _ -> pure $ Left $ ValidationError "HLS process is already running"
    Nothing -> do
      let HLSServerConfig {..} = serverConfig
          workDir = fromMaybe defaultWorkspace startWorkDir
          command = fromMaybe defaultExecutable startExecutable
          finalArgs = fromMaybe defaultArguments startArguments
          finalLogLevel = fromMaybe defaultLogLevel startLogLevel
          finalEnableLogging = fromMaybe defaultEnableLogging startEnableLogging
          baseConfig = HLS.defaultHLSConfig workDir
          processConfig =
            baseConfig
              { HLS.hlsCommand = command,
                HLS.hlsArgs = finalArgs,
                HLS.hlsLogLevel = finalLogLevel,
                HLS.hlsEnableLogging = finalEnableLogging
              }

      Log.logInfo $ "Launching HLS process in " <> T.pack workDir
      Log.logDebug $
        "HLS command: "
          <> T.pack command
          <> " "
          <> T.unwords (map T.pack finalArgs)

      startResult <- HLS.startHLSProcess processConfig
      case startResult of
        Left err -> do
          Log.logError $ "Failed to start HLS: " <> err
          pure $ Left $ ToolExecutionError err
        Right process -> do
          atomically $ writeTVar hlsProcessVar (Just process)
          Log.logInfo "HLS process started successfully"
          let payload =
                object
                  [ "status" .= ("started" :: Text),
                    "workDir" .= workDir,
                    "command" .= command,
                    "args" .= finalArgs,
                    "logLevel" .= finalLogLevel,
                    "enableLogging" .= finalEnableLogging
                  ]
          pure $ Right payload

stopHLSProcessAction :: HLSServerState -> IO (Either MCPError Value)
stopHLSProcessAction HLSServerState {..} = do
  existing <- readTVarIO hlsProcessVar
  case existing of
    Nothing -> pure $ Left $ ValidationError "No HLS process is currently running"
    Just process -> do
      let cfg = HLS.hlsConfig process
      Log.logInfo $ "Stopping HLS process in " <> T.pack (HLS.hlsWorkDir cfg)
      stopResult <- HLS.stopHLSProcess process
      case stopResult of
        Left err -> do
          Log.logWarn $ "Failed to stop HLS: " <> err
          pure $ Left $ ToolExecutionError err
        Right _ -> do
          atomically $ writeTVar hlsProcessVar Nothing
          Log.logInfo "HLS process stopped"
          pure $
            Right $
              object
                [ "status" .= ("stopped" :: Text),
                  "workDir" .= HLS.hlsWorkDir cfg
                ]

restartHLSWithArgs :: HLSServerState -> RestartHLSArgs -> IO (Either MCPError Value)
restartHLSWithArgs state@HLSServerState {hlsProcessVar = processVar} RestartHLSArgs {..} = do
  existing <- readTVarIO processVar
  case existing of
    Nothing -> pure $ Left $ ValidationError "No HLS process is running to restart"
    Just process -> do
      case restartWorkDir of
        Nothing -> do
          Log.logInfo "Restarting HLS process with existing configuration"
          restartResult <- HLS.restartHLSProcess process
          case restartResult of
            Left err -> do
              Log.logWarn $ "Failed to restart HLS: " <> err
              pure $ Left $ ToolExecutionError err
            Right _ -> do
              let cfg = HLS.hlsConfig process
              Log.logInfo "HLS process restarted"
              pure $
                Right $
                  object
                    [ "status" .= ("restarted" :: Text),
                      "workDir" .= HLS.hlsWorkDir cfg,
                      "command" .= HLS.hlsCommand cfg,
                      "args" .= HLS.hlsArgs cfg
                    ]
        Just newDir -> do
          Log.logInfo $ "Restarting HLS process in new workspace: " <> T.pack newDir
          stopResult <- HLS.stopHLSProcess process
          case stopResult of
            Left err -> do
              Log.logWarn $ "Failed to stop HLS before restart: " <> err
              pure $ Left $ ToolExecutionError err
            Right _ -> do
              atomically $ writeTVar processVar Nothing
              let prevCfg = HLS.hlsConfig process
                  freshArgs =
                    defaultStartArgs
                      { startWorkDir = Just newDir,
                        startExecutable = Just (HLS.hlsCommand prevCfg),
                        startArguments = Just (HLS.hlsArgs prevCfg),
                        startLogLevel = Just (HLS.hlsLogLevel prevCfg),
                        startEnableLogging = Just (HLS.hlsEnableLogging prevCfg)
                      }
              startHLSWithArgs state freshArgs

statusPayload :: HLSServerState -> IO Value
statusPayload HLSServerState {..} = do
  existing <- readTVarIO hlsProcessVar
  case existing of
    Nothing ->
      pure $
        object
          [ "status" .= ("not_running" :: Text)
          ]
    Just process -> do
      let cfg = HLS.hlsConfig process
      status <- HLS.getHLSStatus process
      let baseFields =
            [ "status" .= statusToText status,
              "workDir" .= HLS.hlsWorkDir cfg,
              "command" .= HLS.hlsCommand cfg,
              "args" .= HLS.hlsArgs cfg,
              "logLevel" .= HLS.hlsLogLevel cfg,
              "enableLogging" .= HLS.hlsEnableLogging cfg
            ]
          extraFields =
            case status of
              HLS.HLSError err -> ["error" .= err]
              HLS.HLSTerminated info -> ["signal" .= T.pack (show info)]
              _ -> []
      pure $ object (baseFields ++ extraFields)

statusToText :: HLS.HLSStatus -> Text
statusToText status =
  case status of
    HLS.HLSStopped -> "stopped"
    HLS.HLSStarting -> "starting"
    HLS.HLSRunning -> "running"
    HLS.HLSError _ -> "error"
    HLS.HLSTerminated _ -> "terminated"

-- | Stop any active HLS process, logging but ignoring failures.
stopActiveProcess :: HLSServerState -> IO ()
stopActiveProcess HLSServerState {..} = do
  existing <- readTVarIO hlsProcessVar
  case existing of
    Nothing -> pure ()
    Just process -> do
      result <- HLS.stopHLSProcess process
      case result of
        Left err -> Log.logWarn $ "Failed to stop HLS during cleanup: " <> err
        Right _ -> do
          atomically $ writeTVar hlsProcessVar Nothing
          Log.logInfo "HLS process stopped during cleanup"

-- Parent monitoring -----------------------------------------------------------

withParentDeathWatcher :: HLSServerState -> IO a -> IO a
withParentDeathWatcher state action = do
  initialParent <- getParentProcessID
  let CPid rawParent = initialParent
  if rawParent <= 1
    then action
    else bracket (async (watch initialParent)) cancel (const action)
  where
    watch parentPid = loop
      where
        loop = do
          threadDelay parentCheckIntervalMicros
          currentParent <- getParentProcessID
          if currentParent == parentPid
            then loop
            else do
              Log.logWarn "Detected parent process exit; shutting down HLS process"
              stopActiveProcess state
              exitImmediately (ExitFailure 130)

-- Helpers ---------------------------------------------------------------------

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

jsonResponse :: Value -> ToolsCallResponse
jsonResponse payload =
  ToolsCallResponse $
    ToolCallResult
      (V.singleton (TextContent (jsonText payload)))
      Nothing
