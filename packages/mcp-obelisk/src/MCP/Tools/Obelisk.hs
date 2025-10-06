{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.Tools.Obelisk
  ( executeObeliskTool
  ) where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), encode, fromJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Obelisk.ProcessRegistry
import Obelisk.Types
import MCP.SDK.Types

-- | Execute an Obelisk tool call, mapping from the generic request
-- coming from the MCP SDK to the process registry operations.
executeObeliskTool :: ProcessRegistry -> ToolsCallRequest -> IO ToolsCallResponse
executeObeliskTool registry ToolsCallRequest {..} =
  case toolName of
    "obelisk.list" -> do
      projects <- listProjects registry
      let result = ListResult (map unProjectId projects)
      pure $ successResponse (toJSON result)

    "obelisk.start" ->
      withArgs toolArguments $ \argsObj ->
        case fromJSON (Object argsObj) of
          Error err -> pure $ errorResponse $ "Invalid arguments for obelisk.start: " <> T.pack err
          Success StartArgs {..} -> do
            result <- startObeliskWatch registry (ProjectId startProjectPath)
            case result of
              Left errMsg -> pure $ errorResponse errMsg
              Right startRes -> pure $ successResponse (toJSON startRes)

    "obelisk.stop" ->
      withArgs toolArguments $ \argsObj ->
        case fromJSON (Object argsObj) of
          Error err -> pure $ errorResponse $ "Invalid arguments for obelisk.stop: " <> T.pack err
          Success StopArgs {..} -> do
            result <- stopObeliskWatch registry (ProjectId stopProjectPath)
            case result of
              Left errMsg -> pure $ errorResponse errMsg
              Right stopRes -> pure $ successResponse (toJSON stopRes)

    "obelisk.status" ->
      withArgs toolArguments $ \argsObj ->
        case fromJSON (Object argsObj) of
          Error err -> pure $ errorResponse $ "Invalid arguments for obelisk.status: " <> T.pack err
          Success StatusArgs {..} -> do
            let projectId = ProjectId statusProjectPath
            status <- getObeliskStatus registry projectId
            lastMsg <- getLastLogLine registry projectId
            pure $ successResponse $ toJSON StatusResult
              { statusState = status
              , statusLastMessage = lastMsg
              }

    "obelisk.messages" ->
      withArgs toolArguments $ \argsObj ->
        case fromJSON (Object argsObj) of
          Error err -> pure $ errorResponse $ "Invalid arguments for obelisk.messages: " <> T.pack err
          Success MessagesArgs {..} -> do
            result <- getObeliskMessages registry (ProjectId messagesProjectPath) messagesLimit messagesFilter
            case result of
              Left errMsg -> pure $ errorResponse errMsg
              Right messagesRes -> pure $ successResponse (toJSON messagesRes)

    other ->
      pure $ errorResponse $ "Unknown obelisk tool: " <> other
  where
    withArgs Nothing _ = pure $ errorResponse "Missing tool arguments"
    withArgs (Just obj) action = action obj

    successResponse value =
      ToolsCallResponse $
        ToolCallResult
          (V.singleton $ TextContent $ jsonToText value)
          Nothing

    errorResponse msg =
      ToolsCallResponse $
        ToolCallResult
          (V.singleton $ TextContent msg)
          (Just True)

    jsonToText = T.pack . L8.unpack . encode
