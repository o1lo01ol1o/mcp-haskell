{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.Types.GHCID
  ( -- * GHCID Tool Request Types
    GHCIDToolName (..),
    StartGHCIDArgs (..),
    StopGHCIDArgs (..),
    GetMessagesArgs (..),
    ListProcessesArgs (..),
    ProcessStatusArgs (..),
    RestartProcessArgs (..),

    -- * GHCID Tool Response Types
    StartGHCIDResult (..),
    StopGHCIDResult (..),
    MessagesResult (..),
    ProcessListResult (..),
    ProcessStatusResult (..),
    RestartProcessResult (..),

    -- * Tool Definitions
    ghcidTools,

    -- * Utilities
    toolNameToText,
    textToToolName,
  )
where

import Control.Monad (when)
import Data.Aeson
-- MCP SDK imports

-- Internal imports

import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHCID.Filter (FilterRequest)
import GHCID.ProcessRegistry (CabalURI (..), GHCIDHandle, GHCIDStatus (..), ProcessRegistry)
import MCP.SDK.Types (Content (TextContent), Tool (..), ToolCallResult (..), ToolsCallRequest (..), ToolsCallResponse (..))

-- | GHCID tool names
data GHCIDToolName
  = StartGHCID
  | StopGHCID
  | GetMessages
  | ListProcesses
  | ProcessStatus
  | RestartProcess
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GHCIDToolName

instance FromJSON GHCIDToolName

-- | Convert tool name to text
toolNameToText :: GHCIDToolName -> Text
toolNameToText StartGHCID = "ghcid.start"
toolNameToText StopGHCID = "ghcid.stop"
toolNameToText GetMessages = "ghcid.messages"
toolNameToText ListProcesses = "ghcid.list"
toolNameToText ProcessStatus = "ghcid.status"
toolNameToText RestartProcess = "ghcid.restart"

-- | Convert text to tool name
textToToolName :: Text -> Maybe GHCIDToolName
textToToolName "ghcid.start" = Just StartGHCID
textToToolName "ghcid.stop" = Just StopGHCID
textToToolName "ghcid.messages" = Just GetMessages
textToToolName "ghcid.list" = Just ListProcesses
textToToolName "ghcid.status" = Just ProcessStatus
textToToolName "ghcid.restart" = Just RestartProcess
textToToolName _ = Nothing

-- | Tool argument types

-- | Start GHCID process arguments
data StartGHCIDArgs = StartGHCIDArgs
  { startCabalURI :: CabalURI,
    startWorkDir :: FilePath,
    startOptions :: Maybe Value -- Additional options
  }
  deriving (Show, Eq, Generic)

instance ToJSON StartGHCIDArgs where
  toJSON StartGHCIDArgs {..} =
    object
      [ "cabalURI" .= getCabalURI startCabalURI,
        "workDir" .= startWorkDir,
        "options" .= startOptions
      ]

instance FromJSON StartGHCIDArgs where
  parseJSON = withObject "StartGHCIDArgs" $ \o ->
    StartGHCIDArgs
      <$> (CabalURI <$> o .: "cabalURI")
      <*> o .: "workDir"
      <*> o .:? "options"

-- | Stop GHCID process arguments
data StopGHCIDArgs = StopGHCIDArgs
  { stopCabalURI :: CabalURI,
    stopForce :: Bool -- Force stop if graceful fails
  }
  deriving (Show, Eq, Generic)

instance ToJSON StopGHCIDArgs where
  toJSON StopGHCIDArgs {..} =
    object
      [ "cabalURI" .= getCabalURI stopCabalURI,
        "force" .= stopForce
      ]

instance FromJSON StopGHCIDArgs where
  parseJSON = withObject "StopGHCIDArgs" $ \o ->
    StopGHCIDArgs
      <$> (CabalURI <$> o .: "cabalURI")
      <*> o .: "force"

-- | Get messages arguments
data GetMessagesArgs = GetMessagesArgs
  { messagesCabalURI :: CabalURI,
    messagesFilter :: Maybe FilterRequest,
    messagesCount :: Maybe Int -- Limit number of messages
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetMessagesArgs where
  toJSON GetMessagesArgs {..} =
    object
      [ "cabalURI" .= getCabalURI messagesCabalURI,
        "filter" .= messagesFilter,
        "count" .= messagesCount
      ]

instance FromJSON GetMessagesArgs where
  parseJSON = withObject "GetMessagesArgs" $ \o ->
    GetMessagesArgs
      <$> (CabalURI <$> o .: "cabalURI")
      <*> o .:? "filter"
      <*> o .:? "count"

-- | List processes arguments
data ListProcessesArgs = ListProcessesArgs
  { listIncludeStatus :: Bool -- Include detailed status
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListProcessesArgs where
  toJSON ListProcessesArgs {..} =
    object
      [ "includeStatus" .= listIncludeStatus
      ]

instance FromJSON ListProcessesArgs where
  parseJSON = withObject "ListProcessesArgs" $ \o ->
    ListProcessesArgs
      <$> o .: "includeStatus"

-- | Get process status arguments
data ProcessStatusArgs = ProcessStatusArgs
  { statusCabalURI :: CabalURI
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProcessStatusArgs where
  toJSON ProcessStatusArgs {..} =
    object
      [ "cabalURI" .= getCabalURI statusCabalURI
      ]

instance FromJSON ProcessStatusArgs where
  parseJSON = withObject "ProcessStatusArgs" $ \o ->
    ProcessStatusArgs
      <$> (CabalURI <$> o .: "cabalURI")

-- | Restart process arguments
data RestartProcessArgs = RestartProcessArgs
  { restartCabalURI :: CabalURI,
    restartWorkDir :: Maybe FilePath -- New working directory
  }
  deriving (Show, Eq, Generic)

instance ToJSON RestartProcessArgs where
  toJSON RestartProcessArgs {..} =
    object
      [ "cabalURI" .= getCabalURI restartCabalURI,
        "workDir" .= restartWorkDir
      ]

instance FromJSON RestartProcessArgs where
  parseJSON = withObject "RestartProcessArgs" $ \o ->
    RestartProcessArgs
      <$> (CabalURI <$> o .: "cabalURI")
      <*> o .:? "workDir"

-- | Tool result types

-- | Start GHCID result
data StartGHCIDResult = StartGHCIDResult
  { startSuccess :: Bool,
    startMessage :: Text,
    startProcessId :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON StartGHCIDResult

instance FromJSON StartGHCIDResult

-- | Stop GHCID result
data StopGHCIDResult = StopGHCIDResult
  { stopSuccess :: Bool,
    stopMessage :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON StopGHCIDResult

instance FromJSON StopGHCIDResult

-- | Messages result
data MessagesResult = MessagesResult
  { messagesOutput :: Text,
    messagesLines :: [Text],
    messagesTimestamp :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON MessagesResult

instance FromJSON MessagesResult

-- | Process list result
data ProcessListResult = ProcessListResult
  { processURIs :: [CabalURI],
    processStatuses :: Maybe [(CabalURI, GHCIDStatus)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProcessListResult where
  toJSON ProcessListResult {..} =
    object
      [ "processURIs" .= map getCabalURI processURIs,
        "processStatuses" .= fmap (map (\(uri, status) -> object ["uri" .= getCabalURI uri, "status" .= status])) processStatuses
      ]

instance FromJSON ProcessListResult where
  parseJSON = withObject "ProcessListResult" $ \o ->
    ProcessListResult
      <$> (map CabalURI <$> o .: "processURIs")
      <*> o .:? "processStatuses"

-- | Process status result
data ProcessStatusResult = ProcessStatusResult
  { processStatus :: Maybe GHCIDStatus,
    processUptime :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ProcessStatusResult

instance FromJSON ProcessStatusResult

-- | Restart process result
data RestartProcessResult = RestartProcessResult
  { restartSuccess :: Bool,
    restartMessage :: Text,
    restartOldProcessId :: Maybe Text,
    restartNewProcessId :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON RestartProcessResult

instance FromJSON RestartProcessResult

-- | Tool definitions for MCP protocol
ghcidTools :: [Tool]
ghcidTools =
  [ Tool
      { toolNameField = "ghcid.start",
        toolDescription = Just "Start a new ghcid process for monitoring a Haskell project",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "cabalURI",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      ),
                      ( "workDir",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Working directory for the project" :: Text))
                            ]
                      ),
                      ( "options",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("object" :: Text)),
                              ("description", toJSON ("Additional GHCID configuration options" :: Text))
                            ]
                      )
                    ]
              ),
              ("required", toJSON (["cabalURI", "workDir"] :: [Text]))
            ]
      },
    Tool
      { toolNameField = "ghcid.stop",
        toolDescription = Just "Stop a running ghcid process",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "cabalURI",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      ),
                      ( "force",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("boolean" :: Text)),
                              ("description", toJSON ("Force stop even if process is busy" :: Text)),
                              ("default", toJSON False)
                            ]
                      )
                    ]
              ),
              ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      },
    Tool
      { toolNameField = "ghcid.messages",
        toolDescription = Just "Get compiler messages from a running ghcid process",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "cabalURI",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      ),
                      ( "filter",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("object" :: Text)),
                              ("description", toJSON ("Filter options for messages" :: Text))
                            ]
                      ),
                      ( "count",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("integer" :: Text)),
                              ("description", toJSON ("Maximum number of messages to return" :: Text)),
                              ("minimum", toJSON (1 :: Int))
                            ]
                      )
                    ]
              ),
              ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      },
    Tool
      { toolNameField = "ghcid.list",
        toolDescription = Just "List all active ghcid processes",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "includeStatus",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("boolean" :: Text)),
                              ("description", toJSON ("Include detailed status information" :: Text)),
                              ("default", toJSON False)
                            ]
                      )
                    ]
              )
            ]
      },
    Tool
      { toolNameField = "ghcid.status",
        toolDescription = Just "Get status of a specific ghcid process",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "cabalURI",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      )
                    ]
              ),
              ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      },
    Tool
      { toolNameField = "ghcid.restart",
        toolDescription = Just "Restart a ghcid process",
        toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text)),
              ( "properties",
                toJSON $
                  KM.fromList
                    [ ( "cabalURI",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      ),
                      ( "newWorkDir",
                        toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text)),
                              ("description", toJSON ("New working directory (optional)" :: Text))
                            ]
                      )
                    ]
              ),
              ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      }
  ]
