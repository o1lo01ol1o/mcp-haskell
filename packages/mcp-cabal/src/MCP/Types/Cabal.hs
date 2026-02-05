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

module MCP.Types.Cabal
  ( -- * Cabal Tool Request Types
    CabalToolName(..)
  , StartCabalTestArgs(..)
  , StartCabalTestOptions(..)
  , StopCabalTestArgs(..)
  , GetMessagesArgs(..)
  , ListProcessesArgs(..)
  , ProcessStatusArgs(..)

    -- * Cabal Tool Response Types
  , StartCabalTestResult(..)
  , StopCabalTestResult(..)
  , MessagesResult(..)
  , ProcessListResult(..)
  , ProcessStatusResult(..)

    -- * Tool Definitions
  , cabalTools

    -- * Utilities
  , toolNameToText
  , textToToolName
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Cabal.Filter (FilterRequest)
import Cabal.ProcessRegistry (CabalURI (..), CabalTestStatus (..))
import MCP.SDK.Types (Tool (..))

-- | Cabal tool names.
data CabalToolName
  = StartCabalTest
  | StopCabalTest
  | GetMessages
  | ListProcesses
  | ProcessStatus
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CabalToolName

instance FromJSON CabalToolName

-- | Convert tool name to text.
toolNameToText :: CabalToolName -> Text
toolNameToText StartCabalTest = "cabal-test-start"
toolNameToText StopCabalTest = "cabal-test-stop"
toolNameToText GetMessages = "cabal-test-messages"
toolNameToText ListProcesses = "cabal-test-list"
toolNameToText ProcessStatus = "cabal-test-status"

-- | Convert text to tool name.
textToToolName :: Text -> Maybe CabalToolName
textToToolName "cabal-test-start" = Just StartCabalTest
textToToolName "cabal-test-stop" = Just StopCabalTest
textToToolName "cabal-test-messages" = Just GetMessages
textToToolName "cabal-test-list" = Just ListProcesses
textToToolName "cabal-test-status" = Just ProcessStatus
textToToolName "cabal.test.start" = Just StartCabalTest
textToToolName "cabal.test.stop" = Just StopCabalTest
textToToolName "cabal.test.messages" = Just GetMessages
textToToolName "cabal.test.list" = Just ListProcesses
textToToolName "cabal.test.status" = Just ProcessStatus
textToToolName _ = Nothing

-- | Start cabal test process arguments.
data StartCabalTestArgs = StartCabalTestArgs
  { startCabalURI :: CabalURI
  , startWorkDir :: FilePath
  , startTarget :: Maybe Text
  , startOptions :: Maybe StartCabalTestOptions
  } deriving (Show, Eq, Generic)

parseCabalURIField :: Object -> Parser CabalURI
parseCabalURIField o = do
  camel <- o .:? "cabalURI"
  snake <- o .:? "cabal_uri"
  case camel <|> snake of
    Just txt -> pure (CabalURI txt)
    Nothing -> fail "Missing cabalURI"

parseWorkDirField :: Object -> Parser FilePath
parseWorkDirField o = do
  camel <- o .:? "workDir"
  snake <- o .:? "work_dir"
  case camel <|> snake of
    Just dir -> pure dir
    Nothing -> fail "Missing workDir"

parseTargetField :: Object -> Parser (Maybe Text)
parseTargetField o = do
  target <- o .:? "target"
  component <- o .:? "component"
  pure (target <|> component)

-- | Additional launch options supplied with a start request.
data StartCabalTestOptions = StartCabalTestOptions
  { startAdditionalArgs :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON StartCabalTestArgs where
  toJSON StartCabalTestArgs {..} =
    object
      [ "cabalURI" .= getCabalURI startCabalURI
      , "workDir" .= startWorkDir
      , "target" .= startTarget
      , "options" .= startOptions
      ]

instance FromJSON StartCabalTestArgs where
  parseJSON = withObject "StartCabalTestArgs" $ \o ->
    StartCabalTestArgs
      <$> parseCabalURIField o
      <*> parseWorkDirField o
      <*> parseTargetField o
      <*> o .:? "options"

instance ToJSON StartCabalTestOptions where
  toJSON StartCabalTestOptions {..} =
    object $
      if null startAdditionalArgs
        then []
        else ["args" .= startAdditionalArgs]

instance FromJSON StartCabalTestOptions where
  parseJSON = withObject "StartCabalTestOptions" $ \o ->
    StartCabalTestOptions
      <$> o .:? "args" .!= []

-- | Stop cabal test process arguments.
data StopCabalTestArgs = StopCabalTestArgs
  { stopCabalURI :: CabalURI
  , stopForce :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON StopCabalTestArgs where
  toJSON StopCabalTestArgs {..} =
    object
      [ "cabalURI" .= getCabalURI stopCabalURI
      , "force" .= stopForce
      ]

instance FromJSON StopCabalTestArgs where
  parseJSON = withObject "StopCabalTestArgs" $ \o ->
    StopCabalTestArgs
      <$> parseCabalURIField o
      <*> o .:? "force" .!= False

-- | Get messages arguments.
data GetMessagesArgs = GetMessagesArgs
  { messagesCabalURI :: CabalURI
  , messagesFilter :: Maybe FilterRequest
  , messagesCount :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON GetMessagesArgs where
  toJSON GetMessagesArgs {..} =
    object
      [ "cabalURI" .= getCabalURI messagesCabalURI
      , "filter" .= messagesFilter
      , "count" .= messagesCount
      ]

instance FromJSON GetMessagesArgs where
  parseJSON = withObject "GetMessagesArgs" $ \o ->
    GetMessagesArgs
      <$> parseCabalURIField o
      <*> o .:? "filter"
      <*> o .:? "count"

-- | List processes arguments.
data ListProcessesArgs = ListProcessesArgs
  { listIncludeStatus :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ListProcessesArgs where
  toJSON ListProcessesArgs {..} =
    object
      [ "includeStatus" .= listIncludeStatus
      ]

instance FromJSON ListProcessesArgs where
  parseJSON = withObject "ListProcessesArgs" $ \o -> do
    camel <- o .:? "includeStatus"
    snake <- o .:? "include_status"
    let include = fromMaybe False (camel <|> snake)
    pure (ListProcessesArgs include)

-- | Get process status arguments.
data ProcessStatusArgs = ProcessStatusArgs
  { statusCabalURI :: CabalURI
  } deriving (Show, Eq, Generic)

instance ToJSON ProcessStatusArgs where
  toJSON ProcessStatusArgs {..} =
    object
      [ "cabalURI" .= getCabalURI statusCabalURI
      ]

instance FromJSON ProcessStatusArgs where
  parseJSON = withObject "ProcessStatusArgs" $ \o ->
    ProcessStatusArgs
      <$> parseCabalURIField o

-- | Tool result types.

data StartCabalTestResult = StartCabalTestResult
  { startSuccess :: Bool
  , startMessage :: Text
  , startProcessId :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON StartCabalTestResult

instance FromJSON StartCabalTestResult

data StopCabalTestResult = StopCabalTestResult
  { stopSuccess :: Bool
  , stopMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON StopCabalTestResult

instance FromJSON StopCabalTestResult

data MessagesResult = MessagesResult
  { messagesOutput :: Text
  , messagesLines :: [Text]
  , messagesTimestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON MessagesResult

instance FromJSON MessagesResult

data ProcessListResult = ProcessListResult
  { processURIs :: [CabalURI]
  , processStatuses :: Maybe [(CabalURI, CabalTestStatus)]
  } deriving (Show, Eq, Generic)

instance ToJSON ProcessListResult where
  toJSON ProcessListResult {..} =
    object
      [ "processURIs" .= map getCabalURI processURIs
      , "processStatuses" .= fmap (map (\(uri, status) -> object ["uri" .= getCabalURI uri, "status" .= status])) processStatuses
      ]

instance FromJSON ProcessListResult where
  parseJSON = withObject "ProcessListResult" $ \o ->
    ProcessListResult
      <$> (map CabalURI <$> o .: "processURIs")
      <*> o .:? "processStatuses"

data ProcessStatusResult = ProcessStatusResult
  { processStatus :: Maybe CabalTestStatus
  , processUptime :: Maybe Text
  , processLatestMessage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ProcessStatusResult

instance FromJSON ProcessStatusResult

-- | Tool definitions for MCP protocol.
cabalTools :: [Tool]
cabalTools =
  [ Tool
      { toolNameField = "cabal-test-start"
      , toolDescription =
          Just
            "Start a cabal test process (default uses an isolated --builddir under $TMPDIR/mcp-cache/cabal-test/ or MCP_CACHE_DIR)"
      , toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text))
            , ( "properties"
              , toJSON $
                  KM.fromList
                    [ ( "cabalURI"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      )
                    , ( "workDir"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Working directory for the project" :: Text))
                            ]
                      )
                    , ( "target"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Optional cabal test target (e.g. test:unit)" :: Text))
                            ]
                      )
                    , ( "options"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("object" :: Text))
                            , ( "properties"
                              , toJSON $
                                  KM.fromList
                                    [ ( "args"
                                      , toJSON $
                                          KM.fromList
                                            [ ("type", toJSON ("array" :: Text))
                                            , ("items", toJSON $ KM.fromList [("type", toJSON ("string" :: Text))])
                                            , ("description", toJSON ("Additional arguments appended to cabal test" :: Text))
                                            ]
                                      )
                                    ]
                              )
                            , ("description", toJSON ("Additional cabal test options" :: Text))
                            ]
                      )
                    ]
              )
            , ("required", toJSON (["cabalURI", "workDir"] :: [Text]))
            ]
      }
  , Tool
      { toolNameField = "cabal-test-stop"
      , toolDescription = Just "Stop a running cabal test process"
      , toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text))
            , ( "properties"
              , toJSON $
                  KM.fromList
                    [ ( "cabalURI"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      )
                    , ( "force"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("boolean" :: Text))
                            , ("description", toJSON ("Force stop even if process is busy" :: Text))
                            , ("default", toJSON False)
                            ]
                      )
                    ]
              )
            , ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      }
  , Tool
      { toolNameField = "cabal-test-messages"
      , toolDescription = Just "Get output from a cabal test process"
      , toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text))
            , ( "properties"
              , toJSON $
                  KM.fromList
                    [ ( "cabalURI"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      )
                    , ( "filter"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("object" :: Text))
                            , ("description", toJSON ("Filter options for messages" :: Text))
                            ]
                      )
                    , ( "count"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("integer" :: Text))
                            , ("description", toJSON ("Maximum number of messages to return" :: Text))
                            , ("minimum", toJSON (1 :: Int))
                            ]
                      )
                    ]
              )
            , ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      }
  , Tool
      { toolNameField = "cabal-test-list"
      , toolDescription = Just "List all known cabal test processes"
      , toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text))
            , ( "properties"
              , toJSON $
                  KM.fromList
                    [ ( "includeStatus"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("boolean" :: Text))
                            , ("description", toJSON ("Include detailed status information" :: Text))
                            , ("default", toJSON False)
                            ]
                      )
                    ]
              )
            ]
      }
  , Tool
      { toolNameField = "cabal-test-status"
      , toolDescription = Just "Get status of a specific cabal test process"
      , toolInputSchema =
          KM.fromList
            [ ("type", toJSON ("object" :: Text))
            , ( "properties"
              , toJSON $
                  KM.fromList
                    [ ( "cabalURI"
                      , toJSON $
                          KM.fromList
                            [ ("type", toJSON ("string" :: Text))
                            , ("description", toJSON ("Cabal project URI identifier" :: Text))
                            ]
                      )
                    ]
              )
            , ("required", toJSON (["cabalURI"] :: [Text]))
            ]
      }
  ]
