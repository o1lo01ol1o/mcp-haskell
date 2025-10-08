{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Obelisk.Types
  ( ProjectId (..)
  , ObeliskStatus (..)
  , StartResult (..)
  , StopResult (..)
  , StatusResult (..)
  , MessagesResult (..)
  , ListResult (..)
  , StartArgs (..)
  , StopArgs (..)
  , StatusArgs (..)
  , MessagesArgs (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Obelisk.Filter (FilterRequest)

-- | Identifier for an Obelisk project being watched.
newtype ProjectId = ProjectId { unProjectId :: FilePath }
  deriving (Show, Eq, Ord)

-- | Runtime status of an `ob watch` process.
data ObeliskStatus
  = ObStarting
  | ObRunning
  | ObStopped
  | ObErrored Text
  deriving (Show, Eq, Ord)

data StartResult = StartResult
  { startSuccess :: Bool
  , startMessage :: Text
  , startProject :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON StartResult where
  toJSON StartResult {..} =
    object
      [ "success" .= startSuccess
      , "message" .= startMessage
      , "projectPath" .= startProject
      ]

data StopResult = StopResult
  { stopSuccess :: Bool
  , stopMessage :: Text
  , stopProject :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON StopResult where
  toJSON StopResult {..} =
    object
      [ "success" .= stopSuccess
      , "message" .= stopMessage
      , "projectPath" .= stopProject
      ]

data StatusResult = StatusResult
  { statusState :: ObeliskStatus
  , statusLastMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON StatusResult where
  toJSON StatusResult {..} =
    let (statusText, statusError) =
          case statusState of
            ObStarting -> ("starting", Nothing)
            ObRunning -> ("running", Nothing)
            ObStopped -> ("stopped", Nothing)
            ObErrored err -> ("errored", Just err)
    in object
      [ "status" .= (statusText :: Text)
      , "error" .= statusError
      , "lastMessage" .= statusLastMessage
      ]

data MessagesResult = MessagesResult
  { messagesOutput :: Text
  , messagesLines :: [Text]
  , messagesTimestamp :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON MessagesResult where
  toJSON MessagesResult {..} =
    object
      [ "output" .= messagesOutput
      , "lines" .= messagesLines
      , "timestamp" .= messagesTimestamp
      ]

data ListResult = ListResult
  { listEntries :: [FilePath]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ListResult where
  toJSON ListResult {..} = object ["projects" .= listEntries]

-- | Arguments for each tool call
data StartArgs = StartArgs
  { startProjectPath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON StartArgs where
  parseJSON = withObject "StartArgs" $ \o ->
    StartArgs <$> o .: "projectPath"

instance ToJSON StartArgs where
  toJSON StartArgs {..} = object ["projectPath" .= startProjectPath]

data StopArgs = StopArgs
  { stopProjectPath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON StopArgs where
  parseJSON = withObject "StopArgs" $ \o ->
    StopArgs <$> o .: "projectPath"

instance ToJSON StopArgs where
  toJSON StopArgs {..} = object ["projectPath" .= stopProjectPath]

data StatusArgs = StatusArgs
  { statusProjectPath :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON StatusArgs where
  parseJSON = withObject "StatusArgs" $ \o ->
    StatusArgs <$> o .: "projectPath"

instance ToJSON StatusArgs where
  toJSON StatusArgs {..} = object ["projectPath" .= statusProjectPath]

data MessagesArgs = MessagesArgs
  { messagesProjectPath :: FilePath
  , messagesLimit :: Maybe Int
  , messagesFilter :: Maybe FilterRequest
  }
  deriving (Show, Eq, Generic)

instance FromJSON MessagesArgs where
  parseJSON = withObject "MessagesArgs" $ \o ->
    MessagesArgs
      <$> o .: "projectPath"
      <*> o .:? "limit"
      <*> o .:? "filter"

instance ToJSON MessagesArgs where
  toJSON MessagesArgs {..} =
    object
      [ "projectPath" .= messagesProjectPath
      , "limit" .= messagesLimit
      , "filter" .= messagesFilter
      ]
