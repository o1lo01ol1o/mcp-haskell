{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.Types.GHCID
  ( -- * GADT Request Types
    GHCIDRequest(..)
  , GHCIDResponse(..)
  , SomeGHCIDRequest(..)
  , SomeGHCIDResponse(..)
  , RequestId(..)
  
    -- * Request/Response Type Families
  , ResponseType
  , RequestData
  , ResponseData
  
    -- * Specific Request Types
  , StartGHCIDData(..)
  , StopGHCIDData(..)
  , GetMessagesData(..)
  , ListProcessesData(..)
  , ProcessStatusData(..)
  , RestartProcessData(..)
  
    -- * Response Types
  , StartGHCIDResult(..)
  , StopGHCIDResult(..)
  , MessagesResult(..)
  , ProcessListResult(..)
  , ProcessStatusResult(..)
  , RestartProcessResult(..)
  
    -- * JSON Serialization
  , encodeGHCIDRequest
  , decodeGHCIDRequest
  , encodeGHCIDResponse
  
    -- * Request Handlers
  , handleGHCIDRequest
  , RequestHandler(..)
  ) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable

-- Internal imports
import GHCID.ProcessRegistry (CabalURI(..), GHCIDStatus(..), ProcessRegistry, GHCIDHandle)
import GHCID.Filter (FilterRequest)

-- | Request identifier for tracking
newtype RequestId = RequestId { getRequestId :: Text }
  deriving (Show, Eq, Ord)

-- | Type family to map request types to response types
type family ResponseType (req :: * -> *) :: * -> *

-- | Type family to extract request data type
type family RequestData (req :: *) :: *

-- | Type family to extract response data type  
type family ResponseData (resp :: *) :: *

-- Request data types

-- | Start GHCID process request
data StartGHCIDData = StartGHCIDData
  { startCabalURI :: CabalURI
  , startWorkDir :: FilePath
  , startOptions :: Maybe Value  -- Additional options
  } deriving (Show, Eq)

-- | Stop GHCID process request
data StopGHCIDData = StopGHCIDData
  { stopCabalURI :: CabalURI
  , stopForce :: Bool  -- Force stop if graceful fails
  } deriving (Show, Eq)

-- | Get messages request  
data GetMessagesData = GetMessagesData
  { messagesCabalURI :: CabalURI
  , messagesFilter :: Maybe FilterRequest
  , messagesCount :: Maybe Int  -- Limit number of messages
  } deriving (Show, Eq)

-- | List processes request
data ListProcessesData = ListProcessesData
  { listIncludeStatus :: Bool  -- Include detailed status
  } deriving (Show, Eq)

-- | Get process status request
data ProcessStatusData = ProcessStatusData
  { statusCabalURI :: CabalURI
  } deriving (Show, Eq)

-- | Restart process request
data RestartProcessData = RestartProcessData
  { restartCabalURI :: CabalURI
  , restartWorkDir :: Maybe FilePath  -- New working directory
  } deriving (Show, Eq)

-- Response data types

-- | Start GHCID result
data StartGHCIDResult = StartGHCIDResult
  { startSuccess :: Bool
  , startMessage :: Text
  , startProcessId :: Maybe Text
  } deriving (Show, Eq)

-- | Stop GHCID result
data StopGHCIDResult = StopGHCIDResult
  { stopSuccess :: Bool
  , stopMessage :: Text
  } deriving (Show, Eq)

-- | Messages result
data MessagesResult = MessagesResult
  { messagesOutput :: Text
  , messagesLines :: [Text]
  , messagesTimestamp :: UTCTime
  } deriving (Show, Eq)

-- | Process list result
data ProcessListResult = ProcessListResult
  { processURIs :: [CabalURI]
  , processStatuses :: Maybe [(CabalURI, GHCIDStatus)]
  } deriving (Show, Eq)

-- | Process status result
data ProcessStatusResult = ProcessStatusResult
  { processStatus :: Maybe GHCIDStatus
  , processUptime :: Maybe Text
  } deriving (Show, Eq)

-- | Restart process result
data RestartProcessResult = RestartProcessResult
  { restartSuccess :: Bool
  , restartMessage :: Text
  , restartOldProcessId :: Maybe Text
  , restartNewProcessId :: Maybe Text
  } deriving (Show, Eq)

-- | GADT for type-safe GHCID requests
data GHCIDRequest a where
  StartGHCID :: StartGHCIDData -> GHCIDRequest StartGHCIDResult
  StopGHCID :: StopGHCIDData -> GHCIDRequest StopGHCIDResult
  GetMessages :: GetMessagesData -> GHCIDRequest MessagesResult
  ListProcesses :: ListProcessesData -> GHCIDRequest ProcessListResult
  ProcessStatus :: ProcessStatusData -> GHCIDRequest ProcessStatusResult
  RestartProcess :: RestartProcessData -> GHCIDRequest RestartProcessResult

deriving instance Show (GHCIDRequest a)
deriving instance Eq (GHCIDRequest a)

-- | GADT for type-safe GHCID responses
data GHCIDResponse a where
  StartGHCIDResponse :: RequestId -> StartGHCIDResult -> GHCIDResponse StartGHCIDResult
  StopGHCIDResponse :: RequestId -> StopGHCIDResult -> GHCIDResponse StopGHCIDResult
  GetMessagesResponse :: RequestId -> MessagesResult -> GHCIDResponse MessagesResult
  ListProcessesResponse :: RequestId -> ProcessListResult -> GHCIDResponse ProcessListResult
  ProcessStatusResponse :: RequestId -> ProcessStatusResult -> GHCIDResponse ProcessStatusResult
  RestartProcessResponse :: RequestId -> RestartProcessResult -> GHCIDResponse RestartProcessResult
  ErrorResponse :: RequestId -> Text -> GHCIDResponse a

deriving instance Show (GHCIDResponse a)

-- Type family instances
type instance ResponseType GHCIDRequest = GHCIDResponse
type instance RequestData StartGHCIDData = StartGHCIDData
type instance ResponseData StartGHCIDResult = StartGHCIDResult

-- JSON serialization instances

instance ToJSON StartGHCIDData where
  toJSON StartGHCIDData{..} = object
    [ "cabalURI" .= getCabalURI startCabalURI
    , "workDir" .= startWorkDir
    , "options" .= startOptions
    ]

instance FromJSON StartGHCIDData where
  parseJSON = withObject "StartGHCIDData" $ \o -> StartGHCIDData
    <$> (CabalURI <$> o .: "cabalURI")
    <*> o .: "workDir"
    <*> o .:? "options"

instance ToJSON StopGHCIDData where
  toJSON StopGHCIDData{..} = object
    [ "cabalURI" .= getCabalURI stopCabalURI
    , "force" .= stopForce
    ]

instance FromJSON StopGHCIDData where
  parseJSON = withObject "StopGHCIDData" $ \o -> StopGHCIDData
    <$> (CabalURI <$> o .: "cabalURI")
    <*> o .: "force"

instance ToJSON GetMessagesData where
  toJSON GetMessagesData{..} = object
    [ "cabalURI" .= getCabalURI messagesCabalURI
    , "filter" .= messagesFilter
    , "count" .= messagesCount
    ]

instance FromJSON GetMessagesData where
  parseJSON = withObject "GetMessagesData" $ \o -> GetMessagesData
    <$> (CabalURI <$> o .: "cabalURI")
    <*> o .:? "filter"
    <*> o .:? "count"

instance ToJSON ListProcessesData where
  toJSON ListProcessesData{..} = object
    [ "includeStatus" .= listIncludeStatus
    ]

instance FromJSON ListProcessesData where
  parseJSON = withObject "ListProcessesData" $ \o -> ListProcessesData
    <$> o .: "includeStatus"

instance ToJSON ProcessStatusData where
  toJSON ProcessStatusData{..} = object
    [ "cabalURI" .= getCabalURI statusCabalURI
    ]

instance FromJSON ProcessStatusData where
  parseJSON = withObject "ProcessStatusData" $ \o -> ProcessStatusData
    <$> (CabalURI <$> o .: "cabalURI")

instance ToJSON RestartProcessData where
  toJSON RestartProcessData{..} = object
    [ "cabalURI" .= getCabalURI restartCabalURI
    , "workDir" .= restartWorkDir
    ]

instance FromJSON RestartProcessData where
  parseJSON = withObject "RestartProcessData" $ \o -> RestartProcessData
    <$> (CabalURI <$> o .: "cabalURI")
    <*> o .:? "workDir"

-- Response JSON instances

instance ToJSON StartGHCIDResult where
  toJSON StartGHCIDResult{..} = object
    [ "success" .= startSuccess
    , "message" .= startMessage
    , "processId" .= startProcessId
    ]

instance FromJSON StartGHCIDResult where
  parseJSON = withObject "StartGHCIDResult" $ \o -> StartGHCIDResult
    <$> o .: "success"
    <*> o .: "message"
    <*> o .:? "processId"

instance ToJSON StopGHCIDResult where
  toJSON StopGHCIDResult{..} = object
    [ "success" .= stopSuccess
    , "message" .= stopMessage
    ]

instance FromJSON StopGHCIDResult where
  parseJSON = withObject "StopGHCIDResult" $ \o -> StopGHCIDResult
    <$> o .: "success"
    <*> o .: "message"

instance ToJSON MessagesResult where
  toJSON MessagesResult{..} = object
    [ "output" .= messagesOutput
    , "lines" .= messagesLines
    , "timestamp" .= messagesTimestamp
    ]

instance FromJSON MessagesResult where
  parseJSON = withObject "MessagesResult" $ \o -> MessagesResult
    <$> o .: "output"
    <*> o .: "lines"
    <*> o .: "timestamp"

instance ToJSON ProcessListResult where
  toJSON ProcessListResult{..} = object
    [ "uris" .= map getCabalURI processURIs
    , "statuses" .= fmap (map (\(uri, status) -> object ["uri" .= getCabalURI uri, "status" .= status])) processStatuses
    ]

instance FromJSON ProcessListResult where
  parseJSON = withObject "ProcessListResult" $ \o -> ProcessListResult
    <$> (map CabalURI <$> o .: "uris")
    <*> (o .:? "statuses" >>= traverse parseStatuses)
    where
      parseStatuses = traverse $ \statusObj -> withObject "ProcessStatus" (\obj ->
        (,) <$> (CabalURI <$> obj .: "uri") <*> obj .: "status") statusObj

instance ToJSON ProcessStatusResult where
  toJSON ProcessStatusResult{..} = object
    [ "status" .= processStatus
    , "uptime" .= processUptime
    ]

instance FromJSON ProcessStatusResult where
  parseJSON = withObject "ProcessStatusResult" $ \o -> ProcessStatusResult
    <$> o .:? "status"
    <*> o .:? "uptime"

instance ToJSON RestartProcessResult where
  toJSON RestartProcessResult{..} = object
    [ "success" .= restartSuccess
    , "message" .= restartMessage
    , "oldProcessId" .= restartOldProcessId
    , "newProcessId" .= restartNewProcessId
    ]

instance FromJSON RestartProcessResult where
  parseJSON = withObject "RestartProcessResult" $ \o -> RestartProcessResult
    <$> o .: "success"
    <*> o .: "message"
    <*> o .:? "oldProcessId"
    <*> o .:? "newProcessId"

-- GADT JSON serialization

-- | Encode a GHCID request to JSON with type information
encodeGHCIDRequest :: RequestId -> GHCIDRequest a -> Value
encodeGHCIDRequest reqId request = object $ case request of
  StartGHCID dat -> 
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.start" :: Text)
    , "params" .= dat
    ]
  StopGHCID dat ->
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.stop" :: Text)
    , "params" .= dat
    ]
  GetMessages dat ->
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.messages" :: Text)
    , "params" .= dat
    ]
  ListProcesses dat ->
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.list" :: Text)
    , "params" .= dat
    ]
  ProcessStatus dat ->
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.status" :: Text)
    , "params" .= dat
    ]
  RestartProcess dat ->
    [ "id" .= getRequestId reqId
    , "method" .= ("ghcid.restart" :: Text)
    , "params" .= dat
    ]

-- | Decode a GHCID request from JSON (existentially quantified)
data SomeGHCIDRequest = forall a. SomeGHCIDRequest RequestId (GHCIDRequest a)

decodeGHCIDRequest :: Value -> Parser SomeGHCIDRequest
decodeGHCIDRequest = withObject "GHCIDRequest" $ \o -> do
  reqId <- RequestId <$> o .: "id"
  method <- o .: "method"
  case method of
    "ghcid.start" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (StartGHCID dat)
    "ghcid.stop" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (StopGHCID dat)
    "ghcid.messages" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (GetMessages dat)
    "ghcid.list" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (ListProcesses dat)
    "ghcid.status" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (ProcessStatus dat)
    "ghcid.restart" -> do
      params <- o .: "params"
      dat <- parseJSON params
      return $ SomeGHCIDRequest reqId (RestartProcess dat)
    _ -> fail $ "Unknown GHCID method: " ++ T.unpack method

-- | Encode a GHCID response to JSON
encodeGHCIDResponse :: GHCIDResponse a -> Value
encodeGHCIDResponse response = object $ case response of
  StartGHCIDResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  StopGHCIDResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  GetMessagesResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  ListProcessesResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  ProcessStatusResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  RestartProcessResponse reqId result ->
    [ "id" .= getRequestId reqId
    , "result" .= result
    ]
  ErrorResponse reqId err ->
    [ "id" .= getRequestId reqId
    , "error" .= object ["message" .= err]
    ]

-- Note: Removed problematic polymorphic decodeGHCIDResponse function
-- In practice, responses are handled through the handleGHCIDRequest function
-- which directly encodes to JSON using encodeGHCIDResponse

-- | Request handler type
data RequestHandler = RequestHandler
  { handleStart :: StartGHCIDData -> IO StartGHCIDResult
  , handleStop :: StopGHCIDData -> IO StopGHCIDResult
  , handleMessages :: GetMessagesData -> IO MessagesResult
  , handleList :: ListProcessesData -> IO ProcessListResult
  , handleStatus :: ProcessStatusData -> IO ProcessStatusResult
  , handleRestart :: RestartProcessData -> IO RestartProcessResult
  }

-- | Existentially quantified response wrapper
data SomeGHCIDResponse = forall a. SomeGHCIDResponse (GHCIDResponse a)

-- | Handle a GHCID request using the provided handlers
handleGHCIDRequest :: RequestHandler -> RequestId -> SomeGHCIDRequest -> IO Value
handleGHCIDRequest handlers reqId (SomeGHCIDRequest _ request) = do
  result <- case request of
    StartGHCID dat -> do
      res <- handleStart handlers dat
      return $ SomeGHCIDResponse $ StartGHCIDResponse reqId res
    StopGHCID dat -> do
      res <- handleStop handlers dat
      return $ SomeGHCIDResponse $ StopGHCIDResponse reqId res
    GetMessages dat -> do
      res <- handleMessages handlers dat
      return $ SomeGHCIDResponse $ GetMessagesResponse reqId res
    ListProcesses dat -> do
      res <- handleList handlers dat
      return $ SomeGHCIDResponse $ ListProcessesResponse reqId res
    ProcessStatus dat -> do
      res <- handleStatus handlers dat
      return $ SomeGHCIDResponse $ ProcessStatusResponse reqId res
    RestartProcess dat -> do
      res <- handleRestart handlers dat
      return $ SomeGHCIDResponse $ RestartProcessResponse reqId res
  
  case result of
    SomeGHCIDResponse resp -> return $ encodeGHCIDResponse resp