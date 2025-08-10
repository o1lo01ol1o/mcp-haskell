{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Types.Simple where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- Simplified MCP types for testing compilation
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text
  , method :: Text
  , params :: Maybe Value
  , reqId :: Maybe Value
  } deriving (Generic, Show, Eq)

instance FromJSON JsonRpcRequest where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON JsonRpcRequest where
  toJSON = genericToJSON defaultOptions

-- Test that our basic structure compiles
testMessage :: JsonRpcRequest
testMessage = JsonRpcRequest "2.0" "initialize" Nothing Nothing