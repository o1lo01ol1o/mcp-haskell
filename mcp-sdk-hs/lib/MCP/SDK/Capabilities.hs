{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.SDK.Capabilities
  ( -- * Capability types
    ToolsCapability (..),
    ResourcesCapability (..),
    PromptsCapability (..),
    LoggingCapability (..),
    SamplingCapability (..),
    ClientCapabilityBuilder (..),
    ServerCapabilityBuilder (..),

    -- * Capability builders
    buildClientCapabilities,
    buildServerCapabilities,

    -- * Predefined capabilities
    defaultClientCapabilities,
    fullServerCapabilities,
    minimalServerCapabilities,
    toolsOnlyCapabilities,
    resourcesOnlyCapabilities,
    promptsOnlyCapabilities,

    -- * Capability checking utilities
    hasToolsSupport,
    hasResourcesSupport,
    hasPromptsSupport,
    hasLoggingSupport,

    -- * Capability negotiation
    negotiateCapabilities,
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import MCP.SDK.Types

-- | Standard MCP capability builders
data ToolsCapability = ToolsCapability
  { toolsListChanges :: Maybe Bool
  }
  deriving (Eq, Show)

data ResourcesCapability = ResourcesCapability
  { resourcesSubscribe :: Maybe Bool,
    resourcesListChanges :: Maybe Bool
  }
  deriving (Eq, Show)

data PromptsCapability = PromptsCapability
  { promptsListChanges :: Maybe Bool
  }
  deriving (Eq, Show)

data LoggingCapability = LoggingCapability deriving (Eq, Show)

data SamplingCapability = SamplingCapability deriving (Eq, Show)

-- | Capability builder for clients
data ClientCapabilityBuilder = ClientCapabilityBuilder
  { ccbExperimental :: Maybe Object,
    ccbSampling :: Maybe SamplingCapability
  }
  deriving (Eq, Show)

-- | Capability builder for servers
data ServerCapabilityBuilder = ServerCapabilityBuilder
  { scbTools :: Maybe ToolsCapability,
    scbResources :: Maybe ResourcesCapability,
    scbPrompts :: Maybe PromptsCapability,
    scbLogging :: Maybe LoggingCapability,
    scbExperimental :: Maybe Object
  }
  deriving (Eq, Show)

-- | Build client capabilities
buildClientCapabilities :: ClientCapabilityBuilder -> Capabilities
buildClientCapabilities ClientCapabilityBuilder {..} =
  Capabilities ccbExperimental (samplingToObject <$> ccbSampling)
  where
    samplingToObject sc = case toJSON sc of
      Object obj -> obj
      _ -> KeyMap.empty

-- | Build server capabilities
buildServerCapabilities :: ServerCapabilityBuilder -> Capabilities
buildServerCapabilities ServerCapabilityBuilder {..} =
  let standardCaps =
        KeyMap.fromList $
          concat
            [ maybe [] (\t -> [(fromText "tools", toJSON t)]) scbTools,
              maybe [] (\r -> [(fromText "resources", toJSON r)]) scbResources,
              maybe [] (\p -> [(fromText "prompts", toJSON p)]) scbPrompts,
              maybe [] (\l -> [(fromText "logging", toJSON l)]) scbLogging
            ]
      -- Merge standard capabilities with experimental ones
      allCaps = case scbExperimental of
        Just expCaps -> KeyMap.union standardCaps expCaps
        Nothing -> standardCaps
   in Capabilities (Just allCaps) Nothing

-- | Default client capabilities
defaultClientCapabilities :: Capabilities
defaultClientCapabilities =
  buildClientCapabilities
    ClientCapabilityBuilder
      { ccbExperimental = Nothing,
        ccbSampling = Nothing
      }

-- | Default server capabilities with all features enabled
fullServerCapabilities :: Capabilities
fullServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability {toolsListChanges = Just True},
        scbResources =
          Just $
            ResourcesCapability
              { resourcesSubscribe = Just True,
                resourcesListChanges = Just True
              },
        scbPrompts = Just $ PromptsCapability {promptsListChanges = Just True},
        scbLogging = Just LoggingCapability,
        scbExperimental = Nothing
      }

-- | Minimal server capabilities (ping only)
minimalServerCapabilities :: Capabilities
minimalServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Nothing,
        scbResources = Nothing,
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Tools-only server capabilities
toolsOnlyCapabilities :: Capabilities
toolsOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just $ ToolsCapability {toolsListChanges = Just False},
        scbResources = Nothing,
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Resources-only server capabilities
resourcesOnlyCapabilities :: Capabilities
resourcesOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Nothing,
        scbResources =
          Just $
            ResourcesCapability
              { resourcesSubscribe = Just False,
                resourcesListChanges = Just False
              },
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Prompts-only server capabilities
promptsOnlyCapabilities :: Capabilities
promptsOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Nothing,
        scbResources = Nothing,
        scbPrompts = Just $ PromptsCapability {promptsListChanges = Just False},
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- JSON instances for capabilities
instance ToJSON ToolsCapability where
  toJSON (ToolsCapability listChanges) =
    object
      ["listChanged" .= listChanges]

instance FromJSON ToolsCapability where
  parseJSON = withObject "ToolsCapability" $ \o ->
    ToolsCapability <$> o .:? "listChanged"

instance ToJSON ResourcesCapability where
  toJSON (ResourcesCapability subscribe listChanges) =
    object
      [ "subscribe" .= subscribe,
        "listChanged" .= listChanges
      ]

instance FromJSON ResourcesCapability where
  parseJSON = withObject "ResourcesCapability" $ \o ->
    ResourcesCapability <$> o .:? "subscribe" <*> o .:? "listChanged"

instance ToJSON PromptsCapability where
  toJSON (PromptsCapability listChanges) =
    object
      ["listChanged" .= listChanges]

instance FromJSON PromptsCapability where
  parseJSON = withObject "PromptsCapability" $ \o ->
    PromptsCapability <$> o .:? "listChanged"

instance ToJSON LoggingCapability where
  toJSON LoggingCapability = object []

instance FromJSON LoggingCapability where
  parseJSON = withObject "LoggingCapability" $ \_ -> pure LoggingCapability

instance ToJSON SamplingCapability where
  toJSON SamplingCapability = object []

instance FromJSON SamplingCapability where
  parseJSON = withObject "SamplingCapability" $ \_ -> pure SamplingCapability

-- | Capability checking utilities
hasToolsSupport :: Capabilities -> Bool
hasToolsSupport caps = case experimental caps of
  Just obj -> KeyMap.member (fromText "tools") obj
  _ -> False

hasResourcesSupport :: Capabilities -> Bool
hasResourcesSupport caps = case experimental caps of
  Just obj -> KeyMap.member (fromText "resources") obj
  _ -> False

hasPromptsSupport :: Capabilities -> Bool
hasPromptsSupport caps = case experimental caps of
  Just obj -> KeyMap.member (fromText "prompts") obj
  _ -> False

hasLoggingSupport :: Capabilities -> Bool
hasLoggingSupport caps = case experimental caps of
  Just obj -> KeyMap.member (fromText "logging") obj
  _ -> False

-- | Capability negotiation
negotiateCapabilities :: Capabilities -> Capabilities -> Capabilities
negotiateCapabilities client server =
  let -- Extract capabilities from server
      serverCaps = case experimental server of
        Just caps -> caps
        Nothing -> KeyMap.empty

      -- Extract capabilities from client (if any)
      clientCaps = case experimental client of
        Just caps -> caps
        Nothing -> KeyMap.empty

      -- If client has no declared capabilities, assume it supports basic MCP
      -- (tools, resources, prompts) but not advanced features
      basicCapabilities = [fromText "tools", fromText "resources", fromText "prompts"]

      -- Determine which capabilities to negotiate
      supportedByClient =
        if KeyMap.null clientCaps
          then basicCapabilities -- Default to basic capabilities
          else KeyMap.keys clientCaps -- Use declared capabilities

      -- Negotiate intersection: only include server capabilities that client supports
      negotiatedCaps =
        KeyMap.filterWithKey (\k _ -> k `elem` supportedByClient) serverCaps

      -- Negotiate sampling capabilities (client-specific)
      negotiatedSampling = case (sampling client, sampling server) of
        (Just clientSampling, Just _) ->
          -- In practice, this would merge sampling configs
          Just clientSampling
        (Just clientSampling, Nothing) -> Just clientSampling
        (Nothing, Just _) -> Nothing -- Server can't force sampling on client
        (Nothing, Nothing) -> Nothing
   in Capabilities
        (if KeyMap.null negotiatedCaps then Nothing else Just negotiatedCaps)
        negotiatedSampling
