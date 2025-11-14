{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.SDK.Capabilities
  ( -- * Capability types
    ToolsCapability (..),
    ResourcesCapability (..),
    PromptsCapability (..),
    LoggingCapability (..),
    SamplingCapability (..),
    ClientRootsCapability (..),
    ClientCapabilities (..),
    ServerCapabilities (..),
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

import Data.Aeson (Object, Value, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, isJust)
import MCP.SDK.Types

-- | Capability builder for clients
data ClientCapabilityBuilder = ClientCapabilityBuilder
  { ccbExperimental :: Maybe Object,
    ccbSampling :: Maybe SamplingCapability,
    ccbRoots :: Maybe ClientRootsCapability,
    ccbElicitation :: Maybe Value
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
buildClientCapabilities :: ClientCapabilityBuilder -> ClientCapabilities
buildClientCapabilities ClientCapabilityBuilder {..} =
  ClientCapabilities
    { clientElicitation = ccbElicitation,
      clientExperimental = ccbExperimental,
      clientRoots = ccbRoots,
      clientSampling = fmap toJSON ccbSampling
    }

-- | Build server capabilities
buildServerCapabilities :: ServerCapabilityBuilder -> ServerCapabilities
buildServerCapabilities ServerCapabilityBuilder {..} =
  ServerCapabilities
    { serverExperimental = scbExperimental,
      serverLogging = scbLogging,
      serverPrompts = scbPrompts,
      serverResources = scbResources,
      serverTools = scbTools
    }

-- | Default client capabilities
defaultClientCapabilities :: ClientCapabilities
defaultClientCapabilities =
  buildClientCapabilities
    ClientCapabilityBuilder
      { ccbExperimental = Nothing,
        ccbSampling = Nothing,
        ccbRoots = Nothing,
        ccbElicitation = Nothing
      }

-- | Default server capabilities with all features enabled
fullServerCapabilities :: ServerCapabilities
fullServerCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just ToolsCapability {toolsListChanges = Just True},
        scbResources =
          Just
            ResourcesCapability
              { resourcesSubscribe = Just True,
                resourcesListChanges = Just True
              },
        scbPrompts = Just PromptsCapability {promptsListChanges = Just True},
        scbLogging = Just LoggingCapability,
        scbExperimental = Nothing
      }

-- | Minimal server capabilities (ping only)
minimalServerCapabilities :: ServerCapabilities
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
toolsOnlyCapabilities :: ServerCapabilities
toolsOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Just ToolsCapability {toolsListChanges = Just False},
        scbResources = Nothing,
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Resources-only server capabilities
resourcesOnlyCapabilities :: ServerCapabilities
resourcesOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Nothing,
        scbResources =
          Just
            ResourcesCapability
              { resourcesSubscribe = Just False,
                resourcesListChanges = Just False
              },
        scbPrompts = Nothing,
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Prompts-only server capabilities
promptsOnlyCapabilities :: ServerCapabilities
promptsOnlyCapabilities =
  buildServerCapabilities
    ServerCapabilityBuilder
      { scbTools = Nothing,
        scbResources = Nothing,
        scbPrompts = Just PromptsCapability {promptsListChanges = Just False},
        scbLogging = Nothing,
        scbExperimental = Nothing
      }

-- | Capability checking utilities
hasToolsSupport :: ServerCapabilities -> Bool
hasToolsSupport = isJust . serverTools

hasResourcesSupport :: ServerCapabilities -> Bool
hasResourcesSupport = isJust . serverResources

hasPromptsSupport :: ServerCapabilities -> Bool
hasPromptsSupport = isJust . serverPrompts

hasLoggingSupport :: ServerCapabilities -> Bool
hasLoggingSupport = isJust . serverLogging

-- | Capability negotiation
negotiateCapabilities :: ClientCapabilities -> ServerCapabilities -> ServerCapabilities
negotiateCapabilities client server =
  let serverExperimentalCaps = fromMaybe KeyMap.empty (serverExperimental server)
      clientExperimentalCaps = fromMaybe KeyMap.empty (clientExperimental client)
      negotiatedExperimental =
        if KeyMap.null clientExperimentalCaps
          then serverExperimentalCaps
          else KeyMap.intersection serverExperimentalCaps clientExperimentalCaps
   in server
        { serverExperimental =
            if KeyMap.null negotiatedExperimental
              then Nothing
              else Just negotiatedExperimental
        }
