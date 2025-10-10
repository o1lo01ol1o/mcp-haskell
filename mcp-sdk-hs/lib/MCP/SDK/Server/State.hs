{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module MCP.SDK.Server.State
  ( RegisteredTool (..)
  , RegisteredPrompt (..)
  , RegisteredResource (..)
  , ServerContext (..)
  , newServerContext
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import MCP.SDK.Types
  ( PromptDefinition,
    Resource,
    ResourceTemplate,
    ToolDefinition,
  )

-- | Represents a tool that has been registered with the server.
data RegisteredTool m = RegisteredTool
  { rtDefinition :: ToolDefinition m,
    rtEnabled :: Bool
  }
  deriving stock (Show)

-- | Represents a prompt that has been registered with the server.
data RegisteredPrompt m = RegisteredPrompt
  { rpDefinition :: PromptDefinition m,
    rpEnabled :: Bool
  }
  deriving stock (Show)

-- | Represents a resource that has been registered with the server.
data RegisteredResource = RegisteredResource
  { rrResource :: Resource,
    rrEnabled :: Bool
  }
  deriving (Show, Eq)

-- | Holds the dynamic state of the MCP server.
-- Each component (tools, prompts, resources) is stored in a TVar for
-- thread-safe, concurrent access.
data ServerContext m = ServerContext
  { scTools :: TVar (Map Text (RegisteredTool m)),
    scPrompts :: TVar (Map Text (RegisteredPrompt m)),
    scResources :: TVar (Map Text RegisteredResource),
    scResourceTemplates :: TVar (Map Text (ResourceTemplate m))
  }

-- | Creates a new, empty ServerContext.
newServerContext :: IO (ServerContext m)
newServerContext =
  ServerContext
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
