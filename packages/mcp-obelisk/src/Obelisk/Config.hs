{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Config
  ( ObeliskServerConfig (..)
  , defaultServerConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Basic configuration for the Obelisk MCP server.
data ObeliskServerConfig = ObeliskServerConfig
  { serverName :: Text
  , serverVersion :: Text
  , outputBufferSize :: Int
  , serverInstructions :: Text
  }

-- | Default server configuration.
defaultServerConfig :: IO ObeliskServerConfig
defaultServerConfig = pure $ ObeliskServerConfig
  { serverName = "mcp-obelisk"
  , serverVersion = "0.1.0.0"
  , outputBufferSize = 2000
  , serverInstructions = T.unlines
      [ "Use `obelisk-start` to start or restart `ob watch` for your project."
      , "Changes to `.cabal` files require running `obelisk-start` again."
      , "Changes to the Nix environment require restarting the `mcp-obelisk` server."
      , "Use `obelisk-messages` filter options (grep/head/tail/lines) to focus on specific output."
      ]
  }
