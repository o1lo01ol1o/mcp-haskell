{-# LANGUAGE OverloadedStrings #-}

module MCP.SDK.Server.Auth
  ( AuthProvider (..)
  ) where

import Data.Text (Text)
import MCP.SDK.Error (MCPError)
import MCP.SDK.Types.Auth (AuthInfo)

-- | A type class for authentication providers.
-- An authentication provider is responsible for verifying a token and
-- extracting authentication information from it.
class AuthProvider p where
  -- | Verifies a token and returns the authentication information.
  -- This is an IO action because it will likely involve external
  -- communication (e.g., with an auth server).
  verifyToken :: p -> Text -> IO (Either MCPError AuthInfo)
