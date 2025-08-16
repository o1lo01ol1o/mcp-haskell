module MCP.SDK.Types.Auth where

import Data.Text (Text)

-- | Represents the authentication information derived from a validated token.
-- This information is made available to request handlers.
data AuthInfo = AuthInfo
  { -- | The client identifier, if provided by the authentication provider.
    aiClientId :: Maybe Text,
    -- | A list of scopes or permissions granted by the token.
    aiScopes :: [Text],
    -- | The raw token that was used for authentication.
    aiToken :: Text
  }
  deriving (Eq, Show)
