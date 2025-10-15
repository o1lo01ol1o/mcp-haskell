{-# LANGUAGE OverloadedStrings #-}

module HLS.Config
  ( -- * Legacy process configuration (placeholder)
    HLSConfig (..),
    defaultHLSConfig,
    loadHLSConfig,
    -- * Server configuration used by the MCP wrapper
    HLSServerConfig (..),
    defaultServerConfig
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- Legacy configuration type retained for backwards compatibility
data HLSConfig = HLSConfig
  { hlsExecutablePath :: Maybe FilePath,
    workingDirectory :: FilePath,
    serverArgs :: [String],
    formattingProvider :: Text
  }
  deriving (Show, Eq)

defaultHLSConfig :: FilePath -> HLSConfig
defaultHLSConfig workDir =
  HLSConfig
    { hlsExecutablePath = Nothing,
      workingDirectory = workDir,
      serverArgs = ["--lsp"],
      formattingProvider = "ormolu"
    }

loadHLSConfig :: FilePath -> IO HLSConfig
loadHLSConfig workDir = pure (defaultHLSConfig workDir)

-- | Configuration for the MCP-HLS server wrapper.
data HLSServerConfig = HLSServerConfig
  { serverName :: Text,
    serverVersion :: Text,
    defaultWorkspace :: FilePath,
    defaultExecutable :: String,
    defaultArguments :: [String],
    defaultLogLevel :: Text,
    defaultEnableLogging :: Bool,
    instructionsMessage :: Text
  }
  deriving (Show, Eq)

-- | Default server configuration used when no explicit configuration is supplied.
defaultServerConfig :: HLSServerConfig
defaultServerConfig =
  HLSServerConfig
    { serverName = "mcp-hls",
      serverVersion = "0.1.0.0",
      defaultWorkspace = ".",
      defaultExecutable = "haskell-language-server-wrapper",
      defaultArguments = ["--lsp"],
      defaultLogLevel = "info",
      defaultEnableLogging = True,
      instructionsMessage =
        T.unlines
          [ "Use `hls-start` to launch the Haskell Language Server in a workspace.",
            "Provide an optional `work_dir` to select a specific project root.",
            "Use `hls-restart` to refresh the running server after changing configuration files.",
            "Use `hls-stop` when you need to terminate the running HLS instance."
          ]
    }
