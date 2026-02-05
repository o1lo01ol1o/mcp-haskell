{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal.Config
  ( CabalServerConfig(..)
  , defaultServerConfig
  , loadConfig
  , saveConfig
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

-- | Server configuration for MCP-Cabal.
data CabalServerConfig = CabalServerConfig
  { serverName :: Text
  , serverVersion :: Text
  , maxConcurrentProcesses :: Int
  , defaultWorkspace :: FilePath
  , instructionsMessage :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CabalServerConfig where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CabalServerConfig where
  toJSON = genericToJSON defaultOptions

-- | Default server configuration.
defaultServerConfig :: CabalServerConfig
defaultServerConfig = CabalServerConfig
  { serverName = "mcp-cabal"
  , serverVersion = "0.1.0.0"
  , maxConcurrentProcesses = 2
  , defaultWorkspace = "."
  , instructionsMessage = T.unlines
      [ "Use `cabal-test-start` to run `cabal test` for your project."
      , "Provide a `target` (for example test:unit) if you need a specific test suite."
      , "Default runs use an isolated `--builddir` under $TMPDIR/mcp-cache/cabal-test/ (or MCP_CACHE_DIR) to avoid `dist-newstyle/` clutter."
      , "To override the build dir, include an explicit `--builddir` in `options.args`."
      , "If you change the project environment (for example nix-shell or GHC version), restart the `mcp-cabal` server itself."
      ]
  }

-- | Load configuration from file.
loadConfig :: FilePath -> IO (Maybe CabalServerConfig)
loadConfig configPath = do
  exists <- doesFileExist configPath
  if exists
    then do
      result <- eitherDecodeFileStrict configPath
      case result of
        Left err -> do
          putStrLn $ "Failed to parse config: " ++ err
          return Nothing
        Right config -> return $ Just config
    else return Nothing

-- | Save configuration to file.
saveConfig :: FilePath -> CabalServerConfig -> IO (Either String ())
saveConfig configPath config = do
  result <- encodeFile configPath config
  return $ Right result
