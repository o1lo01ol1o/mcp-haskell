{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCID.Config
  ( GHCIDConfig(..)
  , GHCIDServerConfig(..)
  , defaultGHCIDConfig
  , defaultServerConfig
  , loadConfig
  , saveConfig
  , detectProjectConfig
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeBaseName)
import Utils.FileSystem (findCabalFiles)

-- Re-export from Client
import GHCID.Client (GHCIDConfig(..), defaultGHCIDConfig)

-- | Server configuration for MCP-GHCID
data GHCIDServerConfig = GHCIDServerConfig
  { serverName :: Text              -- Server identification
  , serverVersion :: Text          -- Server version
  , maxConcurrentProcesses :: Int  -- Max concurrent GHCID processes
  , defaultWorkspace :: FilePath   -- Default workspace directory
  , autoDiscoverProjects :: Bool   -- Auto-discover Haskell projects
  , retentionPolicy :: Int         -- How many messages to keep (0 = unlimited)
  , instructionsMessage :: Text    -- Guidance presented to MCP clients
  } deriving (Show, Eq, Generic)

instance FromJSON GHCIDServerConfig where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON GHCIDServerConfig where
  toJSON = genericToJSON defaultOptions

-- | Default server configuration
defaultServerConfig :: GHCIDServerConfig
defaultServerConfig = GHCIDServerConfig
  { serverName = "mcp-ghcid"
  , serverVersion = "0.1.0.0"
  , maxConcurrentProcesses = 5
  , defaultWorkspace = "."
  , autoDiscoverProjects = True
  , retentionPolicy = 1000
  , instructionsMessage = T.unlines
      [ "Use `ghcid-start` to launch or restart a ghcid session for your project."
      , "Optionally provide a `component` (for example lib:mypkg or exe:tool) if you need a specific Cabal target."
      , "Changes to `.cabal` files require running `ghcid-restart` so ghcid reloads the build plan."
      , "If you change the project environment (for example nix-shell or GHC version), restart the `mcp-ghcid` server itself."
      ]
  }

-- | Load configuration from file
loadConfig :: FilePath -> IO (Maybe GHCIDServerConfig)
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

-- | Save configuration to file  
saveConfig :: FilePath -> GHCIDServerConfig -> IO (Either String ())
saveConfig configPath config = do
  result <- encodeFile configPath config
  return $ Right result

-- | Detect project configuration from directory
detectProjectConfig :: FilePath -> IO GHCIDConfig
detectProjectConfig workDir = do
  cabalFiles <- findCabalFiles workDir
  let mainCabalFile = case cabalFiles of
        [] -> Nothing
        (file:_) -> Just (workDir </> file)
        
  -- Try to find main modules
  let commonMainPaths = 
        [ workDir </> "src" </> "Main.hs"
        , workDir </> "app" </> "Main.hs"
        , workDir </> "Main.hs"
        , workDir </> "src" </> (takeBaseName workDir ++ ".hs")
        ]
  
  existingMains <- filterM doesFileExist commonMainPaths
  let detectedTargets = take 1 existingMains  -- Use first found main
  
  return $ GHCIDConfig
    { ghcidCommand = "ghcid"
    , ghcidArgs = ["--color=never", "--no-title"]  -- Better for parsing
    , targetFiles = detectedTargets
    , cabalFile = mainCabalFile  
    , workingDir = workDir
    , reloadOnChange = True
    , testCommand = detectTestCommand cabalFiles
    }

detectTestCommand :: [FilePath] -> Maybe String
detectTestCommand cabalFiles
  | not (null cabalFiles) = Just "cabal test"  -- Has cabal file
  | otherwise = Nothing
