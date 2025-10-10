{-# LANGUAGE OverloadedStrings #-}

module HLS.Config
  ( HLSConfig (..)
  , defaultHLSConfig
  , loadHLSConfig
  ) where

import Data.Text (Text)

-- HLS Configuration
data HLSConfig = HLSConfig
  { hlsExecutablePath :: Maybe FilePath
  , workingDirectory :: FilePath
  , serverArgs :: [String]
  , formattingProvider :: Text
  } deriving (Show, Eq)

-- Default HLS Configuration
defaultHLSConfig :: FilePath -> HLSConfig
defaultHLSConfig workDir = HLSConfig
  { hlsExecutablePath = Nothing  -- Use PATH
  , workingDirectory = workDir
  , serverArgs = ["--lsp"]
  , formattingProvider = "ormolu"
  }

-- Load HLS Configuration (placeholder)
loadHLSConfig :: FilePath -> IO HLSConfig
loadHLSConfig workDir = return $ defaultHLSConfig workDir
