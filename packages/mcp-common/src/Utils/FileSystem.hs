{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils.FileSystem where

import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import GHC.Generics (Generic)

-- | Information about a Haskell project
data ProjectInfo = ProjectInfo
  { projectRoot :: FilePath,
    cabalFiles :: [FilePath],
    stackFiles :: [FilePath],
    haskellFiles :: [FilePath]
  }
  deriving (Generic, Show, Eq)

-- Find Haskell Project Files
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  result <- try @SomeException $ listDirectory dir
  case result of
    Left _ -> return []
    Right files -> return $ filter isHaskellFile files
  where
    isHaskellFile file = takeExtension file `elem` [".hs", ".lhs"]

-- Find Cabal Files
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  result <- try @SomeException $ listDirectory dir
  case result of
    Left _ -> return []
    Right files -> return $ filter isCabalFile files
  where
    isCabalFile file = takeExtension file == ".cabal"

-- Find Stack Files
findStackFiles :: FilePath -> IO [FilePath]
findStackFiles dir = do
  result <- try @SomeException $ listDirectory dir
  case result of
    Left _ -> return []
    Right files -> return $ filter isStackFile files
  where
    isStackFile file = file == "stack.yaml"

-- Get Project Information
getProjectInfo :: FilePath -> IO ProjectInfo
getProjectInfo projectRoot = do
  haskellFiles <- findHaskellFiles projectRoot
  cabalFiles <- findCabalFiles projectRoot
  stackFiles <- findStackFiles projectRoot
  
  return ProjectInfo
    { projectRoot = projectRoot
    , cabalFiles = cabalFiles
    , stackFiles = stackFiles
    , haskellFiles = haskellFiles
    }