{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils.FileSystem
  ( ProjectInfo (..)
  , findHaskellFiles
  , findCabalFiles
  , findStackFiles
  , getProjectInfo
  ) where

import Control.Exception (SomeException, try)
import GHC.Generics (Generic)
import System.Directory
import System.FilePath

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
getProjectInfo rootDir = do
  foundHaskellFiles <- findHaskellFiles rootDir
  foundCabalFiles <- findCabalFiles rootDir
  foundStackFiles <- findStackFiles rootDir

  return
    ProjectInfo
      { projectRoot = rootDir
      , cabalFiles = foundCabalFiles
      , stackFiles = foundStackFiles
      , haskellFiles = foundHaskellFiles
      }
