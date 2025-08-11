{-# LANGUAGE OverloadedStrings #-}

-- | Module for testing import management: organize_imports, insert_import, remove_unused_imports
module Demo.Imports 
  ( exportedFunction1
  , exportedFunction2
  , ExportedType(..)
  ) where

-- Imports in wrong order - should be reorganized
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import Control.Monad (when, unless)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing, catMaybes)  -- catMaybes unused
import qualified Data.Map as Map
import Data.List (sort, nub, intercalate, reverse)  -- reverse unused
import System.Environment (getEnv)
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Char8 as BS8  -- Completely unused import
import Data.Char (toUpper, toLower, isDigit)  -- toUpper unused
import qualified Data.Set as Set  -- Unused qualified import

-- Functions that use some but not all imports

-- | Uses Data.Text, Data.Maybe, Control.Monad
exportedFunction1 :: Text -> IO Text
exportedFunction1 input = do
  when (T.null input) $ do
    hPutStrLn stderr "Warning: empty input"
  return $ fromMaybe "default" (Just (T.strip input))

-- | Uses Data.List, Data.Char, System.Environment  
exportedFunction2 :: IO [String]
exportedFunction2 = do
  path <- getEnv "PATH"
  return $ sort $ nub $ filter (all isDigit) $ words path

-- | Uses qualified imports
mapOperations :: [(String, Int)] -> Map.Map String Int -> Map.Map String Int
mapOperations pairs existingMap = 
  foldr (uncurry Map.insert) existingMap pairs

-- | Function that could use additional imports (not currently imported)
-- These should trigger insert_import actions when used:

-- This would need: import Data.Time
-- getCurrentTime :: IO UTCTime

-- This would need: import Control.Concurrent  
-- threadDelay :: Int -> IO ()

-- This would need: import System.Directory
-- doesFileExist :: FilePath -> IO Bool

missingImportFunction1 :: IO ()
missingImportFunction1 = do
  -- If we uncomment these, they should trigger insert_import:
  -- time <- getCurrentTime
  -- putStrLn $ "Current time: " ++ show time
  putStrLn "Function needs getCurrentTime import"

missingImportFunction2 :: Int -> IO ()
missingImportFunction2 delay = do
  -- If we uncomment this, it should trigger insert_import for threadDelay:
  -- threadDelay delay
  putStrLn $ "Would delay for " ++ show delay ++ " microseconds"

missingImportFunction3 :: FilePath -> IO ()
missingImportFunction3 path = do
  -- If we uncomment this, it should trigger insert_import for doesFileExist:
  -- exists <- doesFileExist path
  -- when exists $ putStrLn "File exists"
  putStrLn $ "Would check if file exists: " ++ path

-- | Function that could use different import styles
differentImportStyles :: String -> String
differentImportStyles str = 
  -- Currently using qualified T.map, but could use unqualified if we change import
  T.unpack $ T.map toLower (T.pack str)

-- | Duplicated functionality that suggests needing new imports
duplicatedFunctionality :: [String] -> String
duplicatedFunctionality items = 
  -- This manually implements intercalate functionality
  case items of
    [] -> ""
    [x] -> x
    (x:xs) -> x ++ ", " ++ duplicatedFunctionality xs
  -- Could be replaced with: intercalate ", " items (already imported)

-- | Data type for export testing
data ExportedType = TypeA Int | TypeB Text | TypeC
  deriving (Show, Eq)

-- | Function showing import conflicts that might need qualification
-- If we imported both Data.List.sort and Data.Vector.sort, 
-- this might need qualification
sortingFunction :: [Int] -> [Int]
sortingFunction = sort  -- Currently uses Data.List.sort

-- | Function that might trigger organize imports due to grouping
mixedImportUsage :: Text -> IO ()
mixedImportUsage text = do
  -- Uses System.IO
  hPutStrLn stderr "Processing text"
  -- Uses Control.Exception  
  result <- try @SomeException $ do
    -- Uses Data.Text qualified
    let processed = T.toUpper text
    -- Uses Control.Monad
    unless (T.null processed) $ 
      -- Uses System.IO again
      putStrLn (T.unpack processed)
    return processed
  case result of
    Left _ -> putStrLn "Error occurred"
    Right _ -> return ()

-- | Internal helper function (not exported)
internalHelper :: String -> String  
internalHelper str = map toUpper str