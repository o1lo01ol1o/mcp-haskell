{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.DemoProject
  ( -- * Demo project management
    withDemoProject
  , setupDemoProject
  , cleanupDemoProject
  , demoProjectFiles
    -- * File modification utilities
  , modifyDemoFile
  , resetDemoFile
  , backupDemoFile
  , restoreDemoFile
  , withModifiedFile
    -- * Test expectations
  , ToolTestExpectation(..)
  , ExpectationType(..)
  , verifyExpectation
    -- * Constants
  , demoProjectPath
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, SomeException)
import Control.Monad (when, unless, forM_, filterM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath ((</>), takeDirectory)
import Text.Regex.TDFA
import Test.Utils (ExpectationType(..))

-- | Path to the demo project relative to test directory
demoProjectPath :: String
demoProjectPath = "fixtures/demo-project"

-- | Tool test expectation with description
data ToolTestExpectation = ToolTestExpectation
  { expectationDescription :: Text
  , expectationType :: ExpectationType  
  , expectationTimeout :: Int  -- seconds
  } deriving (Show, Eq)

-- | Demo project files that should be available for testing
demoProjectFiles :: [(FilePath, Text)]
demoProjectFiles =
  [ ("Demo/Basic.hs", "Basic module for hover, goto, references")
  , ("Demo/Diagnostics.hs", "Module with intentional errors and warnings") 
  , ("Demo/Actions.hs", "Functions without type signatures for code actions")
  , ("Demo/Imports.hs", "Complex import scenarios")
  , ("Demo/Advanced.hs", "GADTs and Template Haskell")
  , ("Demo/Completions.hs", "Code completion scenarios")
  , ("Demo/Formatting.hs", "Poorly formatted code")
  , ("Demo/CodeLens.hs", "Code lens generation scenarios")
  , ("Demo/Evaluation.hs", "Expression evaluation scenarios")
  , ("demo-project.cabal", "Comprehensive cabal configuration")
  , ("hie.yaml", "HLS configuration")
  ]

-- | Set up the demo project in a temporary directory
-- Copies the demo project files to a temporary location for testing
setupDemoProject :: FilePath -> IO (Either Text FilePath)
setupDemoProject testRootDir = do
  result <- try $ do
    let sourcePath = testRootDir </> demoProjectPath
        targetPath = testRootDir </> "temp-demo-project"
        
    -- Check if source demo project exists
    sourceExists <- doesDirectoryExist sourcePath
    unless sourceExists $ 
      error $ "Demo project not found at: " ++ sourcePath
      
    -- Clean up any existing temp project
    targetExists <- doesDirectoryExist targetPath
    when targetExists $ removeDirectoryRecursive targetPath
    
    -- Copy demo project to temp location
    copyDirectoryRecursive sourcePath targetPath
    
    -- Verify all expected files are present
    missingFiles <- filterM (fmap not . doesFileExist . (targetPath </>)) 
                            (map fst demoProjectFiles)
    unless (null missingFiles) $
      error $ "Missing demo project files: " ++ show missingFiles
      
    -- Initialize cabal project (basic check)
    -- Just verify cabal file exists, don't actually configure
    cabalExists <- doesFileExist (targetPath </> "demo-project.cabal")
    unless cabalExists $ error "Cabal file not found in demo project"
    
    return targetPath
    
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to setup demo project: " <> T.pack (show ex)
    Right path -> return $ Right path

-- | Clean up the demo project directory
cleanupDemoProject :: FilePath -> IO ()
cleanupDemoProject projectPath = do
  exists <- doesDirectoryExist projectPath
  when exists $ do
    -- Try to clean up, but don't fail if it doesn't work
    result <- try $ removeDirectoryRecursive projectPath
    case result of
      Left (_ :: SomeException) -> return ()  -- Ignore cleanup failures
      Right _ -> return ()

-- | Bracket-style demo project setup and cleanup
withDemoProject :: FilePath -> (FilePath -> IO a) -> IO (Either Text a)
withDemoProject testRootDir action = do
  result <- try $ bracket
    (setupDemoProject testRootDir)
    (\setupResult -> case setupResult of
       Right path -> cleanupDemoProject path
       Left _ -> return ())
    (\setupResult -> case setupResult of
       Right path -> action path
       Left err -> error $ T.unpack err)
       
  case result of
    Left (ex :: SomeException) -> return $ Left $ T.pack $ show ex
    Right res -> return $ Right res

-- | Run action with modified file, then restore original
withModifiedFile :: FilePath -> FilePath -> Text -> IO a -> IO (Either Text a)
withModifiedFile projectPath relativeFilePath newContent action = do
  backupResult <- backupDemoFile projectPath relativeFilePath
  case backupResult of
    Left err -> return $ Left err
    Right _ -> do
      modifyResult <- modifyDemoFile projectPath relativeFilePath newContent
      case modifyResult of
        Left err -> do
          -- Try to restore backup even if modification failed
          _ <- restoreDemoFile projectPath relativeFilePath
          return $ Left err
        Right _ -> do
          -- Run the action with modified file
          actionResult <- try action
          -- Always restore the original file
          _ <- restoreDemoFile projectPath relativeFilePath
          case actionResult of
            Left (ex :: SomeException) -> return $ Left $ T.pack $ show ex
            Right res -> return $ Right res

-- | Create a backup of a demo project file
backupDemoFile :: FilePath -> FilePath -> IO (Either Text ())  
backupDemoFile projectPath relativeFilePath = do
  result <- try $ do
    let originalPath = projectPath </> relativeFilePath
        backupPath = originalPath ++ ".backup"
    
    originalExists <- doesFileExist originalPath
    unless originalExists $
      error $ "Original file not found: " ++ originalPath
      
    copyFile originalPath backupPath
    
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to backup file: " <> T.pack (show ex)
    Right _ -> return $ Right ()

-- | Restore a demo project file from backup
restoreDemoFile :: FilePath -> FilePath -> IO (Either Text ())
restoreDemoFile projectPath relativeFilePath = do
  result <- try $ do
    let originalPath = projectPath </> relativeFilePath
        backupPath = originalPath ++ ".backup"
    
    backupExists <- doesFileExist backupPath
    unless backupExists $
      error $ "Backup file not found: " ++ backupPath
      
    copyFile backupPath originalPath
    removeFile backupPath
    
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to restore file: " <> T.pack (show ex) 
    Right _ -> return $ Right ()

-- | Modify a demo project file with new content
modifyDemoFile :: FilePath -> FilePath -> Text -> IO (Either Text ())
modifyDemoFile projectPath relativeFilePath newContent = do
  result <- try $ do
    let filePath = projectPath </> relativeFilePath
    
    -- Create directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory filePath)
    
    -- Write new content
    T.writeFile filePath newContent
    
    -- Give HLS time to process the change
    threadDelay 500000  -- 0.5 seconds
    
  case result of
    Left (ex :: SomeException) -> return $ Left $ "Failed to modify file: " <> T.pack (show ex)
    Right _ -> return $ Right ()

-- | Reset a demo project file to its original content
resetDemoFile :: FilePath -> FilePath -> IO (Either Text ())
resetDemoFile projectPath relativeFilePath = restoreDemoFile projectPath relativeFilePath

-- | Verify that a test expectation is met
verifyExpectation :: ToolTestExpectation -> Text -> Bool
verifyExpectation expectation actualOutput = 
  case expectationType expectation of
    ShouldSucceed -> 
      not $ anyErrorIndicator actualOutput
      
    ShouldFail ->
      anyErrorIndicator actualOutput
      
    ShouldContainText expectedText ->
      expectedText `T.isInfixOf` actualOutput
      
    ShouldMatchPattern pattern ->
      actualOutput =~ T.unpack pattern
      
    ShouldHaveErrorType expectedError ->
      expectedError `T.isInfixOf` actualOutput && anyErrorIndicator actualOutput
      
    ShouldHaveWarningCount expectedCount ->
      countWarnings actualOutput == expectedCount
      
    ShouldHaveCompletionCount expectedCount ->
      countCompletions actualOutput == expectedCount
      
    ShouldHaveTypeSignature expectedType ->
      expectedType `T.isInfixOf` actualOutput
      
    ShouldHaveImport expectedImport ->
      ("import " <> expectedImport) `T.isInfixOf` actualOutput

-- | Helper functions for expectation verification

anyErrorIndicator :: Text -> Bool
anyErrorIndicator output = 
  any (`T.isInfixOf` T.toLower output) 
    ["error", "fail", "exception", "invalid", "not found", "undefined"]

countWarnings :: Text -> Int  
countWarnings output = 
  (length $ T.splitOn "warning:" output) - 1

countCompletions :: Text -> Int
countCompletions output =
  (length $ T.splitOn "completion:" output) - 1

-- | Copy directory recursively (helper function)
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dest = do
  createDirectoryIfMissing True dest
  contents <- listDirectory src
  forM_ contents $ \item -> do
    let srcPath = src </> item
        destPath = dest </> item
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirectoryRecursive srcPath destPath
      else copyFile srcPath destPath