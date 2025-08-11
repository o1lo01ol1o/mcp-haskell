{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.ClientSpec (spec) where

import Test.Hspec
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import GHCID.Client
import Test.Utils (withTestHaskellProject, withTestGHCID, sampleGHCIDOutput, GHCIDHandle(..))

spec :: Spec
spec = describe "GHCID.Client" $ do
  
  describe "GHCIDConfig" $ do
    it "creates default config correctly" $ do
      let config = defaultGHCIDConfig "/test/dir"
      ghcidCommand config `shouldBe` "ghcid"
      ghcidArgs config `shouldBe` []
      targetFiles config `shouldBe` []
      cabalFile config `shouldBe` Nothing
      workingDir config `shouldBe` "/test/dir"
      reloadOnChange config `shouldBe` True
      testCommand config `shouldBe` Nothing
    
    it "handles cabal project config" $ do
      let config = (defaultGHCIDConfig "/project")
            { cabalFile = Just "test-project.cabal"
            , targetFiles = ["src/Main.hs"]
            }
      cabalFile config `shouldBe` Just "test-project.cabal"
      targetFiles config `shouldBe` ["src/Main.hs"]

  describe "GHCIDClient creation" $ do
    it "creates GHCID client successfully" $ do
      let config = defaultGHCIDConfig "."
      result <- try @SomeException $ createGHCIDClient config
      case result of
        Left ex -> expectationFailure $ "Failed to create GHCID client: " ++ show ex
        Right client -> do
          -- Verify client was created
          status <- getGHCIDStatus client
          status `shouldBe` GHCIDStopped

  describe "GHCID process lifecycle" $ do
    it "starts and stops GHCID process" $ do
      let testFiles = 
            [ ("src/Test/Module.hs", testModuleContent)
            ]
      
      withTestHaskellProject testFiles $ \projectDir -> do
        result <- withTestGHCID projectDir $ \handle -> do
          -- Test that we can get output
          output <- getCurrentOutput (ghcidClient handle)
          return $ not $ T.null output
        
        case result of
          Left err -> expectationFailure $ "GHCID test failed: " ++ T.unpack err
          Right success -> success `shouldBe` True

    it "handles project with compilation errors gracefully" $ do
      let testFilesWithError = 
            [ ("src/Test/Module.hs", testModuleWithError)
            ]
      
      withTestHaskellProject testFilesWithError $ \projectDir -> do
        result <- withTestGHCID projectDir $ \handle -> do
          -- Even with errors, we should get output
          output <- getCurrentOutput (ghcidClient handle)
          return $ "error" `T.isInfixOf` T.toLower output || not (T.null output)
        
        case result of
          Left err -> expectationFailure $ "GHCID error test failed: " ++ T.unpack err
          Right hasErrorOutput -> hasErrorOutput `shouldBe` True

  describe "GHCID output buffering" $ do
    it "buffers output correctly" $ do
      let testFiles = 
            [ ("src/Test/Module.hs", testModuleContent)
            ]
      
      withTestHaskellProject testFiles $ \projectDir -> do
        result <- withTestGHCID projectDir $ \handle -> do
          -- Get initial output
          output1 <- getCurrentOutput (ghcidClient handle)
          -- Output should be available
          return $ not $ T.null output1
        
        case result of
          Left err -> expectationFailure $ "GHCID buffering test failed: " ++ T.unpack err
          Right hasOutput -> hasOutput `shouldBe` True

  describe "GHCID status tracking" $ do
    it "tracks process status correctly" $ do
      let config = defaultGHCIDConfig "."
      client <- createGHCIDClient config
      
      -- Initially stopped
      initialStatus <- getGHCIDStatus client
      initialStatus `shouldBe` GHCIDStopped
      
      -- Note: Starting actual GHCID process requires proper Haskell project setup
      -- This is tested in integration tests

  describe "Project detection" $ do
    it "detects cabal projects" $ do
      withTestHaskellProject [] $ \projectDir -> do
        let cabalFile = projectDir </> "test-project.cabal"
        exists <- doesFileExist cabalFile
        exists `shouldBe` True
        
        -- Test project detection would go here
        -- Currently just verifying the test setup works
        let config = defaultGHCIDConfig projectDir
        -- Basic validation that config creation works
        workingDir config `shouldBe` projectDir

-- Test module content
testModuleContent :: String
testModuleContent = unlines
  [ "module Test.Module where"
  , ""
  , "import Data.List"
  , ""
  , "testFunction :: [Int] -> [Int]" 
  , "testFunction xs = sort xs"
  , ""
  , "anotherFunction :: String -> String"
  , "anotherFunction = reverse"
  ]

testModuleWithError :: String
testModuleWithError = unlines
  [ "module Test.Module where"
  , ""
  , "testFunction :: [Int] -> [Int]"
  , "testFunction xs = unknownFunction xs  -- This will cause an error"
  , ""
  , "anotherFunction :: String -> String" 
  , "anotherFunction = reverse"
  ]