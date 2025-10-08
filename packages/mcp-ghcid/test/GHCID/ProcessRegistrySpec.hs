{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCID.ProcessRegistrySpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Internal imports
import GHCID.ProcessRegistry
import Test.Utils (withTestTimeout)

spec :: Spec
spec = describe "GHCID.ProcessRegistry" $ do
  
  describe "ProcessRegistry creation and shutdown" $ do
    it "can create and shutdown a process registry" $ withTestTimeout 10 $ do
      bracket
        createProcessRegistry
        shutdownProcessRegistry
        (\registry -> do
          -- Verify registry is created
          activeCount <- length <$> listActiveProcesses registry
          activeCount `shouldBe` 0)
    
    it "can handle multiple shutdowns gracefully" $ withTestTimeout 10 $ do
      registry <- createProcessRegistry
      shutdownProcessRegistry registry
      shutdownProcessRegistry registry  -- Should not crash
      
      activeCount <- length <$> listActiveProcesses registry
      activeCount `shouldBe` 0
  
  describe "Process lifecycle management" $ do
    it "can start and stop a GHCID process" $ withTestTimeout 30 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        -- Create a simple cabal project structure
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://example"
            
            -- Start process
            startResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            startResult `shouldSatisfy` either (const False) (const True)
            
            case startResult of
              Left err -> expectationFailure $ "Failed to start process: " ++ T.unpack err
              Right handle -> do
                -- Verify process is listed
                activeProcesses <- listActiveProcesses registry
                activeProcesses `shouldContain` [cabalURI]
                
                -- Check process status
                status <- getProcessStatus handle
                status `shouldNotBe` GHCIDStopped
                
                -- Stop process
                stopResult <- stopGHCIDProcess registry cabalURI
                stopResult `shouldBe` Right ()
                
                -- Verify process is no longer listed
                activeProcessesAfter <- listActiveProcesses registry
                activeProcessesAfter `shouldNotContain` [cabalURI])
    
    it "prevents starting duplicate processes" $ withTestTimeout 20 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://duplicate"
            
            -- Start first process
            firstResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            firstResult `shouldSatisfy` either (const False) (const True)
            
            case firstResult of
              Left err -> expectationFailure $ "Failed to start first process: " ++ T.unpack err
              Right _ -> do
                -- Try to start second process with same URI
                secondResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
                secondResult `shouldSatisfy` either (const True) (const False))
    
    it "can handle process that fails to start" $ withTestTimeout 15 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        -- Don't create cabal file - this should cause GHCID to fail
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://failing"
            
            startResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            startResult `shouldSatisfy` either (const True) (const False))
  
  describe "Process communication" $ do
    it "can read output from GHCID process" $ withTestTimeout 30 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://output"
            
            startResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            case startResult of
              Left err -> expectationFailure $ "Failed to start process: " ++ T.unpack err
              Right handle -> do
                -- Wait a bit for GHCID to produce output
                threadDelay 5000000 -- 5 seconds
                
                -- Try to read output
                output <- getBufferedOutput handle
                output `shouldSatisfy` (not . T.null))
    
    it "handles sendToGHCID correctly (should return error)" $ withTestTimeout 20 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://send"
            
            startResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            case startResult of
              Left err -> expectationFailure $ "Failed to start process: " ++ T.unpack err
              Right handle -> do
                -- GHCID doesn't accept input, so this should fail
                sendResult <- sendToGHCID handle "test message"
                sendResult `shouldSatisfy` either (const True) (const False))
  
  describe "Health monitoring" $ do
    it "can check process health" $ withTestTimeout 20 $
      withSystemTempDirectory "ghcid-test" $ \tmpDir -> do
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://health"
            
            startResult <- startGHCIDProcess registry cabalURI tmpDir Nothing []
            case startResult of
              Left err -> expectationFailure $ "Failed to start process: " ++ T.unpack err
              Right _ -> do
                -- Check health
                healthResult <- checkProcessHealth registry cabalURI
                healthResult `shouldSatisfy` either (const False) (const True)
                
                case healthResult of
                  Left err -> expectationFailure $ "Health check failed: " ++ T.unpack err
                  Right status -> status `shouldNotBe` GHCIDStopped)
    
    it "returns error for non-existent process health check" $ withTestTimeout 5 $ do
      bracket
        createProcessRegistry
        shutdownProcessRegistry
        (\registry -> do
          let cabalURI = CabalURI "test://nonexistent"
          
          healthResult <- checkProcessHealth registry cabalURI
          healthResult `shouldSatisfy` either (const True) (const False))

-- Test files content
testCabalFile :: String
testCabalFile = unlines
  [ "cabal-version: 2.4"
  , "name: test-project"
  , "version: 0.1.0.0"
  , "library"
  , "  exposed-modules: Main"
  , "  build-depends: base"
  , "  hs-source-dirs: src"
  , "  default-language: Haskell2010"
  ]

testMainFile :: String
testMainFile = unlines
  [ "module Main where"
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"Hello, World!\""
  ]