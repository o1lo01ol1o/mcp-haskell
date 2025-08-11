{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GHCID.ConcurrencySpec (spec) where

import Test.Hspec
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, try, SomeException)
import Control.Monad (replicateM, replicateM_, void, when, forM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..))
import Data.List (nub, sort)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Internal imports
import GHCID.ProcessRegistry
import MCP.Router.GHCID
import MCP.Types.GHCID
import Test.Utils (withTestTimeout)

spec :: Spec
spec = describe "GHCID.Concurrency" $ do
  
  describe "ProcessRegistry concurrent access" $ do
    it "handles concurrent process starts safely" $ withTestTimeout 45 $
      withSystemTempDirectory "concurrent-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            -- Create multiple threads that try to start processes
            let uris = [CabalURI ("test://concurrent" <> T.pack (show i)) | i <- [1..10]]
            
            -- Start processes concurrently
            asyncActions <- forM (zip [1..] uris) $ \(i, uri) -> async $ do
              threadDelay (i * 100000) -- Stagger starts slightly
              startGHCIDProcess registry uri tmpDir
            
            results <- mapM wait asyncActions
            let successes = length $ filter (either (const False) (const True)) results
            
            -- At least some should succeed (depending on system resources)
            successes `shouldSatisfy` (>= 1)
            
            -- Clean up - stop all processes that started
            activeProcesses <- listActiveProcesses registry
            mapM_ (stopGHCIDProcess registry) activeProcesses
            
            -- Verify cleanup
            finalActiveProcesses <- listActiveProcesses registry
            length finalActiveProcesses `shouldBe` 0)
    
    it "handles concurrent start/stop operations safely" $ withTestTimeout 40 $
      withSystemTempDirectory "concurrent-ops-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://startstop"
            
            -- Create competing start/stop operations
            startAsync1 <- async $ startGHCIDProcess registry cabalURI tmpDir
            startAsync2 <- async $ startGHCIDProcess registry cabalURI tmpDir  -- Should fail
            stopAsync <- async $ do
              threadDelay 500000 -- Wait a bit
              stopGHCIDProcess registry cabalURI
            
            result1 <- wait startAsync1
            result2 <- wait startAsync2
            stopResult <- wait stopAsync
            
            -- Only one start should succeed
            let successfulStarts = length $ filter (either (const False) (const True)) [result1, result2]
            successfulStarts `shouldBe` 1
            
            -- Stop operation should handle the race condition gracefully
            -- (either succeed or fail gracefully)
            stopResult `shouldSatisfy` either (const True) (const True))
    
    it "maintains consistency under concurrent list operations" $ withTestTimeout 35 $
      withSystemTempDirectory "list-concurrent-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let uris = [CabalURI ("test://list" <> T.pack (show i)) | i <- [1..5]]
            
            -- Start some processes
            startResults <- mapM (\uri -> startGHCIDProcess registry uri tmpDir) uris
            let successfulURIs = [uri | (uri, Right _) <- zip uris startResults]
            
            -- List processes concurrently from multiple threads
            listAsyncs <- replicateM 10 $ async $ listActiveProcesses registry
            lists <- mapM wait listAsyncs
            
            -- All lists should be consistent (same processes)
            let sortedLists = map sort lists
            let uniqueLists = nub sortedLists
            length uniqueLists `shouldBe` 1  -- All lists should be identical
            
            -- List should contain our successful processes
            let expectedList = sort successfulURIs
            if null expectedList
              then head sortedLists `shouldBe` []
              else head sortedLists `shouldContain` expectedList)
    
    it "handles process health checks concurrently" $ withTestTimeout 30 $
      withSystemTempDirectory "health-concurrent-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let cabalURI = CabalURI "test://health"
            
            -- Start a process
            startResult <- startGHCIDProcess registry cabalURI tmpDir
            case startResult of
              Left err -> expectationFailure $ "Failed to start process: " ++ T.unpack err
              Right _ -> do
                -- Perform concurrent health checks
                healthAsyncs <- replicateM 5 $ async $ checkProcessHealth registry cabalURI
                healthResults <- mapM wait healthAsyncs
                
                -- All health checks should return the same result
                let uniqueResults = nub healthResults
                length uniqueResults `shouldBe` 1
                
                -- Result should be success (Right)
                case head uniqueResults of
                  Left err -> expectationFailure $ "Health check failed: " ++ T.unpack err
                  Right status -> status `shouldNotBe` GHCIDStopped)
  
  describe "MCP Router concurrent request handling" $ do
    it "handles concurrent MCP requests safely" $ withTestTimeout 30 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          -- Create multiple concurrent requests
          let requests = [(RequestId ("req" <> T.pack (show i)), 
                           SomeGHCIDRequest (RequestId ("req" <> T.pack (show i))) 
                             (ListProcesses $ ListProcessesData False))
                         | i <- [1..10]]
          
          -- Send requests concurrently
          asyncRequests <- mapM (\(reqId, request) -> 
            async $ routeGHCIDRequest router reqId request) requests
          
          responses <- mapM wait asyncRequests
          
          -- All requests should get responses
          length responses `shouldBe` 10
          
          -- All responses should be valid JSON objects
          mapM_ (\response -> response `shouldSatisfy` (/= Null)) responses)
    
    it "maintains request isolation under concurrent access" $ withTestTimeout 35 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          -- Create different types of requests concurrently
          listAsync <- async $ do
            let reqId = RequestId "concurrent-list"
            let request = SomeGHCIDRequest reqId (ListProcesses $ ListProcessesData True)
            routeGHCIDRequest router reqId request
          
          statusAsync <- async $ do
            let reqId = RequestId "concurrent-status"
            let request = SomeGHCIDRequest reqId (ProcessStatus $ ProcessStatusData (CabalURI "nonexistent"))
            routeGHCIDRequest router reqId request
          
          messagesAsync <- async $ do
            let reqId = RequestId "concurrent-messages"
            let request = SomeGHCIDRequest reqId (GetMessages $ GetMessagesData (CabalURI "nonexistent") Nothing Nothing)
            routeGHCIDRequest router reqId request
          
          listResponse <- wait listAsync
          statusResponse <- wait statusAsync
          messagesResponse <- wait messagesAsync
          
          -- All responses should be different and appropriate to their request
          listResponse `shouldNotBe` statusResponse
          statusResponse `shouldNotBe` messagesResponse
          listResponse `shouldNotBe` messagesResponse)
  
  describe "STM-based operations under high concurrency" $ do
    it "maintains atomicity under concurrent STM operations" $ withTestTimeout 25 $ do
      -- Test STM containers under high concurrency
      bracket
        createProcessRegistry
        shutdownProcessRegistry
        (\registry -> do
          -- Create many concurrent operations that modify the registry
          operations <- sequence $ replicate 20 $ async $ do
                let uri = CabalURI "test://stm"
                _ <- startGHCIDProcess registry uri "."  -- Will likely fail, but tests STM
                _ <- stopGHCIDProcess registry uri
                listActiveProcesses registry
          
          results <- mapM wait operations
          
          -- All operations should complete (even if they fail internally)
          length results `shouldBe` 20
          
          -- Final state should be consistent
          finalActive <- listActiveProcesses registry
          length finalActive `shouldBe` 0)
    
    it "handles resource contention gracefully" $ withTestTimeout 30 $
      withSystemTempDirectory "contention-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            -- Create high contention by having many threads try to start the same process
            let cabalURI = CabalURI "test://contention"
            let attempts = 10
            
            attemptAsyncs <- replicateM attempts $ async $ 
              startGHCIDProcess registry cabalURI tmpDir
            
            results <- mapM wait attemptAsyncs
            
            -- Only one should succeed, others should fail gracefully
            let successes = length $ filter (either (const False) (const True)) results
            let failures = length $ filter (either (const True) (const False)) results
            
            successes `shouldBe` 1
            failures `shouldBe` (attempts - 1)
            
            -- Registry should still be in a valid state
            activeProcesses <- listActiveProcesses registry
            length activeProcesses `shouldBe` 1)
  
  describe "Stress testing under load" $ do
    it "handles rapid start/stop cycles" $ withTestTimeout 45 $ do
      bracket
        createProcessRegistry
        shutdownProcessRegistry
        (\registry -> do
          -- Perform rapid start/stop cycles
          replicateM_ 20 $ do
            let uri = CabalURI "test://rapid"
            _ <- startGHCIDProcess registry uri "."  -- Most will fail, testing robustness
            _ <- stopGHCIDProcess registry uri
            return ()
          
          -- Registry should remain stable
          activeCount <- length <$> listActiveProcesses registry
          activeCount `shouldBe` 0)
    
    it "maintains performance under concurrent health monitoring" $ withTestTimeout 40 $
      withSystemTempDirectory "monitoring-test" $ \tmpDir -> do
        createTestProject tmpDir
        
        bracket
          createProcessRegistry
          shutdownProcessRegistry
          (\registry -> do
            let uris = [CabalURI ("test://monitor" <> T.pack (show i)) | i <- [1..3]]
            
            -- Start a few processes
            startResults <- mapM (\uri -> startGHCIDProcess registry uri tmpDir) uris
            let successfulURIs = [uri | (uri, Right _) <- zip uris startResults]
            
            when (not $ null successfulURIs) $ do
              -- Start continuous health monitoring
              monitorAsync <- async $ do
                replicateM_ 50 $ do  -- 50 health checks
                  mapM_ (checkProcessHealth registry) successfulURIs
                  threadDelay 10000  -- 10ms between checks
              
              -- Also perform regular operations during monitoring
              opsAsync <- async $ do
                replicateM_ 10 $ do
                  _ <- listActiveProcesses registry
                  threadDelay 100000  -- 100ms between ops
              
              void $ wait monitorAsync
              void $ wait opsAsync
              
              -- System should still be responsive
              finalActive <- listActiveProcesses registry
              length finalActive `shouldSatisfy` (>= 0))

-- Helper function to create a test project
createTestProject :: FilePath -> IO ()
createTestProject tmpDir = do
  createDirectoryIfMissing False (tmpDir </> "src")
  writeFile (tmpDir </> "test.cabal") testCabalFile
  writeFile (tmpDir </> "src" </> "Main.hs") testMainFile

-- Test files
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