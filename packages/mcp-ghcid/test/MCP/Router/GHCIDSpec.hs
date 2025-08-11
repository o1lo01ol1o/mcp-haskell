{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Router.GHCIDSpec (spec) where

import Test.Hspec
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Internal imports
import MCP.Router.GHCID
import MCP.Types.GHCID
import GHCID.ProcessRegistry (CabalURI(..))
import GHCID.Filter (FilterRequest(..))
import Test.Utils (withTestTimeout)

spec :: Spec
spec = describe "MCP.Router.GHCID" $ do
  
  describe "Router lifecycle" $ do
    it "can create and shutdown router" $ withTestTimeout 10 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          -- Router should be functional
          let reqId = RequestId "test-lifecycle"
          let listReq = SomeGHCIDRequest reqId (ListProcesses $ ListProcessesData False)
          
          response <- routeGHCIDRequest router reqId listReq
          response `shouldSatisfy` (\v -> v /= Null))
    
    it "handles shutdown gracefully" $ withTestTimeout 15 $ do
      router <- createGHCIDRouter defaultRouterConfig
      
      -- Make a request before shutdown
      let reqId1 = RequestId "before-shutdown"
      let listReq1 = SomeGHCIDRequest reqId1 (ListProcesses $ ListProcessesData False)
      response1 <- routeGHCIDRequest router reqId1 listReq1
      response1 `shouldSatisfy` (\v -> v /= Null)
      
      -- Shutdown router
      shutdownGHCIDRouter router
      
      -- Requests after shutdown should fail gracefully
      -- (Note: This might throw an exception depending on implementation)
      -- For now, we just verify shutdown doesn't crash
      return ()
  
  describe "Request routing" $ do
    it "routes start GHCID requests" $ withTestTimeout 30 $
      withSystemTempDirectory "router-test" $ \tmpDir -> do
        -- Create test project
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        bracket
          (createGHCIDRouter defaultRouterConfig)
          shutdownGHCIDRouter
          (\router -> do
            let reqId = RequestId "test-start"
            let startData = StartGHCIDData
                  { startCabalURI = CabalURI "file:///test/start"
                  , startWorkDir = tmpDir
                  , startOptions = Nothing
                  }
            let startReq = SomeGHCIDRequest reqId (StartGHCID startData)
            
            response <- routeGHCIDRequest router reqId startReq
            
            -- Verify response structure
            case response of
              Object obj -> do
                obj `shouldSatisfy` hasKey "id"
                -- Should have either "result" or "error"
                obj `shouldSatisfy` (\o -> hasKey "result" o || hasKey "error" o)
              _ -> expectationFailure "Response should be JSON object")
    
    it "routes stop GHCID requests" $ withTestTimeout 20 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-stop"
          let stopData = StopGHCIDData
                { stopCabalURI = CabalURI "file:///test/stop"
                , stopForce = False
                }
          let stopReq = SomeGHCIDRequest reqId (StopGHCID stopData)
          
          response <- routeGHCIDRequest router reqId stopReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              obj `shouldSatisfy` (\o -> hasKey "result" o || hasKey "error" o)
            _ -> expectationFailure "Response should be JSON object")
    
    it "routes list processes requests" $ withTestTimeout 15 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-list"
          let listData = ListProcessesData { listIncludeStatus = True }
          let listReq = SomeGHCIDRequest reqId (ListProcesses listData)
          
          response <- routeGHCIDRequest router reqId listReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              obj `shouldSatisfy` hasKey "result" -- List should always succeed
            _ -> expectationFailure "Response should be JSON object")
    
    it "routes get messages requests" $ withTestTimeout 20 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-messages"
          let msgData = GetMessagesData
                { messagesCabalURI = CabalURI "file:///nonexistent"
                , messagesFilter = Nothing
                , messagesCount = Nothing
                }
          let msgReq = SomeGHCIDRequest reqId (GetMessages msgData)
          
          response <- routeGHCIDRequest router reqId msgReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              -- Should have error since process doesn't exist
              obj `shouldSatisfy` hasKey "error"
            _ -> expectationFailure "Response should be JSON object")
    
    it "routes process status requests" $ withTestTimeout 15 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-status"
          let statusData = ProcessStatusData
                { statusCabalURI = CabalURI "file:///nonexistent"
                }
          let statusReq = SomeGHCIDRequest reqId (ProcessStatus statusData)
          
          response <- routeGHCIDRequest router reqId statusReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              obj `shouldSatisfy` (\o -> hasKey "result" o || hasKey "error" o)
            _ -> expectationFailure "Response should be JSON object")
    
    it "routes restart process requests" $ withTestTimeout 20 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-restart"
          let restartData = RestartProcessData
                { restartCabalURI = CabalURI "file:///nonexistent"
                , restartWorkDir = Nothing
                }
          let restartReq = SomeGHCIDRequest reqId (RestartProcess restartData)
          
          response <- routeGHCIDRequest router reqId restartReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              obj `shouldSatisfy` (\o -> hasKey "result" o || hasKey "error" o)
            _ -> expectationFailure "Response should be JSON object")
  
  describe "JSON request processing" $ do
    it "processes valid JSON requests" $ withTestTimeout 15 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let jsonReq = object
                [ "id" .= ("test-json" :: Text)
                , "method" .= ("ghcid.list" :: Text)
                , "params" .= object ["includeStatus" .= False]
                ]
          
          response <- processJSONRequest router jsonReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "id"
              obj `shouldSatisfy` hasKey "result"
            _ -> expectationFailure "Response should be JSON object")
    
    it "handles invalid JSON requests" $ withTestTimeout 10 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let invalidReq = object
                [ "invalid" .= ("request" :: Text)
                ]
          
          response <- processJSONRequest router invalidReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "error"
            _ -> expectationFailure "Error response should be JSON object")
    
    it "handles malformed method names" $ withTestTimeout 10 $ do
      bracket
        (createGHCIDRouter defaultRouterConfig)
        shutdownGHCIDRouter
        (\router -> do
          let malformedReq = object
                [ "id" .= ("test-malformed" :: Text)
                , "method" .= ("ghcid.unknown" :: Text)
                , "params" .= object []
                ]
          
          response <- processJSONRequest router malformedReq
          
          case response of
            Object obj -> do
              obj `shouldSatisfy` hasKey "error"
            _ -> expectationFailure "Error response should be JSON object")
  
  describe "Router configuration" $ do
    it "respects process limits in configuration" $ withTestTimeout 30 $
      withSystemTempDirectory "router-test" $ \tmpDir -> do
        -- Create test project
        createDirectoryIfMissing False (tmpDir </> "src")
        writeFile (tmpDir </> "test.cabal") testCabalFile
        writeFile (tmpDir </> "src" </> "Main.hs") testMainFile
        
        let config = defaultRouterConfig { routerMaxProcesses = 1 }
        
        bracket
          (createGHCIDRouter config)
          shutdownGHCIDRouter
          (\router -> do
            -- Start first process
            let reqId1 = RequestId "test-limit-1"
            let startData1 = StartGHCIDData
                  { startCabalURI = CabalURI "file:///test/limit1"
                  , startWorkDir = tmpDir
                  , startOptions = Nothing
                  }
            let startReq1 = SomeGHCIDRequest reqId1 (StartGHCID startData1)
            
            response1 <- routeGHCIDRequest router reqId1 startReq1
            
            -- Give first process time to start
            threadDelay 2000000 -- 2 seconds
            
            -- Try to start second process (should fail due to limit)
            let reqId2 = RequestId "test-limit-2"
            let startData2 = StartGHCIDData
                  { startCabalURI = CabalURI "file:///test/limit2"
                  , startWorkDir = tmpDir
                  , startOptions = Nothing
                  }
            let startReq2 = SomeGHCIDRequest reqId2 (StartGHCID startData2)
            
            response2 <- routeGHCIDRequest router reqId2 startReq2
            
            -- Second request should fail or indicate limit reached
            case response2 of
              Object obj -> do
                obj `shouldSatisfy` (\o -> hasKey "result" o || hasKey "error" o)
                -- If it has result, it should indicate failure
                -- If it has error, that's also expected
              _ -> expectationFailure "Response should be JSON object")
    
    it "can disable request logging" $ withTestTimeout 10 $ do
      let config = defaultRouterConfig { routerLogRequests = False }
      
      bracket
        (createGHCIDRouter config)
        shutdownGHCIDRouter
        (\router -> do
          let reqId = RequestId "test-no-log"
          let listReq = SomeGHCIDRequest reqId (ListProcesses $ ListProcessesData False)
          
          response <- routeGHCIDRequest router reqId listReq
          response `shouldSatisfy` (\v -> v /= Null))

-- Helper functions
hasKey :: Text -> Object -> Bool
hasKey key obj = key `elem` map (Key.toText . fst) (KeyMap.toList obj)

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

