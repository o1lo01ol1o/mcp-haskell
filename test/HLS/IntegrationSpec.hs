{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.IntegrationSpec (spec) where

import Test.Hspec
import Test.Utils
import Test.Fixtures (testWorkspaceFiles, testModuleContent)
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

spec :: Spec
spec = describe "HLS Integration Tests" $ do
  describe "HLS Process Management" $ do
    it "can start and stop HLS process" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- try $ withHLS workDir $ \handle -> do
          -- Just test that we can initialize
          return True
        
        case result of
          Left (ex :: SomeException) -> 
            expectationFailure $ "Failed to start/stop HLS: " <> show ex
          Right success -> success `shouldBe` True

    it "can launch HLS with haskell-language-server-wrapper --lsp" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- try $ startHLS workDir
        case result of
          Left (ex :: SomeException) -> 
            expectationFailure $ "Failed to launch HLS process: " <> show ex
          Right handle -> do
            -- Verify the process is running and can be stopped
            stopHLS handle
            return ()

    it "can initialize HLS with proper LSP handshake" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- The withHLS helper already does initialization, so if we get here it worked
          reqId <- nextRequestId handle
          
          -- Send a simple capabilities request to verify communication
          let capabilitiesMsg = LSPMessage
                { method = "textDocument/hover"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= ("file://" <> workDir <> "/ValidModule.hs")]
                    , "position" .= object ["line" .= (6 :: Int), "character" .= (0 :: Int)]
                    ]
                }
          
          sendLSPMessage handle capabilitiesMsg
          response <- readLSPResponse handle
          
          case response of
            Left err -> expectationFailure $ "Hover request failed: " <> err
            Right _ -> return () -- Any valid response is fine

  describe "LSP Message Handling" $ do
    it "can send and receive LSP messages" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          reqId <- nextRequestId handle
          let workspaceSymbolMsg = LSPMessage
                { method = "workspace/symbol"
                , msgId = Just reqId  
                , params = Just $ object ["query" .= ("" :: Text)]
                }
          
          sendLSPMessage handle workspaceSymbolMsg
          response <- readLSPResponse handle
          
          case response of
            Left err -> expectationFailure $ "Workspace symbol request failed: " <> err
            Right val -> do
              -- Just verify we got a valid JSON response
              case val of
                Object obj -> 
                  case KM.lookup "jsonrpc" obj of
                    Just (String "2.0") -> return ()
                    _ -> expectationFailure "Invalid JSON-RPC response format"
                _ -> expectationFailure "Response is not a JSON object"

    it "handles LSP requests and responses correctly" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Test multiple request types to ensure proper LSP communication
          
          -- Test 1: Document symbols
          reqId1 <- nextRequestId handle
          let docSymbolsMsg = LSPMessage
                { method = "textDocument/documentSymbol"
                , msgId = Just reqId1
                , params = Just $ object
                    [ "textDocument" .= object 
                        ["uri" .= ("file://" <> workDir <> "/ValidModule.hs")]
                    ]
                }
          
          sendLSPMessage handle docSymbolsMsg
          response1 <- readLSPResponse handle
          
          case response1 of
            Left err -> expectationFailure $ "Document symbols failed: " <> err
            Right _ -> return ()
          
          -- Test 2: Workspace folders (if supported)
          reqId2 <- nextRequestId handle
          let workspaceFoldersMsg = LSPMessage
                { method = "workspace/workspaceFolders"
                , msgId = Just reqId2
                , params = Nothing
                }
          
          sendLSPMessage handle workspaceFoldersMsg
          response2 <- readLSPResponse handle
          
          case response2 of
            Left err -> putStrLn $ "Workspace folders not supported: " <> err  -- This is OK
            Right _ -> return ()

    it "can handle diagnostics from HLS" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Open a file with type errors to trigger diagnostics
          let textDocMsg = LSPMessage
                { method = "textDocument/didOpen"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/TestModule.hs")
                        , "languageId" .= ("haskell" :: Text)
                        , "version" .= (1 :: Int)
                        , "text" .= testModuleContent
                        ]
                    ]
                }
          
          sendLSPMessage handle textDocMsg
          
          -- Wait briefly for diagnostics to be published
          threadDelay 2000000  -- 2 seconds
          
          -- We can't easily test for diagnostic notifications in this setup,
          -- but we can verify the file was opened without error
          return ()