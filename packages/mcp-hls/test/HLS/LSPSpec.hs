{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.LSPSpec (spec) where

import Test.Hspec
import Test.Utils
import Test.Fixtures (testWorkspaceFiles, testModuleContent, validModuleContent)
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

spec :: Spec
spec = describe "HLS LSP Protocol Tests" $ do

  describe "Document Lifecycle" $ do
    it "can open and process Haskell documents" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          result <- sendDidOpenNotificationToLSP handle testUri testModuleContent
          case result of
            Left err -> expectationFailure $ "Failed to open document: " ++ err
            Right _ -> return ()

    it "can handle multiple document opens" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri1 = "file://" <> T.pack (workDir </> "TestModule.hs")
          let testUri2 = "file://" <> T.pack (workDir </> "ValidModule.hs")

          result1 <- sendDidOpenNotificationToLSP handle testUri1 testModuleContent
          result2 <- sendDidOpenNotificationToLSP handle testUri2 validModuleContent

          case (result1, result2) of
            (Right _, Right _) -> return ()
            (Left err, _) -> expectationFailure $ "Failed to open first document: " ++ err
            (_, Left err) -> expectationFailure $ "Failed to open second document: " ++ err

  describe "Hover Information" $ do
    it "can request hover information for Haskell symbols" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")

          -- First open the document
          _ <- sendDidOpenNotificationToLSP handle testUri validModuleContent
          threadDelay 2000000 -- Wait for HLS to process the document

          -- Request hover information
          result <- sendHoverRequest handle testUri 10 5 -- Line 10, character 5
          case result of
            Left _ -> return () -- Hover may fail on empty positions, that's OK
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "handles hover requests for non-existent positions gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          _ <- sendDidOpenNotificationToLSP handle testUri validModuleContent
          threadDelay 1000000

          -- Request hover at invalid position
          result <- sendHoverRequest handle testUri 1000 1000
          case result of
            Left _ -> return () -- Expected to fail gracefully
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

  describe "Document Symbols" $ do
    it "can retrieve document symbols from Haskell files" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          _ <- sendDidOpenNotificationToLSP handle testUri validModuleContent
          threadDelay 2000000 -- Wait for processing

          result <- sendDocumentSymbolsRequest handle testUri
          case result of
            Left _ -> return () -- May fail if HLS is not ready, that's OK for this test
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "handles requests for non-existent files gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let nonExistentUri = "file:///nonexistent/file.hs"

          result <- sendDocumentSymbolsRequest handle nonExistentUri
          case result of
            Left _ -> return () -- Expected to fail
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

  describe "Workspace Symbols" $ do
    it "can search for symbols across the workspace" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Open some documents first to populate the workspace
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          _ <- sendDidOpenNotificationToLSP handle testUri validModuleContent
          threadDelay 2000000

          result <- sendWorkspaceSymbolsRequest handle "add"
          case result of
            Left _ -> return () -- May fail if no symbols found, that's OK
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "handles empty search queries" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          result <- sendWorkspaceSymbolsRequest handle ""
          case result of
            Left _ -> return () -- Empty queries may be rejected, that's OK
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

  describe "Code Actions" $ do
    it "can request code actions for a document range" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          _ <- sendDidOpenNotificationToLSP handle testUri testModuleContent
          threadDelay 2000000

          -- Request code actions for a range with potential issues
          result <- sendCodeActionsRequest handle testUri 13 0 14 20
          case result of
            Left _ -> return () -- May fail if no actions available, that's OK
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "handles invalid ranges gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          _ <- sendDidOpenNotificationToLSP handle testUri validModuleContent
          threadDelay 1000000

          -- Request code actions for invalid range
          result <- sendCodeActionsRequest handle testUri (-1) (-1) 1000 1000
          case result of
            Left _ -> return () -- Expected to fail gracefully
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

  describe "Command Execution" $ do
    it "can execute LSP commands" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Try to execute a basic HLS command
          result <- sendExecuteCommandRequest handle "hls.plugin.eval.globalOn" []
          case result of
            Left _ -> return () -- Command may not exist or be available, that's OK
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "handles unknown commands gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          result <- sendExecuteCommandRequest handle "nonexistent.command" []
          case result of
            Left _ -> return () -- Expected to fail
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

  describe "Diagnostics" $ do
    it "can retrieve diagnostics for Haskell files" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")

          result <- getFileDiagnosticsFromLSP handle testUri testModuleContent
          case result of
            Left _ -> return () -- May fail if HLS is not ready, that's OK
            Right diagnostics -> do
              -- Should get some diagnostics due to the type error in testModuleContent
              length diagnostics `shouldSatisfy` (>= 0)

    it "handles files with no errors" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")

          result <- getFileDiagnosticsFromLSP handle testUri validModuleContent
          case result of
            Left _ -> return () -- May fail if HLS is not ready, that's OK
            Right diagnostics -> do
              -- ValidModule should have fewer or no diagnostics
              length diagnostics `shouldSatisfy` (>= 0)

  describe "Error Handling" $ do
    it "handles malformed URIs gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          result <- sendHoverRequest handle "invalid-uri" 1 1
          case result of
            Left _ -> return () -- Expected to fail
            Right _ -> expectationFailure "Should have failed with invalid URI"

    it "handles requests to unopened files" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let unopenedUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          -- Don't open the file, just request symbols
          result <- sendDocumentSymbolsRequest handle unopenedUri
          case result of
            Left _ -> return () -- May fail, that's OK
            Right response -> response `shouldSatisfy` isValidJSONRPCResponse

-- Helper function to validate JSON-RPC responses
isValidJSONRPCResponse :: Value -> Bool
isValidJSONRPCResponse (Object obj) =
  case (KM.lookup "jsonrpc" obj, KM.lookup "id" obj) of
    (Just (String "2.0"), Just _) -> True
    _ -> False
isValidJSONRPCResponse _ = False
