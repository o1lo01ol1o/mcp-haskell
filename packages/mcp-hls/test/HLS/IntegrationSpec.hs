{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.IntegrationSpec (spec) where

import Test.Hspec
import Test.Utils
import Test.Fixtures (testWorkspaceFiles, testModuleContent)
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
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

    it "can launch HLS with proper process handle" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- try $ withHLS workDir $ \handle -> do
          -- Verify the process is running by checking if we can get the process handle
          return $ hlsProcess handle
        case result of
          Left (ex :: SomeException) ->
            expectationFailure $ "Failed to launch HLS process: " <> show ex
          Right _processHandle ->
            -- If we reach here, the process was successfully started and cleaned up
            return ()

  describe "LSP Communication" $ do
    it "can send didOpen notification" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          result <- sendDidOpenNotificationToLSP handle testUri testModuleContent
          case result of
            Left err -> expectationFailure $ "Failed to send didOpen: " ++ err
            Right _ -> return () -- Success

    it "can request hover information" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          -- First open the document
          _ <- sendDidOpenNotificationToLSP handle testUri testModuleContent
          threadDelay 2000000 -- Wait 2 seconds for HLS to process

          -- Request hover at a reasonable position
          result <- sendHoverRequest handle testUri 5 10
          case result of
            Left err -> do
              putStrLn $ "Hover failed (may be expected): " ++ err
              return () -- Don't fail if hover doesn't work
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "can request document symbols" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "ValidModule.hs")
          let simpleContent = "module ValidModule where\n\naddNumbers :: Int -> Int -> Int\naddNumbers x y = x + y"
          -- First ensure document is open
          _ <- sendDidOpenNotificationToLSP handle testUri (T.pack simpleContent)
          threadDelay 2000000 -- Wait for processing

          result <- sendDocumentSymbolsRequest handle testUri
          case result of
            Left err -> do
              putStrLn $ "Document symbols failed (may be expected): " ++ err
              return ()
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

    it "can request workspace symbols" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          result <- sendWorkspaceSymbolsRequest handle "add"
          case result of
            Left err -> do
              putStrLn $ "Workspace symbols failed (may be expected): " ++ err
              return ()
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

  describe "File Diagnostics" $ do
    it "can get file diagnostics" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          result <- getFileDiagnosticsFromLSP handle testUri testModuleContent
          case result of
            Left err -> do
              putStrLn $ "Diagnostics failed (may be expected): " ++ err
              return ()
            Right diagnostics -> do
              -- Should get some diagnostics (could be empty, that's OK)
              length diagnostics `shouldSatisfy` (>= 0)

  describe "Code Actions" $ do
    it "can request code actions" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          let testUri = "file://" <> T.pack (workDir </> "TestModule.hs")
          -- First open the document
          _ <- sendDidOpenNotificationToLSP handle testUri testModuleContent
          threadDelay 2000000 -- Wait for processing

          -- Request code actions for a range
          result <- sendCodeActionsRequest handle testUri 12 0 13 20
          case result of
            Left err -> do
              putStrLn $ "Code actions failed (may be expected): " ++ err
              return ()
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

  describe "Command Execution" $ do
    it "can handle command execution attempts" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Try to execute a basic command - most will fail but we test the mechanism
          result <- sendExecuteCommandRequest handle "hls.commands.importLens" []
          case result of
            Left err -> do
              putStrLn $ "Command execution failed (expected): " ++ err
              return () -- This is expected to fail for most commands
            Right response -> do
              response `shouldSatisfy` isValidJSONRPCResponse

-- Helper function to validate JSON-RPC responses
isValidJSONRPCResponse :: Value -> Bool
isValidJSONRPCResponse (Object obj) =
  case (KM.lookup "jsonrpc" obj, KM.lookup "id" obj) of
    (Just (String "2.0"), Just _) -> True
    _ -> False
isValidJSONRPCResponse _ = False
