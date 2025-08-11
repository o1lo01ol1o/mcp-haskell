{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.SimpleEndToEndSpec (spec) where

import Test.Hspec
import Test.Utils
import Test.Fixtures (testWorkspaceFiles)
import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Text as T
import System.FilePath ((</>))

spec :: Spec
spec = describe "Simple MCP End-to-End Tests" $ do

  describe "MCP Server Basic Operations" $ do
    it "can start MCP server with demo project" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          -- If we reach here, server started successfully
          return True

        case result of
          Left err -> expectationFailure $ "Failed to start MCP server: " ++ T.unpack err
          Right success -> success `shouldBe` True

    it "can call get_hls_status tool" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          toolResult <- testMCPTool mcpServer "get_hls_status" Nothing 5000000 -- 5 second timeout
          case toolResult of
            Left err -> expectationFailure $ "HLS status call failed: " ++ T.unpack err
            Right response -> do
              -- Verify we got a valid JSON response
              case response of
                Object _ -> return () -- Success
                _ -> expectationFailure "Expected JSON object response"

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()

    it "can call get_hls_version tool" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          toolResult <- testMCPTool mcpServer "get_hls_version" Nothing 5000000 -- 5 second timeout
          case toolResult of
            Left err -> expectationFailure $ "HLS version call failed: " ++ T.unpack err
            Right response -> do
              -- Verify we got a valid JSON response
              case response of
                Object _ -> return () -- Success
                _ -> expectationFailure "Expected JSON object response"

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()

  describe "MCP Tool Testing with Expectations" $ do
    it "tests multiple tools with different timeouts" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          let expectations =
                [ MCPToolExpectation
                    { mcpToolName = "get_hls_status"
                    , mcpToolArgs = Nothing
                    , mcpExpectedResult = ShouldSucceed
                    , mcpTimeout = 5000000 -- 5 seconds
                    }
                , MCPToolExpectation
                    { mcpToolName = "get_hls_version"
                    , mcpToolArgs = Nothing
                    , mcpExpectedResult = ShouldSucceed
                    , mcpTimeout = 3000000 -- 3 seconds
                    }
                , MCPToolExpectation
                    { mcpToolName = "nonexistent_tool"
                    , mcpToolArgs = Nothing
                    , mcpExpectedResult = ShouldFail
                    , mcpTimeout = 2000000 -- 2 seconds
                    }
                ]

          toolTestResults <- runMCPToolTests mcpServer expectations
          let summary = generateMCPTestSummary toolTestResults

          -- Verify that at least 2 out of 3 tests passed (status and version should work)
          passedTests summary `shouldSatisfy` (>= 2)
          totalTests summary `shouldBe` 3

          return ()

        case result of
          Left err -> expectationFailure $ "MCP tool testing failed: " ++ T.unpack err
          Right _ -> return ()

  describe "Error Handling" $ do
    it "handles invalid tool calls gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          toolResult <- testMCPTool mcpServer "invalid_tool_name" Nothing 3000000 -- 3 second timeout
          case toolResult of
            Left _ -> return () -- Expected to fail
            Right response -> do
              -- Check if response indicates error
              let responseText = T.pack $ show response
              if "error" `T.isInfixOf` T.toLower responseText
                then return () -- Error response is expected
                else expectationFailure "Expected error response for invalid tool"

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()

    it "handles timeout scenarios" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          -- Use a very short timeout to test timeout handling
          toolResult <- testMCPTool mcpServer "get_hls_status" Nothing 1000 -- 1 millisecond timeout
          case toolResult of
            Left err -> do
              -- Should timeout
              let errText = T.toLower err
              if "timeout" `T.isInfixOf` errText
                then return ()
                else expectationFailure $ "Expected timeout error, got: " ++ T.unpack err
            Right _ ->
              -- If it somehow succeeded in 1ms, that's also fine
              return ()

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()

  describe "File Operations" $ do
    it "can test file-related tools" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          let testFilePath = workDir </> "TestModule.hs"
          let fileArgs = object ["path" .= T.pack testFilePath]

          toolResult <- testMCPTool mcpServer "get_file_diagnostics" (Just fileArgs) 10000000 -- 10 second timeout
          case toolResult of
            Left err -> do
              -- File diagnostics might fail if HLS is not available, which is OK
              putStrLn $ "File diagnostics failed (may be expected): " ++ T.unpack err
              return ()
            Right response -> do
              -- Any valid response is fine
              case response of
                Object _ -> return ()
                _ -> expectationFailure "Expected JSON object response"

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()

  describe "Performance and Resource Management" $ do
    it "can handle multiple sequential requests" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- withMCPDemoServer workDir $ \mcpServer -> do
          -- Make multiple requests in sequence
          results <- sequence $ replicate 5 $ do
            testMCPTool mcpServer "get_hls_status" Nothing 5000000 -- 5 second timeout

          let successCount = length $ filter isRight results
          -- At least half should succeed
          successCount `shouldSatisfy` (>= 2)
          return ()

        case result of
          Left err -> expectationFailure $ "MCP server error: " ++ T.unpack err
          Right _ -> return ()
      where
        isRight (Right _) = True
        isRight _ = False
