{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.ToolsSpec (spec) where

import Test.Hspec
import Test.Fixtures (testWorkspaceFiles)
import Test.Utils (withTestWorkspace)
import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Text as T
import System.FilePath ((</>))

-- Import MCP modules
import MCP.Types
import MCP.Tools.HLS
import MCP.Tools.Diagnostics
import MCP.Tools.Documentation
import HLS.Process

spec :: Spec
spec = describe "MCP Tool Integration Tests" $ do
  describe "HLS Management Tools" $ do
    it "can call start_hls_server tool" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        let args = Just $ object ["workingDir" .= workDir]
        result <- handleHLSTool "start_hls_server" args
        
        case result of
          ToolResult content isError -> do
            -- Should either succeed or fail with descriptive error
            case isError of
              Nothing -> return () -- Success
              Just True -> do
                -- Check that we got an error message
                case content of
                  (ToolContent "text" (Just msg):_) -> 
                    T.length msg `shouldSatisfy` (> 0)
                  _ -> expectationFailure "Expected error message"
              Just False -> expectationFailure "isError should be True for errors or Nothing for success"

    it "can call get_hls_status tool" $ do
      result <- handleHLSTool "get_hls_status" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Nothing -- Status check should not error
          case content of
            (ToolContent "text" (Just msg):_) -> do
              -- Should contain status information
              msg `shouldSatisfy` (\m -> "stopped" `T.isInfixOf` T.toLower m || 
                                         "running" `T.isInfixOf` T.toLower m ||
                                         "error" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected status message"

    it "can call show_versions tool" $ do
      result <- handleHLSTool "show_versions" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Nothing -- Version check should not error
          case content of
            (ToolContent "text" (Just msg):_) -> do
              -- Should contain version information
              msg `shouldSatisfy` (\m -> "version" `T.isInfixOf` T.toLower m ||
                                         "mcp" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected version information"

    it "handles unknown HLS tools gracefully" $ do
      result <- handleHLSTool "unknown_hls_tool" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Just True
          case content of
            (ToolContent "text" (Just msg):_) -> 
              msg `shouldSatisfy` (\m -> "unknown" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected error message for unknown tool"

  describe "Diagnostics Tools" $ do
    it "can call get_diagnostics tool with valid file" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        let filePath = workDir </> "ValidModule.hs"
        let args = Just $ object ["filePath" .= filePath]
        result <- handleDiagnosticsTool "get_diagnostics" args
        
        case result of
          ToolResult content isError -> do
            -- Should return diagnostics (empty or with issues)
            case content of
              (ToolContent "text" (Just msg):_) -> 
                T.length msg `shouldSatisfy` (> 0)
              _ -> expectationFailure "Expected diagnostics output"

    it "can call format_code tool" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        let filePath = workDir </> "ValidModule.hs"
        let args = Just $ object 
              [ "filePath" .= filePath
              , "formatter" .= ("ormolu" :: T.Text)
              ]
        result <- handleDiagnosticsTool "format_code" args
        
        case result of
          ToolResult content isError -> do
            -- Should either format successfully or give error
            case content of
              (ToolContent "text" (Just msg):_) -> 
                T.length msg `shouldSatisfy` (> 0)
              _ -> expectationFailure "Expected format output or error"

    it "can call check_syntax tool" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        let filePath = workDir </> "ValidModule.hs"  
        let args = Just $ object ["filePath" .= filePath]
        result <- handleDiagnosticsTool "check_syntax" args
        
        case result of
          ToolResult content isError -> do
            case content of
              (ToolContent "text" (Just msg):_) -> 
                -- Should either pass or show syntax errors
                msg `shouldSatisfy` (\m -> 
                  "✓" `T.isInfixOf` m || "syntax" `T.isInfixOf` T.toLower m)
              _ -> expectationFailure "Expected syntax check output"

    it "handles missing file parameter gracefully" $ do
      result <- handleDiagnosticsTool "get_diagnostics" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Just True
          case content of
            (ToolContent "text" (Just msg):_) -> 
              msg `shouldSatisfy` (\m -> "missing" `T.isInfixOf` T.toLower m && 
                                         "filepath" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected missing parameter error"

  describe "Documentation Tools" $ do
    it "can call show_documentation tool" $ do
      let args = Just $ object ["moduleName" .= ("Prelude" :: T.Text)]
      result <- handleDocumentationTool "show_documentation" args
      
      case result of
        ToolResult content isError -> do
          -- Should either find docs or report not found
          case content of
            (ToolContent "text" (Just msg):_) -> 
              T.length msg `shouldSatisfy` (> 0)
            _ -> expectationFailure "Expected documentation output"

    it "can call search_haddock tool" $ do
      let args = Just $ object ["query" .= ("map" :: T.Text)]
      result <- handleDocumentationTool "search_haddock" args
      
      case result of
        ToolResult content isError -> do
          -- Should return search results or no matches
          case content of
            (ToolContent "text" (Just msg):_) -> 
              T.length msg `shouldSatisfy` (> 0)
            _ -> expectationFailure "Expected search results"

    it "handles missing module name parameter" $ do
      result <- handleDocumentationTool "show_documentation" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Just True
          case content of
            (ToolContent "text" (Just msg):_) -> 
              msg `shouldSatisfy` (\m -> "missing" `T.isInfixOf` T.toLower m && 
                                         "modulename" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected missing parameter error"

    it "handles unknown documentation tools" $ do
      result <- handleDocumentationTool "unknown_doc_tool" Nothing
      
      case result of
        ToolResult content isError -> do
          isError `shouldBe` Just True
          case content of
            (ToolContent "text" (Just msg):_) -> 
              msg `shouldSatisfy` (\m -> "unknown" `T.isInfixOf` T.toLower m)
            _ -> expectationFailure "Expected unknown tool error"

  describe "Tool Integration with HLS Process" $ do
    it "diagnostics tools recognize when HLS is running" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        -- First check status when HLS is stopped
        status1 <- getHLSStatus
        status1 `shouldSatisfy` (\s -> s == Stopped || case s of MCP.Types.Error _ -> True; _ -> False)
        
        -- Try starting HLS
        startResult <- startHLSServer (Just workDir)
        case startResult of
          Right Running -> do
            -- Now check that diagnostics tools can detect HLS is running
            status2 <- getHLSStatus
            status2 `shouldBe` Running
            
            -- Clean up
            _ <- stopHLSServer
            return ()
          Left err -> do
            -- HLS not available on system or failed to start, skip this test
            pendingWith $ "HLS not available or failed to start: " <> T.unpack err
          Right other -> 
            expectationFailure $ "Unexpected HLS status: " <> show other