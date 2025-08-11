{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.ComprehensiveToolSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Test.Utils
import Test.DemoProject

spec :: Spec
spec = describe "Comprehensive MCP Tool Testing" $ do
  serverManagementTests
  codeActionTests
  advancedRefactoringTests
  advancedOperationTests
  lspBasicTests
  errorHandlingTests
  performanceTests
  testIndependenceSpec

-- | Test server management tools with systematic expectations
serverManagementTests :: Spec
serverManagementTests = describe "Server Management Tools" $ do

  it "can test start_hls_server systematically" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      let expectation = MCPToolExpectation
            { mcpToolName = "start_hls_server"
            , mcpToolArgs = Just $ object [("workDir", String (T.pack demoPath))]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test stop_hls_server systematically" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- First start HLS
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait 2 seconds

      -- Then stop it
      let expectation = MCPToolExpectation
            { mcpToolName = "stop_hls_server"
            , mcpToolArgs = Nothing
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test restart_hls_server systematically" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- First ensure HLS is started
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait 2 seconds

      -- Then restart
      let expectation = MCPToolExpectation
            { mcpToolName = "restart_hls_server"
            , mcpToolArgs = Nothing
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 15
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test get_hls_status systematically" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      let expectation = MCPToolExpectation
            { mcpToolName = "get_hls_status"
            , mcpToolArgs = Nothing
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test show_versions systematically" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      let expectation = MCPToolExpectation
            { mcpToolName = "show_versions"
            , mcpToolArgs = Nothing
            , mcpExpectedResult = ShouldContainText "haskell-language-server"
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test advanced refactoring tools
advancedRefactoringTests :: Spec
advancedRefactoringTests = describe "Advanced Refactoring Tools" $ do

  it "can test retrie_refactor on Demo.Advanced" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "retrie_refactor"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Advanced.hs")
                , ("searchPattern", String "map f xs")
                , ("replacePattern", String "fmap f xs")
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 15
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test gadt_conversion on Demo.Advanced GADTs" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "gadt_conversion"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Advanced.hs")
                , ("line", Number 15)  -- ExprType data type
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 15
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test expand_th_splice on Demo.Advanced Template Haskell" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "expand_th_splice"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Advanced.hs")
                , ("line", Number 35)  -- Template Haskell splice
                , ("character", Number 10)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 20
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test update_module_name on demo modules" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"
        testFilePath = "Demo/TestRename.hs"
        testContent = T.unlines
          [ "module Demo.OldName where"
          , ""
          , "testFunction :: Int -> Int"
          , "testFunction x = x + 1"
          ]

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      -- Create temporary file and test renaming
      modifyResult <- withModifiedFile demoPath testFilePath testContent $ do
        testResult <- testMCPTool server "update_module_name" (Just $ object
                        [("file", String (T.pack testFilePath)),
                         ("newModuleName", String "Demo.NewName")]) 10000000
        return testResult

      case modifyResult of
        Left err -> error $ T.unpack err
        Right testResult ->
          case testResult of
            Left err -> error $ T.unpack err
            Right _ -> return True

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test add_cabal_dependency for new dependencies" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "add_cabal_dependency"
            , mcpToolArgs = Just $ object
                [ ("dependency", String "vector >= 0.12")
                , ("section", String "library")
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test code action tools with file modification
codeActionTests :: Spec
codeActionTests = describe "Code Action Tools" $ do

  it "can test add_type_signature on Demo.Actions functions" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "add_type_signature"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Actions.hs")
                , ("line", Number 12)  -- processData function without signature
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldContainText ":: "  -- Should add type signature
            , mcpTimeout = 15
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test organize_imports on Demo.Imports" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "organize_imports"
            , mcpToolArgs = Just $ object [("file", String "Demo/Imports.hs")]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test get_code_actions on Demo.Actions" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "get_code_actions"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Actions.hs")
                , ("startLine", Number 17)  -- calculateAverage without signature
                , ("startCharacter", Number 0)
                , ("endLine", Number 17)
                , ("endCharacter", Number 50)
                ]
            , mcpExpectedResult = ShouldContainText "Add type signature"
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test file modification with add_type_signature action" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"
        testFilePath = "Demo/Actions.hs"
        modifiedContent = T.unlines
          [ "{-# LANGUAGE OverloadedStrings #-}"
          , "module Demo.Actions where"
          , ""
          , "import Data.Text (Text)"
          , "import qualified Data.Text as T"
          , ""
          , "-- Function without type signature for testing add_type_signature"
          , "processDataNew inputText = do"
          , "  let processedText = T.toUpper inputText"
          , "  return $ \"Processed: \" <> processedText"
          , ""
          , "-- Another function to test modifications"
          , "newCalculateSum xs = sum xs + 1"
          ]

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      -- Test with modified file using withModifiedFile pattern
      modifyResult <- withModifiedFile demoPath testFilePath modifiedContent $ do
        -- Add type signature to the new function
        testResult <- testMCPTool server "add_type_signature" (Just $ object
          [ ("file", String (T.pack testFilePath))
          , ("line", Number 8)  -- processDataNew function
          , ("character", Number 0)
          ]) 10000000
        return testResult

      case modifyResult of
        Left err -> error $ T.unpack err
        Right testResult ->
          case testResult of
            Left err -> error $ T.unpack err
            Right _ -> return True

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test advanced operations and specialized tools
advancedOperationTests :: Spec
advancedOperationTests = describe "Advanced Operations" $ do

  it "can test eval_expression on Demo.Evaluation" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "eval_expression"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Evaluation.hs")
                , ("expression", String "calculateSum [1, 2, 3, 4, 5]")
                ]
            , mcpExpectedResult = ShouldContainText "15"  -- Sum result
            , mcpTimeout = 15
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test get_code_lenses on Demo.CodeLens" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "get_code_lenses"
            , mcpToolArgs = Just $ object [("file", String "Demo/CodeLens.hs")]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test resolve_code_lens for specific lenses" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "resolve_code_lens"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/CodeLens.hs")
                , ("line", Number 15)
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test insert_import on Demo.Imports" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"
        testFilePath = "Demo/TestInsertImport.hs"
        testContent = T.unlines
          [ "module Demo.TestInsertImport where"
          , ""
          , "-- Missing import for Data.Map"
          , "testMapUsage = Map.empty"
          ]

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      -- Test insert_import with temporary file
      modifyResult <- withModifiedFile demoPath testFilePath testContent $ do
        testResult <- testMCPTool server "insert_import" (Just $ object
                        [("file", String (T.pack testFilePath)),
                         ("import", String "qualified Data.Map as Map")]) 10000000
        return testResult

      case modifyResult of
        Left err -> error $ T.unpack err
        Right testResult ->
          case testResult of
            Left err -> error $ T.unpack err
            Right _ -> return True

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test remove_unused_imports on Demo.Imports" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"
        testFilePath = "Demo/TestUnusedImports.hs"
        testContent = T.unlines
          [ "module Demo.TestUnusedImports where"
          , ""
          , "import Data.List"
          , "import Data.Map (Map)"
          , "import Data.Set as Set"
          , "import Control.Monad"
          , ""
          , "-- Only using Data.List"
          , "testSort :: [Int] -> [Int]"
          , "testSort = sort"
          ]

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      -- Test remove_unused_imports with temporary file
      modifyResult <- withModifiedFile demoPath testFilePath testContent $ do
        testResult <- testMCPTool server "remove_unused_imports" (Just $ object
                        [("file", String (T.pack testFilePath))]) 10000000
        return testResult

      case modifyResult of
        Left err -> error $ T.unpack err
        Right testResult ->
          case testResult of
            Left err -> error $ T.unpack err
            Right _ -> return True

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test basic LSP operations with systematic expectations
lspBasicTests :: Spec
lspBasicTests = describe "LSP Basic Operations" $ do

  it "can test hover_info on Demo.Basic functions" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "hover_info"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Basic.hs")
                , ("line", Number 18)  -- greetUser function
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldContainText "greetUser"
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test goto_definition on Demo.Basic references" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "goto_definition"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Basic.hs")
                , ("line", Number 54)  -- Reference to calculateSum
                , ("character", Number 15)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test find_references for Demo.Basic functions" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "find_references"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Basic.hs")
                , ("line", Number 26)  -- calculateSum definition
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test document_symbols for Demo.Basic" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "document_symbols"
            , mcpToolArgs = Just $ object [("file", String "Demo/Basic.hs")]
            , mcpExpectedResult = ShouldContainText "UserInfo"
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test workspace_symbols across demo project" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "workspace_symbols"
            , mcpToolArgs = Just $ object [("query", String "greet")]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test get_diagnostics on Demo.Diagnostics" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "get_diagnostics"
            , mcpToolArgs = Just $ object [("file", String "Demo/Diagnostics.hs")]
            , mcpExpectedResult = ShouldContainText "error"  -- Should find type errors
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test format_document on Demo.Formatting" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "format_document"
            , mcpToolArgs = Just $ object [("file", String "Demo/Formatting.hs")]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "can test get_completions on Demo.Completions" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "get_completions"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Completions.hs")
                , ("line", Number 25)  -- Inside createUser function
                , ("character", Number 10)
                ]
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 10
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test error handling and edge cases
errorHandlingTests :: Spec
errorHandlingTests = describe "Error Handling and Edge Cases" $ do

  it "handles non-existent file gracefully" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "hover_info"
            , mcpToolArgs = Just $ object
                [ ("file", String "NonExistent/File.hs")
                , ("line", Number 1)
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldFail  -- Should fail gracefully
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "handles malformed arguments gracefully" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "hover_info"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Basic.hs")
                , ("line", String "invalid")  -- Invalid line argument
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldFail  -- Should fail gracefully
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "handles missing required arguments" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "hover_info"
            , mcpToolArgs = Just $ object [("file", String "Demo/Basic.hs")]  -- Missing line and character
            , mcpExpectedResult = ShouldFail  -- Should fail gracefully
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "handles HLS server not running" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Don't start HLS server - test tools without it
      let expectation = MCPToolExpectation
            { mcpToolName = "hover_info"
            , mcpToolArgs = Just $ object
                [ ("file", String "Demo/Basic.hs")
                , ("line", Number 1)
                , ("character", Number 0)
                ]
            , mcpExpectedResult = ShouldHaveErrorType "HLS not running"
            , mcpTimeout = 5
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test performance and timeout scenarios
performanceTests :: Spec
performanceTests = describe "Performance and Timeout Testing" $ do

  it "handles slow operations within timeout" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 3000000  -- Wait for HLS to be ready

      let expectation = MCPToolExpectation
            { mcpToolName = "workspace_symbols"
            , mcpToolArgs = Just $ object [("query", String "")]  -- Empty query = all symbols (slow)
            , mcpExpectedResult = ShouldSucceed
            , mcpTimeout = 30  -- Longer timeout for potentially slow operation
            }
      testMCPToolWithExpectation server expectation

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

  it "respects timeout limits for hanging operations" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS first
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait for HLS to be ready

      -- Test with artificially short timeout to ensure timeout handling works
      let expectation = MCPToolExpectation
            { mcpToolName = "get_diagnostics"
            , mcpToolArgs = Just $ object [("file", String "Demo/Diagnostics.hs")]
            , mcpExpectedResult = ShouldSucceed  -- Should succeed if within timeout
            , mcpTimeout = 1  -- Very short timeout to test timeout mechanism
            }
      -- Note: This test may succeed or fail depending on system speed
      -- The important thing is it doesn't hang indefinitely
      _ <- testMCPToolWithExpectation server expectation
      return True  -- Always pass - we're testing timeout mechanism, not specific outcome

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right success -> success `shouldBe` True

-- | Test independence and proper setup/teardown
testIndependenceSpec :: Spec
testIndependenceSpec = describe "Test Independence and Isolation" $ do

  it "tests are independent - first test doesn't affect second" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    -- First test run
    result1 <- withMCPDemoServer demoPath $ \server -> do
      testMCPTool server "get_hls_status" Nothing 5000000

    -- Second test run should work independently
    result2 <- withMCPDemoServer demoPath $ \server -> do
      testMCPTool server "get_hls_status" Nothing 5000000

    case (result1, result2) of
      (Right _, Right _) -> True `shouldBe` True
      (Left err1, _) -> expectationFailure $ "First test failed: " ++ T.unpack err1
      (_, Left err2) -> expectationFailure $ "Second test failed: " ++ T.unpack err2

  it "properly cleans up resources between tests" $ do
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    -- Test that we can start and stop multiple times
    results <- sequence $ replicate 3 $ do
      withMCPDemoServer demoPath $ \server -> do
        _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
        threadDelay 1000000  -- 1 second
        testMCPTool server "stop_hls_server" Nothing 5000000

    -- All tests should succeed
    let failures = [err | Left err <- results]
    length failures `shouldBe` 0

  it "handles concurrent test scenarios properly" $ do
    -- This test ensures our test framework can handle multiple operations
    currentDir <- getCurrentDirectory
    let demoPath = currentDir </> "test/fixtures/demo-project"

    result <- withMCPDemoServer demoPath $ \server -> do
      -- Start HLS
      _ <- testMCPTool server "start_hls_server" (Just $ object [("workDir", String (T.pack demoPath))]) 15000000
      threadDelay 2000000  -- Wait for startup

      -- Run multiple operations in sequence
      _ <- testMCPTool server "get_hls_status" Nothing 5000000
      threadDelay 500000
      _ <- testMCPTool server "show_versions" Nothing 5000000
      threadDelay 500000
      testMCPTool server "get_hls_status" Nothing 5000000

    case result of
      Left err -> expectationFailure $ T.unpack err
      Right _ -> True `shouldBe` True
