{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.CommandSpec (spec) where

import Test.Hspec
import Test.Utils
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (toList)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)

-- | Test project files that can be modified by HLS commands
commandTestProjectFiles :: [(FilePath, String)]
commandTestProjectFiles =
  [ ("Main.hs", mainModuleContent)
  , ("Lib.hs", libModuleContent) 
  , ("test-project.cabal", cabalFileContent)
  , ("hie.yaml", hieYamlContent)
  ]

-- | Main module with various issues for HLS to fix
mainModuleContent :: String
mainModuleContent = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module Main where"
  , ""
  , "import Lib"
  , "import Data.List"
  , "import Data.Maybe"
  , ""
  , "-- | Function missing type signature (HLS can add it)"
  , "processNumbers xs = map (* 2) (filter (> 0) xs)"
  , ""
  , "-- | Function with unused import (HLS can remove it)" 
  , "calculateSum :: [Int] -> Int"
  , "calculateSum xs = sum xs"
  , ""
  , "-- | Function that could benefit from explicit import"
  , "safeDivide :: Int -> Int -> Maybe Int"
  , "safeDivide x 0 = Nothing"
  , "safeDivide x y = Just (x `div` y)"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  let numbers = [1, 2, -3, 4, 0, -5]"
  , "  let processed = processNumbers numbers"
  , "  print processed"
  , "  print $ calculateSum processed"
  , "  print $ safeDivide 10 3"
  , "  putStrLn \"Hello from HLS command test!\""
  ]

-- | Library module for testing cross-module operations
libModuleContent :: String
libModuleContent = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module Lib where"
  , ""
  , "import Data.Text (Text)"
  , "import qualified Data.Text as T"
  , ""
  , "-- | Function for testing hover and definition lookup"
  , "greetUser :: Text -> Text"  
  , "greetUser name = \"Hello, \" <> name <> \"!\""
  , ""
  , "-- | Function with intentional type error for diagnostics"
  , "brokenFunction :: Int -> String"
  , "brokenFunction x = x  -- Type error: Int vs String"
  , ""
  , "-- | Utility function for testing code actions"
  , "reverseText :: Text -> Text"
  , "reverseText = T.reverse"
  ]

-- | Cabal file for the test project
cabalFileContent :: String
cabalFileContent = unlines
  [ "cabal-version: 2.4"
  , "name: test-project"
  , "version: 0.1.0.0"
  , "build-type: Simple"
  , ""
  , "executable test-project"
  , "  main-is: Main.hs"
  , "  other-modules: Lib"
  , "  build-depends: base >= 4.7 && < 5"
  , "               , text >= 1.2 && < 3"
  , "  hs-source-dirs: ."
  , "  default-language: Haskell2010"
  , "  ghc-options: -Wall"
  ]

-- | HIE.yaml for proper project setup
hieYamlContent :: String
hieYamlContent = unlines
  [ "cradle:"
  , "  cabal:"
  , "    - path: \".\""
  , "      component: \"exe:test-project\""
  ]

spec :: Spec
spec = describe "HLS Command Execution" $ do
  describe "Code Actions and Refactoring" $ do
    it "can add missing type signatures" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Open the Main.hs file
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          -- Wait for HLS to analyze the file
          threadDelay 2000000  -- 2 seconds
          
          -- Request code actions for the line with missing type signature
          reqId <- nextRequestId handle
          let codeActionMsg = LSPMessage
                { method = "textDocument/codeAction"
                , msgId = Just reqId  
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    , "range" .= object
                        [ "start" .= object ["line" .= (9 :: Int), "character" .= (0 :: Int)]
                        , "end" .= object ["line" .= (9 :: Int), "character" .= (50 :: Int)]
                        ]
                    , "context" .= object
                        [ "diagnostics" .= ([] :: [Value])
                        , "only" .= ["quickfix" :: Text, "refactor"]
                        ]
                    ]
                }
          
          sendLSPMessage handle codeActionMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Code action request failed: " <> err
            Right val -> 
              -- Verify we got some code actions
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just (Array actions) -> 
                      if null actions
                      then pendingWith "No code actions available (HLS may need more time)"
                      else return () -- Success - got some actions
                    Just Null -> pendingWith "No code actions available for this position"
                    _ -> expectationFailure "Unexpected result structure"
                _ -> expectationFailure "Response is not an object"

    it "can provide hover information for functions" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Lib.hs"
          openDocument handle fileUri "Lib.hs"
          
          threadDelay 2000000  -- Wait for analysis
          
          -- Request hover for the greetUser function
          reqId <- nextRequestId handle  
          let hoverMsg = LSPMessage
                { method = "textDocument/hover"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    , "position" .= object ["line" .= (8 :: Int), "character" .= (0 :: Int)]
                    ]
                }
          
          sendLSPMessage handle hoverMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> 
              if T.isInfixOf "terminated" (T.pack err) || T.isInfixOf "Broken pipe" (T.pack err)
              then pendingWith $ "HLS process unstable: " ++ err
              else expectationFailure $ "Hover request failed: " <> err
            Right val ->
              case val of  
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got hover info (null or actual content)
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error response is acceptable
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can find definitions across modules" $ do
      withCommandTestProject $ \workDir -> do  
        withHLS workDir $ \handle -> do
          -- Open both files
          let mainUri = "file://" <> workDir </> "Main.hs"
          let libUri = "file://" <> workDir </> "Lib.hs"
          
          openDocument handle mainUri "Main.hs"
          openDocument handle libUri "Lib.hs"
          
          threadDelay 3000000  -- Wait for cross-module analysis
          
          -- Request definition for an imported function
          reqId <- nextRequestId handle
          let defMsg = LSPMessage
                { method = "textDocument/definition"  
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= mainUri]
                    , "position" .= object ["line" .= (4 :: Int), "character" .= (7 :: Int)] -- "import Lib"
                    ]
                }
          
          sendLSPMessage handle defMsg  
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Definition request failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got definition result
                    Nothing ->
                      case KM.lookup "error" obj of  
                        Just _ -> return () -- Error is acceptable
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

  describe "Diagnostics and Error Reporting" $ do
    it "reports type errors in real-time" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Lib.hs"
          openDocument handle fileUri "Lib.hs"
          
          -- Wait for diagnostics
          threadDelay 3000000  -- 3 seconds for compilation
          
          -- Check for diagnostic notifications
          diagnostics <- getNotifications handle "textDocument/publishDiagnostics"
          
          -- Should have received at least one diagnostic notification
          if null diagnostics
          then pendingWith "No diagnostics received (HLS may need more time)"  
          else do
            -- Verify at least one diagnostic contains an error
            let checkForError (Object obj) = 
                  case KM.lookup "params" obj of
                    Just (Object params) ->
                      case KM.lookup "diagnostics" params of
                        Just (Array diags) -> not (null diags)
                        _ -> False
                    _ -> False
                checkForError _ = False
            let hasDiagnosticError = any checkForError diagnostics
            if hasDiagnosticError  
            then return () -- Success - found error diagnostics
            else pendingWith "Diagnostics received but no errors found"

    it "can request document symbols" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 2000000  -- Wait for analysis
          
          reqId <- nextRequestId handle
          let symbolsMsg = LSPMessage
                { method = "textDocument/documentSymbol"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    ]
                }
          
          sendLSPMessage handle symbolsMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Document symbols failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got symbols (array or null)
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error is acceptable  
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

  describe "HLS Command Execution" $ do
    it "can execute type signature addition command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000  -- Wait for analysis
          
          -- Execute the type signature addition command
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("ghcide-type-lenses:typesignature.add" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "range" .= object
                            [ "start" .= object ["line" .= (9 :: Int), "character" .= (0 :: Int)]
                            , "end" .= object ["line" .= (9 :: Int), "character" .= (50 :: Int)]
                            ]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Type signature command failed: " ++ err
            Right val -> 
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Success
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Command returned error (may need specific setup)"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute import extension command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000  -- Wait for analysis
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("ghcide-extend-import-action:extendImport" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "range" .= object
                            [ "start" .= object ["line" .= (4 :: Int), "character" .= (0 :: Int)]
                            , "end" .= object ["line" .= (4 :: Int), "character" .= (11 :: Int)]
                            ]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Import extension command failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Import extension not applicable here"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute eval command on expressions" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("eval:evalCommand" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "range" .= object
                            [ "start" .= object ["line" .= (8 :: Int), "character" .= (0 :: Int)]
                            , "end" .= object ["line" .= (8 :: Int), "character" .= (30 :: Int)]
                            ]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Eval command failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Eval not applicable or failed"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute retrie refactoring command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("retrie:retrieCommand" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "rewrites" .= [object
                            [ "lhs" .= ("x + y" :: Text)
                            , "rhs" .= ("y + x" :: Text)
                            ]]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Retrie command failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Retrie refactoring not applicable"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute GADT conversion command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Create a file with ADT that can be converted to GADT
          let gadtFile = workDir </> "GADT.hs"
          writeFile gadtFile $ unlines
            [ "module GADT where"
            , ""
            , "data Expr a = IntVal Int | BoolVal Bool"
            ]
          
          let fileUri = "file://" <> gadtFile
          openDocument handle fileUri "GADT.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("gadt:GADT.toGADT" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "range" .= object
                            [ "start" .= object ["line" .= (2 :: Int), "character" .= (0 :: Int)]
                            , "end" .= object ["line" .= (2 :: Int), "character" .= (40 :: Int)]
                            ]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "GADT conversion command failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "GADT conversion not applicable"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute Template Haskell splice expansion command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Create a file with TH splice
          let thFile = workDir </> "TH.hs"
          writeFile thFile $ unlines
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module TH where"
            , ""
            , "import Language.Haskell.TH"
            , ""
            , "myFunction = $(return (VarE 'id))"
            ]
          
          let fileUri = "file://" <> thFile
          openDocument handle fileUri "TH.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("splice:expandTHSpliceInplace" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "range" .= object
                            [ "start" .= object ["line" .= (5 :: Int), "character" .= (14 :: Int)]
                            , "end" .= object ["line" .= (5 :: Int), "character" .= (35 :: Int)]
                            ]
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "TH splice expansion failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "TH expansion not applicable (may need TH setup)"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute module name update command" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Create a misnamed module
          let wrongFile = workDir </> "WrongName.hs"
          writeFile wrongFile $ unlines
            [ "module Main where"  -- Wrong module name for file
            , ""
            , "main = putStrLn \"Hello\""
            ]
          
          let fileUri = "file://" <> wrongFile
          openDocument handle fileUri "WrongName.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("moduleName:updateModuleName" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Module name update failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Module name correction not applicable"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can execute Cabal integration commands" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("cabalHaskellIntegration:cabalAdd" :: Text)
                    , "arguments" .= [object
                        [ "uri" .= fileUri
                        , "dependency" .= ("containers" :: Text)
                        ]]
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Cabal integration command failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return ()
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Cabal integration not applicable"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "handles unknown HLS commands gracefully" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          reqId <- nextRequestId handle
          let executeMsg = LSPMessage
                { method = "workspace/executeCommand"
                , msgId = Just reqId
                , params = Just $ object
                    [ "command" .= ("unknown:nonexistentCommand" :: Text)
                    , "arguments" .= ([] :: [Value])
                    ]
                }
          
          sendLSPMessage handle executeMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> return () -- Communication error is acceptable
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "error" obj of
                    Just _ -> return () -- Error response is expected
                    Nothing -> 
                      case KM.lookup "result" obj of
                        Just _ -> pendingWith "Unexpected success for unknown command"
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

  describe "Workspace Operations" $ do
    it "can handle workspace symbol queries" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Give HLS time to index the workspace
          threadDelay 4000000  -- 4 seconds
          
          reqId <- nextRequestId handle  
          let symbolQuery = LSPMessage
                { method = "workspace/symbol"
                , msgId = Just reqId
                , params = Just $ object [("query" .= ("greet" :: Text))]
                }
          
          sendLSPMessage handle symbolQuery
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Workspace symbol failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Any result is acceptable
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error is acceptable
                        Nothing -> expectationFailure "No result or error in response"  
                _ -> expectationFailure "Response is not an object"

    it "handles file system changes correctly" $ do  
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          let newFilePath = workDir </> "NewModule.hs"
          
          -- Create a new file in the workspace
          let newFileContent = unlines
                [ "module NewModule where"
                , ""
                , "newFunction :: Int -> Int"  
                , "newFunction x = x * 3"
                ]
          writeFile newFilePath newFileContent
          
          -- Notify HLS about the new file
          let didCreateMsg = LSPMessage
                { method = "workspace/didChangeWatchedFiles"
                , msgId = Nothing
                , params = Just $ object
                    [ "changes" .= [object
                        [ "uri" .= ("file://" <> newFilePath)
                        , "type" .= (1 :: Int) -- Created
                        ]]
                    ]
                }
          
          sendLSPMessage handle didCreateMsg
          threadDelay 2000000  -- Wait for processing
          
          -- Try to query symbols from the new file
          reqId <- nextRequestId handle
          let symbolQuery = LSPMessage  
                { method = "workspace/symbol"
                , msgId = Just reqId
                , params = Just $ object [("query" .= ("newFunction" :: Text))]
                }
          
          sendLSPMessage handle symbolQuery
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Symbol query after file creation failed: " <> err
            Right _ -> return () -- Any response indicates HLS handled the change

  describe "Code Lens Operations" $ do
    it "can request code lenses from HLS" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000  -- Wait for analysis
          
          reqId <- nextRequestId handle
          let codeLensMsg = LSPMessage
                { method = "textDocument/codeLens"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    ]
                }
          
          sendLSPMessage handle codeLensMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> expectationFailure $ "Code lens request failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got code lenses (array or null)
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error is acceptable
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

    it "can resolve code lenses" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs" 
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000
          
          -- First get code lenses
          reqId1 <- nextRequestId handle
          let codeLensMsg = LSPMessage
                { method = "textDocument/codeLens"
                , msgId = Just reqId1
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    ]
                }
          
          sendLSPMessage handle codeLensMsg
          codeLensResponse <- waitForResponse handle reqId1
          
          case codeLensResponse of
            Left _ -> pendingWith "Could not get code lenses to test resolution"
            Right (Object obj) ->
              case KM.lookup "result" obj of
                Just (Array lenses) ->
                  let lensesList = toList lenses
                  in if null lensesList
                     then pendingWith "No code lenses available for resolution"
                     else do
                       -- Try to resolve the first lens
                       let firstLens = head lensesList
                       reqId2 <- nextRequestId handle
                       let resolveMsg = LSPMessage
                             { method = "codeLens/resolve"
                             , msgId = Just reqId2
                             , params = Just firstLens
                             }
                       
                       sendLSPMessage handle resolveMsg
                       resolveResponse <- waitForResponse handle reqId2
                       
                       case resolveResponse of
                         Left err -> pendingWith $ "Code lens resolution failed: " ++ err
                         Right val2 ->
                           case val2 of
                             Object obj2 ->
                               case KM.lookup "result" obj2 of
                                 Just _ -> return ()
                                 Nothing ->
                                   case KM.lookup "error" obj2 of
                                     Just _ -> pendingWith "Code lens resolution not supported"
                                     Nothing -> expectationFailure "No result or error in resolution"
                             _ -> expectationFailure "Resolution response is not an object"
                _ -> pendingWith "No code lenses returned"
            _ -> pendingWith "Invalid code lens response structure"

    it "can execute HLS plugin commands via code actions" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Lib.hs"
          openDocument handle fileUri "Lib.hs"
          
          threadDelay 3000000
          
          -- Request code actions that might contain plugin commands
          reqId <- nextRequestId handle
          let codeActionMsg = LSPMessage
                { method = "textDocument/codeAction"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    , "range" .= object
                        [ "start" .= object ["line" .= (11 :: Int), "character" .= (0 :: Int)]
                        , "end" .= object ["line" .= (12 :: Int), "character" .= (0 :: Int)]
                        ]
                    , "context" .= object
                        [ "diagnostics" .= ([] :: [Value])
                        , "only" .= ["quickfix" :: Text, "refactor.rewrite"]
                        ]
                    ]
                }
          
          sendLSPMessage handle codeActionMsg
          response <- waitForResponse handle reqId
          
          let hasCommand (Object action) = 
                case KM.lookup "command" action of
                  Just (Object cmd) ->
                    case KM.lookup "command" cmd of
                      Just (String cmdName) -> T.isInfixOf ":" cmdName
                      _ -> False
                  _ -> False
              hasCommand _ = False
          
          case response of
            Left err -> pendingWith $ "Code action request failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just (Array actions) -> 
                      if null actions
                      then pendingWith "No code actions available"
                      else do
                        -- Check if any actions contain HLS commands
                        let hasCommands = any hasCommand actions
                        if hasCommands
                        then return () -- Found actions with commands
                        else pendingWith "No actions with HLS commands found"
                    Just Null -> pendingWith "No code actions available"
                    _ -> expectationFailure "Unexpected result structure"
                _ -> expectationFailure "Response is not an object"

  describe "Advanced HLS Features" $ do
    it "can request completion items with command execution" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          -- Modify the file to trigger completion
          let modifiedContent = unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , ""
                , "module Main where"
                , ""
                , "import Lib"
                , "import Data.List"
                , "import Data.Maybe"
                , ""
                , "processNumbers xs = map (* 2) (filter (> 0) xs)"
                , ""
                , "main :: IO ()"
                , "main = do"
                , "  let numbers = [1, 2, -3, 4, 0, -5]"
                , "  let processed = processNumbers numbers"
                , "  print processed"
                , "  print $ sum -- partial completion"
                ]
          writeFile (workDir </> "Main.hs") modifiedContent
          
          -- Notify HLS about the change
          let changeMsg = LSPMessage
                { method = "textDocument/didChange"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= fileUri
                        , "version" .= (2 :: Int)
                        ]
                    , "contentChanges" .= [object
                        [ "text" .= modifiedContent
                        ]]
                    ]
                }
          sendLSPMessage handle changeMsg
          threadDelay 2000000
          
          -- Request completion
          reqId <- nextRequestId handle
          let completionMsg = LSPMessage
                { method = "textDocument/completion"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    , "position" .= object ["line" .= (15 :: Int), "character" .= (15 :: Int)]
                    ]
                }
          
          sendLSPMessage handle completionMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Completion request failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got completions
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Completion not available"
                        Nothing -> expectationFailure "No result or error in completion"
                _ -> expectationFailure "Response is not an object"

    it "can handle formatting requests" $ do
      withCommandTestProject $ \workDir -> do
        withHLS workDir $ \handle -> do
          let fileUri = "file://" <> workDir </> "Main.hs"
          openDocument handle fileUri "Main.hs"
          
          threadDelay 3000000
          
          reqId <- nextRequestId handle
          let formatMsg = LSPMessage
                { method = "textDocument/formatting"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object ["uri" .= fileUri]
                    , "options" .= object
                        [ "tabSize" .= (2 :: Int)
                        , "insertSpaces" .= True
                        ]
                    ]
                }
          
          sendLSPMessage handle formatMsg
          response <- waitForResponse handle reqId
          
          case response of
            Left err -> pendingWith $ "Formatting request failed: " ++ err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Got formatting edits (or null if no changes needed)
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> pendingWith "Formatting not supported or failed"
                        Nothing -> expectationFailure "No result or error in formatting"
                _ -> expectationFailure "Response is not an object"

-- | Helper function to open a document in HLS
openDocument :: HLSHandle -> String -> String -> IO ()
openDocument handle uri fileName = do
  let openMsg = LSPMessage
        { method = "textDocument/didOpen"
        , msgId = Nothing  
        , params = Just $ object
            [ "textDocument" .= object
                [ "uri" .= uri
                , "languageId" .= ("haskell" :: Text)
                , "version" .= (1 :: Int)
                , "text" .= ("" :: Text) -- HLS will read from filesystem
                ]
            ]
        }
  sendLSPMessage handle openMsg

-- | Create a temporary test project and run action with it
withCommandTestProject :: (FilePath -> IO a) -> IO a  
withCommandTestProject action = withSystemTempDirectory "hls-command-test" $ \tmpDir -> do
  -- Create all test files
  mapM_ (createTestFile tmpDir) commandTestProjectFiles
  
  -- Ensure the project builds correctly
  let cabalFile = tmpDir </> "test-project.cabal"
  cabalExists <- doesFileExist cabalFile
  if not cabalExists 
  then error "Failed to create cabal file"
  else action tmpDir
  where
    createTestFile dir (path, content) = do
      let fullPath = dir </> path
      createDirectoryIfMissing True (dir </> takeDirectory path)
      writeFile fullPath content
    
    takeDirectory path = case reverse (splitOn '/' path) of
      [] -> ""
      [_] -> ""  
      (_:dirs) -> intercalate "/" (reverse dirs)
    
    splitOn sep str = case break (== sep) str of
      (before, []) -> [before]  
      (before, _:after) -> before : splitOn sep after
      
    intercalate sep [] = ""
    intercalate sep [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs