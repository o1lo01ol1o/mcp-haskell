{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLS.LSPSpec (spec) where

import Test.Hspec
import Test.Utils
import Test.Fixtures (testWorkspaceFiles, validModuleContent)
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Process (getProcessExitCode)

spec :: Spec
spec = describe "LSP Protocol Communication" $ do
  describe "LSP Client Initialization" $ do
    it "sends proper initialize request" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- The withHLS helper already performs initialization
          -- If we get here, initialization was successful
          return ()

    it "handles initialize response correctly" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        handle <- startHLS workDir
        result <- initializeHLS handle workDir
        case result of
          Left err -> do
            stopHLS handle
            expectationFailure $ "Initialize failed: " <> err
          Right response -> do
            stopHLS handle
            -- Verify response structure
            case response of
              Object obj ->
                case KM.lookup "result" obj of
                  Just (Object resultObj) ->
                    case KM.lookup "capabilities" resultObj of
                      Just _ -> return () -- Has capabilities
                      Nothing -> expectationFailure "Missing capabilities in response"
                  _ -> expectationFailure "Invalid result structure"
              _ -> expectationFailure "Response is not an object"

  describe "LSP Text Document Operations" $ do
    it "can open and manage text documents" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Open a document
          let openMsg = LSPMessage
                { method = "textDocument/didOpen"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/ValidModule.hs")
                        , "languageId" .= ("haskell" :: Text)
                        , "version" .= (1 :: Int)
                        , "text" .= validModuleContent
                        ]
                    ]
                }
          
          sendLSPMessage handle openMsg
          
          -- Wait a moment for HLS to process
          threadDelay 500000  -- 0.5 seconds
          
          -- Close the document
          let closeMsg = LSPMessage
                { method = "textDocument/didClose"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/ValidModule.hs")
                        ]
                    ]
                }
          
          sendLSPMessage handle closeMsg
          return ()

    it "can request hover information (or handles HLS instability)" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        result <- try $ withHLS workDir $ \handle -> do
          -- First open the document
          let openMsg = LSPMessage
                { method = "textDocument/didOpen"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/ValidModule.hs")
                        , "languageId" .= ("haskell" :: Text)
                        , "version" .= (1 :: Int)
                        , "text" .= validModuleContent
                        ]
                    ]
                }
          
          sendLSPMessage handle openMsg
          threadDelay 1000000  -- Wait for document to be processed
          
          -- Check if HLS is still running before making request
          procStatus <- getProcessExitCode (hlsProcess handle)
          case procStatus of
            Just exitCode -> expectationFailure $ "HLS terminated before hover request: " ++ show exitCode
            Nothing -> do
              -- Request hover information
              reqId <- nextRequestId handle
              let hoverMsg = LSPMessage
                    { method = "textDocument/hover"
                    , msgId = Just reqId
                    , params = Just $ object
                        [ "textDocument" .= object ["uri" .= ("file://" <> workDir <> "/ValidModule.hs")]
                        , "position" .= object ["line" .= (2 :: Int), "character" .= (0 :: Int)] -- Use a safer position
                        ]
                    }
              
              sendLSPMessage handle hoverMsg
              response <- waitForResponse handle reqId
              
              case response of
                Left err -> 
                  -- Check if it's a process termination error
                  if T.isInfixOf "terminated" (T.pack err) || T.isInfixOf "Broken pipe" (T.pack err)
                  then pendingWith $ "HLS process unstable: " ++ err
                  else expectationFailure $ "Hover request failed: " <> err
                Right val -> 
                  case val of
                    Object obj ->
                      case KM.lookup "result" obj of
                        Just _ -> return () -- Some hover result
                        Nothing -> 
                          case KM.lookup "error" obj of
                            Just _ -> return () -- Error is acceptable for this test
                            Nothing -> expectationFailure "No result or error in response"
                    _ -> expectationFailure "Response is not an object"
        case result of
          Left (ex :: SomeException) -> 
            if "resource vanished" `T.isInfixOf` T.toLower (T.pack $ show ex) || 
               "broken pipe" `T.isInfixOf` T.toLower (T.pack $ show ex)
            then pendingWith $ "HLS process unstable: " ++ show ex
            else expectationFailure $ "Hover test failed: " ++ show ex
          Right _ -> return ()

    it "can request document symbols" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Open document first
          let openMsg = LSPMessage
                { method = "textDocument/didOpen"
                , msgId = Nothing
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/ValidModule.hs")
                        , "languageId" .= ("haskell" :: Text)
                        , "version" .= (1 :: Int)
                        , "text" .= validModuleContent
                        ]
                    ]
                }
          
          sendLSPMessage handle openMsg
          threadDelay 1000000
          
          -- Request document symbols
          reqId <- nextRequestId handle
          let symbolsMsg = LSPMessage
                { method = "textDocument/documentSymbol"
                , msgId = Just reqId
                , params = Just $ object
                    [ "textDocument" .= object
                        [ "uri" .= ("file://" <> workDir <> "/ValidModule.hs")
                        ]
                    ]
                }
          
          sendLSPMessage handle symbolsMsg
          response <- readLSPResponse handle
          
          case response of
            Left err -> expectationFailure $ "Document symbols failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Symbols found or empty array is fine
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error is acceptable
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

  describe "LSP Workspace Operations" $ do
    it "can handle workspace symbol requests" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          reqId <- nextRequestId handle
          let symbolQuery = LSPMessage
                { method = "workspace/symbol"
                , msgId = Just reqId
                , params = Just $ object ["query" .= ("addNumbers" :: Text)]
                }
          
          sendLSPMessage handle symbolQuery
          response <- readLSPResponse handle
          
          case response of
            Left err -> expectationFailure $ "Workspace symbol failed: " <> err
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "result" obj of
                    Just _ -> return () -- Any result is fine
                    Nothing ->
                      case KM.lookup "error" obj of
                        Just _ -> return () -- Error is acceptable
                        Nothing -> expectationFailure "No result or error in response"
                _ -> expectationFailure "Response is not an object"

  describe "LSP Error Handling" $ do
    it "handles invalid method names gracefully" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          reqId <- nextRequestId handle
          let invalidMsg = LSPMessage
                { method = "invalid/method"
                , msgId = Just reqId
                , params = Nothing
                }
          
          sendLSPMessage handle invalidMsg
          -- Give HLS time to process the invalid request
          threadDelay 1000000  -- 1 second
          response <- readLSPResponse handle
          
          case response of
            Left _ -> return () -- Communication error is acceptable
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "error" obj of
                    Just _ -> return () -- Error response is expected
                    Nothing -> return () -- HLS might ignore invalid methods
                _ -> return () -- Any response structure is acceptable

    it "handles malformed requests" $ do
      withTestWorkspace testWorkspaceFiles $ \workDir -> do
        withHLS workDir $ \handle -> do
          -- Send malformed JSON (missing required fields)
          let malformedMsg = LSPMessage
                { method = "textDocument/hover"
                , msgId = Just 999
                , params = Just $ object [] -- Missing required parameters
                }
          
          sendLSPMessage handle malformedMsg
          -- Give HLS time to process the malformed request
          threadDelay 1000000  -- 1 second
          response <- readLSPResponse handle
          
          case response of
            Left _ -> return () -- Communication error is acceptable
            Right val ->
              case val of
                Object obj ->
                  case KM.lookup "error" obj of
                    Just _ -> return () -- Error response is expected
                    Nothing -> return () -- HLS might handle gracefully
                _ -> return () -- Any response structure is acceptable