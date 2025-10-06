{-# LANGUAGE OverloadedStrings #-}

module MCP.ObeliskIntegrationSpec (spec) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import Test.Hspec
import Test.Utils
import Data.Aeson.Types (parseMaybe)

spec :: Spec
spec = describe "mcp-obelisk" $ do
  it "streams ob watch output and reports All good" $ do
    maybeExec <- findMCPObeliskExecutable
    execPath <- case maybeExec of
      Nothing -> expectationFailure "Failed to build or locate mcp-obelisk executable" >> fail "no exec"
      Just path -> pure path

    cwd <- getCurrentDirectory
    let repoCandidate = cwd </> "packages" </> "mcp-obelisk" </> "obelisk-init"
    repoExists <- doesDirectoryExist repoCandidate
    let localCandidate = cwd </> "obelisk-init"
    projectPath <-
      if repoExists
        then pure repoCandidate
        else do
          localExists <- doesDirectoryExist localCandidate
          if localExists
            then pure localCandidate
            else expectationFailure "Could not locate obelisk-init project" >> fail "missing project"

    withMCPObeliskServer execPath $ \(hin, hout) -> do
      sendRequest hin initializeRequest
      initResp <- readResponse hout 5000000
      case initResp of
        Left err -> expectationFailure $ "initialize failed: " <> err
        Right val -> validateInitialize val

      sendRequest hin notificationsInitialized

      sendRequest hin listToolsRequest
      _ <- readResponse hout 5000000

      sendRequest hin $ startRequest projectPath
      startResp <- readResponse hout 5000000
      case startResp of
        Left err -> expectationFailure $ "obelisk.start failed: " <> err
        Right val -> validateToolResponse "obelisk.start" val

      statusResult <- pollForStatus (hin, hout) projectPath 40
      case statusResult of
        Left err -> expectationFailure err
        Right (stateTxt, _, _) -> do
          stateTxt `shouldBe` "running"

      finalMessage <- pollForMessage (hin, hout) projectPath "all good" 15
      case finalMessage of
        Left msg -> expectationFailure msg
        Right txt -> txt `shouldSatisfy` (\msg -> "all good" `T.isInfixOf` T.toLower msg)

      sendRequest hin $ stopRequest projectPath
      _ <- readResponse hout 5000000
      pure ()
  where
    initializeRequest = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (1 :: Int)
      , "method" .= ("initialize" :: Text)
      , "params" .= object
          [ "protocolVersion" .= ("2025-06-18" :: Text)
          , "capabilities" .= object []
          , "clientInfo" .= object
              [ "name" .= ("obelisk-test" :: Text)
              , "version" .= ("0.1.0" :: Text)
              ]
          ]
      ]

    notificationsInitialized = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= ("notifications/initialized" :: Text)
      ]

    listToolsRequest = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (2 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("obelisk.list" :: Text)
          ]
      ]

    startRequest projectPath = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (3 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("obelisk.start" :: Text)
          , "arguments" .= object
              [ "projectPath" .= T.pack projectPath
              ]
          ]
      ]

    stopRequest projectPath = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (4 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("obelisk.stop" :: Text)
          , "arguments" .= object
              [ "projectPath" .= T.pack projectPath
              ]
          ]
      ]

    validateInitialize value = do
      let result = parseMaybe (withObject "resp" $ \o -> o .:? "result") value
      case result of
        Just (Just (Object obj)) -> do
          let proto = KM.lookup "protocolVersion" obj
          unless (proto == Just (String "2025-06-18")) $
            expectationFailure "Server responded with unexpected protocol version"
        _ -> expectationFailure $ "Malformed initialize response: " <> show value

    validateToolResponse name value = do
      let isError = parseMaybe (withObject "resp" $ \o -> do
            resultVal <- o .:? "result"
            case resultVal of
              Just (Object resObj) -> do
                contentVal <- resObj .:? "content"
                errFlag <- resObj .:? "isError"
                case errFlag of
                  Just True -> fail "Tool response reported error"
                  _ -> case contentVal of
                    Just (Array arr) | not (null arr) -> pure ()
                    _ -> fail "Tool response missing content"
              _ -> fail "Missing result object"
            ) value
      case isError of
        Nothing -> expectationFailure $ "Malformed response for " <> T.unpack name <> ": " <> show value
        Just _ -> pure ()
