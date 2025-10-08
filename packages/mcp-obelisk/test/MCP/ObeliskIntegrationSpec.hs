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
        Left err -> expectationFailure $ "obelisk-start failed: " <> err
        Right val -> do
          validateToolResponse "obelisk-start" val
          case extractToolText val of
            Just msg -> msg `shouldSatisfy` (\t -> "started" `T.isInfixOf` T.toLower t)
            Nothing -> expectationFailure "obelisk-start response missing content"

      statusResult <- pollForStatus (hin, hout) projectPath 40
      case statusResult of
        Left err -> expectationFailure err
        Right (stateTxt, _, _) -> stateTxt `shouldBe` "running"

      firstAllGood <- pollForMessage (hin, hout) projectPath "all good" 15
      case firstAllGood of
        Left msg -> expectationFailure msg
        Right txt -> txt `shouldSatisfy` assertAllGood

      sendRequest hin $ startRequest projectPath
      restartResp <- readResponse hout 5000000
      case restartResp of
        Left err -> expectationFailure $ "obelisk-start restart failed: " <> err
        Right val -> do
          validateToolResponse "obelisk-start" val
          case extractToolText val of
            Just msg -> msg `shouldSatisfy` (\t -> "restarted" `T.isInfixOf` T.toLower t)
            Nothing -> expectationFailure "obelisk-start restart response missing content"

      restartStatus <- pollForStatus (hin, hout) projectPath 40
      case restartStatus of
        Left err -> expectationFailure err
        Right (stateTxt, _, _) -> stateTxt `shouldBe` "running"

      secondAllGood <- pollForMessage (hin, hout) projectPath "all good" 15
      case secondAllGood of
        Left msg -> expectationFailure msg
        Right txt -> txt `shouldSatisfy` assertAllGood

      let grepFilter = object ["grep" .= ("All good" :: Text)]
      sendRequest hin $ messagesRequest projectPath 5 (Just grepFilter)
      filteredResp <- readResponse hout 5000000
      case filteredResp of
        Left err -> expectationFailure $ "obelisk-messages tail filter failed: " <> err
        Right val -> do
          validateToolResponse "obelisk-messages" val
          case extractToolText val of
            Nothing -> expectationFailure "obelisk-messages response missing content"
            Just payload ->
              case decodeMessagePayload payload of
                Left parseErr -> expectationFailure $ "Failed to decode obelisk-messages payload: " <> parseErr
                Right (outputTxt, linesList) -> do
                  length linesList `shouldSatisfy` (> 0)
                  linesList `shouldSatisfy` (any assertAllGood)

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
          [ "name" .= ("obelisk-list" :: Text)
          ]
      ]

    startRequest projectPath = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (3 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("obelisk-start" :: Text)
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
          [ "name" .= ("obelisk-stop" :: Text)
          , "arguments" .= object
              [ "projectPath" .= T.pack projectPath
              ]
          ]
      ]

    messagesRequest projectPath reqId mFilter = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (reqId :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("obelisk-messages" :: Text)
          , "arguments" .= object (
              [ "projectPath" .= T.pack projectPath
              , "limit" .= (80 :: Int)
              ] ++ maybe [] (\f -> ["filter" .= f]) mFilter
            )
          ]
      ]

    validateInitialize value = do
      let result = parseMaybe (withObject "resp" $ \o -> o .:? "result") value
      case result of
        Just (Just (Object obj)) -> do
          let proto = KM.lookup "protocolVersion" obj
          unless (proto == Just (String "2025-06-18")) $
            expectationFailure "Server responded with unexpected protocol version"
          case KM.lookup "instructions" obj of
            Just (String instr) -> do
              let lowerInstr = T.toLower instr
              unless ("obelisk-start" `T.isInfixOf` lowerInstr
                      && ".cabal" `T.isInfixOf` lowerInstr
                      && "nix" `T.isInfixOf` lowerInstr
                      && "restart" `T.isInfixOf` lowerInstr) $
                expectationFailure "Server instructions missing restart guidance"
            _ -> expectationFailure "Server initialize response missing instructions"
        _ -> expectationFailure $ "Malformed initialize response: " <> show value
