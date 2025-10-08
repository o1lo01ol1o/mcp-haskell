{-# LANGUAGE OverloadedStrings #-}

module MCP.GHCIDIntegrationSpec (spec) where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec
import Test.Utils
  ( assertAllGood
  , findMCPGHCIDExecutable
  , pollForMessage
  , pollForStatus
  , readResponse
  , sendRequest
  , validateToolResponse
  , withMCPGhcidServer
  , withTestHaskellProject
  , extractToolText
  , decodeStatusPayload
  )

spec :: Spec
spec = describe "mcp-ghcid" $ do
  it "starts ghcid, streams All good, and supports restart" $ do
    maybeExec <- findMCPGHCIDExecutable
    execPath <- case maybeExec of
      Nothing -> expectationFailure "Failed to build or locate mcp-ghcid executable" >> fail "no exec"
      Just path -> pure path

    let projectFiles =
          [ ("src/Test/Module.hs", unlines
              [ "module Test.Module where"
              , ""
              , "value :: Int"
              , "value = 42"
              ])
          ]

    withTestHaskellProject projectFiles $ \projectRoot -> do
      let cabalFile = projectRoot </> "test-project.cabal"
      exists <- doesFileExist cabalFile
      unless exists $
        expectationFailure "Generated project is missing test-project.cabal"

      let cabalUri = T.pack cabalFile
          workDirTxt = T.pack projectRoot

      withMCPGhcidServer execPath $ \(hin, hout) -> do
        sendRequest hin initializeRequest
        initResp <- readResponse hout 5000000
        case initResp of
          Left err -> expectationFailure $ "initialize failed: " <> err
          Right val -> validateInitialize val

        sendRequest hin notificationsInitialized

        sendRequest hin listRequest
        _ <- readResponse hout 5000000

        sendRequest hin $ startRequest cabalUri workDirTxt
        startResp <- readResponse hout 10000000
        case startResp of
          Left err -> expectationFailure $ "ghcid-start failed: " <> err
          Right val -> do
            validateToolResponse "ghcid-start" val
            case extractToolText val of
              Just msg -> msg `shouldSatisfy` (\t -> "started" `T.isInfixOf` T.toLower t)
              Nothing -> expectationFailure "ghcid-start response missing content"

        sendRequest hin $ statusRequest cabalUri
        initialStatusResp <- readResponse hout 10000000
        case initialStatusResp of
          Left err -> expectationFailure $ "ghcid-status failed: " <> err
          Right val -> do
            validateToolResponse "ghcid-status" val
            case extractToolText val of
              Nothing -> expectationFailure "ghcid-status response missing content"
              Just payload ->
                case decodeStatusPayload payload of
                  Left parseErr -> expectationFailure $ "Failed to decode ghcid-status payload: " <> parseErr
                  Right (stateTxt, _, _, latestMsg) -> do
                    stateTxt `shouldBe` "starting"
                    case latestMsg of
                      Nothing -> expectationFailure "Expected latest message while ghcid is starting"
                      Just latest -> latest `shouldSatisfy` (not . T.null)

        statusResult <- pollForStatus (hin, hout) cabalUri 40
        case statusResult of
          Left err -> expectationFailure err
          Right (stateTxt, _, _, _) -> stateTxt `shouldBe` "running"

        firstAllGood <- pollForMessage (hin, hout) cabalUri "all good" 20
        case firstAllGood of
          Left msg -> expectationFailure msg
          Right txt -> txt `shouldSatisfy` assertAllGood

        sendRequest hin $ restartRequest cabalUri workDirTxt
        restartResp <- readResponse hout 10000000
        case restartResp of
          Left err -> expectationFailure $ "ghcid-restart failed: " <> err
          Right val -> do
            validateToolResponse "ghcid-restart" val
            case extractToolText val of
              Just msg -> msg `shouldSatisfy` (\t -> "restart" `T.isInfixOf` T.toLower t)
              Nothing -> expectationFailure "ghcid-restart response missing content"

        restartStatus <- pollForStatus (hin, hout) cabalUri 40
        case restartStatus of
          Left err -> expectationFailure err
          Right (stateTxt, _, _, _) -> stateTxt `shouldBe` "running"

        secondAllGood <- pollForMessage (hin, hout) cabalUri "all good" 20
        case secondAllGood of
          Left msg -> expectationFailure msg
          Right txt -> txt `shouldSatisfy` assertAllGood

        sendRequest hin $ stopRequest cabalUri
        stopResp <- readResponse hout 5000000
        case stopResp of
          Left err -> expectationFailure $ "ghcid-stop failed: " <> err
          Right val -> validateToolResponse "ghcid-stop" val
  where
    initializeRequest = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (1 :: Int)
      , "method" .= ("initialize" :: Text)
      , "params" .= object
          [ "protocolVersion" .= ("2025-06-18" :: Text)
          , "capabilities" .= object []
          , "clientInfo" .= object
              [ "name" .= ("ghcid-test" :: Text)
              , "version" .= ("0.1.0" :: Text)
              ]
          ]
      ]

    notificationsInitialized = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= ("notifications/initialized" :: Text)
      ]

    listRequest = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (2 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("ghcid-list" :: Text)
          , "arguments" .= object
              [ "includeStatus" .= False
              ]
          ]
      ]

    startRequest cabalUri workDirTxt = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (3 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("ghcid-start" :: Text)
          , "arguments" .= object
              [ "cabalURI" .= cabalUri
              , "workDir" .= workDirTxt
              , "component" .= ("test-project:lib" :: Text)
              ]
          ]
      ]

    restartRequest cabalUri workDirTxt = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (4 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("ghcid-restart" :: Text)
          , "arguments" .= object
              [ "cabalURI" .= cabalUri
              , "workDir" .= workDirTxt
              , "component" .= ("test-project:lib" :: Text)
              ]
          ]
      ]

    statusRequest cabalUri = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (6 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("ghcid-status" :: Text)
          , "arguments" .= object
              [ "cabalURI" .= cabalUri
              ]
          ]
      ]

    stopRequest cabalUri = object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= (5 :: Int)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object
          [ "name" .= ("ghcid-stop" :: Text)
          , "arguments" .= object
              [ "cabalURI" .= cabalUri
              , "force" .= False
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
          case KM.lookup "instructions" obj of
            Just (String instr) -> do
              let lowerInstr = T.toLower instr
              unless ("ghcid-start" `T.isInfixOf` lowerInstr
                      && "ghcid-restart" `T.isInfixOf` lowerInstr
                      && ".cabal" `T.isInfixOf` lowerInstr
                      && "server" `T.isInfixOf` lowerInstr) $
                expectationFailure "Server instructions missing restart guidance"
            _ -> expectationFailure "Server initialize response missing instructions"
        _ -> expectationFailure $ "Malformed initialize response: " <> show value
