{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, decode)
import MCP.SDK.Types
import MCP.SDK.Protocol
import MCP.SDK.Error
import MCP.SDK.Capabilities

main :: IO ()
main = hspec $ do
  describe "MCP SDK Types" $ do
    it "can serialize/deserialize ClientInfo" $ do
      let clientInfo = ClientInfo "test-client" "1.0.0"
      let encoded = encode clientInfo
      decode encoded `shouldBe` Just clientInfo

    it "can serialize/deserialize ServerInfo" $ do
      let serverInfo = ServerInfo "test-server" "1.0.0"  
      let encoded = encode serverInfo
      decode encoded `shouldBe` Just serverInfo

    it "can serialize/deserialize Capabilities" $ do
      let caps = defaultClientCapabilities
      let encoded = encode caps
      decode encoded `shouldBe` Just caps

  describe "MCP Protocol" $ do
    it "can convert method to text and back" $ do
      let methods = [Initialize, ToolsList, ToolsCall, ResourcesList, ResourcesRead, PromptsGet, PromptsList, Ping]
      mapM_ (\m -> textToMethod (methodToText m) `shouldBe` Right m) methods

  describe "MCP Capabilities" $ do
    it "can build client capabilities" $ do
      let caps = buildClientCapabilities ClientCapabilityBuilder
            { ccbExperimental = Nothing
            , ccbSampling = Nothing
            , ccbRoots = Nothing
            , ccbElicitation = Nothing
            }
      caps `shouldBe` defaultClientCapabilities

    it "can detect capability support" $ do
      let caps = fullServerCapabilities
      hasToolsSupport caps `shouldBe` True
      hasResourcesSupport caps `shouldBe` True
      hasPromptsSupport caps `shouldBe` True

  describe "MCP Errors" $ do
    it "converts errors to appropriate codes" $ do
      mcpErrorToCode (ParseError "test") `shouldBe` ParseErrorCode
      mcpErrorToCode (ValidationError "test") `shouldBe` InvalidParamsCode
      mcpErrorToCode (UnsupportedMethod "test") `shouldBe` MethodNotFoundCode
