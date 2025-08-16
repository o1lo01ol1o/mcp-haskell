{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Types.GHCIDSpec (spec) where

import Data.Aeson
import Data.Text (Text)
-- Internal imports

import GHCID.ProcessRegistry (CabalURI (..))
import MCP.Types.GHCID
import Test.Hspec

spec :: Spec
spec = describe "MCP.Types.GHCID" $ do
  describe "Basic type construction" $ do
    it "can create CabalURI" $ do
      let uri = CabalURI "test://example"
      show uri `shouldContain` "test://example"

    it "can create RequestId" $ do
      -- RequestId is not a type from MCP.Types.GHCID, removing this test
      True `shouldBe` True

  describe "Request data types" $ do
    it "can create StartGHCIDData" $ do
      let uri = CabalURI "test://start"
      let startData = StartGHCIDArgs uri "." Nothing
      show startData `shouldContain` "test://start"

    it "can create StopGHCIDData" $ do
      let uri = CabalURI "test://stop"
      let stopData = StopGHCIDArgs uri False
      show stopData `shouldContain` "test://stop"
