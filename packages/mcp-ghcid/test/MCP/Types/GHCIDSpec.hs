{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module MCP.Types.GHCIDSpec (spec) where

import Test.Hspec
import Data.Aeson
import Data.Text (Text)

-- Internal imports
import MCP.Types.GHCID
import GHCID.ProcessRegistry (CabalURI(..))

spec :: Spec
spec = describe "MCP.Types.GHCID" $ do
  
  describe "Basic type construction" $ do
    it "can create CabalURI" $ do
      let uri = CabalURI "test://example"
      show uri `shouldContain` "test://example"
    
    it "can create RequestId" $ do  
      let reqId = RequestId "req-123"
      show reqId `shouldContain` "req-123"

  describe "Request data types" $ do
    it "can create StartGHCIDData" $ do
      let uri = CabalURI "test://start"
      let startData = StartGHCIDData uri "." Nothing
      show startData `shouldContain` "test://start"
    
    it "can create StopGHCIDData" $ do
      let uri = CabalURI "test://stop"
      let stopData = StopGHCIDData uri False
      show stopData `shouldContain` "test://stop"