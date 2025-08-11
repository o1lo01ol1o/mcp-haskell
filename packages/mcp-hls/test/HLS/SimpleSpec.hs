{-# LANGUAGE OverloadedStrings #-}

module HLS.SimpleSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Simple Test" $ do
  it "works" $ do
    True `shouldBe` True

  describe "Another Group" $ do
    it "also works" $ do
      2 `shouldBe` 2