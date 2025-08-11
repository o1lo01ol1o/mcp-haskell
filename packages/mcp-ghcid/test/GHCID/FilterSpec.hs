{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHCID.FilterSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=), fromJSON, toJSON, Result(..), Value)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)

import GHCID.Filter
import Test.Utils (sampleGHCIDOutput, testFilterFunction)

spec :: Spec
spec = describe "GHCID.Filter" $ do
  
  describe "FilterRequest JSON instances" $ do
    it "parses grep filter correctly" $ do
      let grepJSON = object ["grep" .= ("warning" :: Text)]
      case fromJSON grepJSON of
        Success (GrepRequest pattern) -> pattern `shouldBe` "warning"
        _ -> expectationFailure "Failed to parse grep filter"
    
    it "parses head filter correctly" $ do
      let headJSON = object ["head" .= (5 :: Int)]
      case fromJSON headJSON of
        Success (HeadRequest n) -> n `shouldBe` 5
        _ -> expectationFailure "Failed to parse head filter"
    
    it "parses tail filter correctly" $ do
      let tailJSON = object ["tail" .= (3 :: Int)]
      case fromJSON tailJSON of
        Success (TailRequest n) -> n `shouldBe` 3
        _ -> expectationFailure "Failed to parse tail filter"
    
    it "parses line range filter correctly" $ do
      let linesJSON = object ["lines" .= ("2-5" :: Text)]
      case fromJSON linesJSON of
        Success (LinesRequest start end) -> do
          start `shouldBe` 2
          end `shouldBe` 5
        _ -> expectationFailure "Failed to parse line range filter"
    
    it "parses no filter correctly" $ do
      let noFilterJSON = object []
      case fromJSON noFilterJSON of
        Success NoFilterRequest -> return ()
        _ -> expectationFailure "Failed to parse no filter"
    
    it "rejects multiple filter options" $ do
      let invalidJSON = object ["grep" .= ("test" :: Text), "head" .= (5 :: Int)]
      case fromJSON invalidJSON of
        Success (_ :: FilterRequest) -> expectationFailure "Should reject multiple filter options"
        Error _ -> return ()
    
    it "validates grep patterns" $ do
      let invalidGrepJSON = object ["grep" .= ("" :: Text)]
      case fromJSON invalidGrepJSON of
        Success (_ :: FilterRequest) -> expectationFailure "Should reject empty grep pattern"
        Error _ -> return ()
    
    it "validates line counts" $ do
      let invalidHeadJSON = object ["head" .= (0 :: Int)]
      case fromJSON invalidHeadJSON of
        Success (_ :: FilterRequest) -> expectationFailure "Should reject zero line count"
        Error _ -> return ()
    
    it "roundtrip JSON serialization works" $ do
      let original = GrepRequest "error"
      case fromJSON (toJSON original) of
        Success parsed -> parsed `shouldBe` original
        Error err -> expectationFailure $ "Roundtrip failed: " ++ err

  describe "Filter application" $ do
    it "applies grep filter correctly" $ do
      result <- testFilterFunction sampleGHCIDOutput (GrepRequest "warning")
      case result of
        Left err -> expectationFailure $ "Filter failed: " ++ T.unpack err
        Right filtered -> do
          T.lines filtered `shouldSatisfy` all (T.isInfixOf "warning")
          T.lines filtered `shouldSatisfy` (not . null)
    
    it "applies head filter correctly" $ do
      result <- testFilterFunction sampleGHCIDOutput (HeadRequest 3)
      case result of
        Left err -> expectationFailure $ "Filter failed: " ++ T.unpack err
        Right filtered -> do
          length (T.lines filtered) `shouldBe` 3
    
    it "applies tail filter correctly" $ do
      result <- testFilterFunction sampleGHCIDOutput (TailRequest 2)
      case result of
        Left err -> expectationFailure $ "Filter failed: " ++ T.unpack err  
        Right filtered -> do
          length (T.lines filtered) `shouldBe` 2
    
    it "handles no filter correctly" $ do
      result <- testFilterFunction sampleGHCIDOutput NoFilterRequest
      case result of
        Left err -> expectationFailure $ "Filter failed: " ++ T.unpack err
        Right filtered -> filtered `shouldBe` sampleGHCIDOutput
    
    it "handles empty input" $ do
      result <- testFilterFunction "" (GrepRequest "test")
      case result of
        Left err -> expectationFailure $ "Filter failed: " ++ T.unpack err
        Right filtered -> filtered `shouldBe` ""

  describe "Filter validation" $ do
    it "validates grep patterns properly" $ do
      case validateGrepPattern "valid_pattern" of
        Right _ -> return ()
        Left err -> expectationFailure $ "Valid pattern rejected: " ++ T.unpack err
      
      case validateGrepPattern "" of
        Right _ -> expectationFailure "Empty pattern should be rejected"
        Left _ -> return ()
    
    it "validates line counts properly" $ do
      case validateLineCount 10 of
        Right _ -> return ()
        Left err -> expectationFailure $ "Valid count rejected: " ++ T.unpack err
      
      case validateLineCount 0 of
        Right _ -> expectationFailure "Zero count should be rejected"
        Left _ -> return ()
      
      case validateLineCount (-1) of
        Right _ -> expectationFailure "Negative count should be rejected"
        Left _ -> return ()
    
    it "validates line ranges properly" $ do
      case validateLineRange (1, 5) of
        Right _ -> return ()
        Left err -> expectationFailure $ "Valid range rejected: " ++ T.unpack err
      
      case validateLineRange (5, 1) of
        Right _ -> expectationFailure "Invalid range should be rejected"
        Left _ -> return ()

  describe "Edge cases" $ do
    it "handles large input gracefully" $ do
      let largeInput = T.replicate 1000 "line\n"
      result <- testFilterFunction largeInput (HeadRequest 10)
      case result of
        Left err -> expectationFailure $ "Filter failed on large input: " ++ T.unpack err
        Right filtered -> length (T.lines filtered) `shouldBe` 10
    
    it "handles unicode text properly" $ do
      let unicodeInput = "こんにちは\nwärld\nemoji 🚀"
      result <- testFilterFunction unicodeInput (GrepRequest "emoji")
      case result of
        Left err -> expectationFailure $ "Filter failed on unicode: " ++ T.unpack err
        Right filtered -> filtered `shouldSatisfy` T.isInfixOf "🚀"