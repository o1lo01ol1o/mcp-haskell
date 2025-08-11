{-# LANGUAGE OverloadedStrings #-}

module GHCID.OutputSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import GHCID.Output
import GHCID.Client (MessageSeverity(..), CompilerMessage(..))

spec :: Spec
spec = describe "GHCID.Output" $ do
  
  describe "formatCompilerMessage" $ do
    it "formats error messages correctly" $ do
      now <- getCurrentTime
      let errorMsg = CompilerMessage
            { msgSeverity = Error
            , msgFile = Just "src/Main.hs"
            , msgLine = Just 10
            , msgColumn = Just 5
            , msgText = "Variable not in scope: foo"
            , msgTimestamp = now
            }
      let formatted = formatCompilerMessage errorMsg
      formatted `shouldSatisfy` T.isInfixOf "Error"
      formatted `shouldSatisfy` T.isInfixOf "src/Main.hs"
      formatted `shouldSatisfy` T.isInfixOf "10"
      formatted `shouldSatisfy` T.isInfixOf "5"
      formatted `shouldSatisfy` T.isInfixOf "Variable not in scope: foo"
    
    it "formats warning messages correctly" $ do
      now <- getCurrentTime
      let warningMsg = CompilerMessage
            { msgSeverity = Warning
            , msgFile = Just "src/Lib.hs"
            , msgLine = Just 25
            , msgColumn = Just 12
            , msgText = "Unused import: Data.List"
            , msgTimestamp = now
            }
      let formatted = formatCompilerMessage warningMsg
      formatted `shouldSatisfy` T.isInfixOf "Warning"
      formatted `shouldSatisfy` T.isInfixOf "src/Lib.hs"
      formatted `shouldSatisfy` T.isInfixOf "Unused import"
    
    it "handles messages without location info" $ do
      now <- getCurrentTime
      let msg = CompilerMessage
            { msgSeverity = Info
            , msgFile = Nothing
            , msgLine = Nothing
            , msgColumn = Nothing
            , msgText = "Compilation successful"
            , msgTimestamp = now
            }
      let formatted = formatCompilerMessage msg
      formatted `shouldSatisfy` T.isInfixOf "Info"
      formatted `shouldSatisfy` T.isInfixOf "Compilation successful"
      -- Should not contain location info
      formatted `shouldNotSatisfy` T.isInfixOf ":"
    
    it "handles empty message text" $ do
      now <- getCurrentTime
      let msg = CompilerMessage
            { msgSeverity = Hint
            , msgFile = Just "Test.hs"
            , msgLine = Just 1
            , msgColumn = Just 1
            , msgText = ""
            , msgTimestamp = now
            }
      let formatted = formatCompilerMessage msg
      formatted `shouldSatisfy` T.isInfixOf "Hint"
      formatted `shouldSatisfy` T.isInfixOf "Test.hs"
    
    it "formats all severity levels" $ do
      now <- getCurrentTime
      let severities = [Error, Warning, Info, Hint]
      formattedMessages <- mapM (\sev -> do
        let msg = CompilerMessage sev (Just "Test.hs") (Just 1) (Just 1) "Test message" now
        return $ formatCompilerMessage msg) severities
      
      -- Each should contain the severity name
      formattedMessages !! 0 `shouldSatisfy` T.isInfixOf "Error"
      formattedMessages !! 1 `shouldSatisfy` T.isInfixOf "Warning" 
      formattedMessages !! 2 `shouldSatisfy` T.isInfixOf "Info"
      formattedMessages !! 3 `shouldSatisfy` T.isInfixOf "Hint"

  describe "message severity ordering" $ do
    it "orders severity levels correctly" $ do
      Error `shouldSatisfy` (> Warning)
      Warning `shouldSatisfy` (> Info)
      Info `shouldSatisfy` (> Hint)
    
    it "groups messages by severity" $ do
      now <- getCurrentTime
      let messages = 
            [ CompilerMessage Error (Just "A.hs") (Just 1) (Just 1) "Error 1" now
            , CompilerMessage Warning (Just "B.hs") (Just 2) (Just 2) "Warning 1" now
            , CompilerMessage Error (Just "C.hs") (Just 3) (Just 3) "Error 2" now
            , CompilerMessage Info (Just "D.hs") (Just 4) (Just 4) "Info 1" now
            ]
      
      -- When sorted by severity (descending), errors should come first
      let sortedMessages = [m | m <- messages, msgSeverity m == Error] ++ 
                          [m | m <- messages, msgSeverity m == Warning] ++
                          [m | m <- messages, msgSeverity m == Info] ++
                          [m | m <- messages, msgSeverity m == Hint]
      
      length (filter (\m -> msgSeverity m == Error) sortedMessages) `shouldBe` 2
      length (filter (\m -> msgSeverity m == Warning) sortedMessages) `shouldBe` 1
      length (filter (\m -> msgSeverity m == Info) sortedMessages) `shouldBe` 1

  describe "message parsing utilities" $ do
    it "extracts file information correctly" $ do
      now <- getCurrentTime
      let msg = CompilerMessage Error (Just "src/deep/nested/Module.hs") (Just 42) (Just 15) "Test" now
      msgFile msg `shouldBe` Just "src/deep/nested/Module.hs"
      msgLine msg `shouldBe` Just 42
      msgColumn msg `shouldBe` Just 15
    
    it "handles relative and absolute paths" $ do
      now <- getCurrentTime
      let relativeMsg = CompilerMessage Error (Just "src/Main.hs") (Just 1) (Just 1) "Test" now
          absoluteMsg = CompilerMessage Error (Just "/home/user/project/src/Main.hs") (Just 1) (Just 1) "Test" now
      
      msgFile relativeMsg `shouldBe` Just "src/Main.hs"
      msgFile absoluteMsg `shouldBe` Just "/home/user/project/src/Main.hs"