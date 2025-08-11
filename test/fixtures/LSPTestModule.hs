{-# LANGUAGE OverloadedStrings #-}

-- | Test module for LSP operations
module LSPTestModule where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)

-- | Test function for hover and goto definition
testFunction :: Int -> Int -> Int
testFunction x y = x + y

-- | Function without type signature (for type signature addition testing)
missingTypeSignature a b c = a + b + c

-- | Test data type for symbol navigation
data TestData = TestConstructor 
  { fieldOne :: Int
  , fieldTwo :: Text  
  , fieldThree :: [String]
  }

-- | Test class for completion testing
class TestClass a where
  testMethod :: a -> Text
  anotherMethod :: a -> Int

-- | Instance for testing
instance TestClass Int where
  testMethod x = T.pack $ show x
  anotherMethod = id

-- | Function using imported functions (for completion testing)
processData :: [Int] -> [Int] 
processData xs = sort xs

-- | Function with error for diagnostics testing
problematicFunction :: Int -> String
problematicFunction x = show $ x + "invalid"

-- | Another function referencing testFunction
callerFunction :: Int
callerFunction = testFunction 10 20

-- | Pattern matching function for code action testing
patternFunction :: Maybe Int -> Int
patternFunction Nothing = 0
patternFunction (Just x) = x

-- | Record update syntax
updateRecord :: TestData -> TestData
updateRecord record = record { fieldOne = 42 }