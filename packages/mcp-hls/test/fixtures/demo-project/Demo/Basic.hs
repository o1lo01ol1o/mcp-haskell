{-# LANGUAGE OverloadedStrings #-}

-- | Basic module for testing hover_info, goto_definition, and find_references
-- This module contains simple, well-documented functions that HLS should handle easily
module Demo.Basic
  ( greetUser
  , calculateSum
  , processStringList
  , UserInfo(..)
  , mathConstants
  , MathOperation(..)
  , performOperation
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | User information data type for testing hover and definition
data UserInfo = UserInfo
  { userName :: Text
  , userAge :: Int
  , userEmail :: Text
  } deriving (Show, Eq)

-- | Math operation enumeration for testing
data MathOperation = Add | Subtract | Multiply | Divide
  deriving (Show, Eq)

-- | Simple greeting function - perfect for hover info testing
-- HLS should show: greetUser :: UserInfo -> Text
greetUser :: UserInfo -> Text
greetUser user = "Hello, " <> userName user <> "!"

-- | Basic arithmetic function for testing goto definition
-- HLS should be able to find references to this function
calculateSum :: [Int] -> Int
calculateSum = sum  -- This references Prelude.sum

-- | String processing function with multiple type constraints
-- Tests HLS's ability to show complex type information
processStringList :: [Text] -> Text
processStringList texts = T.intercalate ", " (filter (not . T.null) texts)

-- | Math constants for testing workspace symbols
mathConstants :: [(Text, Double)]
mathConstants =
  [ ("pi", pi)
  , ("e", exp 1.0)
  , ("golden_ratio", (1 + sqrt 5) / 2)
  ]

-- | Pattern matching function for testing completions and hover
performOperation :: MathOperation -> Double -> Double -> Double
performOperation Add x y = x + y
performOperation Subtract x y = x - y  
performOperation Multiply x y = x * y
performOperation Divide x y = if y == 0 then error "Division by zero" else x / y

-- | Helper function that references other functions in this module
-- Perfect for testing find_references across the same file
demonstrateBasicOperations :: UserInfo -> Text
demonstrateBasicOperations user = 
  let greeting = greetUser user  -- References greetUser
      numbers = [1, 2, 3, 4, 5]
      total = calculateSum numbers  -- References calculateSum
      result = performOperation Add (fromIntegral total) 10.5  -- References performOperation
  in greeting <> " Your calculation result is: " <> T.pack (show result)