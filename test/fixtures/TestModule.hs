{-# LANGUAGE OverloadedStrings #-}

-- | A test module for HLS integration testing
module TestModule where

import Data.Text (Text)
import qualified Data.Text as T

-- | A simple function that concatenates two texts
concatenateTexts :: Text -> Text -> Text
concatenateTexts x y = x <> " " <> y

-- | A function with a type error for testing diagnostics
functionWithError :: Int -> String
functionWithError x = x  -- This should cause a type error

-- | A function for testing hover information
calculateLength :: [a] -> Int
calculateLength [] = 0
calculateLength (_:xs) = 1 + calculateLength xs

-- | A data type for testing
data Person = Person
  { name :: Text
  , age :: Int
  } deriving (Show, Eq)

-- | A function using the Person type
greetPerson :: Person -> Text
greetPerson (Person n a) = "Hello, " <> n <> "! You are " <> T.pack (show a) <> " years old."

-- | A function with incomplete pattern match (warning)
partialFunction :: Maybe Int -> Int
partialFunction (Just x) = x
-- Missing Nothing case should generate a warning