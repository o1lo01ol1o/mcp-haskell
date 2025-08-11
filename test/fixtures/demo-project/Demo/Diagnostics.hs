{-# LANGUAGE OverloadedStrings #-}

-- | Module with intentional errors, warnings, and missing signatures
-- Designed to test HLS diagnostics tools: get_diagnostics, check_syntax, etc.
module Demo.Diagnostics where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)  -- This import is unused - should generate warning

-- | Type error: Function claims to return String but returns Int
badTypeFunction :: String
badTypeFunction = 42  -- TYPE ERROR: Int vs String

-- | Missing type signature - should trigger add_type_signature code action
functionWithoutSignature x y = x + y * 2

-- | Unused variable warning
unusedVariableFunction :: Int -> Int
unusedVariableFunction x = 
  let unusedVar = 100  -- WARNING: unused variable
      anotherUnused = "hello"  -- WARNING: unused variable  
  in x + 1

-- | Pattern match warning - incomplete patterns
incompletePatternsFunction :: Maybe Int -> Int
incompletePatternsFunction (Just x) = x
-- Missing: incompletePatternsFunction Nothing = ...
-- Should generate WARNING: Pattern match(es) are non-exhaustive

-- | Name shadowing warning
shadowingFunction :: Int -> Int
shadowingFunction x = 
  let x = 42  -- WARNING: shadows parameter
  in x + 1

-- | Redundant import (Data.List.sort is imported but only used once)
sortNumbers :: [Int] -> [Int]
sortNumbers = sort

-- | Function with partial application that could be improved
inefficientFunction :: [Text] -> [Text]
inefficientFunction texts = map (\t -> T.toUpper t) texts  -- Can be: map T.toUpper

-- | Missing language extension (trying to use OverloadedLists without extension)
-- This should work fine since we have the extension enabled
numberList :: [Int]
numberList = [1, 2, 3, 4, 5]

-- | Duplicate record fields (should cause error if we had DuplicateRecordFields)
data Person = Person 
  { personName :: Text
  , personAge :: Int
  }

data Animal = Animal
  { animalName :: Text  -- Different from personName, so OK
  , animalSpecies :: Text
  }

-- | Function using undefined - should generate warning
dangerousFunction :: Int -> Int
dangerousFunction x = if x > 0 then x else undefined  -- WARNING: undefined

-- | Overlapping patterns (unreachable code)
overlappingPatterns :: Int -> String
overlappingPatterns 1 = "one"
overlappingPatterns 2 = "two" 
overlappingPatterns _ = "other"
overlappingPatterns 3 = "three"  -- WARNING: unreachable pattern

-- | Missing INLINE pragma for simple function (could be suggested)
simpleAdd :: Int -> Int -> Int
simpleAdd x y = x + y

-- | Function that could benefit from where clause refactoring
messyFunction :: Double -> Double -> Double
messyFunction x y = 
  let result1 = x * 2 + y * 3
      result2 = x * 2 - y * 3  -- x * 2 is duplicated
      result3 = (x * 2) / 2    -- x * 2 is duplicated again
  in result1 + result2 + result3

-- | Function with redundant parentheses
redundantParens :: Int -> Int
redundantParens x = ((x + 1) * (2))  -- Parentheses around 2 are redundant

-- | Import that's only used in type signature
-- import qualified Data.Map as Map  -- Would be partially unused if added

-- | Function parameter that's never used
wasteParameters :: Int -> String -> Int -> Int  
wasteParameters x _ z = x + z  -- y parameter is unused (marked with _)

-- | Eta reduction opportunity
etaReductionExample :: [Int] -> [Int]
etaReductionExample xs = map (*2) xs  -- Can be: etaReductionExample = map (*2)