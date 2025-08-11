{-# LANGUAGE OverloadedStrings #-}

-- | Module for testing get_code_lenses and resolve_code_lens functionality
-- Contains functions that should generate helpful code lenses
module Demo.CodeLens where

import Data.Text (Text)
import qualified Data.Text as T

-- | Simple function that should show type signature code lens
-- HLS should offer to add type signature
simpleFunction x = x + 1

-- | Function with complex inferred type - should show type lens
complexInferredType items = 
  filter (/= 0) $ map (\x -> if x > 0 then x * 2 else -x) $ take 10 items

-- | Function that could show inline/expand code lens
helperFunction :: Int -> Int
helperFunction x = x * x + x + 1

-- Main function using helper - might show inline suggestions
mainCalculation :: Int -> Int
mainCalculation x = helperFunction x + helperFunction (x + 1)

-- | Function with explicit type that might show simplification lens
explicitSimpleType :: Int -> Int -> Int
explicitSimpleType x y = x + y

-- | Function that might benefit from eta reduction code lens
etaReductionCandidate :: [Int] -> [Int]
etaReductionCandidate xs = map (* 2) xs  -- Could suggest: etaReductionCandidate = map (* 2)

-- | Function that might show performance/strictness code lenses
lazyFunction :: [Int] -> Int
lazyFunction xs = sum xs  -- Might suggest strict evaluation

-- | Data type that might show derived instance code lenses
data Person = Person 
  { personName :: Text
  , personAge :: Int
  } -- HLS might suggest adding deriving (Show, Eq, Ord)

-- | Function that might show refactoring code lens
refactorCandidate :: Person -> Text
refactorCandidate person = 
  let name = personName person
      age = personAge person
  in name <> " is " <> T.pack (show age) <> " years old"
  -- Might suggest using RecordWildCards

-- | Recursive function that might show optimization code lens
recursiveSum :: [Int] -> Int
recursiveSum [] = 0
recursiveSum (x:xs) = x + recursiveSum xs
-- Might suggest using foldl for better performance

-- | Function with unused parameters - might show code lens to remove them
unusedParamFunction :: Int -> String -> Bool -> Int
unusedParamFunction x _ _ = x * 2  -- Might suggest removing unused params

-- | Function that might show import suggestion code lens
needsImports :: [Int] -> [Int]
needsImports items = sort $ nub items
  -- If sort/nub aren't imported, might show code lens to add imports

-- | Function with magic numbers - might show extract constant code lens
magicNumbersFunction :: Double -> Double
magicNumbersFunction radius = 3.14159 * radius * radius
-- Might suggest extracting pi constant

-- | Function that might show type hole code lens
-- If we had typed holes, HLS would show suggestions
functionWithLogic :: Int -> Int -> String
functionWithLogic x y = 
  if x > y 
    then "greater"
    else if x == y 
           then "equal"
           else "lesser"
-- Might show refactoring to use guards or case expression

-- | Function that could show lens for adding documentation
undocumentedFunction :: [Text] -> Text
undocumentedFunction texts = T.intercalate ", " $ filter (not . T.null) texts

-- | Function that might show benchmark/profile code lens
heavyComputation :: [Int] -> Int
heavyComputation xs = sum $ map (\x -> x * x * x) $ filter (> 0) xs
-- Might suggest adding profiling or benchmark

-- | Class instance that might show code lens for missing methods
class Displayable a where
  display :: a -> Text
  displayList :: [a] -> Text
  displayList items = T.intercalate "\n" (map display items)  -- Default implementation

-- Partial instance - might show code lens for missing methods
instance Displayable Person where
  display person = personName person  -- Missing displayList override

-- | Function with partial patterns - might show code lens for completeness
partialPatterns :: Maybe Int -> Int
partialPatterns (Just x) = x
-- Missing Nothing case - might show code lens to add it

-- | Function that might show code lens for point-free conversion
pointFreeCandidate :: (Int -> Int) -> [Int] -> [Int]
pointFreeCandidate f xs = map f xs  -- Could suggest: pointFreeCandidate f = map f

-- | Function with explicit recursion that might suggest standard library
reinventingMap :: (a -> b) -> [a] -> [b]
reinventingMap _ [] = []
reinventingMap f (x:xs) = f x : reinventingMap f xs
-- Might suggest using map instead

-- | Function with complex boolean logic - might show simplification lens
complexBoolean :: Bool -> Bool -> Bool -> Bool
complexBoolean a b c = (a && b) || (a && c) || (b && c)
-- Might suggest factoring or truth table simplification

-- | Function that might show lens for adding error handling
unsafeFunction :: [Int] -> Int
unsafeFunction xs = head xs  -- Might suggest safe alternatives like listToMaybe

-- | Function with repeated calculations - might show memoization lens
repeatedCalculation :: Int -> Int -> Int
repeatedCalculation x y = 
  let expensive = x * x + y * y
  in if expensive > 100 
     then expensive * 2
     else expensive + 10

-- | Export list might show code lens for organizing/cleaning
-- The module header might show lens for:
-- - Adding missing exports
-- - Removing unused exports  
-- - Organizing export list