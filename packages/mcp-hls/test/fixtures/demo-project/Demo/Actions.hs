{-# LANGUAGE OverloadedStrings #-}

-- | Module designed to test code actions: add_type_signature, organize_imports, get_code_actions
module Demo.Actions where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust, catMaybes)  -- Multiple imports to test organization
import Control.Monad (when, unless, guard)
import Data.List (intercalate, nub, reverse, sort)  -- Will test import organization

-- Functions without type signatures - should trigger add_type_signature actions

-- | Function without type signature #1
simpleAddition x y = x + y

-- | Function without type signature #2
textProcessor text = T.toUpper (T.strip text)

-- | Function without type signature #3
listProcessor items = nub (sort (reverse items))

-- | Function without type signature #4 - complex
complexProcessor maybeList = 
  fromMaybe [] maybeList
    |> filter (> 0) 
    |> map (*2)
    |> take 10
  where
    (|>) = flip ($)

-- | Function without type signature #5 - higher order
higherOrderProcessor f g items = map f (filter g items)

-- | Function without type signature #6 - with guards
guardedFunction x 
  | x < 0 = "negative"
  | x == 0 = "zero"  
  | otherwise = "positive"

-- | Function without type signature #7 - pattern matching
patternMatchFunction [] = 0
patternMatchFunction (x:xs) = x + patternMatchFunction xs

-- | Function without type signature #8 - do notation
doNotationFunction items = do
  item <- items
  guard (isJust item)
  return (fromMaybe 0 item)

-- | Function without type signature #9 - let bindings
letBindingFunction x y =
  let squared = x * x
      doubled = y * 2
      combined = squared + doubled
  in combined / 2

-- | Function without type signature #10 - where clause
whereClauseFunction x y = result + helper x - helper y
  where
    result = 100
    helper z = z * z + z

-- | Function without type signature #11 - case expression  
caseExpressionFunction maybeValue = 
  case maybeValue of
    Nothing -> "empty"
    Just val -> "value: " <> show val

-- | Function without type signature #12 - lambda
lambdaFunction items = 
  map (\x -> x * 2 + 1) (filter (\y -> y > 0) items)

-- | Function without type signature #13 - record syntax
data Config = Config 
  { configName :: Text
  , configValue :: Int
  , configEnabled :: Bool
  } deriving (Show)

configProcessor config = Config
  { configName = T.toUpper (configName config)
  , configValue = configValue config * 2
  , configEnabled = not (configEnabled config)
  }

-- | Function without type signature #14 - multi-parameter
multiParamFunction a b c d = 
  if a then b + c 
  else c + d

-- | Function without type signature #15 - nested functions
nestedFunction x = outerHelper x
  where
    outerHelper y = innerHelper y + 1
      where
        innerHelper z = z * z

-- Functions that could benefit from refactoring actions

-- | Function that could use point-free style
pointFreeCandidate xs = map (\x -> x + 1) xs  -- Could be: map (+1)

-- | Function with redundant lambda
redundantLambda items = filter (\x -> isJust x) items  -- Could be: filter isJust

-- | Function that could use function composition
compositionCandidate x = reverse (sort (nub x))  -- Could use (.)

-- | Function with repeated expressions - extract to where clause
repeatedExpression x y = 
  let calculation = x * x + y * y
  in if calculation > 100 
     then calculation * 2
     else calculation / 2

-- | Function that could benefit from case-to-if transformation
caseToIf flag = case flag of
  True -> 1
  False -> 0

-- | Function with magic numbers that could be extracted to constants
magicNumbers radius = 3.14159 * radius * radius * 2  -- Pi should be extracted

-- | Function that could use existing standard library functions
reinventingWheel [] = []
reinventingWheel (x:xs) = if x `elem` xs then reinventingWheel xs else x : reinventingWheel xs
-- This reinvents 'nub' function

-- | Function with complex boolean expressions that could be simplified
complexBoolean a b c d = 
  (a && b) || (c && d) || ((a || c) && (b || d) && not (a && b && c && d))