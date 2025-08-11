{-# LANGUAGE OverloadedStrings #-}

-- | Module for testing eval_expression functionality 
-- Contains expressions that should be evaluable by HLS
module Demo.Evaluation where

import Data.Text (Text)
import qualified Data.Text as T

-- | Simple arithmetic expressions that should evaluate
simpleArithmetic :: Int
simpleArithmetic = 2 + 3 * 4  -- Should evaluate to 14

-- | Boolean expressions  
booleanLogic :: Bool
booleanLogic = True && (False || True)  -- Should evaluate to True

-- | String operations
stringOperations :: String
stringOperations = "Hello, " ++ "World!"  -- Should evaluate to "Hello, World!"

-- | Text operations
textOperations :: Text
textOperations = T.append "Haskell " "is great!"  -- Should evaluate

-- | List operations
listOperations :: [Int]
listOperations = [1, 2, 3] ++ [4, 5, 6]  -- Should evaluate to [1,2,3,4,5,6]

-- | List comprehensions
listComprehension :: [Int]
listComprehension = [x * 2 | x <- [1..5]]  -- Should evaluate to [2,4,6,8,10]

-- | Function applications
functionApplication :: Int
functionApplication = length [1, 2, 3, 4, 5]  -- Should evaluate to 5

-- | Higher-order functions
higherOrderExample :: [Int]
higherOrderExample = map (+1) [1, 2, 3]  -- Should evaluate to [2,3,4]

-- | Filter expressions
filterExample :: [Int]
filterExample = filter even [1..10]  -- Should evaluate to [2,4,6,8,10]

-- | Fold expressions
foldExample :: Int
foldExample = foldr (+) 0 [1, 2, 3, 4]  -- Should evaluate to 10

-- | Pattern matching evaluation
patternMatchEval :: Int
patternMatchEval = 
  let f [] = 0
      f (x:_) = x
  in f [42, 99, 123]  -- Should evaluate to 42

-- | Case expression evaluation
caseEvaluation :: String
caseEvaluation = 
  case Just 42 of
    Nothing -> "empty"
    Just x -> "value: " ++ show x  -- Should evaluate to "value: 42"

-- | Let expression evaluation
letEvaluation :: Int
letEvaluation = 
  let x = 10
      y = 20
  in x + y  -- Should evaluate to 30

-- | Where clause evaluation
whereEvaluation :: Int
whereEvaluation = x + y
  where
    x = 15
    y = 25  -- Should evaluate to 40

-- | Lambda evaluation
lambdaEvaluation :: Int
lambdaEvaluation = (\x -> x * 2) 21  -- Should evaluate to 42

-- | Complex expression evaluation
complexExpression :: [Int]
complexExpression = 
  take 3 $ 
  map (* 2) $ 
  filter (> 0) $ 
  [-2, -1, 0, 1, 2, 3, 4]  -- Should evaluate to [2,4,6]

-- | Recursive function evaluation
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialExample :: Int
factorialExample = factorial 5  -- Should evaluate to 120

-- | Fibonacci evaluation
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacciExample :: Int
fibonacciExample = fibonacci 7  -- Should evaluate to 13

-- | List range evaluation
rangeEvaluation :: [Int]
rangeEvaluation = [1..5]  -- Should evaluate to [1,2,3,4,5]

-- | Infinite list (taking finite portion)
infiniteListExample :: [Int]
infiniteListExample = take 5 [1..]  -- Should evaluate to [1,2,3,4,5]

-- | Tuple evaluation
tupleEvaluation :: (Int, String, Bool)
tupleEvaluation = (42, "answer", True)  -- Should evaluate as is

-- | Record evaluation
data Point = Point { x :: Double, y :: Double } deriving (Show)

recordEvaluation :: Point
recordEvaluation = Point { x = 3.0, y = 4.0 }  -- Should evaluate

-- | Record field access
fieldAccess :: Double
fieldAccess = x recordEvaluation  -- Should evaluate to 3.0

-- | Maybe evaluation
maybeEvaluation :: Maybe Int
maybeEvaluation = Just (2 + 3)  -- Should evaluate to Just 5

-- | Either evaluation
eitherEvaluation :: Either String Int
eitherEvaluation = Right (10 * 5)  -- Should evaluate to Right 50

-- | Type class method evaluation
showEvaluation :: String
showEvaluation = show (42 :: Int)  -- Should evaluate to "42"

-- | Numeric conversions
numericConversion :: Double
numericConversion = fromIntegral (10 :: Int) + 3.5  -- Should evaluate to 13.5

-- | String to number parsing (might not always evaluate)
-- parseExample = read "123" :: Int  -- Should evaluate to 123 if safe

-- | Conditional evaluation
conditionalEval :: String
conditionalEval = if 3 > 2 then "yes" else "no"  -- Should evaluate to "yes"

-- | Guards evaluation
guardedFunction :: Int -> String
guardedFunction x 
  | x < 0 = "negative"
  | x == 0 = "zero"
  | otherwise = "positive"

guardedExample :: String
guardedExample = guardedFunction 5  -- Should evaluate to "positive"

-- | Enum evaluation
data Color = Red | Green | Blue deriving (Show, Enum)

enumEvaluation :: Color
enumEvaluation = toEnum 1  -- Should evaluate to Green

-- | List of expressions for systematic evaluation testing
evaluationTests :: [(String, String)]
evaluationTests = 
  [ ("2 + 3", show (2 + 3))
  , ("length [1,2,3]", show (length [1,2,3]))
  , ("map (*2) [1,2,3]", show (map (*2) [1,2,3]))
  , ("filter even [1..6]", show (filter even [1..6]))
  , ("take 3 [1..]", show (take 3 [1..]))
  , ("factorial 4", show (factorial 4))
  , ("reverse \"hello\"", show (reverse "hello"))
  ]

-- | Function demonstrating evaluation in context
evaluateInContext :: Int
evaluateInContext = 
  let numbers = [1, 2, 3, 4, 5]  -- This should be evaluable
      doubled = map (*2) numbers  -- This should show [2,4,6,8,10]
      summed = sum doubled  -- This should evaluate to 30
  in summed + 10  -- Final result should be 40