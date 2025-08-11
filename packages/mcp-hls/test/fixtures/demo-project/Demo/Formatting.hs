{-# LANGUAGE OverloadedStrings #-}

-- | Module for testing format_document functionality
-- Contains poorly formatted code that should be cleaned up by HLS formatting
module Demo.Formatting where

import Data.Text(Text)
import qualified Data.Text as T
import Data.List(sort,nub,intercalate)  -- Bad spacing in imports
import           Control.Monad(when,unless) -- Extra spaces
import Data.Maybe (fromMaybe,isJust, catMaybes )  -- Inconsistent spacing

-- | Badly formatted function signature
badlyFormattedFunction::Int->String->Bool->Text
badlyFormattedFunction x y z=T.pack$show x++y++show z  -- No spaces around operators

-- | Function with poor indentation
poorIndentation :: [Int] -> [Int]
poorIndentation xs = 
    let filtered = filter (>0) xs  -- Inconsistent indentation
        sorted = sort filtered
     doubled = map (*2) sorted  -- Wrong indentation level
    in  take 10 doubled  -- Extra spaces

-- | Function with inconsistent spacing around operators
inconsistentSpacing :: Int -> Int -> Int
inconsistentSpacing x y = x+y*2-3/4  -- No spaces around operators

-- | Badly formatted case expression
badCaseFormatting :: Maybe Int -> Int
badCaseFormatting x=case x of
  Nothing->0
  Just val->val*2  -- No spaces around ->

-- | Poor record formatting
data BadRecord = BadRecord{
  field1::Int,field2::String,field3::Bool}deriving(Show,Eq)  -- All on one line, no spaces

-- | Better record that should format nicely
data GoodRecord = GoodRecord
  { goodField1 :: Int
  , goodField2 :: String  
  , goodField3 :: Bool
  } deriving (Show, Eq)

-- | Function with bad list formatting
badListFormatting :: [Int]
badListFormatting=[1,2,3,4,5,6,7,8,9,10]  -- Should break into multiple lines

-- | Function with inconsistent do-block formatting
badDoBlock :: IO ()
badDoBlock=do
putStrLn"Hello"  -- Missing space
     putStrLn "World"  -- Wrong indentation
  when True$putStrLn "Done"  -- Missing spaces around $

-- | Function with poor where clause formatting
badWhereClause :: Int -> Int
badWhereClause x=result where{
result=helper x*2;helper y=y+1}  -- Semicolons instead of newlines

-- | Function with bad lambda formatting
badLambda :: [Int] -> [Int]
badLambda xs=map(\x->x*2+1)(filter(\y->y>0)xs)  -- No spaces in lambdas

-- | Function with bad operator precedence layout
badOperators :: Int -> Int -> Int -> Bool
badOperators x y z=x+y*z>10&&z-y<5||x==0  -- Should use parentheses and spaces

-- | Function with bad if-then-else formatting
badIfThenElse :: Bool -> Int -> Int -> Int
badIfThenElse cond x y=if cond then x*2else y+1  -- Poor spacing

-- | Function with bad pattern matching format
badPatternMatch :: [Int] -> Int
badPatternMatch[]=0
badPatternMatch(x:xs)=x+badPatternMatch xs  -- Should be properly spaced

-- | Badly formatted type signature with constraints
badConstraints::(Show a,Eq a,Ord a)=>[a]->[a]
badConstraints=nub.sort  -- Should have proper spacing

-- | Function with bad string formatting
badStrings :: String
badStrings="This is a very long string that should probably be broken into multiple lines because it exceeds reasonable line length limits and becomes hard to read"

-- | Function with bad numeric formatting
badNumbers :: [Double]
badNumbers=[1.0,2.5,3.14159,100000.0,0.001]  -- Could use better formatting

-- | Function with complex expression that needs formatting
complexExpression :: [Text] -> Map Text Int -> (Text, Int)
complexExpression items counts=let processed=map(T.strip.T.toLower)items;filtered=filter(not.T.null)processed;unique=nub filtered;sorted=sort unique;combined=T.intercalate","sorted;count=Map.findWithDefault 0 combined counts in(combined,count)  -- Needs serious reformatting

-- | Deeply nested structure that should be formatted
deeplyNested :: [[[[Int]]]]
deeplyNested=[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]

-- | Function with bad comment formatting
badComments :: Int -> String  --This comment has no space
badComments x {- this is a bad
block comment -} = show x{-another bad comment-}

-- | Function with mixed indentation (tabs vs spaces) - simulated
mixedIndentation :: Int -> Int
mixedIndentation x = 
    if x > 0 
	then x + 1  -- This line uses different indentation
    else x - 1

-- | Import formatting test - should be handled by organize imports
import qualified Data.Map as Map  -- This import is out of place
import System.IO  -- This should be moved up with other imports

-- | Function using the late imports
lateImportUsage :: IO ()
lateImportUsage = do
  hPutStrLn stderr "Error message"
  let mapping = Map.fromList [("a", 1), ("b", 2)]
  print mapping

-- | Function with bad record update syntax  
badRecordUpdate :: GoodRecord -> GoodRecord
badRecordUpdate r=r{goodField1=42,goodField2="updated"}  -- Needs spacing

-- | Function with poorly formatted guards
badGuards :: Int -> String
badGuards x|x<0="negative"|x==0="zero"|otherwise="positive"  -- All on one line

-- | Long line that should be broken up
veryLongFunction :: String -> String -> String -> String -> String -> String -> String
veryLongFunction a b c d e f = a ++ " " ++ b ++ " " ++ c ++ " " ++ d ++ " " ++ e ++ " " ++ f