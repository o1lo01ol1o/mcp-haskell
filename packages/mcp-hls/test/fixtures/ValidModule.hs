{-# LANGUAGE OverloadedStrings #-}

-- | A completely valid module for testing successful operations
module ValidModule where

import Data.Maybe (fromMaybe)

-- | A simple pure function
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- | A function using Maybe
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- | A higher-order function
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = 
  case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs