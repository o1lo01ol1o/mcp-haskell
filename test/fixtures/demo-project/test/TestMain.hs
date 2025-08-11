-- | Simple test executable for demo project
module Main where

import Demo.Basic
import Demo.Evaluation

main :: IO ()
main = do
  putStrLn "Demo Project Test Executable"
  putStrLn $ "Sample calculation: " ++ show simpleArithmetic
  putStrLn $ "Sample greeting: " ++ show (greetUser (UserInfo "Test" 25 "test@example.com"))
  putStrLn "All modules loaded successfully!"