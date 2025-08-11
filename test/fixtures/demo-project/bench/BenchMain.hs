-- | Simple benchmark for demo project
module Main where

import Criterion.Main
import Demo.Evaluation
import Demo.Basic

main :: IO ()
main = defaultMain
  [ bench "simple arithmetic" $ whnf (\_ -> simpleArithmetic) ()
  , bench "factorial 10" $ whnf factorial 10  
  , bench "fibonacci 10" $ whnf fibonacci 10
  ]