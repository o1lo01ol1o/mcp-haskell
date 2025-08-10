{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.Runner
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Control.Exception (try, SomeException)

import qualified HLS.IntegrationSpec
import qualified HLS.LSPSpec
import qualified HLS.CommandSpec
import qualified MCP.ToolsSpec

main :: IO ()
main = do
  -- Check if HLS is available
  hlsAvailable <- checkHLSAvailable
  
  if hlsAvailable
    then do
      putStrLn "✓ haskell-language-server-wrapper found on PATH"
      putStrLn "Running HLS integration tests..."
      
      -- Run the test suite
      hspecWith defaultConfig { configColorMode = ColorAlways } $ do
        HLS.IntegrationSpec.spec
        HLS.LSPSpec.spec
        HLS.CommandSpec.spec
        MCP.ToolsSpec.spec
        
    else do
      putStrLn "✗ haskell-language-server-wrapper not found on PATH"
      putStrLn "Please install Haskell Language Server to run integration tests"
      putStrLn ""
      putStrLn "Installation instructions:"
      putStrLn "  - Using GHCup: ghcup install hls"
      putStrLn "  - Using Stack: stack install haskell-language-server"
      putStrLn "  - Using Cabal: cabal install haskell-language-server"
      
      -- Still run the test suite but it will likely fail
      putStrLn ""
      putStrLn "Attempting to run tests anyway..."
      hspecWith defaultConfig { configColorMode = ColorAlways } $ do
        HLS.IntegrationSpec.spec
        HLS.LSPSpec.spec
        HLS.CommandSpec.spec
        MCP.ToolsSpec.spec

-- | Check if HLS is available on the system
checkHLSAvailable :: IO Bool
checkHLSAvailable = do
  result <- try $ readProcess "haskell-language-server-wrapper" ["--version"] ""
  case result of
    Left (_ :: SomeException) -> return False
    Right output -> do
      putStrLn $ "Found HLS: " <> take 100 output
      return True