{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.Runner
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Control.Exception (try, SomeException, bracket_)
import Data.Maybe (fromMaybe)
import qualified Test.Utils (cleanupAllProcesses)

import qualified HLS.IntegrationSpec
import qualified HLS.LSPSpec
import qualified HLS.CommandSpec
import qualified HLS.SignalHandlingSpec
import qualified MCP.ToolsSpec
import qualified MCP.SimpleEndToEndSpec
import qualified MCP.FullPipelineSpec
import qualified MCP.ComprehensiveToolSpec

main :: IO ()
main = bracket_
  Test.Utils.cleanupAllProcesses  -- Setup: clean up any orphaned processes from previous runs
  Test.Utils.cleanupAllProcesses  -- Cleanup: kill all processes on exit
  $ do
    -- Check if HLS is available
    hlsAvailable <- checkHLSAvailable
    
    -- Check test mode from environment variable
    testMode <- lookupEnv "TEST_MODE"
    let mode = fromMaybe "comprehensive" testMode
    
    if hlsAvailable
      then do
        putStrLn "✓ haskell-language-server-wrapper found on PATH"
        case mode of
          "fast" -> do
            putStrLn "Running FAST test suite..."
            runFastTests
          "comprehensive" -> do
            putStrLn "Running COMPREHENSIVE test suite..."
            runComprehensiveTests
          _ -> do
            putStrLn $ "Unknown test mode: " ++ mode ++ ". Using comprehensive."
            runComprehensiveTests
          
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
        runComprehensiveTests

-- | Run fast test suite (basic functionality, no comprehensive tool testing)
runFastTests :: IO ()
runFastTests = do
  hspecWith defaultConfig { configColorMode = ColorAlways } $ do
    describe "Fast Test Suite" $ do
      HLS.IntegrationSpec.spec
      HLS.LSPSpec.spec
      HLS.CommandSpec.spec
      HLS.SignalHandlingSpec.spec
      MCP.ToolsSpec.spec
      MCP.SimpleEndToEndSpec.spec
      -- Skip comprehensive tool testing in fast mode

-- | Run comprehensive test suite (all tests including tool coverage)
runComprehensiveTests :: IO ()
runComprehensiveTests = do
  hspecWith defaultConfig { configColorMode = ColorAlways } $ do
    describe "Comprehensive Test Suite" $ do
      HLS.IntegrationSpec.spec
      HLS.LSPSpec.spec
      HLS.CommandSpec.spec
      HLS.SignalHandlingSpec.spec
      MCP.ToolsSpec.spec
      MCP.SimpleEndToEndSpec.spec
      MCP.FullPipelineSpec.spec
      MCP.ComprehensiveToolSpec.spec

-- | Check if HLS is available on the system
checkHLSAvailable :: IO Bool
checkHLSAvailable = do
  result <- try $ readProcess "haskell-language-server-wrapper" ["--version"] ""
  case result of
    Left (_ :: SomeException) -> return False
    Right output -> do
      putStrLn $ "Found HLS: " <> take 100 output
      return True