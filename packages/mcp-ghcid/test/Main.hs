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

-- Import all GHCID test specs
import qualified GHCID.ClientSpec
import qualified GHCID.FilterSpec
import qualified GHCID.OutputSpec
import qualified GHCID.ProcessRegistrySpec
import qualified GHCID.ResourceLeakSpec
import qualified GHCID.ConcurrencySpec
import qualified MCP.GHCIDToolsSpec
import qualified MCP.Types.GHCIDSpec
import qualified MCP.Router.GHCIDSpec

main :: IO ()
main = bracket_
  Test.Utils.cleanupAllProcesses  -- Setup: clean up any orphaned processes
  Test.Utils.cleanupAllProcesses  -- Cleanup: kill all processes on exit
  $ do
    -- Check if ghcid is available
    ghcidAvailable <- checkGHCIDAvailable
    
    -- Check test mode from environment variable
    testMode <- lookupEnv "TEST_MODE"
    let mode = fromMaybe "comprehensive" testMode
    
    if ghcidAvailable
      then do
        putStrLn "✓ ghcid found on PATH"
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
        putStrLn "✗ ghcid not found on PATH"
        putStrLn "Please install ghcid to run integration tests"
        putStrLn ""
        putStrLn "Installation instructions:"
        putStrLn "  - Using Cabal: cabal install ghcid"
        putStrLn "  - Using Stack: stack install ghcid"
        
        -- Still run unit tests that don't require ghcid
        putStrLn ""
        putStrLn "Running unit tests only..."
        runUnitTests

-- | Run fast test suite (unit tests only)
runFastTests :: IO ()
runFastTests = do
  hspecWith defaultConfig { configColorMode = ColorAlways } $ do
    describe "Fast Test Suite (Unit Tests Only)" $ do
      GHCID.FilterSpec.spec
      GHCID.OutputSpec.spec
      MCP.Types.GHCIDSpec.spec
      -- Skip integration tests in fast mode

-- | Run unit tests only (no external dependencies)
runUnitTests :: IO ()
runUnitTests = do
  hspecWith defaultConfig { configColorMode = ColorAlways } $ do
    describe "Unit Test Suite" $ do
      GHCID.FilterSpec.spec
      GHCID.OutputSpec.spec
      MCP.Types.GHCIDSpec.spec

-- | Run comprehensive test suite (all tests including integration)
runComprehensiveTests :: IO ()
runComprehensiveTests = do
  hspecWith defaultConfig { configColorMode = ColorAlways } $ do
    describe "Comprehensive Test Suite" $ do
      -- Unit tests
      GHCID.FilterSpec.spec
      GHCID.OutputSpec.spec
      MCP.Types.GHCIDSpec.spec
      
      -- Integration tests (require external dependencies)
      GHCID.ClientSpec.spec
      GHCID.ProcessRegistrySpec.spec
      MCP.GHCIDToolsSpec.spec
      MCP.Router.GHCIDSpec.spec
      
      -- Advanced tests (resource management and concurrency)
      GHCID.ResourceLeakSpec.spec
      GHCID.ConcurrencySpec.spec

-- | Check if ghcid is available on the system
checkGHCIDAvailable :: IO Bool
checkGHCIDAvailable = do
  result <- try $ readProcess "ghcid" ["--version"] ""
  case result of
    Left (_ :: SomeException) -> return False
    Right output -> do
      putStrLn $ "Found ghcid: " <> take 100 output
      return True