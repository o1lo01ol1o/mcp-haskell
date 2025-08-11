{-# LANGUAGE OverloadedStrings #-}

module Test.Fixtures where

import Data.Text (Text)
import qualified Data.Text as T

-- | Sample Haskell files for testing
testModuleContent :: Text
testModuleContent = "{-# LANGUAGE OverloadedStrings #-}\n\
\-- | A test module for HLS integration testing\n\
\module TestModule where\n\
\\n\
\import Data.Text (Text)\n\
\import qualified Data.Text as T\n\
\\n\
\-- | A simple function that concatenates two texts\n\
\concatenateTexts :: Text -> Text -> Text\n\
\concatenateTexts x y = x <> \" \" <> y\n\
\\n\
\-- | A function with a type error for testing diagnostics\n\
\functionWithError :: Int -> String\n\
\functionWithError x = x  -- This should cause a type error\n\
\\n\
\-- | A function for testing hover information\n\
\calculateLength :: [a] -> Int\n\
\calculateLength [] = 0\n\
\calculateLength (_:xs) = 1 + calculateLength xs"

validModuleContent :: Text
validModuleContent = "{-# LANGUAGE OverloadedStrings #-}\n\
\-- | A completely valid module for testing successful operations\n\
\module ValidModule where\n\
\\n\
\import Data.Maybe (fromMaybe)\n\
\\n\
\-- | A simple pure function\n\
\addNumbers :: Int -> Int -> Int\n\
\addNumbers x y = x + y\n\
\\n\
\-- | A function using Maybe\n\
\safeDiv :: Int -> Int -> Maybe Int\n\
\safeDiv _ 0 = Nothing\n\
\safeDiv x y = Just (x `div` y)"

-- | Cabal file content for test workspace
testCabalFile :: Text
testCabalFile = "cabal-version: 2.4\n\
\name: test-project\n\
\version: 0.1.0.0\n\
\build-type: Simple\n\
\\n\
\library\n\
\  exposed-modules: TestModule, ValidModule\n\
\  build-depends: base >= 4.7 && < 5, text >= 1.2 && < 3\n\
\  hs-source-dirs: .\n\
\  default-language: Haskell2010\n\
\  ghc-options: -Wall"

-- | hie.yaml configuration  
hieYamlContent :: Text
hieYamlContent = "cradle:\n\
\  cabal:\n\
\    - path: \".\"\n\
\      component: \"lib:test-project\""

-- | Test files for creating workspace
testWorkspaceFiles :: [(FilePath, String)]
testWorkspaceFiles =
  [ ("TestModule.hs", T.unpack testModuleContent)
  , ("ValidModule.hs", T.unpack validModuleContent) 
  , ("test-project.cabal", T.unpack testCabalFile)
  , ("hie.yaml", T.unpack hieYamlContent)
  ]