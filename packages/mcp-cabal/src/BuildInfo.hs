{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BuildInfo
  ( gitCommitHash
  , gitCommitDate
  , gitBranch
  , gitIsDirty
  ) where

import Control.Exception (SomeException, try)
import Language.Haskell.TH
import System.Exit
import System.Process

-- | Git commit hash embedded at compile time
gitCommitHash :: String
gitCommitHash = $(do
  result <- runIO $ try $ readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
  case result of
    Left (_ :: SomeException) -> stringE "unknown"
    Right (ExitSuccess, output, _) -> stringE (take 12 $ filter (/= '\n') output)
    Right (_, _, _) -> stringE "unknown"
  )

-- | Git commit date embedded at compile time
gitCommitDate :: String
gitCommitDate = $(do
  result <- runIO $ try $ readProcessWithExitCode "git" ["show", "-s", "--format=%ci", "HEAD"] ""
  case result of
    Left (_ :: SomeException) -> stringE "unknown"
    Right (ExitSuccess, output, _) -> stringE (filter (/= '\n') output)
    Right (_, _, _) -> stringE "unknown"
  )

-- | Git branch embedded at compile time
gitBranch :: String
gitBranch = $(do
  result <- runIO $ try $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
  case result of
    Left (_ :: SomeException) -> stringE "unknown"
    Right (ExitSuccess, output, _) -> stringE (filter (/= '\n') output)
    Right (_, _, _) -> stringE "unknown"
  )

-- | Whether the build was from a dirty working tree
gitIsDirty :: Bool
gitIsDirty = $(do
  result <- runIO $ try $ readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case result of
    Left (_ :: SomeException) -> [| False |]
    Right (ExitSuccess, "", _) -> [| False |]
    Right (ExitSuccess, _, _) -> [| True |]
    Right (_, _, _) -> [| False |]
  )
