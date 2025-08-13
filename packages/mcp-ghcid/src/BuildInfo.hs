{-# LANGUAGE TemplateHaskell #-}
module BuildInfo where

import Language.Haskell.TH
import System.Process
import System.Exit
import Control.Exception (try, SomeException)

-- | Safely run git command, returning "unknown" if git is not available
safeGitCommand :: [String] -> Q String
safeGitCommand args = do
  result <- runIO $ try $ readProcessWithExitCode "git" args ""
  case result of
    Left (_ :: SomeException) -> return "unknown"
    Right (ExitSuccess, output, _) -> return (filter (/= '\n') output)
    Right (_, _, _) -> return "unknown"

-- | Git commit hash embedded at compile time
gitCommitHash :: String
gitCommitHash = $(safeGitCommand ["rev-parse", "HEAD"] >>= \hash -> 
  stringE (take 12 hash))

-- | Git commit date embedded at compile time  
gitCommitDate :: String
gitCommitDate = $(safeGitCommand ["show", "-s", "--format=%ci", "HEAD"] >>= stringE)

-- | Git branch embedded at compile time
gitBranch :: String
gitBranch = $(safeGitCommand ["rev-parse", "--abbrev-ref", "HEAD"] >>= stringE)

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