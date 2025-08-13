{-# LANGUAGE TemplateHaskell #-}
module BuildInfo where

import Language.Haskell.TH
import System.Process
import System.Exit

-- | Git commit hash embedded at compile time
gitCommitHash :: String
gitCommitHash = $(do
  result <- runIO $ readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
  case result of
    (ExitSuccess, output, _) -> stringE (take 12 $ filter (/= '\n') output)
    _ -> stringE "unknown"
  )

-- | Git commit date embedded at compile time  
gitCommitDate :: String
gitCommitDate = $(do
  result <- runIO $ readProcessWithExitCode "git" ["show", "-s", "--format=%ci", "HEAD"] ""
  case result of
    (ExitSuccess, output, _) -> stringE (filter (/= '\n') output)
    _ -> stringE "unknown"
  )

-- | Git branch embedded at compile time
gitBranch :: String
gitBranch = $(do
  result <- runIO $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
  case result of
    (ExitSuccess, output, _) -> stringE (filter (/= '\n') output)
    _ -> stringE "unknown"
  )

-- | Whether the build was from a dirty working tree
gitIsDirty :: Bool
gitIsDirty = $(do
  result <- runIO $ readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case result of
    (ExitSuccess, "", _) -> [| False |]
    (ExitSuccess, _, _) -> [| True |]
    _ -> [| False |]
  )