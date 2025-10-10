{-# LANGUAGE ScopedTypeVariables #-}

module GHCID.Wrapper
  ( runGhcidWrapper,
  )
where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception, SomeException, bracket, handle, mask, throwTo, try)
import Control.Monad (void, when)
import Data.Maybe (isNothing)
import Data.Function ((&))
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getParentProcessID, getProcessID)
import System.Posix.Signals
import System.Posix.Types (CPid (..), ProcessGroupID)
import System.Process.Typed (Process, getExitCode, inherit, proc, setStderr, setStdin, setStdout, startProcess, stopProcess, waitExitCode)

-- | Wrapper outcome used to drive exit codes and cleanup.
data WrapperOutcome
  = ChildExited ExitCode
  | ParentLost
  | SignalOutcome Signal
  deriving (Eq, Show)

newtype TerminationSignal = TerminationSignal Signal
  deriving (Eq, Show)

instance Exception TerminationSignal

parentCheckIntervalMicros :: Int
parentCheckIntervalMicros = 500000 -- 500ms

parseWrapperArgs :: [String] -> Either String (CPid, String, [String])
parseWrapperArgs args =
  case break (== "--") args of
    (opts, "--" : cmd : cmdArgs) -> do
      parentPidStr <- lookupParent opts
      parentPidInt <- parseInt parentPidStr
      pure (CPid (fromIntegral parentPidInt), cmd, cmdArgs)
    _ -> Left "Expected `--parent-pid <pid> -- <ghcid-command>`"
  where
    lookupParent [] = Left "Missing --parent-pid argument"
    lookupParent ("--parent-pid" : pidStr : _) = Right pidStr
    lookupParent (_ : rest) = lookupParent rest

    parseInt :: String -> Either String Int
    parseInt s =
      case reads s of
        [(n, "")] -> Right n
        _ -> Left "Invalid parent PID"

parentWatcher :: CPid -> IO ()
parentWatcher expectedParent = loop
  where
    loop = do
      threadDelay parentCheckIntervalMicros
      currentParent <- getParentProcessID
      if currentParent == expectedParent
        then loop
        else pure ()

setupSignalHandlers :: IO ()
setupSignalHandlers = do
  mainThread <- myThreadId
  let handler sig = throwTo mainThread (TerminationSignal sig)
      install sig = void $ installHandler sig (Catch $ handler sig) Nothing
  mapM_ install [sigTERM, sigINT, sigQUIT]

startGhcidProcess :: String -> [String] -> IO (Process () () ())
startGhcidProcess cmd cmdArgs = do
  let processConfig =
        proc cmd cmdArgs
          & setStdin inherit
          & setStdout inherit
          & setStderr inherit
  startProcess processConfig

cleanupProcess :: ProcessGroupID -> Process () () () -> IO ()
cleanupProcess pgid process = do
  mExit <- getExitCode process
  case mExit of
    Just _ -> pure ()
    Nothing -> do
      let send sig = void (try (signalProcessGroup sig pgid) :: IO (Either SomeException ()))
      _ <- send sigTERM
      threadDelay 500000
      stillRunning <- isNothing <$> getExitCode process
      when stillRunning $ do
        _ <- send sigKILL
        threadDelay 200000
      void $ handle (\(_ :: SomeException) -> pure ExitSuccess) (waitExitCode process)
  handle (\(_ :: SomeException) -> pure ()) (stopProcess process)

runGhcidWrapper :: [String] -> IO ()
runGhcidWrapper args =
  case parseWrapperArgs args of
    Left err -> do
      hPutStrLn stderr $ "mcp-ghcid wrapper error: " <> err
      exitFailure
    Right (parentPid, cmd, cmdArgs) -> do
      setupSignalHandlers
      CPid selfRaw <- getProcessID
      let groupId :: ProcessGroupID
          groupId = fromIntegral selfRaw
      outcome <-
        mask $ \restore ->
          bracket (startGhcidProcess cmd cmdArgs) (cleanupProcess groupId) $ \process -> do
            let childExit = waitExitCode process
                parentExit = parentWatcher parentPid
            result <- try $ restore (race childExit parentExit)
            case result of
              Left (TerminationSignal sig) -> pure (SignalOutcome sig)
              Right (Left exitCode) -> pure (ChildExited exitCode)
              Right (Right ()) -> do
                hPutStrLn stderr "mcp-ghcid wrapper: parent process exited unexpectedly; terminating ghcid"
                pure ParentLost
      case outcome of
        ChildExited exitCode -> exitWith exitCode
        ParentLost -> exitWith (ExitFailure 130)
        SignalOutcome sig -> exitWith (ExitFailure (128 + fromIntegral sig))
