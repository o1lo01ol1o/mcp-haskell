{-# LANGUAGE OverloadedStrings #-}

module Utils.Logging
  ( LogLevel (..)
  , defaultLogLevel
  , setLogLevel
  , getLogLevel
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Exception (IOException, catch, try)
import Control.Monad (when)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (XdgDirectory (XdgState), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (AppendMode)
  , hFlush
  , hSetBuffering
  , openFile
  , openTempFile
  , stderr
  )
import System.IO.Unsafe (unsafePerformIO)

-- | Logging levels used across MCP servers.
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord)

defaultLogLevel :: LogLevel
defaultLogLevel = Warn

{-# NOINLINE loggingLevelRef #-}
loggingLevelRef :: IORef LogLevel
loggingLevelRef = unsafePerformIO (newIORef defaultLogLevel)

setLogLevel :: LogLevel -> IO ()
setLogLevel = atomicWriteIORef loggingLevelRef

getLogLevel :: IO LogLevel
getLogLevel = readIORef loggingLevelRef

-- | Handle destination and whether we should mirror to stderr.
data LoggingTarget = LoggingTarget
  { targetHandle :: Handle
  , targetAlsoStderr :: Bool
  }

{-# NOINLINE loggingTarget #-}
loggingTarget :: LoggingTarget
loggingTarget = unsafePerformIO acquireLoggingTarget

acquireLoggingTarget :: IO LoggingTarget
acquireLoggingTarget = do
  mPath <- lookupEnv "MCP_LOG_FILE"
  path <- case mPath of
    Just explicit -> pure explicit
    Nothing -> do
      let fallbackPath :: IOException -> IO FilePath
          fallbackPath _ = pure "/tmp/mcp-hls.log"
      base <- getXdgDirectory XdgState "mcp-hls" `catch` fallbackPath
      pure (base </> "mcp.log")

  let ensureDir :: IO ()
      ensureDir = createDirectoryIfMissing True (takeDirectory path)
      fallbackDir :: IOException -> IO ()
      fallbackDir _ = pure ()

  ensureDir `catch` fallbackDir

  handle <- openWithFallback path
  safeSetLineBuffering handle
  alsoStderrRequested <- fmap isJust (lookupEnv "MCP_LOG_STDERR")
  let alsoStderr = alsoStderrRequested && handle /= stderr
  pure LoggingTarget { targetHandle = handle, targetAlsoStderr = alsoStderr }
  where
    openWithFallback :: FilePath -> IO Handle
    openWithFallback requestedPath = do
      primary <- try (openFile requestedPath AppendMode) :: IO (Either IOException Handle)
      case primary of
        Right h -> pure h
        Left _ -> do
          fallback <- try (openFile "/tmp/mcp-hls.log" AppendMode) :: IO (Either IOException Handle)
          case fallback of
            Right h -> pure h
            Left _ -> do
              tmpDirResult <- try getXdgTempDir :: IO (Either IOException FilePath)
              case tmpDirResult of
                Right tmpDir -> do
                  tmp <- try (openTempFile tmpDir "mcp-hls.log") :: IO (Either IOException (FilePath, Handle))
                  case tmp of
                    Right (_fp, h) -> pure h
                    Left _ -> pure stderr
                Left _ -> pure stderr

    -- Prefer XDG temp when available; fallback to /tmp.
    getXdgTempDir :: IO FilePath
    getXdgTempDir = pure "/tmp"

    safeSetLineBuffering :: Handle -> IO ()
    safeSetLineBuffering h =
      hSetBuffering h LineBuffering `catch` ignoreIOException

    ignoreIOException :: IOException -> IO ()
    ignoreIOException _ = pure ()

-- | Core logging primitive.
logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = do
  currentLevel <- getLogLevel
  when (level >= currentLevel) $ do
    let line = "[" <> T.pack (show level) <> "] " <> msg
        LoggingTarget handle alsoStderr = loggingTarget
    TIO.hPutStrLn handle line
    hFlush handle
    when alsoStderr $ TIO.hPutStrLn stderr line

-- | Convenience helpers for each log level.
logDebug, logInfo, logWarn, logError :: Text -> IO ()
logDebug = logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error
