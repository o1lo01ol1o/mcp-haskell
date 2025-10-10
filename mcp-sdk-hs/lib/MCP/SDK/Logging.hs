{-# LANGUAGE OverloadedStrings #-}

module MCP.SDK.Logging
  ( LogLevel (..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , withLogging
  , setLogLevel
  , getLogLevel
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Logger as Logger
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , IOMode (AppendMode)
  , hFlush
  , hSetBuffering
  , openFile
  , stderr
  )
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger (fromLogStr)

-- Log Level
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

-- | Destination for logs.
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
  let acquireDefault = do
        base <- getXdgDirectory XdgState "mcp-hls"
        pure (base </> "mcp.log")

  path <- case mPath of
    Just explicit -> pure explicit
    Nothing -> acquireDefault `catch` fallback

  catch (createDirectoryIfMissing True (takeDirectory path)) fallbackDir
  handle <- catch (openFile path AppendMode) fallbackOpen
  hSetBuffering handle LineBuffering
  alsoStderr <- fmap isJust (lookupEnv "MCP_LOG_STDERR")
  pure LoggingTarget { targetHandle = handle, targetAlsoStderr = alsoStderr }

  where
    fallback :: IOException -> IO FilePath
    fallback _ = pure "/tmp/mcp-hls.log"

    fallbackDir :: IOException -> IO ()
    fallbackDir _ = pure ()

    fallbackOpen :: IOException -> IO Handle
    fallbackOpen _ = openFile "/tmp/mcp-hls.log" AppendMode

-- | Write a formatted message to the active targets.
logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = do
  currentLevel <- getLogLevel
  when (level >= currentLevel) $ do
    let line = "[" <> T.pack (show level) <> "] " <> msg
        LoggingTarget h writeStderr = loggingTarget
    TIO.hPutStrLn h line
    hFlush h
    when writeStderr $ TIO.hPutStrLn stderr line

logDebug, logInfo, logWarn, logError :: Text -> IO ()
logDebug = logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error

withLogging :: LoggingT IO a -> IO a
withLogging action =
  Logger.runLoggingT action $ \_ _ level msg ->
    let txt = TE.decodeUtf8With TE.lenientDecode (fromLogStr msg)
    in logMessage (fromLevel level) txt
  where
    fromLevel Logger.LevelDebug = Debug
    fromLevel Logger.LevelInfo = Info
    fromLevel Logger.LevelWarn = Warn
    fromLevel Logger.LevelError = Error
    fromLevel (Logger.LevelOther _) = Info
