{-# LANGUAGE OverloadedStrings #-}

module Utils.Logging
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
import Control.Monad.Logger (LoggingT, runLoggingT)
import qualified Control.Monad.Logger as Logger
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
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
    Nothing -> acquireDefault `catch` \(_ :: IOException) -> pure "/tmp/mcp-hls.log"

  catch (createDirectoryIfMissing True (takeDirectory path)) $ \(_ :: IOException) -> pure ()
  handle <- catch (openFile path AppendMode) $ \(_ :: IOException) -> openFile "/tmp/mcp-hls.log" AppendMode
  hSetBuffering handle LineBuffering
  alsoStderr <- fmap isJust (lookupEnv "MCP_LOG_STDERR")
  pure LoggingTarget { targetHandle = handle, targetAlsoStderr = alsoStderr }

logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = do
  currentLevel <- getLogLevel
  when (level >= currentLevel) $ do
    let line = "[" <> T.pack (show level) <> "] " <> msg
        target = loggingTarget
    TIO.hPutStrLn (targetHandle target) line
    hFlush (targetHandle target)
    when (targetAlsoStderr target) $ TIO.hPutStrLn stderr line

logDebug, logInfo, logWarn, logError :: Text -> IO ()
logDebug = logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error

withLogging :: LoggingT IO a -> IO a
withLogging action =
  runLoggingT action $ \_ level msg ->
    let text = TE.decodeUtf8With TE.lenientDecode (fromLogStr msg)
    in logMessage (fromLevel level) text
  where
    fromLevel Logger.LevelDebug = Debug
    fromLevel Logger.LevelInfo = Info
    fromLevel Logger.LevelWarn = Warn
    fromLevel Logger.LevelError = Error
