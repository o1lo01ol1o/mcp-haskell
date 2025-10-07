{-# LANGUAGE OverloadedStrings #-}

module Utils.Logging where

import Control.Exception (IOException, catch)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (AppendMode), hFlush, hSetBuffering, openFile, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- Log Level
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord)

-- Internal logging target
data LogTarget = LogTarget
  { ltHandle :: Handle
  , ltAlsoStderr :: Bool
  }

{-# NOINLINE logTarget #-}
logTarget :: LogTarget
logTarget = unsafePerformIO $ do
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
  pure LogTarget { ltHandle = handle, ltAlsoStderr = alsoStderr }

-- Log Message
logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = do
  let line = "[" <> T.pack (show level) <> "] " <> msg
  TIO.hPutStrLn (ltHandle logTarget) line
  hFlush (ltHandle logTarget)
  if ltAlsoStderr logTarget
    then TIO.hPutStrLn stderr line
    else pure ()

-- Convenience functions
logDebug, logInfo, logWarn, logError :: Text -> IO ()
logDebug = logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error
