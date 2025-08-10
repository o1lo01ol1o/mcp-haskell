{-# LANGUAGE OverloadedStrings #-}

module Utils.Logging where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- Log Level
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord)

-- Log Message
logMessage :: LogLevel -> Text -> IO ()
logMessage level msg = TIO.hPutStrLn stderr $ "[" <> T.pack (show level) <> "] " <> msg

-- Convenience functions
logDebug, logInfo, logWarn, logError :: Text -> IO ()
logDebug = logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error