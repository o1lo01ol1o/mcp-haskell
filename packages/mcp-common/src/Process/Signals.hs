{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process.Signals 
  ( SignalInfo(..)
  , setupSignalHandlers
  , installSignalHandler
  , getSignalName
  , handleSignal
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Time
import Data.Text (Text)
import System.Posix.Signals hiding (SignalInfo)
import Foreign.C.Types (CInt)

-- | Information about a received signal
data SignalInfo = SignalInfo
  { signalNumber :: CInt
  , signalName :: Text
  , receivedAt :: UTCTime
  } deriving (Show, Eq, Ord)

-- | Get human-readable name for signal
getSignalName :: Signal -> Text
getSignalName sig
  | sig == sigTERM = "SIGTERM"
  | sig == sigINT  = "SIGINT" 
  | sig == sigHUP  = "SIGHUP"
  | sig == sigPIPE = "SIGPIPE"
  | sig == sigABRT = "SIGABRT"
  | sig == sigQUIT = "SIGQUIT"
  | sig == sigUSR1 = "SIGUSR1"
  | sig == sigUSR2 = "SIGUSR2"
  | otherwise = "UNKNOWN"

-- | Setup signal handlers for all relevant signals
-- This will be called from HLS process initialization
setupSignalHandlers :: TVar (Maybe SignalInfo) -> IO ()
setupSignalHandlers signalVar = do
  -- Install handlers for all signals we want to monitor (silently)
  mapM_ (installSignalHandler signalVar) 
    [ sigTERM  -- Graceful termination request
    , sigINT   -- Interrupt (Ctrl+C)
    , sigHUP   -- Hangup (terminal disconnect) 
    , sigPIPE  -- Broken pipe (LSP communication failure)
    , sigABRT  -- Process abort
    , sigQUIT  -- Quit signal
    , sigUSR1  -- User-defined signal 1
    , sigUSR2  -- User-defined signal 2
    ]

-- | Install a signal handler for a specific signal (silently)
installSignalHandler :: TVar (Maybe SignalInfo) -> Signal -> IO ()
installSignalHandler signalVar sig = do
  result <- try $ installHandler sig (Catch (handleSignal signalVar sig)) Nothing
  
  case result of
    Left (_ :: SomeException) -> return ()  -- Silently ignore installation failures
    Right _ -> return ()

-- | Signal handler that records signal information (silently)
handleSignal :: TVar (Maybe SignalInfo) -> Signal -> IO ()
handleSignal signalVar sig = do
  timestamp <- getCurrentTime
  let sigInfo = SignalInfo
        { signalNumber = fromIntegral $ fromEnum sig
        , signalName = getSignalName sig  
        , receivedAt = timestamp
        }
  
  -- Atomically update the signal status
  atomically $ writeTVar signalVar (Just sigInfo)