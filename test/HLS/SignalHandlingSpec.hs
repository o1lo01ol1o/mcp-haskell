{-# LANGUAGE OverloadedStrings #-}

module HLS.SignalHandlingSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Time
import System.Posix.Signals
import HLS.Signals

spec :: Spec
spec = describe "HLS.Signals" $ do
  signalHandlerSpec
  signalNameSpec
  signalTimingSpec

signalHandlerSpec :: Spec
signalHandlerSpec = describe "Signal Handler Functions" $ do
  
  it "can setup signal handlers without error" $ do
    signalVar <- newTVarIO Nothing
    setupSignalHandlers signalVar
    -- If we get here without exception, setup worked
    True `shouldBe` True

  it "handleSignal records SIGTERM correctly" $ do
    testSignalHandling sigTERM "SIGTERM"

  it "handleSignal records SIGINT correctly" $ do  
    testSignalHandling sigINT "SIGINT"

  it "handleSignal records SIGHUP correctly" $ do
    testSignalHandling sigHUP "SIGHUP"

  it "handleSignal records SIGPIPE correctly" $ do
    testSignalHandling sigPIPE "SIGPIPE"

  it "handleSignal records SIGABRT correctly" $ do
    testSignalHandling sigABRT "SIGABRT"

  it "handleSignal records SIGQUIT correctly" $ do
    testSignalHandling sigQUIT "SIGQUIT"

  it "handleSignal records SIGUSR1 correctly" $ do
    testSignalHandling sigUSR1 "SIGUSR1"

  it "handleSignal records SIGUSR2 correctly" $ do
    testSignalHandling sigUSR2 "SIGUSR2"

signalNameSpec :: Spec  
signalNameSpec = describe "Signal Name Mapping" $ do
  
  it "maps SIGTERM to correct name" $ do
    getSignalName sigTERM `shouldBe` "SIGTERM"

  it "maps SIGINT to correct name" $ do
    getSignalName sigINT `shouldBe` "SIGINT"

  it "maps SIGHUP to correct name" $ do
    getSignalName sigHUP `shouldBe` "SIGHUP"

  it "maps SIGPIPE to correct name" $ do
    getSignalName sigPIPE `shouldBe` "SIGPIPE"

  it "maps SIGABRT to correct name" $ do
    getSignalName sigABRT `shouldBe` "SIGABRT"

  it "maps SIGQUIT to correct name" $ do
    getSignalName sigQUIT `shouldBe` "SIGQUIT"

  it "maps SIGUSR1 to correct name" $ do
    getSignalName sigUSR1 `shouldBe` "SIGUSR1"

  it "maps SIGUSR2 to correct name" $ do
    getSignalName sigUSR2 `shouldBe` "SIGUSR2"

signalTimingSpec :: Spec
signalTimingSpec = describe "Signal Timing and Detection" $ do

  it "records accurate timestamps for signal reception" $ do
    signalVar <- newTVarIO Nothing
    
    beforeTime <- getCurrentTime
    handleSignal signalVar sigINT
    afterTime <- getCurrentTime
    
    maybeSignal <- readTVarIO signalVar
    case maybeSignal of
      Nothing -> expectationFailure "Signal should have been recorded"
      Just sigInfo -> do
        let timestamp = receivedAt sigInfo
        timestamp `shouldSatisfy` (>= beforeTime)
        timestamp `shouldSatisfy` (<= afterTime)

  it "handles rapid signal succession correctly" $ do
    signalVar <- newTVarIO Nothing
    
    -- Send multiple signals rapidly (TVar should record the last one)
    handleSignal signalVar sigTERM
    threadDelay 10000  -- 0.01 second
    handleSignal signalVar sigINT  
    threadDelay 10000  -- 0.01 second
    handleSignal signalVar sigHUP
    
    -- Should record the last signal (due to TVar overwriting)
    maybeSignal <- readTVarIO signalVar
    case maybeSignal of
      Nothing -> expectationFailure "Signal should have been recorded"
      Just sigInfo -> signalName sigInfo `shouldBe` "SIGHUP"

-- Helper Functions

-- | Test that a signal handler correctly records signal information
testSignalHandling :: Signal -> Text -> IO ()
testSignalHandling sig expectedName = do
  signalVar <- newTVarIO Nothing
  
  -- Simulate signal reception
  handleSignal signalVar sig
  
  -- Verify signal was recorded
  maybeSignal <- readTVarIO signalVar
  case maybeSignal of
    Nothing -> expectationFailure "Signal should have been recorded"
    Just sigInfo -> do
      signalName sigInfo `shouldBe` expectedName
      signalNumber sigInfo `shouldBe` fromIntegral (fromEnum sig)
      
      -- Verify timestamp is recent (within last 5 seconds)
      currentTime <- getCurrentTime
      let timeDiff = diffUTCTime currentTime (receivedAt sigInfo)
      timeDiff `shouldSatisfy` (< 5.0)