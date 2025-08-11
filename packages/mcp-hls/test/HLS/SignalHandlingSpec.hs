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
  
  describe "Signal handling infrastructure" $ do
    it "can create default shutdown config" $ do
      let config = defaultShutdownConfig
      shutdownGracePeriod config `shouldBe` 30
    
    it "can install signal handlers without error" $ do
      let config = defaultShutdownConfig
      handler <- installHLSSignalHandlers config
      -- If we get here without exception, install worked
      True `shouldBe` True
      
    it "can create shutdown reasons" $ do
      let manualShutdown = ManualShutdown
      let signalShutdown = SignalShutdown sigTERM
      show manualShutdown `shouldContain` "ManualShutdown"
      show signalShutdown `shouldContain` "SignalShutdown"