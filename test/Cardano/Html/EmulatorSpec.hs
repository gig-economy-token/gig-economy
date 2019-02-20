{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Html.EmulatorSpec (spec) where

import Test.Hspec
import Cardano.Emulator
import Cardano.Html.Emulator
import Data.IORef
import qualified Wallet.Emulator.Types as Emulator
import qualified Ledger
import Control.Monad (forM_)

newtype MockEmulator a = MockEmulator { runMockEmulator :: IORef SimulatedChain -> IO a }
    deriving Functor

instance Monad MockEmulator where
  return a = MockEmulator (\_ -> pure a)
  (MockEmulator a) >>= famb = MockEmulator (\sc -> a sc >>= \a' -> runMockEmulator (famb a') sc)

-- I need Applicative as constraint for Monad, so implemented a simple Monad-based one
instance Applicative MockEmulator where
  pure = return
  ff <*> fa = do
              f <- ff
              a <- fa
              pure (f a)

instance HasSimulatedChain MockEmulator where
  readSimulatedChain = MockEmulator (\scref-> readIORef scref)
  modifySimulatedChain f = MockEmulator (\scref -> atomicModifyIORef' scref (\sc -> (f sc, ())))

spec :: Spec
spec = do
  describe "HasSimulatedChain monad" $ do
    it "readEmulatorState works as expected on empty chain" $ do
      ref <- newIORef emptySimulatedChain
      es <- runMockEmulator readEmulatorState ref
      Emulator._chainNewestFirst es `shouldBe` []
      Emulator._emulatorLog es `shouldBe` []

    it "appendStepAndNotify >> readEmulatorState generates a single extra block" $ do
      ref <- newIORef emptySimulatedChain
      es <- runMockEmulator (appendStepAndNotifyKnownWallets (pure ()) >> readEmulatorState) ref
      -- Single block in the blockchain
      (length $ Emulator._chainNewestFirst es) `shouldBe` 1

      -- log entries = 1 from appendStepAndNotify + n from initialTx
      (length $ Emulator._emulatorLog es) `shouldBe` 1 + length initialTx
      -- the first entry is from appendStepAndNotify
      (Emulator._emulatorLog es !! 0) `shouldBe` Emulator.SlotAdd (Ledger.Slot 1)
      -- The other entries are from initialTx
      forM_ [1..length initialTx] $ \pos -> do
        (Emulator._emulatorLog es !! pos) `shouldSatisfy` isTxnValidate

isTxnValidate :: Emulator.EmulatorEvent -> Bool
isTxnValidate (Emulator.TxnValidate _) = True
isTxnValidate _ = False
