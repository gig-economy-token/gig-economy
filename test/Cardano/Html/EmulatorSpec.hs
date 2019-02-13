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

    it "appendStepAndNotify >> readEmulatorState generates a single block" $ do
      ref <- newIORef emptySimulatedChain
      es <- runMockEmulator (appendStepAndNotifyKnownWallets (pure ()) >> readEmulatorState) ref
      (length $ Emulator._chainNewestFirst es) `shouldBe` 1
      (length $ Emulator._emulatorLog es) `shouldBe` 2
      (Emulator._emulatorLog es !! 0) `shouldBe` Emulator.SlotAdd (Ledger.Slot 1)
      (Emulator._emulatorLog es !! 1) `shouldSatisfy` isTxnValidate

isTxnValidate :: Emulator.EmulatorEvent -> Bool
isTxnValidate (Emulator.TxnValidate _) = True
isTxnValidate _ = False
