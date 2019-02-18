{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- Logic and helpers for running the smart contract guessing game
module Cardano.GuessingGame where

import qualified Data.Set as Set

import Cardano.GameContract
import qualified Ledger
import Ledger.Ada
import qualified Ledger.Interval as Interval
import qualified Wallet.Emulator as Emulator


simulateOnEmptyBlockchain :: (Either Emulator.AssertionError [[Ledger.Tx]], Emulator.EmulatorState)
simulateOnEmptyBlockchain = Emulator.runTraceChain emptyBlockchain someOperation
  where
    emptyBlockchain = []

simulateWithSampleWallets :: (Either Emulator.AssertionError [[Ledger.Tx]], Emulator.EmulatorState)
simulateWithSampleWallets = Emulator.runTraceTxPool [sampleTransaction] someOperation

-- Random operation on the emulator based on the game smart contract
someOperation :: Emulator.Trace Emulator.MockWallet [[Ledger.Tx]]
someOperation = do
  let [w1, w2] = Emulator.Wallet <$> [1, 2]
  tx0 <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
  tx1 <- Emulator.walletAction w1 $ startGame
  tx2 <- Emulator.walletAction w2 $ lock "asdf" (adaValueOf 4)
  pure $ [tx0, tx1, tx2]

-- Sample mining transaction that fills wallet 1 and 2 with some nano ADA
sampleTransaction :: Ledger.Tx
sampleTransaction = Ledger.Tx
  { Ledger.txInputs = Set.empty
  , Ledger.txOutputs = [
            Ledger.TxOutOf
              { Ledger.txOutAddress = Ledger.pubKeyAddress pk1
              , Ledger.txOutValue = adaValueOf 40
              , Ledger.txOutType = Ledger.PayToPubKey pk1
              },
            Ledger.TxOutOf
              { Ledger.txOutAddress = Ledger.pubKeyAddress pk2
              , Ledger.txOutValue = adaValueOf 60
              , Ledger.txOutType = Ledger.PayToPubKey pk2
              }
  ]
  , Ledger.txForge = adaValueOf 100
  , Ledger.txFee = fromInt 0
  , Ledger.txValidRange = $$(Interval.always)
  }
  where
    pk1 = Ledger.PubKey 1
    pk2 = Ledger.PubKey 2
