{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.TestHelpers where

import qualified Data.Set as Set

import qualified Ledger
import qualified Ledger.Interval as Interval
import qualified Wallet.Emulator as Emulator
import qualified Data.Map as Map
import Control.Lens

-- Creates a Tx where the specified wallets have received the specified amout via mining
createMiningTransaction :: [(Emulator.Wallet, Int)] -> Ledger.Tx
createMiningTransaction wallets = Ledger.Tx
  { Ledger.txInputs = Set.empty
  , Ledger.txOutputs = outputs
  , Ledger.txForge = Ledger.Value total
  , Ledger.txFee = Ledger.Value 0
  , Ledger.txValidRange = ($$(Interval.always))
  }
  where
    outputs = mkTxOutOf <$> wallets
    total = sum $ snd <$> wallets
    mkTxOutOf :: (Emulator.Wallet, Int) -> Ledger.TxOut
    mkTxOutOf (n, q) = Ledger.TxOutOf
                        { Ledger.txOutAddress = Ledger.pubKeyAddress pk
                        , Ledger.txOutValue = Ledger.Value q
                        , Ledger.txOutType = Ledger.PayToPubKey pk
                        }
        where
          pk = Ledger.PubKey (Emulator.getWallet n)

-- Consolidates the results of a wallet from the emulator
-- to a single number that we can assert easily
getResultingFunds :: Emulator.WalletState -> Int
getResultingFunds ws = Map.foldr (+) 0 $ (Ledger.getValue . Ledger.txOutValue) <$> view Emulator.ownFunds ws
