{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.TestHelpers where

import qualified Data.Set as Set

import qualified Ledger
import qualified Ledger.Interval as Interval
import qualified Wallet.Emulator as Emulator
import qualified Data.Map as Map
import Control.Lens

-- Sample mining transaction that fills wallet 1 and 2 with some nano ADA
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
          pk = case n of Emulator.Wallet n' -> Ledger.PubKey n'

getResultingFunds :: Emulator.WalletState -> Int
getResultingFunds ws = Map.foldr (+) 0 $ (Ledger.getValue . Ledger.txOutValue) <$> view Emulator.ownFunds ws
