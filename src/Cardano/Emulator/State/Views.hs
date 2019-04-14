{-# LANGUAGE TemplateHaskell #-}

module Cardano.Emulator.State.Views
  ( statusW
  , walletW
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Import as I

import Cardano.Helpers
import Ledger.Types
import Ledger.Value.TH
import Wallet.Emulator

statusW :: EmulatorState -> I.Widget
statusW EmulatorState{..} = $(I.widgetFile "status")

walletsW :: M.Map Wallet WalletState -> I.Widget
walletsW wallets = $(I.widgetFile "status/wallets")

walletW :: Wallet -> WalletState -> I.Widget
walletW wallet walletState = $(I.widgetFile "status/wallet")

blocksW :: Blockchain -> I.Widget
blocksW blocks = $(I.widgetFile "status/blocks")

transactionsW :: Tx -> I.Widget
transactionsW Tx{..} = $(I.widgetFile "status/transactions")

inputsW :: [TxIn] -> I.Widget
inputsW ins = $(I.widgetFile "status/inputs")

outputsW :: [TxOut] -> I.Widget
outputsW outs = $(I.widgetFile "status/outputs")

logsW :: [EmulatorEvent] -> I.Widget
logsW logs = $(I.widgetFile "status/logs")

logW :: EmulatorEvent -> I.Widget
logW (SlotAdd s)                  = $(I.widgetFile "status/log/slot-add")
logW (TxnSubmit txId)             = $(I.widgetFile "status/log/txn-submit")
logW (TxnValidate txId)           = $(I.widgetFile "status/log/txn-validate")
logW (TxnValidationFail txId err) = $(I.widgetFile "status/log/txn-validation-fail")
logW (WalletError w err)          = $(I.widgetFile "status/log/wallet-error")
logW (WalletInfo w info)          = $(I.widgetFile "status/log/wallet-info")

txIdToString :: TxId -> String
txIdToString (TxIdOf txId) = show txId

values :: Value -> Int
values = sum . map snd . getValue
