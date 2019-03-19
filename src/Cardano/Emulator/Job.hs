module Cardano.Emulator.Job where

import Wallet.Emulator
import Ledger
import Cardano.Helpers


employerWallet :: Wallet
employerWallet = Wallet 2001

employeeWallet :: Wallet
employeeWallet = Wallet 2002

arbiterWallet :: Wallet
arbiterWallet = Wallet 2003

miningTx :: Tx
miningTx = createMiningTransaction [(employerWallet, 100)]

wallets :: [Wallet]
wallets = [employerWallet, employeeWallet, arbiterWallet]
