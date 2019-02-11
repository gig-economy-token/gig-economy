module Cardano.Emulator.GuessingGame where

import Wallet.Emulator
import Ledger
import Cardano.Helpers


player1Wallet :: Wallet
player1Wallet = Wallet 1001

player2Wallet :: Wallet
player2Wallet = Wallet 1002

miningTx :: Tx
miningTx = createMiningTransaction [(player1Wallet, 100), (player2Wallet, 100)]

wallets :: [Wallet]
wallets = [player1Wallet, player2Wallet]
