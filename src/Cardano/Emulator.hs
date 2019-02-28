module Cardano.Emulator where

import Wallet.Emulator
import Ledger
import qualified Cardano.Emulator.GuessingGame
import qualified Cardano.Emulator.Job

data SimulatedChain = SimulatedChain
  { scEmulatorState :: EmulatorState
  , scTrace         :: Trace MockWallet ()
  }

emptySimulatedChain :: SimulatedChain
emptySimulatedChain = SimulatedChain
                        { scEmulatorState = emptyEmulatorState
                        , scTrace = pure ()
                        }

defaultSimulatedChain :: SimulatedChain
defaultSimulatedChain = fromTx initialTx

initialTx :: [Tx]
initialTx = [ Cardano.Emulator.GuessingGame.miningTx
            , Cardano.Emulator.Job.miningTx
            ]

allKnownWallets :: [Wallet]
allKnownWallets = mconcat
                    [ Cardano.Emulator.GuessingGame.wallets
                    , Cardano.Emulator.Job.wallets
                    ]

fromTx :: [Tx] -> SimulatedChain
fromTx tx = SimulatedChain
              { scEmulatorState = emulatorState'
              , scTrace = op
              }
  where
    emulatorState' = snd $ runTraceTxPool tx op
    op = processPending >>= walletsNotifyBlock allKnownWallets >> pure ()
