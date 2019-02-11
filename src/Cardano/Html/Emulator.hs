{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Html.Emulator where

import Import
import Yesod.Core.Types

import qualified Wallet.Emulator as Emulator
import Cardano.Emulator


readSimulatedChain :: Handler SimulatedChain
readSimulatedChain = readSimulatedChainRef >>= readIORef

readSimulatedChainRef :: Handler (IORef SimulatedChain)
readSimulatedChainRef = (simulatedChain . rheSite . handlerEnv) <$> ask

readEmulatorState :: Handler Emulator.EmulatorState
readEmulatorState = scEmulatorState <$> readSimulatedChain

-- Append a step with notifications to all known wallets and re-simulate
appendStep :: Emulator.Trace Emulator.MockWallet () -> Handler ()
appendStep newStep = appendStep' stepAndNotify
  where
    stepAndNotify = do
                    newStep
                    Emulator.processPending >>= Emulator.walletsNotifyBlock allKnownWallets >> pure ()

-- Append a step and re-simulate
appendStep' :: Emulator.Trace Emulator.MockWallet () -> Handler ()
appendStep' newStep = do
                        scRef <- readSimulatedChainRef
                        sc <- readIORef scRef
                        let prevTrace = scTrace sc
                            newTrace = prevTrace >> newStep
                            (_, newEmulatorState) = Emulator.runTraceTxPool initialTx newTrace
                            sc' = SimulatedChain
                                      { scEmulatorState = newEmulatorState
                                      , scTrace = newTrace
                                      }
                        atomicModifyIORef' scRef (\_ -> (sc', ()))
