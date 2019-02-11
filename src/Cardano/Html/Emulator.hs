{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Html.Emulator where

import Import
import Yesod.Core.Types

import qualified Wallet.Emulator as Emulator
import Cardano.Emulator


readSimulatedChain :: Handler SimulatedChain
readSimulatedChain = (simulatedChain . rheSite . handlerEnv) <$> ask >>= readIORef

readEmulatorState :: Handler Emulator.EmulatorState
readEmulatorState = scEmulatorState <$> readSimulatedChain

appendStep :: Emulator.Trace Emulator.MockWallet () -> Handler ()
appendStep newStep = appendStep' stepAndNotify
  where
    stepAndNotify = do
                    newStep
                    Emulator.processPending >>= Emulator.walletsNotifyBlock allKnownWallets >> pure ()

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
  where
    readSimulatedChainRef :: Handler (IORef SimulatedChain)
    readSimulatedChainRef = (simulatedChain . rheSite . handlerEnv) <$> ask
