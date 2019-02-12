{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
module Cardano.Html.Emulator
  ( HasSimulatedChain(..)
  , appendStepAndNotifyKnownWallets
  ) where

import Import
import Yesod.Core.Types

import qualified Wallet.Emulator as Emulator
import Cardano.Emulator


class Monad m => HasSimulatedChain m where
  readSimulatedChain              :: m SimulatedChain
  readEmulatorState               :: m Emulator.EmulatorState
  appendStep                      :: Emulator.Trace Emulator.MockWallet () -> m ()

instance HasSimulatedChain Handler where
  readSimulatedChain = readSimulatedChainRef >>= readIORef
  readEmulatorState = scEmulatorState <$> readSimulatedChain
  appendStep newStep = do
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

readSimulatedChainRef :: MonadReader (HandlerData c App) m => m (IORef SimulatedChain)
readSimulatedChainRef = (simulatedChain . rheSite . handlerEnv) <$> ask

-- Helper for most cases
appendStepAndNotifyKnownWallets :: HasSimulatedChain m => Emulator.Trace Emulator.MockWallet () -> m ()
appendStepAndNotifyKnownWallets newStep = appendStep stepAndNotify
  where
    stepAndNotify = do
                    newStep
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock allKnownWallets
                    pure ()
