{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Cardano.Html.Emulator
  ( HasSimulatedChain(..)
  , appendStepAndNotifyKnownWallets
  , appendStep
  , readEmulatorState
  ) where

import Import
import Yesod.Core.Types
import Wallet.Emulator

import Cardano.Emulator

class Monad m => HasSimulatedChain m where
  readSimulatedChain    :: m SimulatedChain
  modifySimulatedChain  :: (SimulatedChain -> SimulatedChain) -> m ()

instance HasSimulatedChain Handler where
  readSimulatedChain = readSimulatedChainRef >>= readIORef
  modifySimulatedChain f = do
                            scRef <- readSimulatedChainRef
                            atomicModifyIORef' scRef (\sc -> (f sc, ()))

readEmulatorState :: HasSimulatedChain m => m EmulatorState
readEmulatorState = scEmulatorState <$> readSimulatedChain

appendStep :: HasSimulatedChain m => Trace MockWallet () -> m ()
appendStep newStep = modifySimulatedChain f
  where
    f sc = sc'
      where
        prevTrace = scTrace sc
        newTrace = prevTrace >> newStep
        (_, newEmulatorState) = runTraceTxPool initialTx newTrace
        sc' = SimulatedChain
                { scEmulatorState = newEmulatorState
                , scTrace = newTrace
                }

appendStepAndNotifyKnownWallets :: HasSimulatedChain m => Trace MockWallet () -> m ()
appendStepAndNotifyKnownWallets newStep = appendStep stepAndNotify
  where
    stepAndNotify = do
                    newStep
                    _ <- processPending >>= walletsNotifyBlock allKnownWallets
                    pure ()

-- Only for HasSimulatedChain Handler
readSimulatedChainRef :: MonadReader (HandlerData c App) m => m (IORef SimulatedChain)
readSimulatedChainRef = (simulatedChain . rheSite . handlerEnv) <$> ask
