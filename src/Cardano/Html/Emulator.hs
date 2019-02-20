{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Cardano.Html.Emulator
  ( HasSimulatedChain(..)
  , appendStepAndNotifyKnownWallets
  , appendStep
  , readEmulatorState
  , readWatchedAddresses
  , readWatchedAddresses'
  ) where

import Import
import Yesod.Core.Types
import Wallet.Emulator
import Wallet.Emulator.AddressMap
import qualified Data.Map as Map

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

-- The emulator does not offer the watchedAddresses endpoint
-- but it's offered by the WalletAPI,
-- so it's morally right to expose watched addresses because
-- it is expected to be available on the real chain and real wallets.
readWatchedAddresses :: HasSimulatedChain m => Wallet -> m AddressMap
readWatchedAddresses w = do
                          emState <- readEmulatorState
                          pure (readWatchedAddresses' emState w)

readWatchedAddresses' :: EmulatorState -> Wallet -> AddressMap
readWatchedAddresses' emState w = _addressMap walletState
  where
    ws = _walletStates emState
    walletState :: WalletState
    walletState = fromMaybe (emptyWalletState w) (Map.lookup w ws)

appendStep :: HasSimulatedChain m => Trace MockWallet a -> m ()
appendStep newStep = modifySimulatedChain f
  where
    f sc = sc'
      where
        prevTrace = scTrace sc
        newTrace = void $ prevTrace >> newStep
        (_, newEmulatorState) = runTraceTxPool initialTx newTrace
        sc' = SimulatedChain
                { scEmulatorState = newEmulatorState
                , scTrace = newTrace
                }

appendStepAndNotifyKnownWallets :: HasSimulatedChain m => Trace MockWallet a -> m ()
appendStepAndNotifyKnownWallets newStep = appendStep stepAndNotify
  where
    stepAndNotify = do
                    _ <- newStep
                    _ <- processPending >>= walletsNotifyBlock allKnownWallets
                    pure ()

-- Only for HasSimulatedChain Handler
readSimulatedChainRef :: MonadReader (HandlerData c App) m => m (IORef SimulatedChain)
readSimulatedChainRef = (simulatedChain . rheSite . handlerEnv) <$> ask
