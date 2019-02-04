{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Cardano.Html.Emulator where

import Import
import Yesod.Core.Types

import qualified Wallet.Emulator as Emulator


readEmulatorState :: Handler Emulator.EmulatorState
readEmulatorState = (simulatedChain . rheSite . handlerEnv) <$> ask >>= readIORef

updateEmulatorState :: (Emulator.EmulatorState -> (Emulator.EmulatorState, a)) -> Handler a
updateEmulatorState f = do
  ref <- (simulatedChain . rheSite . handlerEnv) <$> ask
  atomicModifyIORef' ref f

simulateStep :: forall a. Emulator.Trace Emulator.MockWallet a -> Handler (Either Emulator.AssertionError a)
simulateStep op = updateEmulatorState modifyOp
  where
    modifyOp :: Emulator.EmulatorState -> (Emulator.EmulatorState, Either Emulator.AssertionError a)
    modifyOp (Emulator.EmulatorState {..}) = (b, a)
      where
        (a, b) = Emulator.runTraceChain (reverse _chainNewestFirst) op
