module EmulatorState.Handlers where

import EmulatorState.Persistence
import Import

postEmulatorStateDeleteR :: Handler Html
postEmulatorStateDeleteR = do
  resetEmulatorState
  redirect BlockchainStatusR
