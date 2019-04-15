module EmulatorState.Persistence where

import Cardano.Emulator
import Cardano.Html.Emulator

resetEmulatorState :: HasSimulatedChain m => m ()
resetEmulatorState = modifySimulatedChain $ const defaultSimulatedChain
