module EmulatorState.PersistenceSpec where

import qualified Data.Map as M

import Cardano.Emulator
import Cardano.Emulator.Job
import Control.Monad.State
import EmulatorState.Persistence
import Ledger.Ada
import Test.Hspec
import Wallet.Emulator

spec :: Spec
spec =
  describe "resetEmulatorState" $
    it "sets back the emulator state to its original state" $
      let SimulatedChain{..} = execState resetEmulatorState emptySimulatedChain
          walletsAndValues =
            [ (employerWallet, adaValueOf 100)
            , (employeeWallet, adaValueOf 0)
            , (arbiterWallet,  adaValueOf 0)
            ]
      in fundsDistribution scEmulatorState `shouldBe` M.fromList walletsAndValues
