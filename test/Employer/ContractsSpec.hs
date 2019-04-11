module Employer.ContractsSpec where

import Employer.Contracts
import Test.Hspec
import Wallet.Emulator
import Wallet.API
import qualified Ledger as L
import qualified Ledger.Value as L
import Data.Either

spec :: Spec
spec =
  describe "postJobOffer" $
    it "creates a job offer with status opened" $ do
      let (foo, bar) =
            runTraceTxPool [] $ do
              walletAction employerWallet $ do
                let value = L.singleton (L.currencySymbol 1) 10
                payToPublicKey_ defaultSlotRange value (PubKey 2)

      print bar

      foo `shouldSatisfy` isRight


employerWallet :: Wallet
employerWallet = Wallet 1

initialTxPool :: TxPool
initialTxPool = pure L.Tx
  { txInputs = mempty
  , txOutputs = mempty
  , txForge = undefined
  , txFee = fromInteger 0
  , txValidRange = undefined
  }
