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
      let (foo, bar@EmulatorState{..}) =
            runTraceTxPool initialTxPool $ do
              processPending >>= walletsNotifyBlock wallets

              walletAction employerWallet $
                postJobOffer JobOffer
                  { jobOfferId          = 1
                  , jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 20
                  , jobOfferStatus      = Opened
                  }

              processPending >>= walletsNotifyBlock wallets

              --   let value = L.singleton (L.currencySymbol 1) 10
              --   payToPublicKey_ defaultSlotRange value (PubKey 1)

      print foo
      print bar

      _emulatorLog `shouldBe` []

wallets :: [Wallet]
wallets = [employerWallet, employeeWallet]

employerWallet :: Wallet
employerWallet = Wallet 1

employeeWallet :: Wallet
employeeWallet = Wallet 2

initialTxPool :: TxPool
initialTxPool = pure L.Tx
  { txInputs     = mempty
  , txOutputs    = pure $ L.pubKeyTxOut value (PubKey 1)
  , txForge      = value
  , txFee        = fromInteger 0
  , txValidRange = defaultSlotRange
  }
  where
    value = L.singleton (L.currencySymbol 1) 10
