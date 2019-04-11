module Employer.ContractsSpec where

import Employer.Contracts
import Test.Hspec
import Wallet.Emulator
import Wallet.API
import qualified Ledger as L
import qualified Ledger.Value as L
import Data.Either
import Control.Monad

spec :: Spec
spec =
  describe "postJobOffer" $ do
    it "creates a job offer when there are enough funds" $ do
      let result =
            evalTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              (tx : _) <- walletAction employerWallet $
                postJobOffer JobOffer
                  { jobOfferId          = 1
                  , jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 10
                  , jobOfferStatus      = Opened
                  }

              addBlocksAndNotify wallets 1

              assertOwnFundsEq employerWallet (mkValue 0)
              assertIsValidated tx


      result `shouldSatisfy` isRight

    it "does not create a job offer when there are not enough funds" $ do
      let result =
            evalTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              void $ walletAction employerWallet $
                postJobOffer JobOffer
                  { jobOfferId          = 1
                  , jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 20
                  , jobOfferStatus      = Opened
                  }

              addBlocksAndNotify wallets 1

              assertOwnFundsEq employerWallet (mkValue 10)


      result `shouldSatisfy` isRight


-- TODO: move this out to a separate module

wallets :: [Wallet]
wallets = [employerWallet, employeeWallet]

employerWallet :: Wallet
employerWallet = Wallet 1

employeeWallet :: Wallet
employeeWallet = Wallet 2

initialTxPool :: TxPool
initialTxPool = pure L.Tx
  { txInputs     = mempty
  , txOutputs    = pure $ L.pubKeyTxOut (mkValue 10) (PubKey 1)
  , txForge      = mkValue 10
  , txFee        = fromInteger 0
  , txValidRange = defaultSlotRange
  }
  where

mkValue :: Int -> L.Value
mkValue = L.singleton (L.currencySymbol 1)
