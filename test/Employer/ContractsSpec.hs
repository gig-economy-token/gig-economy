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
spec = do
  describe "openJobOffer" $ do
    it "creates a job offer when there are enough funds" $ do
      let result =
            evalTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              (tx : _) <- walletAction employerWallet $
                openJobOffer JobOffer
                  { jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 10
                  }

              addBlocksAndNotify wallets 1

              assertOwnFundsEq employerWallet (mkValue 0)
              assertIsValidated tx


      result `shouldSatisfy` isRight

    it "does not create a job offer when there are not enough funds" $ do
      let result =
            evalTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              runEmployerAction $
                openJobOffer JobOffer
                  { jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 20
                  }

              addBlocksAndNotify wallets 1

              assertOwnFundsEq employerWallet (mkValue 10)


      result `shouldSatisfy` isRight

  describe "closeOffer" $ do
    it "redeems employer funds if the offer is closed" $ do
      let (result, bar) =
            runTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              runEmployerAction $
                openJobOffer JobOffer
                  { jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 5
                  }

              assertOwnFundsEq employerWallet (mkValue 5)

              runEmployerAction closeJobOffer

              assertOwnFundsEq employerWallet (mkValue 10)


      mapM_ print (_emulatorLog bar)

      result `shouldSatisfy` isRight

    it "does nothing if there was no job offer available<Paste>" $ do
      let (result, bar) =
            runTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              runEmployerAction closeJobOffer

              assertOwnFundsEq employerWallet (mkValue 10)


      mapM_ print (_emulatorLog bar)

      result `shouldSatisfy` isRight

  describe "applyJobOffer" $ do
    it "..." $ do
      let (result, bar) =
            runTraceTxPool initialTxPool $ do
              addBlocksAndNotify wallets 1

              runEmployeeAction subscribeToEmployer

              runEmployerAction $
                openJobOffer JobOffer
                  { jobOfferTitle       = "Foo"
                  , jobOfferDescription = "Bar"
                  , jobOfferPayout      = 5
                  }

              assertOwnFundsEq employerWallet (mkValue 5)

              runEmployeeAction applyJobOffer

              assertOwnFundsEq employeeWallet (mkValue 5)


      mapM_ print (_emulatorLog bar)

      result `shouldSatisfy` isRight


-- Runners

runEmployerAction :: m () -> Trace m ()
runEmployerAction = void .runWalletActionAndProcessPending wallets employerWallet

runEmployeeAction :: m () -> Trace m ()
runEmployeeAction = void . runWalletActionAndProcessPending wallets employeeWallet

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
