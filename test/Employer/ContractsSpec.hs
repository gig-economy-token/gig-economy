module Employer.ContractsSpec where

import qualified Ledger as L
import qualified Ledger.Ada as L

import Control.Monad
import Data.Either
import Employer.Contracts
import Test.Hspec
import Wallet.API
import Wallet.Emulator

spec :: Spec
spec = do
  it "..." $ do
    let result =
          evalTraceTxPool initialTxPool $ do
            addBlocksAndNotify wallets 1

            _ <- runEmployeeAction subscribeToEmployer

            assertEmployeeFundsEq (L.adaValueOf 0)
            assertEmployerFundsEq (L.adaValueOf 10)

            (tx1 : _) <- walletAction employerWallet $
              openJobOffer JobOffer
                { jobOfferTitle       = "Foo"
                , jobOfferDescription = "Bar"
                , jobOfferPayout      = L.adaValueOf 5
                }

            addBlocksAndNotify wallets 1

            assertEmployeeFundsEq (L.adaValueOf 0)
            assertEmployerFundsEq (L.adaValueOf 5)

            (tx2 : _) <- walletAction employerWallet $
              openJobOffer JobOffer
                { jobOfferTitle       = "Foo"
                , jobOfferDescription = "Bar"
                , jobOfferPayout      = L.adaValueOf 4
                }

            addBlocksAndNotify wallets 1

            assertEmployeeFundsEq (L.adaValueOf 0)
            assertEmployerFundsEq (L.adaValueOf 1)

            _ <- runEmployerAction $ closeJobOffer (L.hashTx tx2)

            assertEmployeeFundsEq (L.adaValueOf 0)
						-- FIXME: not returning the funds
            -- assertEmployerFundsEq (L.adaValueOf 5)

            _ <- runEmployerAction $ closeJobOffer (L.hashTx tx2)

            -- assertEmployerFundsEq (L.adaValueOf 5)
            -- assertEmployeeFundsEq (L.adaValueOf 0)

            _ <- runEmployeeAction $ applyJobOffer (L.hashTx tx1)
            _ <- runEmployeeAction $ applyJobOffer (L.hashTx tx2)

            -- assertEmployerFundsEq (L.adaValueOf 5)
            -- assertEmployeeFundsEq (L.adaValueOf 5)
            pure ()


    result `shouldSatisfy` isRight

-- Runners

runEmployerAction :: m () -> Trace m [L.Tx]
runEmployerAction = runWalletActionAndProcessPending wallets employerWallet

runEmployeeAction :: m () -> Trace m ()
runEmployeeAction = void . runWalletActionAndProcessPending wallets employeeWallet

-- Assertions

assertEmployerFundsEq :: L.Value -> Trace m ()
assertEmployerFundsEq = assertOwnFundsEq employerWallet

assertEmployeeFundsEq :: L.Value -> Trace m ()
assertEmployeeFundsEq = assertOwnFundsEq employeeWallet

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
  , txOutputs    = pure $ L.pubKeyTxOut (L.adaValueOf 10) (PubKey 1)
  , txForge      = L.adaValueOf 10
  , txFee        = fromInteger 0
  , txValidRange = defaultSlotRange
  }
  where
