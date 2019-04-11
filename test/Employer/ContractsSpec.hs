module Employer.ContractsSpec where

import Employer.Contracts
import Test.Hspec
import Wallet.Emulator
import Wallet.API
import qualified Ledger as L
import qualified Ledger.Ada as L
import qualified Ledger.Value as L
import Data.Either
import Control.Monad

spec :: Spec
spec = do
  it "..." $ do
    let result =
          evalTraceTxPool initialTxPool $ do
            addBlocksAndNotify wallets 1

            assertEmployerFundsEq (L.adaValueOf 10)

            _ <- runEmployerAction $
              openJobOffer JobOffer
                { jobOfferTitle       = "Foo"
                , jobOfferDescription = "Bar"
                , jobOfferPayout      = 10
                }

            assertEmployerFundsEq (L.adaValueOf 0)


    result `shouldSatisfy` isRight

-- Runners

runEmployerAction :: m () -> Trace m [L.Tx]
runEmployerAction = runWalletActionAndProcessPending wallets employerWallet

runEmployeeAction :: m () -> Trace m ()
runEmployeeAction = void . runWalletActionAndProcessPending wallets employeeWallet

-- Assertions

assertEmployerFundsEq :: L.Value -> Trace m ()
assertEmployerFundsEq = assertOwnFundsEq employerWallet

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
