{-# LANGUAGE OverloadedStrings #-}
module Cardano.JobContractSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Ledger
import Cardano.JobContract
import ArbitraryInstances ()
import qualified Wallet.Emulator as Emulator
import qualified Wallet.Emulator.AddressMap as AM
import Cardano.Helpers
import Data.Either
import qualified Data.Map as Map
import Handler.Job.Employee.View
import Data.Maybe

spec :: Spec
spec = do
  describe "parseJobOffer" $ do
    prop "parseJobOffer . Ledger.lifted == id" $ \jobOffer -> do
      let datascript = Ledger.DataScript (Ledger.lifted jobOffer)
          jobOffer' = parseJobOffer datascript
      jobOffer' `shouldBe` Just jobOffer

  describe "parseJobAcceptance" $ do
    prop "parseJobAcceptance . Ledger.lifted == id" $ \jobAcceptance -> do
      let datascript = Ledger.DataScript (Ledger.lifted jobAcceptance)
          jobAcceptance' = parseJobAcceptance datascript
      jobAcceptance' `shouldBe` Just jobAcceptance

  describe "Basic use cases" $ do
    it "Employer posts a job, employee reads it back" $ do
      let wallets@[wEmployer, wEmployee] = Emulator.Wallet <$> [1, 2]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = JobOffer "Description" 4
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()
          walletStates = Emulator._walletStates state
          Just wsEmployee = Map.lookup wEmployee walletStates
          am = Emulator._addressMap wsEmployee

      result `shouldSatisfy` isRight
      extractJobOffers (JobBoard am) `shouldBe` Just [jobOffer]

    it "Employer posts a job, employee accepts it, employer sees acceptance" $ do
      let wallets@[wEmployer, wEmployee] = Emulator.Wallet <$> [1, 2]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = JobOffer "Description" 4
          jobAcceptance = JobAcceptance "John Doe"
          (result, _state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.walletAction wEmployer $ subscribeToJobAcceptanceBoard jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ acceptOffer jobOffer jobAcceptance
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()
          _walletStates = Emulator._walletStates _state
          Just _wsEmployer = Map.lookup wEmployer _walletStates
          _am = AM.getAddressMap $ Emulator._addressMap _wsEmployer
          acceptances = do
                        txin <- Map.lookup (jobAddress jobOffer) _am
                        let values = snd <$> Map.toList txin
                            parseAcceptance ds = do
                                                  ds' <- extractDataScript (Ledger.txOutType ds)
                                                  parseJobAcceptance ds'
                        pure $ catMaybes $ parseAcceptance <$> values

      result `shouldSatisfy` isRight
      acceptances `shouldBe` Just [jobAcceptance]

extractDataScript :: Ledger.TxOutType -> Maybe Ledger.DataScript
extractDataScript (Ledger.PayToScript s) = Just s
extractDataScript _               = Nothing
