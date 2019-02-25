{-# LANGUAGE OverloadedStrings #-}
module Cardano.JobContractSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Ledger
import Cardano.JobContract
import ArbitraryInstances ()
import qualified Wallet.Emulator as Emulator
import Cardano.Helpers
import Data.Either
import qualified Data.Map as Map

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
      extractJobOffers am `shouldBe` Just [jobOffer]

{-
    it "Employer posts a job, then closes it, employee can't read it anymore" $ do
      let wallets@[wEmployer, wEmployee] = Emulator.Wallet <$> [1, 2]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = JobOffer "Description" 4
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ closeOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()
          walletStates = Emulator._walletStates state
          Just wsEmployee = Map.lookup wEmployee walletStates
          am = Emulator._addressMap wsEmployee

      result `shouldSatisfy` isRight
      extractJobOffers am `shouldBe` Just []
-}
    it "Employer posts a job, employee accepts it, employer sees acceptance" $ do
      let wallets@[wEmployer, wEmployee] = Emulator.Wallet <$> [1, 2]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = JobOffer "Description" 4
          jobAcceptance = JobAcceptance "John Doe"
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.walletAction wEmployer $ subscribeToJobAcceptanceBoard jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ acceptOffer jobOffer jobAcceptance
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()
          walletStates = Emulator._walletStates state
          Just wsEmployer = Map.lookup wEmployer walletStates
          am = Emulator._addressMap wsEmployer
          acceptances = extractJobAcceptances am jobOffer

      result `shouldSatisfy` isRight
      acceptances `shouldBe` Just [jobAcceptance]
