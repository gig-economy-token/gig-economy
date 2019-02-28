{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.JobContractSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Ledger
import Cardano.JobContract
import ArbitraryInstances
import qualified Wallet.Emulator as Emulator
import qualified Wallet.Emulator.AddressMap as AM
import Cardano.Helpers
import Data.Either
import qualified Data.Map as Map
import Data.Maybe

spec :: Spec
spec = do
  describe "parseJobOffer" $ do
    prop "fromJust . parseJobOffer . Ledger.lifted == id :: JobOffer -> JobOffer" $ \jobOffer -> do
      let datascript = Ledger.DataScript (Ledger.lifted jobOffer)
          jobOffer' = parseJobOffer datascript
      jobOffer' `shouldBe` Just jobOffer

  describe "parseJobApplication" $ do
    prop "fromJust . parseJobApplication . Ledger.lifted == id :: JobApplication -> JobApplication" $ \jobApplication -> do
      let datascript = Ledger.DataScript (Ledger.lifted jobApplication)
          jobApplication' = parseJobApplication datascript
      jobApplication' `shouldBe` Just jobApplication

  describe "toJobOffer - (toJobOfferForm, PubKey) isomorphism" $ do
    prop "->" $ \(jof, k) -> (toJobOfferForm $ toJobOffer jof k) `shouldBe` jof
    prop "<-" $ \jo -> (toJobOffer (toJobOfferForm jo) (joOfferer jo)) `shouldBe` jo

  describe "Basic use cases" $ do
    prop "Employer posts a job, employee reads it back" $
      \(Different (employeePk, employerPk), jobOfferForm) -> do
      let wallets@[wEmployer, wEmployee] = walletFromPubKey <$> [employerPk, employeePk]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = toJobOffer jobOfferForm employerPk
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOfferForm
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()

      result `shouldSatisfy` isRight
      getEmulatorErrors state `shouldBe` []
      extractJobOffers (getAddressMap state wEmployee) `shouldBe` Just [jobOffer]

    prop "Employer posts a job, then closes it, employee can't read it anymore" $
      \(Different (employeePk, employerPk), jobOfferForm) -> do
      let wallets@[wEmployee, wEmployer] = walletFromPubKey <$> [employeePk, employerPk]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOfferForm
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ closeOffer jobOfferForm
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()

      result `shouldSatisfy` isRight
      getEmulatorErrors state `shouldBe` []
      extractJobOffers (getAddressMap state wEmployee) `shouldBe` Just []

    prop "employer posts a job, employee applies to it, employer sees applications" $
      \(Different (employeePk, employerPk), jobOfferForm) -> do
      let wallets@[wEmployer, wEmployee] = walletFromPubKey <$> [employerPk, employeePk]
          initialTx = createMiningTransaction [(wEmployer, 1)]
          jobOffer = toJobOffer jobOfferForm employerPk
          (result, state) = Emulator.runTraceTxPool [initialTx] $ do
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ subscribeToJobBoard
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployer $ postOffer jobOfferForm
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              _ <- Emulator.walletAction wEmployee $ applyToOffer jobOffer
              _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
              pure ()

      result `shouldSatisfy` isRight
      getEmulatorErrors state `shouldBe` []
      extractJobApplications (getAddressMap state wEmployer) jobOffer `shouldBe` Just [JobApplication employeePk]

walletFromPubKey :: Ledger.PubKey -> Emulator.Wallet
walletFromPubKey (Ledger.PubKey x) = Emulator.Wallet x

getAddressMap :: Emulator.EmulatorState -> Emulator.Wallet -> AM.AddressMap
getAddressMap state wallet = Emulator._addressMap walletState
  where
    walletStates = Emulator._walletStates state
    Just walletState = Map.lookup wallet walletStates

getEmulatorErrors :: Emulator.EmulatorState -> [Ledger.ValidationError]
getEmulatorErrors state = catMaybes $ getValidationFail <$> (Emulator._emulatorLog state)
  where
    getValidationFail :: Emulator.EmulatorEvent -> Maybe Ledger.ValidationError
    getValidationFail (Emulator.TxnValidationFail _ b) = Just b
    getValidationFail _ = Nothing
