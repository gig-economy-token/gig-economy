{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.JobContractSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Ledger
import qualified Ledger.Ada as Ada
import Cardano.JobContract
import ArbitraryInstances
import qualified Wallet.Emulator as Emulator
import qualified Wallet.Emulator.AddressMap as AM
import Cardano.Helpers
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import qualified Wallet.API as WalletAPI

spec :: Spec
spec = do
  cathegoriesSpec
  jobBoardSpec
  escrowSpec


cathegoriesSpec :: Spec
cathegoriesSpec = do
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

  describe "parseJobEscrow" $ do
    prop "fromJust . parseJobEscrow . Ledger.lifted == id :: EscrowSetup -> EscrowSetup" $ \escrowSetup -> do
      parseJobEscrow (Ledger.DataScript (Ledger.lifted escrowSetup)) `shouldBe` Just escrowSetup

  describe "toJobOffer - (toJobOfferForm, PubKey) isomorphism" $ do
    prop "->" $ \(jof, k) -> (toJobOfferForm $ toJobOffer jof k) `shouldBe` jof
    prop "<-" $ \jo -> (toJobOffer (toJobOfferForm jo) (joOfferer jo)) `shouldBe` jo


jobBoardSpec :: Spec
jobBoardSpec = do
  describe "Basic use cases" $ do
    prop "Employer posts a job, employee reads it back" $
      \(Different2 (employeePk, employerPk), jobOfferForm) -> do
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
      \(Different2 (employeePk, employerPk), jobOfferForm) -> do
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
      \(Different2 (employeePk, employerPk), jobOfferForm) -> do
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


escrowSpec :: Spec
escrowSpec = do
    describe "employer creates an escrow, and then" $ do
      prop "releases it" $ 
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer')]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf (joPayout jobOffer'))
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ escrowAcceptEmployer jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` []
        resultingFunds state wEmployer `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployee `shouldBe` joPayout jobOffer'

      prop "employee rejects it back to the employer" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf 100)
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ escrowRejectEmployee jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` []
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` joPayout jobOffer

      prop "arbiter decides the employee should receive the funds" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)

            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.walletAction wArbiter $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf (joPayout jobOffer))
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                x <- Emulator.walletAction wArbiter $ escrowAcceptArbiter jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure x

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` []
        resultingFunds state wEmployer `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployee `shouldBe` joPayout jobOffer

      prop "arbiter decides the employer should receive back the funds" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)

            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.walletAction wArbiter $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf (joPayout jobOffer))
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wArbiter $ escrowRejectArbiter jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` []
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` joPayout jobOffer


      prop "employee tries unsuccessfully to release it without employers approval" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf $ joPayout jobOffer)
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ escrowAcceptEmployer jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` [Ledger.ScriptFailure ["Bad acceptance by employer"]]
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` 0

      prop "employer tries to get their money back without employees approval" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf $ joPayout jobOffer)
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ escrowRejectEmployee jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` [Ledger.ScriptFailure ["Bad reject by employee"]]
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` 0

      prop "employer tries to get their money back by pretending to be the arbiter" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf $ joPayout jobOffer)
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ escrowRejectArbiter jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` [Ledger.ScriptFailure ["Bad reject by arbiter"]]
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` 0

      prop "employee tries to get the money by pretending to be the arbiter" $
        \(Different3 (employerKP, employeeKP, arbiterKP), jobOffer') -> do
        let 
            keypairs = [employerKP, employeeKP, arbiterKP]
            wallets@[wEmployer, wEmployee, wArbiter] = (walletFromPubKey . WalletAPI.pubKey) <$> keypairs
            initialTx = createMiningTransaction [(wEmployer, joPayout jobOffer)]

            jobOffer = jobOffer' { joOfferer = WalletAPI.pubKey employerKP }
            jobApplication = JobApplication (WalletAPI.pubKey employeeKP)
           
            (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ subscribeToEscrow jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployer $ createEscrow jobOffer jobApplication (WalletAPI.pubKey arbiterKP) (Ada.adaValueOf $ joPayout jobOffer)
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                _ <- Emulator.walletAction wEmployee $ escrowAcceptArbiter jobOffer jobApplication
                _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock wallets
                pure ()

        result `shouldSatisfy` isRight
        getEmulatorErrors state `shouldBe` [Ledger.ScriptFailure ["Bad acceptance by arbiter"]]
        resultingFunds state wEmployee `shouldBe` 0
        resultingFunds state wArbiter `shouldBe` 0
        resultingFunds state wEmployer `shouldBe` 0

    prop "failure" $ \(Different3 (a,b,c) :: Different3 Int) -> do
      a `shouldNotBe` b
      a `shouldNotBe` c
      b `shouldNotBe` c

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

resultingFunds :: Emulator.EmulatorState -> Emulator.Wallet -> Int
resultingFunds state wallet = fromMaybe 0 (getResultingFunds <$> (Map.lookup wallet (Emulator._walletStates state)))
