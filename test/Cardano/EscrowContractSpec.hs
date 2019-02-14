{-# LANGUAGE OverloadedStrings #-}
module Cardano.EscrowContractSpec (spec) where

import Cardano.Helpers
import Test.Hspec
import Ledger
import qualified Wallet.Emulator as Emulator
import Cardano.EscrowContract
import Data.Either
import qualified Data.Map as Map
import Data.Maybe

spec :: Spec
spec = do
    describe "Escrow contract" $ do
        it "complete successful transaction" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                contractNo = ContractNo 43
                initialTx = createMiningTransaction [(w1, 100), (w2, 100)]
                (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ watchSmartContract contractNo
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ watchSmartContract contractNo
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1234 (Value 100)
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1234
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            result `shouldSatisfy` isRight
            getResultingFunds ws1 `shouldBe` 0
            getResultingFunds ws2 `shouldBe` 200

        it "complete two successful transactions in a row" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                contractNo = ContractNo 43
                initialTx = createMiningTransaction [(w1, 200), (w2, 100)]
                update = Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2] >> pure ()
                (_result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    update
                    _ <- Emulator.walletAction w1 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w2 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1235 (Value 100)
                    update
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1235
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1234 (Value 100)
                    update
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1234
                    update
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            (catMaybes $ extractErrors <$> Emulator._emulatorLog state) `shouldBe` []
            getResultingFunds ws1 `shouldBe` 0
            getResultingFunds ws2 `shouldBe` 300

        it "Acumulate funds using twice the same password, and redeem all at once" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                contractNo = ContractNo 43
                initialTx = createMiningTransaction [(w1, 200), (w2, 100)]
                update = Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2] >> pure ()
                (_result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    update
                    _ <- Emulator.walletAction w1 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w2 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1234 (Value 100)
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1234 (Value 100)
                    update
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1234
                    update
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            (catMaybes $ extractErrors <$> Emulator._emulatorLog state) `shouldBe` []
            getResultingFunds ws1 `shouldBe` 0
            getResultingFunds ws2 `shouldBe` 300

        it "XXX - Investigate - lock with two passwords makes the funds non-redeemable" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                contractNo = ContractNo 43
                initialTx = createMiningTransaction [(w1, 200), (w2, 100)]
                update = Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2] >> pure ()
                (_result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    update
                    _ <- Emulator.walletAction w1 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w2 $ watchSmartContract contractNo
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1235 (Value 100)
                    update
                    _ <- Emulator.walletAction w1 $ depositADA contractNo 1234 (Value 100)
                    update
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1235
                    update
                    _ <- Emulator.walletAction w2 $ withdrawADA contractNo 1234
                    update
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            (catMaybes $ extractErrors <$> Emulator._emulatorLog state) `shouldBe` [
                                              ScriptFailure ["1234 - 1235"],
                                              ScriptFailure ["1235 - 1234"]]
            getResultingFunds ws1 `shouldBe` 0
            getResultingFunds ws2 `shouldBe` 100


extractErrors :: Emulator.EmulatorEvent -> Maybe ValidationError
extractErrors (Emulator.TxnValidationFail _ e) = Just e
extractErrors _ = Nothing

