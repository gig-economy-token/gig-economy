{-# LANGUAGE OverloadedStrings #-}
module Cardano.GuessingGameSpec (spec) where

import Cardano.TestHelpers
import Test.Hspec
import qualified Wallet.Emulator as Emulator
import Cardano.GameContract
import Data.Either
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Basic emulated runs on the guessing game contract" $ do
        it "player 1 guesses right" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                initialTx = createMiningTransaction [(w1, 40), (w2, 60)]
                (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ startGame
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ lock "asdf" 4
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ guess "asdf"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            result `shouldSatisfy` isRight
            getResultingFunds ws1 `shouldBe` 44
            getResultingFunds ws2 `shouldBe` 56

        it "player 1 guesses wrong" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                initialTx = createMiningTransaction [(w1, 40), (w2, 60)]
                (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ startGame
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ lock "asdf" 4
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ guess "blah"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            result `shouldSatisfy` isRight
            getResultingFunds ws1 `shouldBe` 40
            getResultingFunds ws2 `shouldBe` 56

        it "player 1 guesses wrong several times, eventually player 2 reclaims back the funds" $ do
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                initialTx = createMiningTransaction [(w1, 40), (w2, 60)]
                (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ startGame
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ lock "asdf" 4
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ guess "blah"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ guess "nope"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w1 $ guess "foo"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    _ <- Emulator.walletAction w2 $ guess "asdf"
                    _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                    pure ()
                walletStates = Emulator._walletStates state
                Just ws1 = Map.lookup w1 walletStates
                Just ws2 = Map.lookup w2 walletStates
                
            result `shouldSatisfy` isRight
            getResultingFunds ws1 `shouldBe` 40
            getResultingFunds ws2 `shouldBe` 60
