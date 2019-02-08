{-# LANGUAGE OverloadedStrings #-}
module Cardano.GuessingGameSpec (spec) where

import Cardano.TestHelpers
import Test.Hspec
import qualified Wallet.Emulator as Emulator
import Cardano.GameContract
import Data.Either
import qualified Data.Map as Map
import Control.Monad

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
            -- TODO: Look for a way to check and assert the locked funds of the smart contract
            -- getFundsFromSmartContract `shouldBe` 4

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

        it "Multi-step play" $ do
            -- The multi-step is based on accumulating steps on a trace that is stored in a IORef
            -- on the main thread.
            -- Effectively, on each page refresh we run the whole trace (very inneficcient),
            -- but it works, and for a prototype phase is good enough.
            -- Possible performance optimization:
            --    Store in another IORef the state of the last emulator run,
            --    and return that emulator state without repeating the process
            --    until the user executes another action
            let [w1, w2] = Emulator.Wallet <$> [1, 2]
                initialTx = createMiningTransaction [(w1, 40), (w2, 60)]

                trace :: Emulator.Trace Emulator.MockWallet ()
                trace = do
                        _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                        pure ()

                simulateAndAssertFunds tr walls = do
                  let (result, state) = Emulator.runTraceTxPool [initialTx] $ do
                                              tr
                  print (Emulator._emulatorLog state)
                  result `shouldSatisfy` isRight
                  forM_ walls (\(w, funds) -> do
                    let ws = Map.lookup w $ Emulator._walletStates state
                    getResultingFunds <$> ws `shouldBe` Just funds)
                  
            simulateAndAssertFunds trace [(w1, 40), (w2, 60)]

            -- Create a new trace and save it to the global variable
            let trace2 = do
                          trace
                          _ <- Emulator.walletAction w1 $ startGame
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          _ <- Emulator.walletAction w2 $ lock "asdf" 4
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          pure ()
                          
            simulateAndAssertFunds trace2 [(w1, 40), (w2, 56)]

            -- Create a new trace and save it to the global variable
            let trace3 = do
                          trace2
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          _ <- Emulator.walletAction w1 $ guess "asdf"
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          pure ()
                          
            simulateAndAssertFunds trace3 [(w1, 44), (w2, 56)]

            let trace4 = do
                          trace
                          _ <- Emulator.walletAction w1 $ startGame
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          _ <- Emulator.walletAction w2 $ lock "asdf" 400
                          _ <- Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
                          pure ()
                          
            simulateAndAssertFunds trace4 [(w1, 44), (w2, 56)]

