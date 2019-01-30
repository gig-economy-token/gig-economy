{-# LANGUAGE OverloadedStrings #-}
module CardanoStuffSpec (spec) where

import CardanoStuff
import Test.Hspec
import Language.PlutusTx.Plugin

spec :: Spec
spec = do
    describe "we can compile Plutus code" $ do
        it "compiles integerOne" $ do
            -- These values are hardcoded to the results
            getSerializedPlc integerOne `shouldBe` "\246\246\SOH\NUL\NUL\196\246\192\246\b\SOH"
            getSerializedPir integerOne `shouldBe` "\131\NUL\246\131\ENQ\246\192\246\b\SOH"
