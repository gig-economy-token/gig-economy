{-# LANGUAGE OverloadedStrings #-}
module Cardano.JobContractSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Ledger
import Cardano.JobContract
import ArbitraryInstances ()

spec :: Spec
spec = do
  describe "parseJobOffer . Ledger.lifted isomorphism" $ do
    prop "parseJobOffer . Ledger.lifted == id" $ \jobOffer -> do
      let datascript = Ledger.DataScript (Ledger.lifted jobOffer)
          jobOffer' = parseJobOffer datascript
      jobOffer' `shouldBe` Just jobOffer
