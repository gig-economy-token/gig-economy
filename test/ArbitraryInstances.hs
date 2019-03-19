{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances where

-- Various orphan instances for testing are thrown in this file

import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Cardano.JobContract
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Ledger.Types
import qualified Wallet.API

instance Arbitrary JobOffer where
  arbitrary = JobOffer
                <$> arbitrary
                <*> (getPositive <$> arbitrary)
                <*> arbitrary
  shrink = genericShrink

instance Arbitrary JobApplication where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary JobOfferForm where
  arbitrary = JobOfferForm
                <$> arbitrary
                <*> (getPositive <$> arbitrary)
  shrink = genericShrink

instance Arbitrary EscrowSetup where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary B8.ByteString where
  arbitrary = B8.pack <$> genericArbitrary

instance Arbitrary Ledger.Types.PubKey where
  arbitrary = (Ledger.Types.PubKey . getNonNegative) <$> arbitrary

instance Arbitrary Wallet.API.KeyPair where
  arbitrary = (Wallet.API.keyPair . getNonNegative) <$> arbitrary

data Different2 a = Different2 { fromDifferent2 :: (a, a) }
  deriving (Show)

instance (Arbitrary a, Eq a) => Arbitrary (Different2 a) where
  arbitrary = Different2 <$> arbitrary `suchThat` (\(a, b) -> a /= b)
  shrink = const []

data Different3 a = Different3 { fromDifferent3 :: (a, a, a) }
  deriving (Show)

instance (Arbitrary a, Eq a) => Arbitrary (Different3 a) where
  arbitrary = Different3 <$> arbitrary `suchThat` (\(a, b, c) -> a /= b && a /= c && b /= c)
  shrink = const []
