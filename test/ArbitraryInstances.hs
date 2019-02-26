{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances () where

-- Random orphan instances for testing are thrown in this file

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Cardano.JobContract
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Ledger.Types

instance Arbitrary JobOffer where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary JobAcceptance where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary B8.ByteString where
  arbitrary = B8.pack <$> genericArbitrary

instance Arbitrary Ledger.Types.PubKey where
  arbitrary = Ledger.Types.PubKey <$> arbitrary
