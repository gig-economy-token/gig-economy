{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
module Cardano.JobContract.Types where

import Prelude hiding ((++))
import qualified Language.PlutusTx            as PlutusTx
import           Ledger hiding (inputs, out)
import GHC.Generics
import           Data.ByteString.Lazy (ByteString)

-- Datatype for posting job offers
data JobOffer = JobOffer
  { joDescription :: ByteString
  , joPayout      :: Int
  , joOfferer     :: PubKey
  }
  deriving (Show, Eq, Ord, Generic)
PlutusTx.makeLift ''JobOffer

data JobOfferForm = JobOfferForm
  { jofDescription :: ByteString
  , jofPayout      :: Int
  }
  deriving (Show, Eq, Ord, Generic)

toJobOffer :: JobOfferForm -> PubKey -> JobOffer
toJobOffer JobOfferForm{..} pk = JobOffer {..}
  where
    joDescription=jofDescription
    joPayout=jofPayout
    joOfferer=pk

toJobOfferForm :: JobOffer -> JobOfferForm
toJobOfferForm JobOffer {..} = JobOfferForm {..}
  where
    jofDescription=joDescription
    jofPayout=joPayout

-- Datatype for accepting a job offer
data JobApplication = JobApplication
  { jaAcceptor    :: PubKey
  }
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''JobApplication

-- Datatype for setting up the escrow
data EscrowSetup = EscrowSetup
  { esJobOffer        :: JobOffer
  , esJobApplication  :: JobApplication
  , esArbiter         :: PubKey
  }
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''EscrowSetup

-- Datatype for the result of the escrow
data EscrowResult
  = EscrowAcceptedByEmployer Signature
  | EscrowRejectedByEmployee Signature
  | EscrowAcceptedByArbiter Signature
  | EscrowRejectedByArbiter Signature
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''EscrowResult
