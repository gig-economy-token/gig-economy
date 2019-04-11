{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Employer.Contracts where

import qualified Language.PlutusTx as P
import qualified Ledger as L
import qualified Ledger.Validation as L
import qualified Ledger.Value as L
import qualified Wallet.API as W

import Data.ByteString.Lazy

data JobOffer = JobOffer
  { jobOfferId          :: Int
  , jobOfferTitle       :: ByteString
  , jobOfferDescription :: ByteString
  , jobOfferPayout      :: Int
  } deriving (Eq, Show)

P.makeLift ''JobOffer

data JobCompleted = JobCompleted Int
  deriving (Eq, Show)

P.makeLift ''JobCompleted

postJobOffer :: (Monad m, W.WalletAPI m) => JobOffer -> m ()
postJobOffer jobOffer@JobOffer{..} =
  let value = L.singleton (L.currencySymbol 1) jobOfferPayout
      bar   = L.DataScript $ L.lifted jobOffer
  in W.payToScript_ W.defaultSlotRange employerAddress value bar

employerAddress :: L.Address
employerAddress = L.scriptAddress employerValidator

employerValidator :: L.ValidatorScript
employerValidator = L.ValidatorScript (L.fromCompiledCode $$(P.compile [||
  \(JobOffer joId _ _ _) (JobCompleted jcId) (_ :: L.PendingTx) ->
    if $$(P.eq) joId jcId
    then ()
    else ($$(P.error) ($$(P.traceH) "asd" ()))
  ||]))

completeJob :: (Monad m, W.WalletAPI m) => Int -> m ()
completeJob a = W.collectFromScript W.defaultSlotRange employerValidator (foo a)

foo :: Int -> L.RedeemerScript
foo s = L.RedeemerScript $ L.lifted (JobCompleted s)
