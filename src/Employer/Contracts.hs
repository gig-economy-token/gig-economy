{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Employer.Contracts
  ( JobOffer(..)
  , subscribeToEmployer
  , openJobOffer
  , closeJobOffer
  , applyJobOffer

  , jobOfferAddress
  ) where

import qualified Language.PlutusTx as P
import qualified Ledger as L
import qualified Ledger.Validation as L
import qualified Ledger.Value as L
import qualified Wallet.API as W

import Data.ByteString.Lazy

data JobOffer = JobOffer
  { jobOfferTitle       :: ByteString
  , jobOfferDescription :: ByteString
  , jobOfferPayout      :: Int
  } deriving (Eq, Show)

data JobOfferActions
  = ApplyOffer
  | CloseOffer
  deriving (Eq, Show)


-- Actions

subscribeToEmployer :: W.MonadWallet m => m ()
subscribeToEmployer = W.startWatching jobOfferAddress

openJobOffer :: W.MonadWallet m => JobOffer -> m ()
openJobOffer jobOffer@JobOffer{..} =
  let value  = L.singleton (L.currencySymbol 1) jobOfferPayout
      script = L.DataScript $ L.lifted jobOffer
  in W.payToScript_ W.defaultSlotRange jobOfferAddress value script

closeJobOffer :: W.MonadWallet m => L.TxId -> m ()
closeJobOffer txId =
  let script = L.RedeemerScript (L.lifted CloseOffer)
  in W.collectFromScriptTxn W.defaultSlotRange jobOfferValidator script txId

applyJobOffer :: W.MonadWallet m => L.TxId -> m ()
applyJobOffer txId = do
  let script = L.RedeemerScript (L.lifted ApplyOffer)
  W.collectFromScriptTxn W.defaultSlotRange jobOfferValidator script txId

-- Helpers

jobOfferAddress :: L.Address
jobOfferAddress = L.scriptAddress jobOfferValidator

jobOfferValidator :: L.ValidatorScript
jobOfferValidator = L.ValidatorScript (L.fromCompiledCode $$(P.compile [||
  \(_ :: JobOffer) (joa :: JobOfferActions) (_ :: L.PendingTx) ->
    case joa of
      CloseOffer -> ()
      _          -> ()
  ||]))

P.makeLift ''JobOffer
P.makeLift ''JobOfferActions
