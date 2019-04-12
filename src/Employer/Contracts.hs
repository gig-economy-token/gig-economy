{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Employer.Contracts
  ( JobOffer(..)
  , subscribeToEmployer
  , openJobOffer
  , closeJobOffer
  , applyJobOffer
  ) where

import qualified Language.PlutusTx as P
import qualified Ledger as L
import qualified Ledger.Validation as L
import qualified Wallet.API as W

import Data.ByteString.Lazy

data JobOffer = JobOffer
  { jobOfferTitle       :: ByteString
  , jobOfferDescription :: ByteString
  , jobOfferPayout      :: L.Value
  } deriving (Eq, Show)

data JobOfferActions
  = ApplyOffer
  | CloseOffer
  deriving (Eq, Show)


-- Actions

subscribeToEmployer :: W.MonadWallet m => m ()
subscribeToEmployer = W.startWatching jobOfferAddress

openJobOffer :: W.MonadWallet m => JobOffer -> m ()
openJobOffer jobOffer@JobOffer{..} = do
  pk <- W.ownPubKey
  let script = L.DataScript $ L.lifted (jobOffer, pk)
  W.payToScript_ W.defaultSlotRange jobOfferAddress jobOfferPayout script

closeJobOffer :: W.MonadWallet m => L.TxId -> m ()
closeJobOffer txId =
  let script = L.RedeemerScript $ L.lifted CloseOffer
  in W.collectFromScriptTxn W.defaultSlotRange jobOfferValidator script txId

applyJobOffer :: W.MonadWallet m => L.TxId -> m ()
applyJobOffer txId =
  let script = L.RedeemerScript $ L.lifted ApplyOffer
  in W.collectFromScriptTxn W.defaultSlotRange jobOfferValidator script txId

-- Helpers

jobOfferAddress :: L.Address
jobOfferAddress = L.scriptAddress jobOfferValidator

jobOfferValidator :: L.ValidatorScript
jobOfferValidator = L.ValidatorScript (L.fromCompiledCode $$(P.compile [||
  \((_, joOwner) :: (JobOffer, W.PubKey)) (joa :: JobOfferActions) (p :: L.PendingTx) ->
    let signedBy :: L.PendingTx -> W.PubKey -> Bool
        signedBy = $$(L.txSignedBy)

    in case joa of
      CloseOffer ->
        if p `signedBy` joOwner
          then ()
          else $$(P.error) ($$(P.traceH) "You are not the job offer owner" ())
      _ -> ()
  ||]))

P.makeLift ''JobOffer
P.makeLift ''JobOfferActions
