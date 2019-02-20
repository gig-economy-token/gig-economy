{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
module Cardano.JobContract
  ( postOffer
  , acceptOffer
  , subscribeToJobBoard
  , JobOffer(..)
  , JobAcceptance(..)
  ) where

import qualified Language.PlutusTx            as PlutusTx
--import qualified Language.PlutusTx.Prelude    as P
import           Ledger
import           Ledger.Validation
import           Wallet

import           Data.ByteString.Lazy (ByteString)
--import qualified Data.ByteString.Lazy.Char8   as C

-- Datatype for posting job offers
data JobOffer = JobOffer
  { joDescription :: ByteString
  , joPayout      :: Int
  }
PlutusTx.makeLift ''JobOffer

-- Datatype for accepting a job offer
data JobAcceptance = JobAcceptance
  { jaAcceptor    :: ByteString
  }
PlutusTx.makeLift ''JobAcceptance

jobValidator :: ValidatorScript
jobValidator = ValidatorScript ($$(Ledger.compileScript [||
    \(JobAcceptance {}) (JobOffer {}) (_ :: PendingTx) ->

    ()  -- FIXME: We don't validate anything!

    ||]))

jobBoardAddress :: Address
jobBoardAddress = Ledger.scriptAddress jobValidator

postOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> Value -> m ()
postOffer offer vl = do
    let ds = DataScript (Ledger.lifted (offer))
    payToScript_ defaultSlotRange jobBoardAddress vl ds

acceptOffer :: (WalletAPI m, WalletDiagnostics m) => JobAcceptance -> m ()
acceptOffer acceptance = do
    let redeemer = RedeemerScript (Ledger.lifted acceptance)
    collectFromScript defaultSlotRange jobValidator redeemer

subscribeToJobBoard :: WalletAPI m => m ()
subscribeToJobBoard = startWatching jobBoardAddress
