{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Employer.Contracts where

import qualified Language.PlutusTx as P
import qualified Ledger as L
import qualified Ledger.Value as L
import qualified Wallet.API as W

import Cardano.JobContract.Types

postJobOffer :: (Monad m, W.WalletAPI m) => JobOffer -> m ()
postJobOffer JobOffer{..} = do
  let value = L.singleton (L.currencySymbol 1) joPayout
  W.payToScript_ W.defaultSlotRange employerAddress value undefined

employerAddress :: L.Address
employerAddress = L.scriptAddress employerValidator

employerValidator :: L.ValidatorScript
employerValidator = L.ValidatorScript (L.fromCompiledCode $$(P.compile [||
    ()
  ||]))
