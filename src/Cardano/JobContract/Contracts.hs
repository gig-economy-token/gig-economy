{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
module Cardano.JobContract.Contracts
  ( jobBoard
  , jobBoardAddress
  , jobAcceptanceBoard
  , jobAddress
  ) where

import Prelude hiding ((++))
import qualified Language.PlutusTx            as PlutusTx
import           Ledger hiding (inputs, out)
import           Ledger.Ada.TH as Ada
import qualified Ledger.Validation as Validation
import Cardano.JobContract.Types

-- Job board:
-- anyone can post a JobOffer here,
-- and only whoever posts the offer can close it.
-- () -> JobOffer {} -> PendingTx -> ()
jobBoard :: ValidatorScript
jobBoard = ValidatorScript ($$(Ledger.compileScript [||
  \() (JobOffer {joOfferer}) (t :: Validation.PendingTx) ->
    let
        adaValueIn :: Value -> Int
        adaValueIn v = $$(Ada.toInt) ($$(Ada.fromValue) v)
    in

    let Validation.PendingTx {
          pendingTxInputs=[
            Validation.PendingTxIn {
              pendingTxInValue=val
            }
          ],
          pendingTxOutputs=[
            Validation.PendingTxOut {
              pendingTxOutValue=val',
              pendingTxOutData=Validation.PubKeyTxOut pubkey
            }
          ]
        } = t  -- It's fine if this fails matching,
               -- as it will cause the validator to error out and reject the transaction.

        valueIsSame = $$(PlutusTx.eq) (adaValueIn val) (adaValueIn val')

        inSignerIsSameAsOutSigner = $$(Validation.eqPubKey) pubkey joOfferer

        (++) :: [a] -> [a] -> [a]
        (++) = $$(PlutusTx.append)

        msgs :: [String]
        msgs = (if valueIsSame then [] else "Value is not same":[]) ++
               (if inSignerIsSameAsOutSigner then [] else "Different signer":[])

        validate :: [String] -> ()
        validate [] = ()
        validate xs = errorWith xs
          where
            errorWith :: [String] -> ()
            errorWith [] = $$(PlutusTx.error) ()
            errorWith (y:ys) = $$(PlutusTx.traceH) y (errorWith ys)

    in validate msgs
    
  ||]))

-- Job acceptance board
-- JobOffer -> (() -> JobAcceptance -> PendingTx -> ())
-- We `scriptApply` a JobOffer to obtain a single unique job address
-- Validation: It can only be closed by a transaction signed by the offerer
-- XXX: Shall we do the escrow here?? Maybe providing oracles.
jobAcceptanceBoard :: ValidatorScript
jobAcceptanceBoard = ValidatorScript ($$(Ledger.compileScript [||
  \(_ :: JobOffer) () (_ :: JobApplication) (_ :: Validation.PendingTx) ->
    ()  -- FIXME: We don't validate anything!
  ||]))

jobEscrow :: ValidatorScript
jobEscrow = ValidatorScript ($$(Ledger.compileScript [||
  \(_ :: JobOffer) (_ :: JobApplication) (_result :: EscrowResult) (_setup :: EscrowSetup) (_ :: Validation.PendingTx) ->
    ()  -- FIXME: We don't validate anything!
  ||]))

-- Addresses
jobBoardAddress :: Address
jobBoardAddress = Ledger.scriptAddress jobBoard

jobAddress :: JobOffer -> Address
jobAddress jobOffer = Ledger.scriptAddress (ValidatorScript sc)
  where
    sc = (getValidator jobAcceptanceBoard) `applyScript` (Ledger.lifted jobOffer)

jobEscrowAddress :: JobOffer -> JobApplication -> Address
jobEscrowAddress jobOffer jobApplication = Ledger.scriptAddress (ValidatorScript sc)
  where
    offerScript = Ledger.lifted jobOffer
    applicationScript = Ledger.lifted jobApplication
    escrowScript = getValidator jobEscrow
    sc = (escrowScript `applyScript` offerScript) `applyScript` applicationScript
