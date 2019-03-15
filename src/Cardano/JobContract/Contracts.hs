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
  , jobEscrowAddress
  , jobEscrowContract
  ) where

import Prelude hiding ((++), and)
import qualified Language.PlutusTx            as PlutusTx
import           Ledger hiding (inputs, out)
import qualified Ledger.Value.TH
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

jobEscrowContract :: ValidatorScript
jobEscrowContract = ValidatorScript ($$(Ledger.compileScript [||
  \ (result :: EscrowResult)
    (setup :: EscrowSetup)
    (tx :: Validation.PendingTx)
    ->
    let EscrowSetup {
          esJobOffer=JobOffer {
            joOfferer=employerPubKey
          },
          esJobApplication=JobApplication {
            jaAcceptor=employeePubKey
          },
          esArbiter=arbiterPubKey
        } = setup
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
        } = tx -- It's fine if this fails matching,
               -- as it will cause the validator to error out and reject the transaction.

    in
    let
      eqPubKey = $$(Validation.eqPubKey)
    in
    let
      --txSignedBy = $$(Validation.txSignedBy)

      signedBy' :: Signature -> PubKey -> Bool
      signedBy' (Signature sig) (PubKey pk) = sig == pk

      and = $$(PlutusTx.and)
      eqVal = $$(Ledger.Value.TH.eq)

    in case result of
      EscrowAcceptedByEmployer sig ->
        if (and (and (signedBy' sig employerPubKey) (eqPubKey pubkey employeePubKey)) (eqVal val val'))
        then ()
        else $$(PlutusTx.traceH) "Bad acceptance by employer" ($$(PlutusTx.error) ())
      EscrowRejectedByEmployee sig ->
        if (and (and (signedBy' sig employeePubKey) (eqPubKey pubkey employerPubKey)) (eqVal val val'))
        then ()
        else $$(PlutusTx.traceH) "Bad reject by employee" ($$(PlutusTx.error) ())
      EscrowAcceptedByArbiter sig ->
        if (and (and (signedBy' sig arbiterPubKey) (eqPubKey pubkey employeePubKey)) (eqVal val val'))
        then ()
        else $$(PlutusTx.traceH) "Bad acceptance by arbiter" ($$(PlutusTx.error) ())
      EscrowRejectedByArbiter sig ->
        if (and (and (signedBy' sig arbiterPubKey) (eqPubKey pubkey employerPubKey)) (eqVal val val'))
        then ()
        else $$(PlutusTx.traceH) "Bad reject by arbiter" ($$(PlutusTx.error) ())
  ||]))

-- Addresses
jobBoardAddress :: Address
jobBoardAddress = Ledger.scriptAddress jobBoard

jobAddress :: JobOffer -> Address
jobAddress jobOffer = Ledger.scriptAddress (ValidatorScript sc)
  where
    sc = (getValidator jobAcceptanceBoard) `applyScript` (Ledger.lifted jobOffer)

jobEscrowAddress :: Address
jobEscrowAddress = Ledger.scriptAddress jobEscrowContract
