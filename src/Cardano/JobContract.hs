{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
module Cardano.JobContract
  ( postOffer
  , closeOffer
  , acceptOffer
  , subscribeToJobBoard
  , subscribeToJobAcceptanceBoard
  , JobOffer(..)
  , JobAcceptance(..)
  , jobBoardAddress
  , jobAddress
  , parseJobOffer
  , parseJobAcceptance
  , extractJobOffers
  , extractJobAcceptances
  ) where

import qualified Language.PlutusTx            as PlutusTx
import           Ledger
import           Ledger.Ada.TH as Ada
import qualified Ledger.Validation as Validation
import           Wallet hiding (addresses)
import Language.PlutusTx.Evaluation (evaluateCekTrace)
import Language.PlutusCore.Evaluation.Result (EvaluationResult, EvaluationResultF(..))
import Language.PlutusCore (Term(..), Constant(..))
import Cardano.ScriptMagic
import GHC.Generics
--import qualified Data.Set as Set
import qualified Data.Map as Map
import Wallet.Emulator.AddressMap (AddressMap(..))
import Data.Maybe

import           Data.ByteString.Lazy (ByteString)

-- Datatype for posting job offers
data JobOffer = JobOffer
  { joDescription :: ByteString
  , joPayout      :: Int
  }
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''JobOffer

-- Datatype for accepting a job offer
data JobAcceptance = JobAcceptance
  { jaAcceptor    :: ByteString
  }
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''JobAcceptance

-- Job board:
-- anyone can post a JobOffer here,
-- and only whoever posts the offer can close it.
-- () -> JobOffer {} -> PendingTx -> ()
jobBoard :: ValidatorScript
jobBoard = ValidatorScript ($$(Ledger.compileScript [||
  \() (JobOffer {}) (t :: Validation.PendingTx) ->
    let
        adaValueIn :: Value -> Int
        adaValueIn v = $$(Ada.toInt) ($$(Ada.fromValue) v)
    in

    let Validation.PendingTx {
          pendingTxInputs=[
            txin@Validation.PendingTxIn {
              pendingTxInValue=val
            }
          ],
          pendingTxOutputs=[
            Validation.PendingTxOut {
              pendingTxOutValue=val',
              pendingTxOutData=Validation.PubKeyTxOut pubkey
            }
          ]
        } = t
        valueIsSame = $$(PlutusTx.eq) (adaValueIn val) (adaValueIn val')
        inSignerIsSameAsOutSigner = $$(Validation.txInSignedBy) txin pubkey
    in
    if ($$(PlutusTx.and) valueIsSame inSignerIsSameAsOutSigner)
    then ()
    else $$(PlutusTx.error) ()
    
  ||]))

-- Job acceptance board
-- JobOffer -> (() -> JobAcceptance -> PendingTx -> ())
-- We `scriptApply` a JobOffer to obtain a single unique job address
-- Validation: It can only be closed by a transaction signed by the offerer
-- XXX: Shall we do the escrow here?? Maybe providing oracles.
jobAcceptanceBoard :: ValidatorScript
jobAcceptanceBoard = ValidatorScript ($$(Ledger.compileScript [||
  \(_ :: JobOffer) () (_ :: JobAcceptance) (_ :: Validation.PendingTx) ->
    ()  -- FIXME: We don't validate anything!
  ||]))

jobBoardAddress :: Address
jobBoardAddress = Ledger.scriptAddress jobBoard

jobAddress :: JobOffer -> Address
jobAddress jobOffer = Ledger.scriptAddress (ValidatorScript sc)
  where
    sc = (getValidator jobAcceptanceBoard) `applyScript` (Ledger.lifted jobOffer)

postOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> m ()
postOffer offer = do
    let ds = DataScript (Ledger.lifted (offer))
    payToScript_ defaultSlotRange jobBoardAddress ($$(adaValueOf) 0) ds

closeOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> m ()
closeOffer _offer = error "TODO"
--    let ds = DataScript (Ledger.lifted offer)
--        inputs = Set.fromList
--        out = _
--    _ <- createTxAndSubmit defaultSlotRange inputs [out]
--    pure ()

acceptOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobAcceptance -> m ()
acceptOffer offer acceptance = do
    let ds = DataScript (Ledger.lifted acceptance)
    payToScript_ defaultSlotRange (jobAddress offer) ($$(adaValueOf) 0) ds

subscribeToJobBoard :: WalletAPI m => m ()
subscribeToJobBoard = startWatching jobBoardAddress

subscribeToJobAcceptanceBoard :: WalletAPI m => JobOffer -> m ()
subscribeToJobAcceptanceBoard offer = startWatching (jobAddress offer)

parseJobOffer :: DataScript -> Maybe JobOffer
parseJobOffer ds = JobOffer <$> desc <*> payout
  where
    desc = getBS $ evaluateCekTrace (scriptToUnderlyingScript (readDesc `applyScript` ds'))
    payout = getInt $ evaluateCekTrace (scriptToUnderlyingScript (readPayout `applyScript` ds'))

    getBS :: (a, EvaluationResult) -> Maybe ByteString
    getBS (_, EvaluationSuccess (Constant _ (BuiltinBS _ _ x))) = Just x
    getBS _ = Nothing

    getInt :: (a, EvaluationResult) -> Maybe Int
    getInt (_, EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (fromIntegral x)
    getInt _ = Nothing

    ds' :: Script
    ds' = getDataScript ds

    readDesc = $$(Ledger.compileScript [|| \(JobOffer {joDescription}) -> joDescription ||]) 
    readPayout = $$(Ledger.compileScript [|| \(JobOffer {joPayout}) -> joPayout ||]) 

parseJobAcceptance :: DataScript -> Maybe JobAcceptance
parseJobAcceptance ds = JobAcceptance <$> acceptor
  where
    acceptor = getBS $ evaluateCekTrace (scriptToUnderlyingScript (readAcceptor `applyScript` ds'))

    getBS :: (a, EvaluationResult) -> Maybe ByteString
    getBS (_, EvaluationSuccess (Constant _ (BuiltinBS _ _ x))) = Just x
    getBS _ = Nothing

    ds' :: Script
    ds' = getDataScript ds

    readAcceptor = $$(Ledger.compileScript [|| \(JobAcceptance {jaAcceptor}) -> jaAcceptor ||]) 


extractJobOffers :: AddressMap -> Maybe [JobOffer]
extractJobOffers (AddressMap am) = do
                                              addresses <- Map.lookup jobBoardAddress am
                                              pure $ catMaybes $ parseOffer <$> Map.toList addresses
  where
    parseOffer :: (TxOutRef, TxOut) -> Maybe JobOffer
    parseOffer (_, tx) = do
                      ds <- extractDataScript (txOutType tx)
                      parseJobOffer ds
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing


extractJobAcceptances :: AddressMap -> JobOffer -> Maybe [JobAcceptance]
extractJobAcceptances (AddressMap am) jobOffer = do
                                              addresses <- Map.lookup (jobAddress jobOffer) am
                                              pure $ catMaybes $ parseAcc <$> Map.toList addresses
  where
    parseAcc :: (TxOutRef, TxOut) -> Maybe JobAcceptance
    parseAcc (_, tx) = do
                      ds <- extractDataScript (txOutType tx)
                      parseJobAcceptance ds
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing
