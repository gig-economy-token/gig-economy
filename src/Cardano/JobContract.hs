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
  , JobOffer(..)
  , JobOfferForm(..)
  , JobAcceptance(..)
  , jobBoardAddress
  , jobAddress
  , parseJobOffer
  , parseJobAcceptance
  , extractJobOffers
  , extractJobAcceptances
  , toJobOffer
  , toJobOfferForm
  ) where

import Prelude hiding ((++))
import qualified Language.PlutusTx            as PlutusTx
import           Ledger hiding (inputs, out)
import           Ledger.Ada.TH as Ada
import qualified Ledger.Validation as Validation
import           Wallet hiding (addresses)
import Language.PlutusTx.Evaluation (evaluateCekTrace)
import Language.PlutusCore.Evaluation.Result (EvaluationResult, EvaluationResultF(..))
import Language.PlutusCore (Term(..), Constant(..))
import Cardano.ScriptMagic
import GHC.Generics
import qualified Data.Map as Map
import Wallet.Emulator.AddressMap (AddressMap(..))
import Data.Maybe
import qualified Data.Set as Set
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
data JobAcceptance = JobAcceptance
  { jaAcceptor    :: PubKey
  }
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''JobAcceptance

data ConsList a = Cons a (ConsList a)
                | Nil
  deriving (Show, Eq, Generic)
PlutusTx.makeLift ''ConsList

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
        } = t
        valueIsSame = $$(PlutusTx.eq) (adaValueIn val) (adaValueIn val')
        inSignerIsSameAsOutSigner = $$(Validation.eqPubKey) pubkey joOfferer
    in
    let
        (++) :: ConsList a -> ConsList a -> ConsList a
        (++) Nil b = b
        (++) (Cons x xs) b = xs ++ (Cons x b)

        sing :: a -> ConsList a
        sing a = Cons a Nil

        msgs :: ConsList String
        msgs = (if valueIsSame then Nil else sing "Value is not same") ++
               (if inSignerIsSameAsOutSigner then Nil else sing "Different signer")

        validate :: ConsList String -> ()
        validate Nil = ()
        validate xs = errorWith xs

        errorWith :: ConsList String -> ()
        errorWith Nil = $$(PlutusTx.error) ()
        errorWith (Cons x xs) = $$(PlutusTx.traceH) x (errorWith xs)
    in validate msgs
    
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

postOffer :: (WalletAPI m, WalletDiagnostics m) => JobOfferForm -> m ()
postOffer jof = do
    pk <- pubKey <$> myKeyPair
    let offer = toJobOffer jof pk
    let ds = DataScript (Ledger.lifted offer)
    startWatching (jobAddress offer)
    _ <- payToScript defaultSlotRange jobBoardAddress ($$(adaValueOf) 0) ds
    pure ()

closeOffer :: (WalletAPI m, WalletDiagnostics m) => JobOfferForm -> m ()
closeOffer jof = do
    AddressMap _am <- watchedAddresses
    pk <- pubKey <$> myKeyPair
    let _ds = DataScript (Ledger.lifted (toJobOffer jof pk))
        mtxid = do
                  allJobs <- Map.lookup jobBoardAddress _am
                  let p :: TxOut -> Bool
                      p TxOutOf { txOutType = PayToScript ds } = ds == _ds 
                      p _ = False
                  case Map.toList $ Map.filter p allJobs of
                      [] -> Nothing
                      [x] -> pure x
                      _ -> error "closeOffer: multiple entries found"
        
    (txid, tx) <- case mtxid of
                      Nothing -> error "No entries found to close"
                      Just a -> pure a
    let inputs = Set.singleton $ TxInOf
                                  { txInRef=txid
                                  , txInType=ConsumeScriptAddress jobBoard unitRedeemer
                                  }
    out <- ownPubKeyTxOut ($$(adaValueOf) 0)
    _ <- createTxAndSubmit defaultSlotRange inputs [out]
    pure ()

acceptOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> m ()
acceptOffer offer = do
    pk <- pubKey <$> myKeyPair
    let acceptance = JobAcceptance {
                        jaAcceptor = pk
                      }
    let ds = DataScript (Ledger.lifted acceptance)
    payToScript_ defaultSlotRange (jobAddress offer) ($$(adaValueOf) 0) ds

subscribeToJobBoard :: WalletAPI m => m ()
subscribeToJobBoard = startWatching jobBoardAddress

parseJobOffer :: DataScript -> Maybe JobOffer
parseJobOffer ds = JobOffer <$> desc <*> payout <*> pk
  where
    desc = getBS $ evaluateCekTrace (scriptToUnderlyingScript (readDesc `applyScript` ds'))
    payout = getInt $ evaluateCekTrace (scriptToUnderlyingScript (readPayout `applyScript` ds'))
    pk = PubKey <$> (getInt $ evaluateCekTrace (scriptToUnderlyingScript (readPk `applyScript` ds')))

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
    readPk = $$(Ledger.compileScript [|| \(JobOffer {joOfferer = PubKey k}) -> k ||]) 

parseJobAcceptance :: DataScript -> Maybe JobAcceptance
parseJobAcceptance ds = JobAcceptance <$> acceptor
  where
    acceptor = PubKey <$> (getInt $ evaluateCekTrace (scriptToUnderlyingScript (readAcceptor `applyScript` ds')))

    getInt :: (a, EvaluationResult) -> Maybe Int
    getInt (_, EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (fromIntegral x)
    getInt _ = Nothing

    ds' :: Script
    ds' = getDataScript ds

    readAcceptor = $$(Ledger.compileScript [|| \(JobAcceptance {jaAcceptor=PubKey k}) -> k ||]) 


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
