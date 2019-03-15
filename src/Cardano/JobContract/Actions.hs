{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
module Cardano.JobContract.Actions
  ( postOffer
  , closeOffer
  , applyToOffer
  , subscribeToJobBoard
  , subscribeToJobApplicationBoard
  , parseJobOffer
  , parseJobApplication
  , parseJobEscrow
  , extractJobOffers
  , extractJobApplications
  , subscribeToEscrow
  , createEscrow
  , escrowAcceptEmployer
  , escrowRejectEmployee
  , escrowAcceptArbiter
  , escrowRejectArbiter
  ) where

import Prelude hiding ((++))
import Control.Monad (guard)
import Control.Lens
import           Ledger hiding (inputs, out, getPubKey)
import           Ledger.Ada.TH as Ada
import           Ledger.Value as Value
import Data.Foldable (foldl')
import           Wallet hiding (addresses, getPubKey)
import Language.PlutusTx.Evaluation (evaluateCek)
import Language.PlutusCore.Evaluation.Result (EvaluationResult(..), EvaluationResultDef)
import Language.PlutusCore (Term(..), Constant(..))
import Cardano.ScriptMagic
import qualified Data.Map as Map
import Wallet.Emulator.AddressMap (AddressMap(..))
import Data.Maybe
import qualified Data.Set as Set
import           Data.ByteString.Lazy (ByteString)
import Cardano.JobContract.Types
import Cardano.JobContract.Contracts

postOffer :: (WalletAPI m, WalletDiagnostics m) => JobOfferForm -> m ()
postOffer jof = do
    pk <- pubKey <$> myKeyPair
    let offer = toJobOffer jof pk
    let ds = DataScript (Ledger.lifted offer)
    subscribeToJobApplicationBoard offer
    _ <- payToScript defaultSlotRange jobBoardAddress ($$(adaValueOf) 0) ds
    pure ()

closeOffer :: (WalletAPI m, WalletDiagnostics m) => JobOfferForm -> m ()
closeOffer jof = do
    AddressMap am <- watchedAddresses
    pk <- pubKey <$> myKeyPair
    let _ds = DataScript (Ledger.lifted (toJobOffer jof pk))

        mtxid = do
                  allJobs <- maybe (Left "No address") Right $ Map.lookup jobBoardAddress am
                  let p :: TxOut -> Bool
                      p TxOutOf { txOutType = PayToScript ds } = ds == _ds 
                      p _ = False
                  case Map.toList $ Map.filter p allJobs of
                      [] -> Left "closeOffer: No entries found to close"
                      [x] -> pure x
                      _ -> Left "closeOffer: multiple entries found"
        
    (txid, tx) <- case mtxid of
                      Left err -> error err
                      Right a -> pure a
    let inputs = Set.singleton $ TxInOf
                                  { txInRef=txid
                                  , txInType=ConsumeScriptAddress jobBoard unitRedeemer
                                  }
    out <- ownPubKeyTxOut ($$(adaValueOf) 0)
    _ <- createTxAndSubmit defaultSlotRange inputs [out]
    pure ()

applyToOffer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> m ()
applyToOffer offer = do
    pk <- pubKey <$> myKeyPair
    let application = JobApplication {
                        jaAcceptor = pk
                      }
    let ds = DataScript (Ledger.lifted application)
    subscribeToEscrow
    payToScript_ defaultSlotRange (jobAddress offer) ($$(adaValueOf) 0) ds

subscribeToJobBoard :: WalletAPI m => m ()
subscribeToJobBoard = startWatching jobBoardAddress

subscribeToJobApplicationBoard :: WalletAPI m => JobOffer -> m ()
subscribeToJobApplicationBoard offer = startWatching (jobAddress offer)

subscribeToEscrow :: WalletAPI m => m ()
subscribeToEscrow = startWatching jobEscrowAddress

applyEvalScript :: Script -> Script -> EvaluationResultDef
applyEvalScript s1 s2 = evaluateCek (scriptToUnderlyingScript (s1 `applyScript` s2))

getBS :: EvaluationResultDef -> Maybe ByteString
getBS (EvaluationSuccess (Constant _ (BuiltinBS _ _ x))) = Just x
getBS _ = Nothing

getInt :: EvaluationResultDef -> Maybe Int
getInt (EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (fromIntegral x)
getInt _ = Nothing

getPubKey :: EvaluationResultDef -> Maybe PubKey
getPubKey (EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (PubKey $ fromIntegral x)
getPubKey _ = Nothing

parseJobOffer :: DataScript -> Maybe JobOffer
parseJobOffer (DataScript ds) = JobOffer
    <$> (getBS $ $$(Ledger.compileScript [|| \(JobOffer {joDescription}) -> joDescription ||]) `applyEvalScript` ds)
    <*> (getInt $ $$(Ledger.compileScript [|| \(JobOffer {joPayout}) -> joPayout ||]) `applyEvalScript` ds)
    <*> (getPubKey $ $$(Ledger.compileScript [|| \(JobOffer {joOfferer = PubKey k}) -> k ||]) `applyEvalScript` ds)

parseJobApplication :: DataScript -> Maybe JobApplication
parseJobApplication (DataScript ds) = JobApplication
    <$> (getPubKey $ $$(Ledger.compileScript [|| \(JobApplication {jaAcceptor=PubKey k}) -> k ||]) `applyEvalScript` ds)

parseJobEscrow :: DataScript -> Maybe EscrowSetup
parseJobEscrow (DataScript ds') = EscrowSetup
    <$> parseJobOffer (DataScript ($$(Ledger.compileScript [|| \(EscrowSetup {esJobOffer=o}) -> o ||]) `applyScript` ds'))
    <*> parseJobApplication (DataScript ($$(Ledger.compileScript [|| \(EscrowSetup {esJobApplication=a}) -> a ||]) `applyScript` ds'))
    <*> (getPubKey $ $$(Ledger.compileScript [|| \(EscrowSetup {esArbiter=PubKey k}) -> k ||]) `applyEvalScript` ds')


extractJobOffers :: AddressMap -> Maybe [JobOffer]
extractJobOffers (AddressMap am) = do
                              addresses <- Map.lookup jobBoardAddress am
                              pure $ catMaybes $ (parseTx parseJobOffer) <$> Map.elems addresses


extractJobApplications :: AddressMap -> JobOffer -> Maybe [JobApplication]
extractJobApplications (AddressMap am) jobOffer = do
                              addresses <- Map.lookup (jobAddress jobOffer) am
                              pure $ catMaybes $ (parseTx parseJobApplication) <$> Map.elems addresses


extractAllJobEscrows :: AddressMap -> [EscrowSetup]
extractAllJobEscrows (AddressMap am) = escrows
  where
    escrows = catMaybes $ (parseTx parseJobEscrow) <$> addresses
    addresses :: [TxOut]
    addresses = concat $ Map.elems <$> (Map.elems am)


parseTx :: (DataScript -> Maybe a) -> TxOut -> Maybe a
parseTx f tx = do
                ds <- extractDataScript (txOutType tx)
                f ds
  where
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing

createEscrow :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> PubKey -> Value ->  m ()
createEscrow offer application arbiterPubKey cost = payToScript_ defaultSlotRange jobEscrowAddress cost ds
  where
    setup = EscrowSetup
                { esJobOffer = offer
                , esJobApplication = application
                , esArbiter = arbiterPubKey
                }
    ds = DataScript (Ledger.lifted setup)

escrowAcceptEmployer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowAcceptEmployer offer application = do
    kp <- myKeyPair
    let action = EscrowAcceptedByEmployer (signature kp)
        rs = RedeemerScript (Ledger.lifted action)
    collectMatchingFromScriptToPubKey defaultSlotRange jobEscrowContract rs (matchesEscrow offer application) (jaAcceptor application)

escrowRejectEmployee :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowRejectEmployee offer application = do
    kp <- myKeyPair
    let action = EscrowRejectedByEmployee (signature kp)
        rs = RedeemerScript (Ledger.lifted action)
    collectMatchingFromScriptToPubKey defaultSlotRange jobEscrowContract rs (matchesEscrow offer application) (joOfferer offer)

escrowAcceptArbiter :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowAcceptArbiter offer application = do
    kp <- myKeyPair
    let action = EscrowAcceptedByArbiter (signature kp)
        rs = RedeemerScript (Ledger.lifted action)
    collectMatchingFromScriptToPubKey defaultSlotRange jobEscrowContract rs (matchesEscrow offer application) (jaAcceptor application)

escrowRejectArbiter :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowRejectArbiter offer application = do
    kp <- myKeyPair
    let action = EscrowRejectedByArbiter (signature kp)
        rs = RedeemerScript (Ledger.lifted action)
    collectMatchingFromScriptToPubKey defaultSlotRange jobEscrowContract rs (matchesEscrow offer application) (joOfferer offer)

matchesEscrow :: JobOffer -> JobApplication -> (TxOutRef, TxOut) -> Bool
matchesEscrow offer application (_, tx) = isJust $ do
                                      es <- parseTx parseJobEscrow tx
                                      guard (esJobOffer es == offer)
                                      guard (esJobApplication es == application)

knownMatchingEscrowOutputs :: (WalletAPI m, WalletDiagnostics m) => EscrowSetup -> m [(TxOutRef, TxOut)]
knownMatchingEscrowOutputs setup = do
    (AddressMap am) <- watchedAddresses
    let
        outs :: [(TxOutRef, TxOut)]
        outs = concat $ Map.toList <$> Map.elems am

        isInterestingTx :: (TxOutRef, TxOut) -> Bool
        isInterestingTx (_, tx) = case parseTx parseJobEscrow tx of
                                        Just x -> x == setup 
                                        Nothing -> False
        outs' :: [(TxOutRef, TxOut)]
        outs' = filter isInterestingTx outs
        
    pure outs'


collectFromScriptToPubKey :: (Monad m, WalletAPI m) => SlotRange -> ValidatorScript -> RedeemerScript -> PubKey -> m ()
collectFromScriptToPubKey range scr red destination = do
    am <- watchedAddresses
    let addr = scriptAddress scr
        outputs' :: [(TxOutRef, TxOut)]
        outputs' = am ^. at addr . to (Map.toList . fromMaybe Map.empty)

        con :: (TxOutRef, TxOut) -> TxIn
        con (r, _) = scriptTxIn r scr red

        ins :: [TxIn]
        ins = con <$> outputs'

        value' = foldl' Value.plus Value.zero $ fmap (txOutValue . snd) outputs'

        oo = pubKeyTxOut value' destination
    _ <- createTxAndSubmit range (Set.fromList ins) [oo]
    pure ()

collectOutputsFromScriptToPubKey :: (Monad m, WalletAPI m) => SlotRange -> ValidatorScript -> RedeemerScript -> [(TxOutRef, TxOut)] -> PubKey -> m ()
collectOutputsFromScriptToPubKey range scr red outputs' destination = do
    let
        con :: (TxOutRef, TxOut) -> TxIn
        con (r, _) = scriptTxIn r scr red

        ins :: [TxIn]
        ins = con <$> outputs'

        value' = foldl' Value.plus Value.zero $ fmap (txOutValue . snd) outputs'

        oo = pubKeyTxOut value' destination
    _ <- createTxAndSubmit range (Set.fromList ins) [oo]
    pure ()

collectMatchingFromScriptToPubKey ::
  (Monad m, WalletAPI m) =>
  SlotRange ->                    -- Valid range
  ValidatorScript ->              -- Validator
  RedeemerScript ->               -- Redeemer to be used for all cases
  ((TxOutRef, TxOut) -> Bool) ->  -- Predicate to decide which outputs to be collected
  PubKey ->                       -- Destination
  m ()
collectMatchingFromScriptToPubKey range scr red p destination = do
    am <- watchedAddresses
    let
        addr = scriptAddress scr

        outputs' :: [(TxOutRef, TxOut)]
        outputs' = filter p $ am ^. at addr . to (Map.toList . fromMaybe Map.empty)

        con :: (TxOutRef, TxOut) -> TxIn
        con (r, _) = scriptTxIn r scr red

        ins :: [TxIn]
        ins = con <$> outputs'

        value' = foldl' Value.plus Value.zero $ fmap (txOutValue . snd) outputs'

        oo = pubKeyTxOut value' destination
    _ <- createTxAndSubmit range (Set.fromList ins) [oo]
    pure ()
