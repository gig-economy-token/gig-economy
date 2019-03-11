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
  , parseJobOffer
  , parseJobApplication
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
import Control.Lens
import           Ledger hiding (inputs, out)
import           Ledger.Ada.TH as Ada
import           Ledger.Value as Value
import Data.Foldable (foldl')
import           Wallet hiding (addresses)
import Language.PlutusTx.Evaluation (evaluateCekTrace)
import Language.PlutusCore.Evaluation.Result (EvaluationResult(..))
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
    startWatching (jobAddress offer)
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
    payToScript_ defaultSlotRange (jobAddress offer) ($$(adaValueOf) 0) ds

subscribeToJobBoard :: WalletAPI m => m ()
subscribeToJobBoard = startWatching jobBoardAddress

parseJobOffer :: DataScript -> Maybe JobOffer
parseJobOffer ds = JobOffer <$> desc <*> payout <*> pk
  where
    desc = getBS $ evaluateCekTrace (scriptToUnderlyingScript (readDesc `applyScript` ds'))
    payout = getInt $ evaluateCekTrace (scriptToUnderlyingScript (readPayout `applyScript` ds'))
    pk = PubKey <$> (getInt $ evaluateCekTrace (scriptToUnderlyingScript (readPk `applyScript` ds')))

    getBS :: (a, EvaluationResult (Term b c d)) -> Maybe ByteString
    getBS (_, EvaluationSuccess (Constant _ (BuiltinBS _ _ x))) = Just x
    getBS _ = Nothing

    getInt :: (a, EvaluationResult (Term b c d)) -> Maybe Int
    getInt (_, EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (fromIntegral x)
    getInt _ = Nothing

    ds' :: Script
    ds' = getDataScript ds

    readDesc = $$(Ledger.compileScript [|| \(JobOffer {joDescription}) -> joDescription ||]) 
    readPayout = $$(Ledger.compileScript [|| \(JobOffer {joPayout}) -> joPayout ||]) 
    readPk = $$(Ledger.compileScript [|| \(JobOffer {joOfferer = PubKey k}) -> k ||]) 

parseJobApplication :: DataScript -> Maybe JobApplication
parseJobApplication ds = JobApplication <$> acceptor
  where
    acceptor = PubKey <$> (getInt $ evaluateCekTrace (scriptToUnderlyingScript (readAcceptor `applyScript` ds')))

    getInt :: (a, EvaluationResult (Term b c d)) -> Maybe Int
    getInt (_, EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just (fromIntegral x)
    getInt _ = Nothing

    ds' :: Script
    ds' = getDataScript ds

    readAcceptor = $$(Ledger.compileScript [|| \(JobApplication {jaAcceptor=PubKey k}) -> k ||]) 


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


extractJobApplications :: AddressMap -> JobOffer -> Maybe [JobApplication]
extractJobApplications (AddressMap am) jobOffer = do
                                              addresses <- Map.lookup (jobAddress jobOffer) am
                                              pure $ catMaybes $ parseAcc <$> Map.toList addresses
  where
    parseAcc :: (TxOutRef, TxOut) -> Maybe JobApplication
    parseAcc (_, tx) = do
                      ds <- extractDataScript (txOutType tx)
                      parseJobApplication ds
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing

subscribeToEscrow :: WalletAPI m => JobOffer -> JobApplication -> m ()
subscribeToEscrow jo ja = startWatching (jobEscrowAddress jo ja)

createEscrow :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> PubKey -> Value ->  m ()
createEscrow offer application arbiterPubKey cost = do
    pk <- pubKey <$> myKeyPair
    let setup = EscrowSetup
                { esEmployer = pk
                , esEmployee = jaAcceptor application
                , esArbiter = arbiterPubKey
                }
    let ds = DataScript (Ledger.lifted setup)
    payToScript_ defaultSlotRange (jobEscrowAddress offer application) cost ds

escrowAcceptEmployer :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowAcceptEmployer offer application = do
    kp <- myKeyPair
    let action = EscrowAcceptedByEmployer (signature kp)
    let rs = RedeemerScript (Ledger.lifted action)
    collectFromScriptToPubKey defaultSlotRange (jobEscrowContract offer application) rs (jaAcceptor application)

escrowRejectEmployee :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowRejectEmployee offer application = do
    kp <- myKeyPair
    let action = EscrowRejectedByEmployee (signature kp)
    let rs = RedeemerScript (Ledger.lifted action)
    collectFromScriptToPubKey defaultSlotRange (jobEscrowContract offer application) rs (joOfferer offer)

escrowAcceptArbiter :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowAcceptArbiter offer application = do
    kp <- myKeyPair
    let action = EscrowAcceptedByArbiter (signature kp)
    let rs = RedeemerScript (Ledger.lifted action)
    collectFromScriptToPubKey defaultSlotRange (jobEscrowContract offer application) rs (jaAcceptor application)

escrowRejectArbiter :: (WalletAPI m, WalletDiagnostics m) => JobOffer -> JobApplication -> m ()
escrowRejectArbiter offer application = do
    kp <- myKeyPair
    let action = EscrowRejectedByArbiter (signature kp)
    let rs = RedeemerScript (Ledger.lifted action)
    collectFromScriptToPubKey defaultSlotRange (jobEscrowContract offer application) rs (joOfferer offer)
collectFromScriptToPubKey :: (Monad m, WalletAPI m) => SlotRange -> ValidatorScript -> RedeemerScript -> PubKey -> m ()
collectFromScriptToPubKey range scr red destination = do
    am <- watchedAddresses
    let addr = scriptAddress scr
        outputs' = am ^. at addr . to (Map.toList . fromMaybe Map.empty)
        con (r, _) = scriptTxIn r scr red
        ins        = con <$> outputs'
        value' = foldl' Value.plus Value.zero $ fmap (txOutValue . snd) outputs'

        oo = pubKeyTxOut value' destination
    (const ()) <$> createTxAndSubmit range (Set.fromList ins) [oo]
