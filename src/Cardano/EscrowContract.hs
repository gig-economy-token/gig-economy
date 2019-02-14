-- | A guessing game
-- copied from Language.PlutusTx.Coordination.Contracts.Game for experimenting

{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
module Cardano.EscrowContract
    ( watchSmartContract
    , depositADA
    , withdrawADA
    , ContractNo(..)
    ) where

import qualified Language.PlutusTx            as PlutusTx
import qualified Language.PlutusTx.Prelude    as P
import           Ledger
import           Wallet
import           Ledger.Validation
import           Playground.Contract
import           Prelude (Int, Maybe(..), otherwise, (==), ($))

newtype ContractNo = ContractNo Int
newtype LockingPin = LockingPin Int
newtype UnlockingPin = UnlockingPin Int

PlutusTx.makeLift ''LockingPin
PlutusTx.makeLift ''UnlockingPin
PlutusTx.makeLift ''ContractNo

myFirstValidator :: ContractNo -> ValidatorScript
myFirstValidator contractNo = ValidatorScript $ applyScript valScript (lifted contractNo)
    where
      valScript = fromCompiledCode $$(PlutusTx.compile
          [|| \(_ :: ContractNo) (LockingPin submittedPIN) (UnlockingPin myPIN) (_ :: PendingTx) ->

           let and = $$(P.and)
               error = $$(P.error)
               traceH = $$(P.traceH)
           in
           if | (and (submittedPIN == myPIN) (myPIN == 1234)) -> ()
              | (and (submittedPIN == myPIN) (myPIN == 1235)) -> ()--error (traceH "Success 1235" ())
              | (and (submittedPIN == 1234) (myPIN == 1234)) -> error (traceH "1234 - 1234" ())
              | (and (submittedPIN == 1234) (myPIN == 1235)) -> error (traceH "1234 - 1235" ())
              | (and (submittedPIN == 1235) (myPIN == 1234)) -> error (traceH "1235 - 1234" ())
              | (and (submittedPIN == 1235) (myPIN == 1235)) -> error (traceH "1235 - 1235" ())
              | otherwise -> error (traceH "unknown - unknown" ())
           ||])

interval' :: Interval Slot
interval' = Interval Nothing Nothing

smartContractAddress :: ContractNo -> Address
smartContractAddress cn = scriptAddress (myFirstValidator cn)

watchSmartContract :: ContractNo -> MockWallet ()
watchSmartContract cn = startWatching (smartContractAddress cn)

depositADA :: ContractNo -> Int -> Value -> MockWallet ()
depositADA cn pin val = payToScript_ interval' (smartContractAddress cn) val (DataScript (lifted (LockingPin pin)))

withdrawADA :: ContractNo -> Int -> MockWallet ()
withdrawADA cn pin = collectFromScript interval' (myFirstValidator cn) (RedeemerScript (lifted (UnlockingPin pin)))
