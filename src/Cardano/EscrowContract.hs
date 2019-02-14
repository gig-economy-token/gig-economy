-- | A guessing game
-- copied from Language.PlutusTx.Coordination.Contracts.Game for experimenting

{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
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

newtype ContractNo = ContractNo Int

PlutusTx.makeLift ''ContractNo

myFirstValidator :: ContractNo -> ValidatorScript
myFirstValidator contractNo = ValidatorScript $ applyScript valScript (lifted contractNo)
    where
      valScript = fromCompiledCode $$(PlutusTx.compile
          [|| \(_ :: ContractNo) (submittedPIN :: Int) (myPIN :: Int) (_ :: PendingTx) ->
           if submittedPIN == myPIN
           then ()
           else $$(P.error) ($$(P.traceH) "Please supply the correct PIN number to withdraw ada." ())
           ||])

interval' :: Interval Slot
interval' = Interval Nothing Nothing

smartContractAddress :: ContractNo -> Address
smartContractAddress cn = scriptAddress (myFirstValidator cn)

watchSmartContract :: ContractNo -> MockWallet ()
watchSmartContract cn = startWatching (smartContractAddress cn)

depositADA :: ContractNo -> Int -> Value -> MockWallet ()
depositADA cn pin val = payToScript_ interval' (smartContractAddress cn) val (DataScript (lifted pin))

withdrawADA :: ContractNo -> Int -> MockWallet ()
withdrawADA cn pin = collectFromScript interval' (myFirstValidator cn) (RedeemerScript (lifted pin))
