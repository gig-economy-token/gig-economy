{-# LANGUAGE NoImplicitPrelude    #-}

module Cardano.ScriptMagic where

import Import ((.))
import qualified Language.PlutusCore as PLC
import Ledger.Types
import Unsafe.Coerce (unsafeCoerce)

type UnderlyingScript = PLC.Program PLC.TyName PLC.Name ()

dataScriptToUnderlyingScript :: DataScript -> UnderlyingScript
dataScriptToUnderlyingScript = scriptToUnderlyingScript . getDataScript

redeemerScriptToUnderlyingScript :: RedeemerScript -> UnderlyingScript
redeemerScriptToUnderlyingScript = scriptToUnderlyingScript . getRedeemer

validatorScriptToUnderlyingScript :: ValidatorScript -> UnderlyingScript
validatorScriptToUnderlyingScript = scriptToUnderlyingScript . getValidator

-- I do this evil here so it's contained
-- Script is a newtype over UnderlyingScript
scriptToUnderlyingScript :: Script -> UnderlyingScript
scriptToUnderlyingScript = unsafeCoerce
