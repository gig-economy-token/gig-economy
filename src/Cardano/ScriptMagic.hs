{-# LANGUAGE NoImplicitPrelude    #-}

module Cardano.ScriptMagic where

import qualified Language.PlutusCore as PLC
import Ledger.Types
import Unsafe.Coerce (unsafeCoerce)

type UnderlyingScript = PLC.Program PLC.TyName PLC.Name ()

-- I do this evil here so it's contained
-- Both Script, DataScript, RedeemerScript and ValidatorScript are newtypes over UnderlyingScript
scriptToUnderlyingScript :: Script -> UnderlyingScript
scriptToUnderlyingScript = unsafeCoerce

dataScriptToUnderlyingScript :: DataScript -> UnderlyingScript
dataScriptToUnderlyingScript = unsafeCoerce

redeemerScriptToUnderlyingScript :: RedeemerScript -> UnderlyingScript
redeemerScriptToUnderlyingScript = unsafeCoerce

validatorScriptToUnderlyingScript :: ValidatorScript -> UnderlyingScript
validatorScriptToUnderlyingScript = unsafeCoerce
