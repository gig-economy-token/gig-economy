{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Randomly-named module for importing Cardano/Plutus libraries
-- in order to force the system to compile them in

-- This file is mostly copied from
-- https://github.com/input-output-hk/plutus/blob/master/plutus-tutorial/tutorial/Tutorial/01-plutus-tx.md

module CardanoStuff where


-- Main Plutus Tx module
import Language.PlutusTx
-- Additional support for lifting
import Language.PlutusTx.Lift

-- Used for examples
import Language.PlutusCore
import Language.PlutusCore.Pretty
import Language.PlutusCore.Quote
import Language.PlutusCore.Evaluation.CkMachine

{- |
>>> pretty $ getPlc integerOne
(program 1.0.0
  (con 8 ! 1)
)
-}
integerOne :: CompiledCode Int
integerOne = $$( -- The splice inserts the `Q (CompiledCode Int)` into the program
    -- compile turns the `Q Int` into a `Q (CompiledCode Int)`
    compile
        -- The quote has type `Q Int`
        [||
          -- We don't like unbounded integers in Plutus Core, so we have to pin
          -- down this numeric literal to an `Int` not an `Integer`
          (1 :: Int)
        ||])
