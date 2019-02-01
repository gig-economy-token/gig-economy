{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- To silence that we are importing lots of modules for no reason

-- Randomly-named module for importing Cardano/Plutus libraries
-- in order to force the system to compile them in

-- This file is mostly copied from
-- https://github.com/input-output-hk/plutus/blob/master/plutus-tutorial/tutorial/Tutorial/01-plutus-tx.md

module Cardano.BasicTests where


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

{- |
 - >>> pretty $ getPlc integerIdentity
 - (program 1.0.0
 -   (lam ds [(con integer) (con 8)] ds)
 -   )
 -   -}
integerIdentity :: CompiledCode (Int -> Int)
integerIdentity = $$(compile [|| \(x:: Int) -> x ||])

plusOne :: Int -> Int
plusOne x = x + 1

functions :: CompiledCode Int
functions = $$(compile [||
    let
        plusOneLocal :: Int -> Int
        plusOneLocal x = x + 1
        -- This won't work.
        -- nonLocalDoesntWork = plusOne 1
        localWorks = plusOneLocal 1
        -- You can of course bind this to a name, but for the purposes
        -- of this tutorial we won't since TH requires it to be in
        -- another module.
        thWorks = $$([|| \(x::Int) -> x + 1 ||]) 1
    in localWorks + thWorks
    ||])

{- |
>>> pretty $ getPir matchMaybe
(program
  (let
    (nonrec)
    (datatypebind
      (datatype
        (tyvardecl Maybe (fun (type) (type)))
        (tyvardecl a (type))
        Maybe_match
        (vardecl Just (fun a [Maybe a])) (vardecl Nothing [Maybe a])
      )
    )
    (lam
      ds
      [Maybe [(con integer) (con 8)]]
      [
        [
          {
            [ { Maybe_match [(con integer) (con 8)] } ds ]
            [(con integer) (con 8)]
          }
          (lam n [(con integer) (con 8)] n)
        ]
        (con 8 ! 0)
      ]
    )
  )
)
-}
matchMaybe :: CompiledCode (Maybe Int -> Int)
matchMaybe = $$(compile [|| \(x:: Maybe Int) -> case x of
    Just n -> n
    Nothing -> 0
   ||])

data EndDate = Fixed Int | Never

shouldEnd :: CompiledCode (EndDate -> Int -> Bool)
shouldEnd = $$(compile [|| \(end::EndDate) (current::Int) -> case end of
    Fixed n -> n <= current
    Never -> False
   ||])


{- |
>>> let program = addOneToN 4
>>> pretty program
(program 1.0.0
  [
    [
      (lam
        addInteger
        (fun [(con integer) (con 8)] (fun [(con integer) (con 8)] [(con integer) (con 8)]))
        (lam ds [(con integer) (con 8)] [ [ addInteger ds ] (con 8 ! 1) ])
      )
      { (builtin addInteger) (con 8) }
    ]
    (con 8 ! 4)
  ]
)
>>> pretty $ runCk program
(con 8 ! 5)
-}
addOneToN :: Int -> Program TyName Name ()
addOneToN n =
    let addOne = $$(compile [|| \(x:: Int) -> x + 1 ||])
    in (getPlc addOne) `applyProgram` unsafeLiftProgram n
