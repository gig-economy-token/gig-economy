{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Cardano.Html.Template where

import qualified Data.Set as Set

import qualified Ledger
import qualified Wallet.Emulator as Emulator
import qualified Wallet.Emulator.AddressMap as AM
import qualified Wallet.API as WalletAPI
import qualified Data.Map as Map
import Import

-- TODO: Make this into a decent EventTrigger displayer
-- the trigger API sounds quite interesting, with support for:
-- * logic combinators (and, or, not, T, F)
-- * checking when there are enough funds
-- Maybe we can make it into a system that posts new contracts automatically or something
showTriggers :: Map.Map WalletAPI.EventTrigger (WalletAPI.EventHandler Emulator.MockWallet) -> Html
showTriggers t = [shamlet|
$if null triggers
  No triggers
$else
  <ul>
    $forall (_k, _v) <- triggers
      <li>TODO: pretty print triggers
|]
  where
    triggers = Map.toList t

showAddressMap :: AM.AddressMap -> Html
showAddressMap m = [shamlet|
<ul>
  $forall address <- Map.toList $ AM.getAddressMap m
    <li>#{show $ fst address}
      <ul>
        $forall map <- Map.toList $ snd address
          <li>#{show $ fst map}
          <li>#{show $ snd map}
|]

showWalletStates :: Map.Map Emulator.Wallet Emulator.WalletState -> Html
showWalletStates w = [shamlet|
$if null wallets
  No wallets
$else
  <ul>
    $forall (wallet, state) <- wallets
      <li>
        Wallet #{Emulator.getWallet wallet}
        <ul>
          <li>Own Keypair: #{show $ Emulator._ownKeyPair state}
          <li>Slot: #{show $ Emulator._walletSlot state}
          <li>Address map #{showAddressMap (Emulator._addressMap state)}
          <li>Triggers: #{showTriggers $ Emulator._triggers state}
|]
  where
    wallets = Map.toList w

showEmulatorEvent :: Emulator.EmulatorEvent -> Html
showEmulatorEvent (Emulator.TxnSubmit txID) = [shamlet|
TxnSubmit #{show txID}
|]
showEmulatorEvent (Emulator.TxnValidate txID) = [shamlet|
TxnValidate #{show txID}
|]
showEmulatorEvent (Emulator.TxnValidationFail txID e) = [shamlet|
TxnValidationError #{show txID} #{show e}
|]
showEmulatorEvent (Emulator.SlotAdd slot) = [shamlet|
SlotAdd #{show slot}
|]
showEmulatorEvent (Emulator.WalletError w e) = [shamlet|
WalletError #{show w} - #{show e}
|]
showEmulatorEvent (Emulator.WalletInfo w i) = [shamlet|
WalletInfo #{show w} - #{i}
|]

showEmulatorLog :: [Emulator.EmulatorEvent] -> Html
showEmulatorLog es = [shamlet|
$if null es
  No events
$else
  <ul>
    $forall e <- es
      <li> #{showEmulatorEvent e}
|]

showTx :: Ledger.Tx -> Html
showTx tx = [shamlet|
Tx
<ul>
  <li>txInputs:
    $if null $ Set.toList $ Ledger.txInputs tx
      No inputs
    $else
      <ul>
        $forall input <- Set.toList $ Ledger.txInputs tx
          <li>#{show input}

  <li>txOutputs:
    $if null $ Ledger.txOutputs tx
      No outputs
    $else
      <ul>
        $forall output <- Ledger.txOutputs tx
          <li> #{show $ Ledger.txOutType output}
            <ul>
              <li> #{show $ Ledger.txOutAddress output}
              <li> #{show $ Ledger.txOutValue output}

  <li>txForge: #{show $ Ledger.txForge tx}
  <li>txFee: #{show $ Ledger.txFee tx}
  <li>txValidRange: #{show $ Ledger.txValidRange tx}
|]

showChain :: Ledger.Blockchain -> Html
showChain bs = [shamlet|
$if null bs
  No blocks
$else
  <ol>
    $forall block <- reverse bs
      <li>
        $if null block
          Empty block
        $else
          <ul>
            $forall tx <- block
              <li> #{showTx tx}
|]

showUtxoIndex :: Ledger.UtxoIndex -> Html
showUtxoIndex i = [shamlet|
$if null entries
  No entries
$else
  <ul>
    $forall (k, v) <- entries
      <li>#{show k}
        <ul>
          <li>#{show v}
|]
  where
    entries = Map.toList $ Ledger.getIndex i

showTxPool :: [Ledger.Tx] -> Html
showTxPool pool = [shamlet|
$if null pool
  No entries
$else
  <ul>
    $forall tx <- pool
      <li>#{showTx tx}
|]

showEmulatorState :: Emulator.EmulatorState -> Html
showEmulatorState state = [shamlet|
<h2>EmulatorState
<ul>
  <li> _walletStates #{showWalletStates (Emulator._walletStates state)}
  <li> _emulatorLog #{showEmulatorLog (Emulator._emulatorLog state)}
  <li> _txPool
    <ul>
      <li>#{showTxPool $ Emulator._txPool state}
  <li> _index
    <ul>
      <li>#{showUtxoIndex $ Emulator._index state}
  <li>_chainNewestFirst - in actual order
    <p>#{showChain $ Emulator._chainNewestFirst state}
|]
