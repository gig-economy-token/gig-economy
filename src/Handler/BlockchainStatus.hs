{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.BlockchainStatus where

import Import

import Cardano.Emulator.State.Views
import qualified Cardano.Html.Emulator as CardanoHtml

getBlockchainStatusR :: Handler Html
getBlockchainStatusR = do
    emulatorState <- CardanoHtml.readEmulatorState
    defaultLayout $ statusW emulatorState

postBlockchainStatusR :: Handler Html
postBlockchainStatusR = do
    CardanoHtml.appendStepAndNotifyKnownWallets (pure ())
    getBlockchainStatusR
