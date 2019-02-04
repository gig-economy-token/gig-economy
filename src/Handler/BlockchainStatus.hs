{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.BlockchainStatus where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Cardano.Html.Template as CardanoHtml

getBlockchainStatusR :: Handler Html
getBlockchainStatusR = do
    emulatorState <- CardanoHtml.readEmulatorState
    CardanoHtml.simulateStep (Emulator.processPending)  -- FIXME: remove
    defaultLayout $ do
        setTitle "Emulated blockchain status"
        let 
            contentTitle = "Current emulator state" :: String
            content = CardanoHtml.showEmulatorState emulatorState

        $(widgetFile "blockchainStatus")
