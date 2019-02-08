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
    print $ Emulator.fundsDistribution emulatorState
    defaultLayout $ do
        setTitle "Emulated blockchain status"
        let
            contentTitle = "Current emulator state" :: String
            content = CardanoHtml.showEmulatorState emulatorState

        $(widgetFile "blockchainStatus")

postBlockchainStatusR :: Handler Html
postBlockchainStatusR = do
    _ <- CardanoHtml.simulateStep (Emulator.processPending >>= Emulator.walletsNotifyBlock [Emulator.Wallet 1])
    getBlockchainStatusR
