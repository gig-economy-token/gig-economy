{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.GuessingGame.Player2 where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Cardano.Html.Template as CardanoHtml

getPlayer2R :: Handler Html
getPlayer2R = do
    emulatorState <- CardanoHtml.readEmulatorState
    defaultLayout $ do
        setTitle "Player 2 status"
        let
            contentTitle = "FIXME" :: String
            content = "FIXME" :: String

        $(widgetFile "guessing-game/player2")

postPlayer2DepositFundsR :: Handler Html
postPlayer2DepositFundsR = undefined

postPlayer2LockR :: Handler Html
postPlayer2LockR = undefined
