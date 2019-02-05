{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.GuessingGame.Player1 where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Cardano.Html.Template as CardanoHtml

getPlayer1R :: Handler Html
getPlayer1R = do
    emulatorState <- CardanoHtml.readEmulatorState
    defaultLayout $ do
        setTitle "Player 1 status"
        let
            contentTitle = "FIXME" :: String
            content = "FIXME" :: String

        $(widgetFile "guessing-game/player1")

postPlayer1DepositFundsR :: Handler Html
postPlayer1DepositFundsR = undefined

postPlayer1StartGameR :: Handler Html
postPlayer1StartGameR = undefined

postPlayer1GuessR :: Handler Html
postPlayer1GuessR = undefined
