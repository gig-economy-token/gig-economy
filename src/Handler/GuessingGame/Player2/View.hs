{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GuessingGame.Player2.View where

import Import

import Cardano.Emulator.GuessingGame
import Handler.GuessingGame.View

renderLayout' :: Html -> Html -> (Widget, Enctype) -> Handler Html
renderLayout' action content (form, enctype) = do
    status <- getPlayerStatus player2Wallet
    defaultLayout $ do
        setTitle "Player 2 status"
        $(widgetFile "guessing-game/player2")
