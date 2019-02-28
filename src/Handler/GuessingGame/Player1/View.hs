{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.GuessingGame.Player1.View
  ( renderLayout
  ) where

import Import

import Cardano.Emulator.GuessingGame
import Handler.GuessingGame.View

renderLayout :: Html -> Html -> (Widget, Enctype) -> Handler Html
renderLayout action content (guessForm, guessFormEnctype) = do
    status <- getPlayerStatus player1Wallet
    defaultLayout $ do
        setTitle "Player 1"
        $(widgetFile "guessing-game/player1")
