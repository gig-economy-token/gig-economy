{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.GuessingGame.Player1.View
  ( renderLayout
  ) where

import Import

import Cardano.Helpers
import qualified Wallet.Emulator as Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.GuessingGame
import qualified Data.Map as Map
import Handler.GuessingGame.View

renderLayout :: Html -> Html -> (Widget, Enctype) ->Handler Html
renderLayout action content (guessForm, guessFormEnctype) = do
    emulatorState <- readEmulatorState
    let
        status = PlayerStatus
            { funds = fromMaybe 0 $ getResultingFunds <$> Map.lookup player1Wallet (Emulator._walletStates emulatorState)
            }
    defaultLayout $ do
        setTitle "Player 1"
        $(widgetFile "guessing-game/player1")
