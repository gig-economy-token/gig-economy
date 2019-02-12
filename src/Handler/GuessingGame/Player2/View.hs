{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GuessingGame.Player2.View where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Data.Map as Map
import Cardano.Helpers
import Cardano.Emulator.GuessingGame
import Handler.GuessingGame.View

renderLayout :: Html -> Html -> Handler Html
renderLayout action content = do
    emulatorState <- CardanoHtml.readEmulatorState
    defaultLayout $ do
        setTitle "Player 2 status"
        let
            status = PlayerStatus
              { funds = fromMaybe 0 $ getResultingFunds <$> Map.lookup player2Wallet (Emulator._walletStates emulatorState)
              }

        $(widgetFile "guessing-game/player2")
