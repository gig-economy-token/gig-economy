{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GuessingGame.Player2 where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Data.Map as Map
import Cardano.Helpers
import Cardano.Emulator.GuessingGame

renderLayout :: Html -> Html -> Handler Html
renderLayout action content = do
    emulatorState <- CardanoHtml.readEmulatorState
    defaultLayout $ do
        setTitle "Player 2 status"
        let
            fundsInWallet2 = fromMaybe 0 $ getResultingFunds <$> Map.lookup player2Wallet (Emulator._walletStates emulatorState)
            status = [shamlet|
<ul>
  <li>Funds in wallet: #{fundsInWallet2}
|]

        $(widgetFile "guessing-game/player2")


getPlayer2R :: Handler Html
getPlayer2R = renderLayout "Status" "Player 2 locks some funds"

postPlayer2LockR :: Handler Html
postPlayer2LockR = undefined
