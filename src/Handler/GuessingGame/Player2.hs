{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GuessingGame.Player2 where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import qualified Data.Map as Map
import Ledger
import Cardano.Helpers
import Cardano.Emulator.GuessingGame
import Cardano.GameContract

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
postPlayer2LockR = do
  secret' <- lookupPostParam "secret"
  funds' <- lookupPostParam "funds"
  let
    valid = do
            secret <- unpack <$> secret'
            funds <- do
                        f <- unpack <$> funds'
                        f' <- readMay f
                        f'' <- if f' < 1 then Nothing else pure f'
                        pure $ Value f''
            pure (secret, funds)
  case valid of
      Just (secret, funds) -> do
          CardanoHtml.appendStep $ ((Emulator.walletAction player2Wallet $ lock secret funds) >> pure ())
          renderLayout "Player 2 locked X funds" "Funds"
      Nothing ->
          renderLayout "Player 2 Failed to lock funds" ""
