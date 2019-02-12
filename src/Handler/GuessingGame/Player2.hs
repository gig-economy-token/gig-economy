{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GuessingGame.Player2 where

import Import

import qualified Wallet.Emulator as Emulator
import qualified Cardano.Html.Emulator as CardanoHtml
import Ledger
import Cardano.Emulator.GuessingGame
import Cardano.GameContract
import Handler.GuessingGame.Player2.View

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
          CardanoHtml.appendStepAndNotifyKnownWallets $ ((Emulator.walletAction player2Wallet $ lock secret funds) >> pure ())
          renderLayout "Player 2 locked X funds" "Funds"
      Nothing ->
          renderLayout "Player 2 Failed to lock funds" ""
