{-# LANGUAGE NoImplicitPrelude  #-}
module Handler.GuessingGame.Player1
  ( getPlayer1R
  , postPlayer1StartGameR
  , postPlayer1GuessR
  ) where

import Import

import qualified Wallet.Emulator as Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.GuessingGame
import Cardano.GameContract
import Handler.GuessingGame.Player1.View

getPlayer1R :: Handler Html
getPlayer1R = renderLayout "Status" "Player 1 starts the game and tries to guess"

postPlayer1StartGameR :: Handler Html
postPlayer1StartGameR = do
          appendStep $ ((Emulator.walletAction player1Wallet $ startGame) >> pure ())
          renderLayout "- Start game" "Started new Game"

postPlayer1GuessR :: Handler Html
postPlayer1GuessR = do
      guess' <- lookupPostParam "guess"
      let
        valid = unpack <$> guess'
      case valid of
        Nothing -> renderLayout "- Guess" "Invalid form submitted"
        Just g -> do
            appendStepAndNotifyKnownWallets $ ((Emulator.walletAction player1Wallet $ guess g) >> pure ())
            renderLayout "- Guess" "Guessed"
