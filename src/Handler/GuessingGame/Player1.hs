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
import Yesod.Form.Bootstrap3

newtype GuessAction = GuessAction Text deriving Show

guessForm :: Html -> MForm Handler (FormResult GuessAction, Widget)
guessForm = renderDivs $ GuessAction
              <$> areq guessField (bfs ("Guess" :: Text)) Nothing
  where
    guessField = checkBool (/= "") ("Please write a guess" :: Text) textField

getPlayer1R :: Handler Html
getPlayer1R = do
              form <- generateFormPost guessForm
              renderLayout "Status" "Player 1 starts the game and tries to guess" form

postPlayer1StartGameR :: Handler Html
postPlayer1StartGameR = do
          form <- generateFormPost guessForm
          appendStep $ ((Emulator.walletAction player1Wallet $ startGame) >> pure ())
          renderLayout "- Start game" "Started new Game" form

postPlayer1GuessR :: Handler Html
postPlayer1GuessR = do
      ((result, widget), enctype) <- runFormPost guessForm
      case result of
        FormSuccess (GuessAction g) -> do
            appendStepAndNotifyKnownWallets (do
                            _ <- Emulator.walletAction player1Wallet $ guess (unpack g)
                            pure ())
            renderLayout "- Guess" "Guessed" (widget, enctype)
        FormMissing -> getPlayer1R
        FormFailure _ -> renderLayout "- Guess" "Invalid form submitted" (widget, enctype)
