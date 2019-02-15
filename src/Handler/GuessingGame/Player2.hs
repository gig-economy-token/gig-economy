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
import Yesod.Form.Bootstrap3

getPlayer2R :: Handler Html
getPlayer2R = do
              form <- generateFormPost lockForm
              renderLayout' "Status" "Player 2 locks some funds" form

data LockAction = LockAction
  { laSecret :: Text
  , laFunds :: Int
  } deriving Show

lockForm :: Html -> MForm Handler (FormResult LockAction, Widget)
lockForm = renderDivs $ LockAction
      <$> areq secretField (bfs ("Secret" :: Text)) Nothing
      <*> areq fundsField (bfs ("Funds to lock" :: Text)) Nothing
  where
    secretField = checkBool (/= "") ("Please write a secret to lock the funds" :: Text) textField
    fundsField = checkBool (> 0) ("Please input the funds you want to lock" :: Text) intField

postPlayer2LockR :: Handler Html
postPlayer2LockR = do
  ((result, widget), enctype) <- runFormPost lockForm
  case result of
      FormSuccess LockAction {..} -> do
          CardanoHtml.appendStepAndNotifyKnownWallets $
              do
                _ <- Emulator.walletAction player2Wallet $ lock (unpack laSecret) (Value laFunds)
                pure ()
          form <- generateFormPost lockForm
          renderLayout' "Player 2 locked X funds" "Funds" form
      FormMissing -> getPlayer2R
      FormFailure _ -> renderLayout' "Player 2 error" "Funds" (widget, enctype)
