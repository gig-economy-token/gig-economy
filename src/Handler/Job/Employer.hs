{-# LANGUAGE NoImplicitPrelude  #-}
module Handler.Job.Employer
  ( getEmployerR
  , postEmployerPostOfferR
  , postEmployerCloseOfferR
  ) where

import Import

import Handler.Job.Employer.View
import Yesod.Form.Bootstrap3
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Wallet.Emulator
import qualified Data.ByteString.Lazy.Char8 as B8

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = appendStepAndNotifyKnownWallets (walletAction employerWallet op)

jobOfferForm :: Html -> MForm Handler (FormResult JobOfferForm, Widget)
jobOfferForm = renderDivs $ JobOfferForm
              <$> areq descField (bfs ("Job description" :: Text)) Nothing
              <*> areq payoutField (bfs ("Job payout" :: Text)) Nothing
  where
    descField = convertField (B8.pack . unpack) (pack . B8.unpack) $
                checkBool (/= "") ("Please enter a description" :: Text) $
                textField
    payoutField = checkBool (> 0) ("Please enter a positive payout" :: Text) $
                  intField

getEmployerR :: Handler Html
getEmployerR = do
  jobform <- generateFormPost jobOfferForm
  renderLayout jobform

postEmployerPostOfferR :: Handler Html
postEmployerPostOfferR = do
  ((result, widget), enctype) <- runFormPost jobOfferForm
  case result of
    FormSuccess job -> do
        doOnBlockchain (postOffer job)
        renderLayout (widget, enctype)
    FormMissing -> renderLayout (widget, enctype)
    FormFailure _ -> renderLayout (widget, enctype)

postEmployerCloseOfferR :: Handler Html
postEmployerCloseOfferR = do
  ((result, widget), enctype) <- runFormPost jobOfferForm
  case result of
    FormSuccess job -> do
        doOnBlockchain (closeOffer job)
        renderLayout (widget, enctype)
    FormMissing -> renderLayout (widget, enctype)
    FormFailure _ -> renderLayout (widget, enctype)
