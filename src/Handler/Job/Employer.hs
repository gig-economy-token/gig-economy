{-# LANGUAGE NoImplicitPrelude  #-}
module Handler.Job.Employer where

import Import

import Handler.Job.Employer.View
import Handler.Job.Forms
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Wallet.Emulator
import Ledger.Ada
import qualified Wallet.API

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = runOnBlockchain employerWallet op

getEmployerR :: Handler Html
getEmployerR = renderLayout

postEmployerPostOfferR :: Handler Html
postEmployerPostOfferR = do
  ((result, widget), enctype) <- runFormPost jobOfferForm
  case result of
    FormSuccess job -> do
        doOnBlockchain (postOffer job)
        renderLayout
    FormMissing -> renderLayoutWithError (widget, enctype) "No form found in request"
    FormFailure t -> renderLayoutWithError (widget, enctype) (intercalate "\n" t)

postEmployerCloseOfferR :: Handler Html
postEmployerCloseOfferR = do
  ((result, widget), enctype) <- runFormPost jobOfferForm
  case result of
    FormSuccess job -> do
        doOnBlockchain (closeOffer job)
        renderLayout
    FormMissing -> renderLayoutWithError (widget, enctype) "No form found in request"
    FormFailure t -> renderLayoutWithError (widget, enctype) (intercalate "\n" t)

postEmployerStartEscrowR :: Handler Html
postEmployerStartEscrowR = do
  ((result, widget), enctype) <- runFormPost (hiddenJobEscrowForm Nothing)
  case result of
    FormSuccess (job, application) -> do
      doOnBlockchain (createEscrow job application (Wallet.API.PubKey (getWallet arbiterWallet)) (adaValueOf $ joPayout job))
      renderLayout
    FormMissing -> renderLayoutWithError (widget, enctype) "No form found in request"
    FormFailure t -> renderLayoutWithError (widget, enctype) (intercalate "\n" t)

postEmployerAcceptEscrowR :: Handler Html
postEmployerAcceptEscrowR = do
  ((result, widget), enctype) <- runFormPost (hiddenJobEscrowForm Nothing)
  case result of
    FormSuccess (job, application) -> do
      doOnBlockchain (createEscrow job application (Wallet.API.PubKey (getWallet arbiterWallet)) (adaValueOf $ joPayout job))
      renderLayout
    FormMissing -> renderLayoutWithError (widget, enctype) "No form found in request"
    FormFailure t -> renderLayoutWithError (widget, enctype) (intercalate "\n" t)
