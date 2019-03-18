{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee where

import Import

import Wallet.Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.Job
import Handler.Job.Employee.View
import Handler.Job.Forms
import Cardano.JobContract

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = runOnBlockchain employeeWallet op

getEmployeeR :: Handler Html
getEmployeeR = renderLayout

postEmployeeAcceptOfferR :: Handler Html
postEmployeeAcceptOfferR = do
      ((result, _), _) <- runFormPost (hiddenJobOfferForm Nothing)
      case result of
        FormSuccess jobOffer -> do
          doOnBlockchain (applyToOffer jobOffer)
          renderLayout
        FormMissing -> renderLayoutWithError "No form found on the request"
        FormFailure t -> renderLayoutWithError (intercalate "\n" t)

postEmployeeSubscribeR :: Handler Html
postEmployeeSubscribeR = do
      doOnBlockchain subscribeToJobBoard
      renderLayout

postEmployeeRejectEscrowR :: Handler Html
postEmployeeRejectEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormMissing -> renderLayoutWithError "No form found on the request"
        FormFailure t -> renderLayoutWithError (intercalate "\n" t)
        FormSuccess (job, application) -> do
                                          doOnBlockchain (escrowRejectEmployee job application)
                                          renderLayout
