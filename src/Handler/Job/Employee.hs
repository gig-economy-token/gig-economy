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
doOnBlockchain op = appendStepAndNotifyKnownWallets (walletAction employeeWallet op)

getEmployeeR :: Handler Html
getEmployeeR = do
      renderLayout

postEmployeeAcceptOfferR :: Handler Html
postEmployeeAcceptOfferR = do
      ((result, _), _) <- runFormPost (hiddenJobOfferForm Nothing)
      case result of
        FormSuccess jobOffer -> do
          doOnBlockchain (acceptOffer jobOffer (JobAcceptance { jaAcceptor = "John Doe" }))
          renderLayout
        FormMissing -> renderLayout
        FormFailure _ -> renderLayout

postEmployeeSubscribeR :: Handler Html
postEmployeeSubscribeR = do
          doOnBlockchain subscribeToJobBoard
          renderLayout
