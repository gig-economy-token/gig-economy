{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Arbiter where

import Import

import Wallet.Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.Job
import Handler.Job.Arbiter.View
import Handler.Job.Forms
import Cardano.JobContract

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = runOnBlockchain arbiterWallet op

getArbiterR :: Handler Html
getArbiterR = do
      renderLayout

postArbiterSubscribeToMainJobBoardR :: Handler Html
postArbiterSubscribeToMainJobBoardR = do
      doOnBlockchain subscribeToJobBoard
      renderLayout

postArbiterMonitorApplicantsR :: Handler Html
postArbiterMonitorApplicantsR = do
      ((result, _), _) <- runFormPost (hiddenJobOfferForm Nothing)
      case result of
        FormSuccess offer -> doOnBlockchain (subscribeToJobApplicationBoard offer)
        _ -> pure ()
      renderLayout

postArbiterMonitorEscrowR :: Handler Html
postArbiterMonitorEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormSuccess (_offer, _application) -> doOnBlockchain subscribeToEscrow
        _ -> pure ()
      renderLayout

postArbiterAcceptEscrowR :: Handler Html
postArbiterAcceptEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormSuccess (offer, application) -> doOnBlockchain (escrowAcceptArbiter offer application)
        _ -> pure ()
      renderLayout

postArbiterRejectEscrowR :: Handler Html
postArbiterRejectEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormSuccess (offer, application) -> doOnBlockchain (escrowRejectArbiter offer application)
        _ -> pure ()
      renderLayout
