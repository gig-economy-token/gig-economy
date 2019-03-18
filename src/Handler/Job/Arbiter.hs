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
getArbiterR = renderLayout

postArbiterMonitorEscrowR :: Handler Html
postArbiterMonitorEscrowR = do
      doOnBlockchain subscribeToEscrow
      renderLayout

postArbiterAcceptEscrowR :: Handler Html
postArbiterAcceptEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormMissing -> renderLayoutWithError "No form found on the request"
        FormFailure t -> renderLayoutWithError (intercalate "\n" t)
        FormSuccess (offer, application) -> do
                                            doOnBlockchain (escrowAcceptArbiter offer application)
                                            renderLayout

postArbiterRejectEscrowR :: Handler Html
postArbiterRejectEscrowR = do
      ((result, _), _) <- runFormPost (hiddenJobEscrowForm Nothing)
      case result of
        FormMissing -> renderLayoutWithError "No form found on the request"
        FormFailure t -> renderLayoutWithError (intercalate "\n" t)
        FormSuccess (offer, application) -> do
                                            doOnBlockchain (escrowRejectArbiter offer application)
                                            renderLayout
