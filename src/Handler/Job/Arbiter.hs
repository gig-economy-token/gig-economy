{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Arbiter where

import Import

import Wallet.Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.Job
import Handler.Job.Arbiter.View
-- import Handler.Job.Forms
-- import Cardano.JobContract

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = appendStepAndNotifyKnownWallets (walletAction arbiterWallet op)

getArbiterR :: Handler Html
getArbiterR = do
      _ <- error "Implement"
      renderLayout

postArbiterAcceptEscrowR :: Handler Html
postArbiterAcceptEscrowR = do
      _ <- error "Implement"
      renderLayout

postArbiterRejectEscrowR :: Handler Html
postArbiterRejectEscrowR = do
      _ <- error "Implement"
      renderLayout
