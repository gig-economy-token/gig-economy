{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee where

import Import

import Wallet.Emulator
import Cardano.Html.Emulator
import Cardano.Emulator.Job
import Handler.Job.Employee.View
import Cardano.JobContract

doOnBlockchain :: HasSimulatedChain m => MockWallet () -> m ()
doOnBlockchain op = appendStepAndNotifyKnownWallets (walletAction employeeWallet op)

getEmployeeR :: Handler Html
getEmployeeR = do
          renderLayout

postEmployeeAcceptOfferR :: Handler Html
postEmployeeAcceptOfferR = do
          --doOnBlockchain (acceptOffer (JobAcceptance { jaAcceptor = "John Doe" }))
          renderLayout

postEmployeeSubscribeR :: Handler Html
postEmployeeSubscribeR = do
          doOnBlockchain subscribeToJobBoard
          renderLayout
