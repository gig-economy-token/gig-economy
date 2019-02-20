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

getEmployeeR :: Handler Html
getEmployeeR = do
          renderLayout

postEmployeeAcceptOfferR :: Handler Html
postEmployeeAcceptOfferR = do
          renderLayout

postEmployeeSubscribeR :: Handler Html
postEmployeeSubscribeR = do
          putStrLn "ASDFADSFDSAFSDAFASDF"
          appendStep $ (walletAction employeeWallet subscribeToJobBoard)
          renderLayout
