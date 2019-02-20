{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee where

import Import

import Handler.Job.Employee.View

getEmployeeR :: Handler Html
getEmployeeR = do
              renderLayout

postEmployeeAcceptOfferR :: Handler Html
postEmployeeAcceptOfferR = do
              renderLayout

postEmployeeSubscribeR :: Handler Html
postEmployeeSubscribeR = do
              renderLayout
