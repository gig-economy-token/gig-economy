{-# LANGUAGE NoImplicitPrelude  #-}
module Handler.Job.Employer
  ( getEmployerR
  , postEmployerPostOfferR
  ) where

import Import

import Handler.Job.Employer.View

getEmployerR :: Handler Html
getEmployerR = do
              renderLayout

postEmployerPostOfferR :: Handler Html
postEmployerPostOfferR = do
              renderLayout
