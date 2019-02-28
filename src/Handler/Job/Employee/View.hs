{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Employee.View where

import Import
import Wallet.Emulator
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Handler.Job.Forms

renderLayout :: Handler Html
renderLayout = do
    offers <- mkJobBoard employeeWallet
    defaultLayout $ do
        $(widgetFile "job/employee")

newtype JobBoard = JobBoard [(JobOffer, (Widget, Enctype))]

mkJobBoard :: Wallet -> Handler (Maybe JobBoard)
mkJobBoard w = do
                am <- readWatchedAddresses w
                let offers = extractJobOffers am
                case offers of
                  Just o -> do
                        offersWithForms <- forM o $ \offer -> do
                                (widget, enctype) <- generateFormPost (hiddenJobOfferForm (Just offer))
                                pure (offer, (widget, enctype))
                        pure $ Just $ JobBoard offersWithForms
                  Nothing -> pure Nothing
