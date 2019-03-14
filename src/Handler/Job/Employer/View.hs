{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.Job.Employer.View
  ( renderLayout
  ) where

import Import
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Handler.Job.Forms
import Wallet.Emulator.AddressMap

renderLayout :: (Widget, Enctype) -> Handler Html
renderLayout (postOfferForm, postOfferEnctype) = do
    acceptanceListing <- mkAcceptanceListing
    funds <- fundsInWallet employerWallet
    defaultLayout $ do
        $(widgetFile "job/employer")

data JobEntry = JobEntry
  { jeOffer :: JobOffer
  , jeForm :: (Widget, Enctype)
  , jeApplications :: [(JobApplication, Widget, Enctype)]
  }

mkAcceptanceListing :: Handler [JobEntry]
mkAcceptanceListing =
  do
    am <- readWatchedAddresses employerWallet
    let offers = fromMaybe [] $ extractJobOffers am
    mapM (mkJobEntry am) offers
  where
    mkJobEntry :: AddressMap -> JobOffer -> Handler JobEntry
    mkJobEntry am offer = do
          (widget, enctype) <- generateFormPost (hiddenJobOfferForm (Just offer))
          let applications = fromMaybe [] $ extractJobApplications am offer
          applicationsWithForms <- mapM (mkEscrowWithForm offer) applications
          pure JobEntry
                { jeOffer = offer
                , jeForm = (widget, enctype)
                , jeApplications = applicationsWithForms
                }
    mkEscrowWithForm :: JobOffer -> JobApplication -> Handler (JobApplication, Widget, Enctype)
    mkEscrowWithForm o a = do
                   (widget, enctype) <- generateFormPost (hiddenJobEscrowForm (Just (o, a)))
                   pure (a, widget, enctype)
