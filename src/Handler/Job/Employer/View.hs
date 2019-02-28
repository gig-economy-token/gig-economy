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

renderLayout :: (Widget, Enctype) -> Handler Html
renderLayout (postOfferForm, postOfferEnctype) = do
    acceptanceListing <- mkAcceptanceListing
    defaultLayout $ do
        $(widgetFile "job/employer")

data JobEntry = JobEntry
  { jeOffer :: JobOffer
  , jeForm :: (Widget, Enctype)
  , jeAcceptances :: [JobAcceptance]
  }

mkAcceptanceListing :: Handler [JobEntry]
mkAcceptanceListing = do
                        am <- readWatchedAddresses employerWallet
                        let offers = fromMaybe [] $ extractJobOffers am
                        forM offers $ \offer -> do
                              (widget, enctype) <- generateFormPost (hiddenJobOfferForm (Just offer))
                              pure JobEntry
                                    { jeOffer = offer
                                    , jeForm = (widget, enctype)
                                    , jeAcceptances = fromMaybe [] $ extractJobAcceptances am offer
                                    }
