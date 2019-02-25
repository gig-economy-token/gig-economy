{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.Job.Employer.View
  ( renderLayout
  ) where

import Import
import Cardano.JobContract
import Wallet.Emulator.AddressMap (AddressMap(..))
import Cardano.Emulator.Job
import Cardano.Html.Emulator

renderLayout :: (Widget, Enctype) -> Handler Html
renderLayout (postOfferForm, postOfferEnctype) = do
    acceptanceListing <- mkAcceptanceMap <$> readWatchedAddresses employerWallet
    defaultLayout $ do
        $(widgetFile "job/employer")

mkAcceptanceMap :: AddressMap -> [(JobOffer, [JobAcceptance])]
mkAcceptanceMap am = offersAcceptances
  where
    offers = fromMaybe [] $ extractJobOffers am
    offersAcceptances = (\o -> (o, fromMaybe [] $ extractJobAcceptances am o)) <$> offers
