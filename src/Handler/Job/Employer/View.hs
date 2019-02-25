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
import Text.Blaze.Html (ToMarkup(..), Html)
import qualified Data.Map as Map

renderLayout :: (Widget, Enctype) -> Handler Html
renderLayout (postOfferForm, postOfferEnctype) = do
    acceptanceListing <- mkAcceptanceMap <$> readWatchedAddresses employerWallet
    defaultLayout $ do
        $(widgetFile "job/employer")

newtype Acceptances = Acceptances (Map JobOffer [JobAcceptance])

mkAcceptanceMap :: AddressMap -> Acceptances
mkAcceptanceMap am = Acceptances $ Map.fromList offersAcceptances
  where
    offers = fromMaybe [] $ extractJobOffers am
    offersAcceptances = (\o -> (o, fromMaybe [] $ extractJobAcceptances am o)) <$> offers

instance ToMarkup Acceptances where
  toMarkup (Acceptances m) = [shamlet|
<ul>
  $forall pair <- Map.toList m
    <li>#{renderPair pair}
|]
    where
      renderPair :: (JobOffer, [JobAcceptance]) -> Html
      renderPair (jo, accs) = [shamlet|
#{show jo}
<ul>
  $forall acc <- accs
    <li>#{show acc}
|]
