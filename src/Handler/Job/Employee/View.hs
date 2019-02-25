{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee.View where

import Import
import Wallet.Emulator.AddressMap
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Cardano.Html.Template.Instances ()
import Text.Blaze.Html (ToMarkup(..), Html)
import qualified Data.ByteString.Lazy.Char8 as B8

renderLayout :: Handler Html
renderLayout = do
    jobBoard <- JobBoard <$> readWatchedAddresses employeeWallet
    defaultLayout $ do
        $(widgetFile "job/employee")

newtype JobBoard = JobBoard AddressMap

instance ToMarkup JobBoard where
  toMarkup (JobBoard am) =
      case extractJobOffers am of
          Nothing -> "You are not subscribed yet!"
          Just [] -> "No offers have been posted yet."
          Just tx -> renderBoard tx

renderBoard :: [JobOffer] -> Html
renderBoard offers = [shamlet|
<ul>
  $forall o <- offers
    <li>Offer: #{joPayout o} - #{B8.unpack $ joDescription o}
|]
