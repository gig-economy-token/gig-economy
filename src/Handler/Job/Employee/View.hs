{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee.View where

import Import
import Ledger.Types
import Wallet.Emulator.AddressMap
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Cardano.Html.Template.Instances ()
import Text.Blaze.Html (ToMarkup(..), Html)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B8

renderLayout :: Handler Html
renderLayout = do
    jobBoard <- JobBoard <$> readWatchedAddresses employeeWallet
    defaultLayout $ do
        $(widgetFile "job/employee")

newtype JobBoard = JobBoard AddressMap

instance ToMarkup JobBoard where
  toMarkup jb =
      case extractJobOffers jb of
          Nothing -> "You are not subscribed yet!"
          Just [] -> "No offers have been posted yet."
          Just tx -> renderBoard tx

renderBoard :: [JobOffer] -> Html
renderBoard offers = [shamlet|
<ul>
  $forall o <- offers
    <li>Offer: #{joPayout o} - #{B8.unpack $ joDescription o}
|]

extractJobOffers :: JobBoard -> Maybe [JobOffer]
extractJobOffers (JobBoard (AddressMap am)) = do
                                              addresses <- Map.lookup jobBoardAddress am
                                              pure $ catMaybes $ parseOffer <$> Map.toList addresses
  where
    parseOffer :: (TxOutRef, TxOut) -> Maybe JobOffer
    parseOffer (_, tx) = do
                      ds <- extractDataScript (txOutType tx)
                      parseJobOffer ds
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing
