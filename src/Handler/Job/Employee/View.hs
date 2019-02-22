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
  toMarkup (JobBoard (AddressMap am)) =
      case Map.lookup jobBoardAddress am of
          Nothing -> "You are not subscribed yet!"
          Just x | x == Map.empty -> "No offers have been posted yet."
          Just tx -> renderBoard tx

renderBoard :: Map.Map TxOutRef TxOut -> Html
renderBoard tx' = [shamlet|
<ul>
  $forall o <- offers
    <li>Offer: #{joPayout o} - #{B8.unpack $ joDescription o}
|]
  where
    offers :: [JobOffer]
    offers = catMaybes $ parseOffer <$> Map.toList tx'
    extractDataScript :: TxOutType -> Maybe DataScript
    extractDataScript (PayToScript s) = Just s
    extractDataScript _               = Nothing
    parseOffer :: (TxOutRef, TxOut) -> Maybe JobOffer
    parseOffer (_, tx) = do
                      ds <- extractDataScript (txOutType tx)
                      parseJobOffer ds
