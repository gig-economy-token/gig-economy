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
import Cardano.Html.Template.Instances ()
import qualified Data.ByteString.Lazy.Char8 as B8

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

hiddenJobOfferForm :: Maybe JobOffer -> Html -> MForm Handler (FormResult JobOffer, Widget)
hiddenJobOfferForm jo = renderDivs $ JobOffer
              <$> areq hiddenField ("" { fsId = Just "a" }) (joDescription <$> jo)
              <*> areq hiddenField ("" { fsId = Just "b" }) (joPayout <$> jo)

instance PathPiece B8.ByteString where
  fromPathPiece t = Just (B8.pack $ unpack t)
  toPathPiece b = pack $ B8.unpack b
