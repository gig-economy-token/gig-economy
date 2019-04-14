{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee.View
  ( renderLayout
  , renderLayoutWithError
  ) where

import Import
import Wallet.Emulator
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Emulator.State.Views
import Cardano.Html.Emulator
import Handler.Job.Forms

renderLayout :: Handler Html
renderLayout = renderLayout' Nothing

renderLayoutWithError :: Text -> Handler Html
renderLayoutWithError e = renderLayout' (Just e)

renderLayout' :: Maybe Text -> Handler Html
renderLayout' errMsg = do
    offers <- mkJobBoard employeeWallet
    mwalletState <- walletStateByWallet employeeWallet
    case mwalletState of
      Nothing          -> notFound
      Just walletState -> defaultLayout $(widgetFile "job/employee")

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
