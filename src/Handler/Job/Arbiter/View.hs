{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Arbiter.View where

import Import
import Wallet.Emulator
import Wallet.Emulator.AddressMap
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Handler.Job.Forms

data EscrowUI = EscrowUI JobOffer JobApplication (Widget, Enctype)
data JobWithApplications
  = JobWithApplications JobOffer [(JobApplication, Widget, Enctype)]
  | JobNotSubscribed JobOffer (Widget, Enctype)

data JobBoardActivity
  = NotSubscribed
  | Jobs [JobWithApplications]

renderLayout :: Handler Html
renderLayout = do
    escrows <- mkEscrows arbiterWallet
    activity <- mkJobBoard arbiterWallet
    funds <- fundsInWallet arbiterWallet
    defaultLayout $ do
        $(widgetFile "job/arbiter")

newtype JobBoard = JobBoard [(JobOffer, (Widget, Enctype))]


mkJobBoard :: Wallet -> Handler JobBoardActivity
mkJobBoard w = do
                am <- readWatchedAddresses w
                let offers = extractJobOffers am
                case offers of
                  Nothing -> pure NotSubscribed
                  Just ox -> Jobs <$> mapM (mkOfferApplicationForm am) ox
  where
    mkOfferApplicationForm :: AddressMap -> JobOffer -> Handler JobWithApplications
    mkOfferApplicationForm am offer = do
            case extractJobApplications am offer of
              Nothing -> do
                          we <- generateFormPost (hiddenJobOfferForm (Just offer))
                          pure $ JobNotSubscribed offer we
              Just apps -> do
                  applications <- forM apps (\app -> do
                                        (widget, enctype) <- generateFormPost (hiddenJobEscrowForm $ Just (offer, app))
                                        pure (app, widget, enctype))
                  pure $ JobWithApplications offer applications

mkEscrows :: Wallet -> Handler [EscrowUI]
mkEscrows wallet = do
  am <- readWatchedAddresses wallet
  let offers = fromMaybe [] $ extractJobOffers am
      applications = (\x -> (x, extractJobApplications am x)) <$> offers

  escrows <- forM applications $ \(o, max') ->
                  case max' of
                    Nothing -> pure []
                    Just ax -> do
                              forM ax (\a -> do
                                          
                                          (w, e) <- generateFormPost (hiddenJobEscrowForm $ Just (o, a))
                                          pure $ EscrowUI o a (w, e))

  pure $ concat escrows
