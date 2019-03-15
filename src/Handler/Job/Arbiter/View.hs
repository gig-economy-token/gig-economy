{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Arbiter.View where

import Import
import Wallet.Emulator
import Cardano.JobContract
import Cardano.Emulator.Job
import Cardano.Html.Emulator
import Handler.Job.Forms

data EscrowUI = EscrowUI JobOffer JobApplication (Widget, Enctype)
data Escrows
  = NotSubscribedToEscrows
  | Escrows [EscrowUI]

renderLayout :: Handler Html
renderLayout = do
    escrows <- mkEscrows arbiterWallet
    funds <- fundsInWallet arbiterWallet
    defaultLayout $ do
        $(widgetFile "job/arbiter")

mkEscrows :: Wallet -> Handler Escrows
mkEscrows wallet = do
  am <- readWatchedAddresses wallet
  case extractJobEscrows am of
    Nothing -> pure NotSubscribedToEscrows
    Just xs -> Escrows <$> mapM toEscrowUI xs

  where
    toEscrowUI :: EscrowSetup -> Handler EscrowUI
    toEscrowUI (EscrowSetup {..}) = do
                    (w, e) <- generateFormPost (hiddenJobEscrowForm $ Just (esJobOffer, esJobApplication))
                    pure $ EscrowUI esJobOffer esJobApplication (w, e)
    

{-
                (w, e) <- generateFormPost (hiddenJobEscrowForm $ Just (o, a))
  let offers = fromMaybe [] $ extractJobOffers am
      applications = (\x -> (x, extractJobApplications am x)) <$> offers

  escrows <- forM applications $ \(o, max') ->
                  case max' of
                    Nothing -> pure []
                    Just ax -> do
                              forM ax (\a -> do
                                          (w, e) <- generateFormPost (hiddenJobEscrowForm $ Just (o, a))
                                          pure $ EscrowUI o a (w, e))

  pure $ Escrows $ concat escrows-}
