{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Arbiter.View
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

data EscrowUI = EscrowUI JobOffer JobApplication (Widget, Enctype)
data Escrows
  = NotSubscribedToEscrows
  | Escrows [EscrowUI]


renderLayoutWithError :: Text -> Handler Html
renderLayoutWithError e = renderLayout' (Just e)

renderLayout :: Handler Html
renderLayout = renderLayout' Nothing

renderLayout' :: Maybe Text -> Handler Html
renderLayout' errMsg = do
  escrows <- mkEscrows arbiterWallet
  mwalletState <- walletStateByWallet arbiterWallet
  case mwalletState of
    Nothing          -> notFound
    Just walletState -> defaultLayout $(widgetFile "job/arbiter")

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
