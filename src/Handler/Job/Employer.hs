{-# LANGUAGE NoImplicitPrelude  #-}
module Handler.Job.Employer
  ( getEmployerR
  , postEmployerPostOfferR
  ) where

import Import

import Handler.Job.Employer.View
--import Cardano.Html.Emulator
import Yesod.Form.Bootstrap3

data PostJobAction = PostJobAction
  { _pjaDescription  :: Text
  , _pjaPayout       :: Integer
  }

postJobForm :: Html -> MForm Handler (FormResult PostJobAction, Widget)
postJobForm = renderDivs $ PostJobAction
              <$> areq descField (bfs ("Job description" :: Text)) Nothing
              <*> areq payoutField (bfs ("Job payout" :: Text)) Nothing
  where
    descField = checkBool (/= "") ("Please enter a description" :: Text) textField
    payoutField = checkBool (> 0) ("Please enter a positive payout" :: Text) intField

getEmployerR :: Handler Html
getEmployerR = do
  jobform <- generateFormPost postJobForm
  renderLayout jobform

postEmployerPostOfferR :: Handler Html
postEmployerPostOfferR = do
  ((result, widget), enctype) <- runFormPost postJobForm    
  case result of
    FormSuccess _job -> do
        --appendStepAndNotifyKnownWallets (undefined)
        void $ error "FIXME Post contract here"
        renderLayout (widget, enctype)
    FormMissing -> renderLayout (widget, enctype)
    FormFailure _ -> renderLayout (widget, enctype)
