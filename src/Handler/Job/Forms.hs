{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Forms where

import Import
import Instances ()
import Cardano.JobContract
import qualified Data.ByteString.Lazy.Char8 as B8
import Yesod.Form.Bootstrap3

jobOfferForm :: Html -> MForm Handler (FormResult JobOfferForm, Widget)
jobOfferForm = renderDivs $ JobOfferForm
              <$> areq descField (bfs ("Job description" :: Text)) Nothing
              <*> areq payoutField (bfs ("Job payout" :: Text)) Nothing
  where
    descField = convertField (B8.pack . unpack) (pack . B8.unpack) $
                checkBool (/= "") ("Please enter a description" :: Text) $
                textField
    payoutField = checkBool (> 0) ("Please enter a positive payout" :: Text) $
                  intField

hiddenJobOfferForm :: Maybe JobOffer -> Html -> MForm Handler (FormResult JobOffer, Widget)
hiddenJobOfferForm jo = renderDivsNoLabels $ JobOffer
              <$> areq hiddenField ("" { fsId = Just "a" }) (joDescription <$> jo)
              <*> areq hiddenField ("" { fsId = Just "b" }) (joPayout <$> jo)
              <*> areq hiddenField ("" { fsId = Just "c" }) (joOfferer <$> jo)

hiddenJobEscrowForm :: Maybe (JobOffer, JobApplication) -> Html -> MForm Handler (FormResult (JobOffer, JobApplication), Widget)
hiddenJobEscrowForm joa = renderDivsNoLabels $ (\a b c d -> (JobOffer a b c, JobApplication d))
              <$> areq hiddenField ("" { fsId = Just "a" }) (joDescription . fst <$> joa)
              <*> areq hiddenField ("" { fsId = Just "b" }) (joPayout . fst <$> joa)
              <*> areq hiddenField ("" { fsId = Just "c" }) (joOfferer . fst <$> joa)
              <*> areq hiddenField ("" { fsId = Just "d" }) (jaAcceptor . snd <$> joa)
