{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Forms where

import Import
import Instances ()
import Cardano.JobContract

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
