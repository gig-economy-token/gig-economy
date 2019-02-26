{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Job.Forms where

import Import
import Instances ()
import Cardano.JobContract

hiddenJobOfferForm :: Maybe JobOffer -> Html -> MForm Handler (FormResult JobOffer, Widget)
hiddenJobOfferForm jo = renderDivs $ JobOffer
              <$> areq hiddenField ("" { fsId = Just "a" }) (joDescription <$> jo)
              <*> areq hiddenField ("" { fsId = Just "b" }) (joPayout <$> jo)
              <*> areq hiddenField ("" { fsId = Just "c" }) (joOfferer <$> jo)
