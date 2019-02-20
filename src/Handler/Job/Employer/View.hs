{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.Job.Employer.View
  ( renderLayout
  ) where

import Import

renderLayout :: Handler Html
renderLayout = do
    defaultLayout $ do
        $(widgetFile "job/employer")
