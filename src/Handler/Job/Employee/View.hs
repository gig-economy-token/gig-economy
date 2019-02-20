{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Job.Employee.View where

import Import

renderLayout :: Handler Html
renderLayout = do
    defaultLayout $ do
        $(widgetFile "job/employee")
