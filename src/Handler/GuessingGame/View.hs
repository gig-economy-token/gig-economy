{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.GuessingGame.View where

import Import

import Text.Blaze.Html

data PlayerStatus = PlayerStatus {
    funds :: Int
  }

instance ToMarkup PlayerStatus where
  toMarkup PlayerStatus {..} = [shamlet|
<ul>
  <li>Funds in wallet: #{funds}
|]
