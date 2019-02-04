{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.BlockchainStatus where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Yesod.Core.Types

import qualified Cardano.GuessingGame as GuessingGame
import qualified Cardano.Html.Template as CardanoHtml

getBlockchainStatusR :: Handler Html
getBlockchainStatusR = do
    x :: Int <- handlerResource <$> ask

    defaultLayout $ do
        setTitle "Emulated blockchain status"
        let 
            (_result, simulatorStatus) = GuessingGame.simulateWithSampleWallets
            contentTitle = "Sample transaction result" :: String
            content = CardanoHtml.showEmulatorState simulatorStatus

        $(widgetFile "blockchainStatus")
