{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.Job.View where

import Import

import Wallet.Emulator hiding (addressMap)
import Wallet.Emulator.AddressMap
import Text.Blaze.Html
import Cardano.Helpers
import Cardano.Html.Emulator
import Cardano.Html.Template
import qualified Data.Map as Map

data PlayerStatus = PlayerStatus
  { funds :: Int
  , addressMap :: AddressMap
  }

getPlayerStatus :: Wallet -> Handler PlayerStatus
getPlayerStatus w = do
    es <- readEmulatorState
    let status = PlayerStatus
                  { funds = fromMaybe 0 $ getResultingFunds <$> Map.lookup w (_walletStates es)
                  , addressMap = readWatchedAddresses' es w
                  }
    pure status

instance ToMarkup PlayerStatus where
  toMarkup ps = [shamlet|
<h4>Funds in wallet: #{funds ps}
<h4>Address map
<p>#{showAddressMap $ addressMap ps}
|]
