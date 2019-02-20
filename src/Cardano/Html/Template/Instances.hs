{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Html.Template.Instances where

import Import hiding (Value)
import Ledger
import Wallet.API
import Ledger.Ada.TH
import Ledger.Value.TH
import Text.Blaze.Html (ToMarkup(..), Html)
--import Codec.Serialise
--import Data.Aeson (toJSON)
--import Language.PlutusTx.Evaluation (evaluateCekTrace)
import Cardano.ScriptMagic

instance ToMarkup Slot where
  toMarkup (Slot x) = toMarkup $ "Slot " <> show x

instance ToMarkup PubKey where
  toMarkup (PubKey x) = toMarkup $ "PubKey " <> show x

instance ToMarkup KeyPair where
  toMarkup (KeyPair (_, PubKey pub)) = toMarkup $ "KeyPair with pubkey " <> show pub

instance ToMarkup Value where
  toMarkup (Value x) = toMarkup r
    where
      r = case items of
                [] -> "0"
                [i] -> i
                ix -> mconcat $ ["["] ++ ix ++ ["]"]
      items = toItem <$> x
      toItem :: (CurrencySymbol, Int) -> Html
      toItem (cs, v) | cs == $$(currencySymbol) 0 = toMarkup $ "₳" <> show v
      toItem (cs, v) = toMarkup $ show cs <> " " <> show v

instance ToMarkup Ada where
  toMarkup ada = "₳" <> toMarkup ($$(toInt) ada)

instance ToMarkup TxOutType where
  toMarkup (PayToPubKey pk) = "Pay to " <> (toMarkup pk)
  toMarkup (PayToScript datascript) = "Pay to script " <> (toMarkup datascript)

instance ToMarkup Script where
  toMarkup s = toMarkup prettyScript
    where
      prettyScript = show {-$ evaluateCekTrace-} (scriptToUnderlyingScript s)

instance ToMarkup DataScript where
  toMarkup (DataScript datascript) = "DataScript " <> toMarkup datascript

showAddr :: Show a => a -> String
showAddr a = (take 10 $ show a) <> "..."

instance Show a => ToMarkup (AddressOf a) where
  toMarkup (AddressOf x) = "AddressOf " <> (toMarkup (showAddr x))

instance ToMarkup TxOutRef where
  toMarkup TxOutRefOf {..} = "TxOutRef #" <> (toMarkup txOutRefIdx) <> " - " <> (toMarkup (showAddr (getTxId txOutRefId)))

instance ToMarkup TxOut where
  toMarkup TxOutOf {..} = "TxOut " <> toMarkup txOutType <> " - " <> toMarkup txOutValue <> " - " <> toMarkup txOutAddress

instance ToMarkup TxIn where
  toMarkup TxInOf {..} = "TxIn from " <> toMarkup txInRef <> " - " <> toMarkup txInType

instance ToMarkup TxId where
  toMarkup (TxIdOf x) = "TxId " <> toMarkup (showAddr x)

instance ToMarkup SlotRange where
  toMarkup (Interval f t) = "[" <> from <> ", " <> to <> ")"
    where
      from = fromMaybe "-∞" (toMarkup <$> f)
      to = fromMaybe "∞" (toMarkup <$> t)

instance ToMarkup TxInType where
  toMarkup (ConsumePublicKeyAddress sig) = "ConsumePublicKeyAddress " <> toMarkup (show sig)
  toMarkup (ConsumeScriptAddress a b) = "ConsumeScriptTxIn " <> toMarkup a <> " - " <> toMarkup b

instance ToMarkup ValidatorScript where
  toMarkup (ValidatorScript s) = "ValidatorScript " <> toMarkup s

instance ToMarkup RedeemerScript where
  toMarkup (RedeemerScript s) = "RedeemerScript " <> toMarkup s
