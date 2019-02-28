{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Import
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Ledger.Types

instance PathPiece B8.ByteString where
  fromPathPiece t = Just (B8.pack $ unpack t)
  toPathPiece b = pack $ B8.unpack b

instance PathPiece Ledger.Types.PubKey where
  fromPathPiece t = Ledger.Types.PubKey <$> readMay t
  toPathPiece (Ledger.Types.PubKey pk) = pack $ show pk
