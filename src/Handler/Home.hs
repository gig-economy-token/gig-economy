{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import qualified Data.Set as Set

import Cardano.GameContract
--import qualified Wallet
import qualified Ledger
import qualified Ledger.Interval as Interval
import qualified Wallet.Emulator as Emulator

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        let
            emptyBlockchain = []
            -- subtitle = show $ Emulator.runTraceChain emptyBlockchain someOperation
            subtitle = show $ Emulator.runTraceTxPool [sampleTransaction] someOperation
        $(widgetFile "homepage")


-- Random operation on the emulator
someOperation :: Emulator.Trace Emulator.MockWallet String
someOperation = do
  let [w1, w2] = Emulator.Wallet <$> [1, 2]
  Emulator.processPending >>= Emulator.walletsNotifyBlock [w1, w2]
  tx1 <- Emulator.walletAction w1 $ startGame
  tx2 <- Emulator.walletAction w2 $ lock "asdf" 4
  pure $ show tx1 <> show tx2

-- Sample mining transaction that fills wallet 1 and 2 with some nano ADA
sampleTransaction :: Ledger.Tx
sampleTransaction = Ledger.Tx
  { Ledger.txInputs = Set.empty
  , Ledger.txOutputs = [
            Ledger.TxOutOf
              { Ledger.txOutAddress = Ledger.pubKeyAddress pk1
              , Ledger.txOutValue = Ledger.Value 40
              , Ledger.txOutType = Ledger.PayToPubKey pk1
              },
            Ledger.TxOutOf
              { Ledger.txOutAddress = Ledger.pubKeyAddress pk2
              , Ledger.txOutValue = Ledger.Value 60
              , Ledger.txOutType = Ledger.PayToPubKey pk2
              }
  ]
  , Ledger.txForge = Ledger.Value 100
  , Ledger.txFee = Ledger.Value 0
  , Ledger.txValidRange = $$(Interval.always)
  }
  where
    pk1 = Ledger.PubKey 1
    pk2 = Ledger.PubKey 2

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        let subtitle = "Here goes some Cardano!" :: String
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]
