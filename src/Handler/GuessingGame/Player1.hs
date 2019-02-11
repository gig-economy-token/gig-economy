{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
module Handler.GuessingGame.Player1
  ( getPlayer1R
  , postPlayer1DepositFundsR
  , postPlayer1StartGameR
  , postPlayer1GuessR
  ) where

import Import

import Cardano.Helpers
import qualified Wallet.API as WalletAPI
import qualified Wallet.Emulator as Emulator
import Cardano.Html.Emulator
import qualified Data.Map as Map

wallet1 :: Emulator.Wallet
wallet1 = Emulator.Wallet 1

renderLayout :: Html -> Html -> Handler Html
renderLayout action content = do
    emulatorState <- readEmulatorState
    let
        fundsInWallet1 :: Int
        fundsInWallet1 = fromMaybe 0 $ getResultingFunds <$> Map.lookup wallet1 (Emulator._walletStates emulatorState)
    defaultLayout $ do
        setTitle "Player 1"
        let status = [shamlet|
<ul>
  <li>Funds in wallet: #{fundsInWallet1}
|]
        $(widgetFile "guessing-game/player1")

getPlayer1R :: Handler Html
getPlayer1R = renderLayout "Status" "Player 1 starts the game and tries to guess"

postPlayer1DepositFundsR :: Handler Html
postPlayer1DepositFundsR = do
  error "Not implemented yet"
  --appendStep $ _f $ Emulator.liftMockWallet wallet1 (WalletAPI.submitTxn $ createMiningTransaction [(wallet1, 50)])
  --_ <- appendStep $ (Emulator.processPending >>= Emulator.walletsNotifyBlock [wallet1] >>= \x -> pure ())
  renderLayout "- Mine funds" "Player1 mined 50 nanoADA"

postPlayer1StartGameR :: Handler Html
postPlayer1StartGameR = renderLayout "- Start game" "FIXME: Start Game"

postPlayer1GuessR :: Handler Html
postPlayer1GuessR = renderLayout "- Guess" "FIXME: Guess"
