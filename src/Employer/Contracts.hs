module Employer.Contracts where

import Cardano.JobContract.Types
import Wallet.API

postJobOffer :: (Monad m, WalletAPI m) => JobOffer -> m ()
postJobOffer JobOffer{..} = do

  payToScript_ undefined undefined (Value [(_, joPayout)]) undefined
