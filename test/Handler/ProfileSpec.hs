{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get ProfileR
            statusIs 200

        it "asserts user's information is shown" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get ProfileR
            let (Entity _ user) = userEntity
            htmlAnyContain ".username" . unpack $ userIdent user
