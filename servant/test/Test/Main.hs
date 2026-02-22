{-# LANGUAGE OverloadedStrings #-}

module Test.Main (main) where

import Main (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /users" $ do
    it "responds with 200" $ do
      get "/users" `shouldRespondWith` 200
    it "responds with [User]" $ do
      let users = "[{\"age\":372,\"email\":\"isaac@newton.co.uk\",\"name\":\"Isaac Newton\",\"registration_date\":\"1683-03-01\"},{\"age\":136,\"email\":\"ae@mc2.org\",\"name\":\"Albert Einstein\",\"registration_date\":\"1905-12-01\"}]"
      get "/users" `shouldRespondWith` users
