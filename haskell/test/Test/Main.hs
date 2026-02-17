module Test.Main (main) where

import Data.List (isInfixOf)
import Greeting (greeting)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Greeting.greeting" $ do
    it "Returns a greeting" $ do
      greeting "Test" `shouldBe` "Hello, Test!"

    it "Contains the name" $ do
      property $ \name -> name `isInfixOf` greeting name `shouldBe` True
