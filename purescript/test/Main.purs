module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  myspec

myspec :: Spec Unit
myspec = describe "A test example" $ do
  pending "@TODO"

  it "adds 1 and 1" do
    (1 + 1) `shouldEqual` 2
