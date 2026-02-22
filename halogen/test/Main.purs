module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  myspec

myspec :: Spec Unit
myspec = describe "A test example" $ do
  pending "@TODO"

  it "adds 1 and 1" do
    (1 + 1) `shouldEqual` 2
