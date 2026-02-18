module Test.Main (main) where

import qualified Test.Greeting
import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.Greeting.spec
