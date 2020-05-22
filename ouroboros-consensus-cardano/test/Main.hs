module Main (main) where

import           Test.Tasty

import qualified Test.ThreadNet.Cardano (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.ThreadNet.Cardano.tests
  ]
