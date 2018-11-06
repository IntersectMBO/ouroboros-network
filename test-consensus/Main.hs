module Main (main) where

import           Test.Tasty

import qualified Test.Dynamic (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Dynamic.tests
  ]
