module Main (main) where

import           Test.Tasty

import qualified Test.Ouroboros.Storage (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests
  ]
