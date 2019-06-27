module Main (main) where

import           Test.Tasty

import qualified Test.Mux (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "mux"
    [ -- network logic
      Test.Mux.tests
    ]
