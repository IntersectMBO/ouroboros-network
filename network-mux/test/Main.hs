module Main (main) where

import Test.Tasty

import Test.Mux qualified (tests)
import Test.Mux.Timeout qualified (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "mux"
    [ -- network logic
      Test.Mux.tests
    , Test.Mux.Timeout.tests
    ]
