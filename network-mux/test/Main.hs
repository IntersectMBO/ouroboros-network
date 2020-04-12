module Main (main) where

import           Test.Tasty

import qualified Test.Mux (tests)
import qualified Test.Mux.Timeout (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "mux"
    [ -- network logic
      Test.Mux.tests
    , Test.Mux.Timeout.tests
    ]
