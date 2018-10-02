module Main (main) where

import Test.Tasty

import qualified Chain (tests)
import qualified Test.Sim (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"
  [ Chain.tests
  , Test.Sim.tests
  ]
