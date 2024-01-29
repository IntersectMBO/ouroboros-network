module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Ouroboros.Network.ConnectionManager qualified as ConnectionManager
import Test.Ouroboros.Network.RateLimiting qualified as RateLimiting
import Test.Ouroboros.Network.Server2.Sim qualified as Server2
import Test.Simulation.Network.Snocket qualified as Snocket

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-framework:sim-tests"
  [ ConnectionManager.tests
  , Server2.tests
  , RateLimiting.tests
  , Snocket.tests
  ]


