module Main (main) where

import           Main.Utf8 (withUtf8)
import           Test.Tasty

import qualified Test.Ouroboros.Network.ConnectionManager as ConnectionManager
import qualified Test.Ouroboros.Network.RateLimiting as RateLimiting
import qualified Test.Ouroboros.Network.Server2.Sim as Server2
import qualified Test.Simulation.Network.Snocket as Snocket

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


