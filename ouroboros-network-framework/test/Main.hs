module Main (main) where

import Test.Tasty

import Test.Ouroboros.Network.ConnectionManager qualified as ConnectionManager
import Test.Ouroboros.Network.Driver qualified as Driver
import Test.Ouroboros.Network.RateLimiting qualified as RateLimiting
import Test.Ouroboros.Network.RawBearer qualified as RawBearer
import Test.Ouroboros.Network.Server2 qualified as Server2
import Test.Ouroboros.Network.Socket qualified as Socket
import Test.Ouroboros.Network.Subscription qualified as Subscription
import Test.Simulation.Network.Snocket qualified as Snocket

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-framework"
  [ ConnectionManager.tests
  , Driver.tests
  , Server2.tests
  , Socket.tests
  , Subscription.tests
  , RateLimiting.tests
  , Snocket.tests
  , RawBearer.tests
  ]


