module Main (main) where

import           Main.Utf8 (withUtf8)
import           Test.Tasty

import qualified Test.Ouroboros.Network.ConnectionManager as ConnectionManager
import qualified Test.Ouroboros.Network.Driver as Driver
import qualified Test.Ouroboros.Network.RateLimiting as RateLimiting
import qualified Test.Ouroboros.Network.Server2 as Server2
import qualified Test.Ouroboros.Network.Socket as Socket
import qualified Test.Ouroboros.Network.Subscription as Subscription
import qualified Test.Simulation.Network.Snocket as Snocket

main :: IO ()
main = withUtf8 $ defaultMain tests

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
  ]


