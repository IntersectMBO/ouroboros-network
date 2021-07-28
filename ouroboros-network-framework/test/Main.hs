module Main (main) where

import           Test.Tasty

import qualified Test.Ouroboros.Network.Driver as Driver
import qualified Test.Ouroboros.Network.Socket as Socket
import qualified Test.Ouroboros.Network.Subscription as Subscription
import qualified Test.Ouroboros.Network.RateLimiting as RateLimiting

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-framework"
  [ Driver.tests
  , Socket.tests
  , Subscription.tests
  , RateLimiting.tests
  ]


