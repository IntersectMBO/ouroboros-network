module Main (main) where

import           Test.Tasty

import qualified Test.Network.TypedProtocol.PingPong.Codec as PingPong
import qualified Test.Network.TypedProtocol.ReqResp.Codec as ReqResp
import qualified Test.Ouroboros.Network.ConnectionManager as ConnectionManager
import qualified Test.Ouroboros.Network.ConnectionManager.Server as Server
import qualified Test.Ouroboros.Network.Driver as Driver
import qualified Test.Ouroboros.Network.Socket as Socket
import qualified Test.Ouroboros.Network.Subscription as Subscription
import qualified Test.Ouroboros.Network.RateLimiting as RateLimiting

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  , Driver.tests
  , Server.tests
  , Socket.tests
  , ConnectionManager.tests
  , Subscription.tests
  , RateLimiting.tests
  ]


