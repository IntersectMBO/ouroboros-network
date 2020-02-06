module Main (main) where

import           Test.Tasty

import qualified Test.Network.TypedProtocol.PingPong.Codec as PingPong
import qualified Test.Network.TypedProtocol.ReqResp.Codec as ReqResp
import qualified Test.Ouroboros.Network.Driver as Driver

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  , Driver.tests
  ]


