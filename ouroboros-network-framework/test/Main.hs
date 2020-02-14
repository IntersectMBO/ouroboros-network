module Main (main) where

import           Test.Tasty

import qualified Network.TypedProtocol.PingPong.Codec.Tests as PingPong
import qualified Network.TypedProtocol.ReqResp.Codec.Tests as ReqResp

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  ]


