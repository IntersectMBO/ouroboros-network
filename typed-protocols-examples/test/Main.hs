module Main (main) where

import           Test.Tasty

import qualified Network.TypedProtocol.PingPong.Tests as PingPong
import qualified Network.TypedProtocol.ReqResp.Tests as ReqResp

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  , ReqResp.tests
  ]

