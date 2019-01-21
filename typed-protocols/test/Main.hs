module Main (main) where

import           Test.Tasty

import qualified Test.Network.TypedProtocol.PingPong as PingPong (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-transitions"
  [ PingPong.tests
  ]

