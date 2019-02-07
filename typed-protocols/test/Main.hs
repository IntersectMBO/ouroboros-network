module Main (main) where

import           Test.Tasty

import qualified Network.TypedProtocol.PingPong.Tests as PingPong

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-protocols"
  [ PingPong.tests
  ]

