module Main (main) where

import Test.Tasty

import Cardano.Network.Protocol.Handshake.Test qualified (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-protocols"

  [ -- protocols
    Cardano.Network.Protocol.Handshake.Test.tests
  ]
