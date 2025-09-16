module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Ouroboros.Network.PeerSelection.RelayAccessPoint qualified as RelayAccessPoint

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-api"
  [ RelayAccessPoint.tests
  ]


