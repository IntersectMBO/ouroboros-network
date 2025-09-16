module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Cardano.Network.NodeToClient.Version qualified as NodeToClient.Version
import Test.Cardano.Network.NodeToNode.Version qualified as NodeToNode.Version
import Test.Ouroboros.Network.PeerSelection.RelayAccessPoint qualified as RelayAccessPoint

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-api"
  [ NodeToClient.Version.tests
  , NodeToNode.Version.tests
  , RelayAccessPoint.tests
  ]


