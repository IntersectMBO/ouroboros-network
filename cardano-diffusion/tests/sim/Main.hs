module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Cardano.Network.Diffusion.Policies qualified (tests)
import Test.Cardano.Network.Diffusion.Testnet qualified (tests)
import Test.Cardano.Network.OrphanInstances.Tests qualified (tests)
import Test.Cardano.Network.PeerSelection qualified (tests)
import Test.Cardano.Network.PeerSelection.LocalRootPeers qualified
import Test.Cardano.Network.PeerSelection.MockEnvironment qualified
import Test.Cardano.Network.PeerSelection.PublicRootPeers qualified

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano-diffusion-sim-tests"
  [ Test.Cardano.Network.OrphanInstances.Tests.tests

    -- network logic
  , Test.Cardano.Network.Diffusion.Policies.tests
  , Test.Cardano.Network.PeerSelection.LocalRootPeers.tests
  , Test.Cardano.Network.PeerSelection.PublicRootPeers.tests
  , Test.Cardano.Network.PeerSelection.MockEnvironment.tests
  , Test.Cardano.Network.PeerSelection.tests

  , Test.Cardano.Network.Diffusion.Testnet.tests
  ]
