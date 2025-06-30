module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.ChainProducerState qualified (tests)
import Test.Ouroboros.Network.BlockFetch qualified (tests)
import Test.Ouroboros.Network.Diffusion.Policies qualified (tests)
import Test.Ouroboros.Network.Diffusion.Testnet.Cardano qualified (tests)
import Test.Ouroboros.Network.KeepAlive qualified (tests)
import Test.Ouroboros.Network.LedgerPeers qualified (tests)
import Test.Ouroboros.Network.MockNode qualified (tests)
import Test.Ouroboros.Network.NodeToClient.Version qualified (tests)
import Test.Ouroboros.Network.NodeToNode.Version qualified (tests)
import Test.Ouroboros.Network.OrphanInstances.Tests qualified (tests)
import Test.Ouroboros.Network.PeerSelection qualified (tests)
import Test.Ouroboros.Network.PeerSelection.Cardano.LocalRootPeers qualified
import Test.Ouroboros.Network.PeerSelection.Cardano.MockEnvironment qualified
import Test.Ouroboros.Network.PeerSelection.Cardano.PublicRootPeers qualified
import Test.Ouroboros.Network.PeerSelection.KnownPeers qualified
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers qualified
import Test.Ouroboros.Network.PeerSelection.PeerMetric qualified
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS qualified
import Test.Ouroboros.Network.TxSubmission qualified (tests)
import Test.Ouroboros.Network.Version qualified (tests)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network:sim-tests"
    -- data structures
  [ Test.ChainProducerState.tests

    -- network logic
  , Test.Ouroboros.Network.Diffusion.Policies.tests
  , Test.Ouroboros.Network.LedgerPeers.tests
  , Test.Ouroboros.Network.BlockFetch.tests
  , Test.Ouroboros.Network.KeepAlive.tests
  , Test.Ouroboros.Network.TxSubmission.tests
  , Test.Ouroboros.Network.NodeToClient.Version.tests
  , Test.Ouroboros.Network.NodeToNode.Version.tests
  , Test.Ouroboros.Network.OrphanInstances.Tests.tests
  , Test.Ouroboros.Network.PeerSelection.KnownPeers.tests
  , Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.PeerMetric.tests
  , Test.Ouroboros.Network.PeerSelection.RootPeersDNS.tests
  , Test.Ouroboros.Network.PeerSelection.tests
  , Test.Ouroboros.Network.Version.tests

    -- cardano specific logic
  , Test.Ouroboros.Network.Diffusion.Testnet.Cardano.tests
  , Test.Ouroboros.Network.PeerSelection.Cardano.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.Cardano.MockEnvironment.tests
  , Test.Ouroboros.Network.PeerSelection.Cardano.PublicRootPeers.tests


    -- pseudo system-level
  , Test.Ouroboros.Network.MockNode.tests
  ]
