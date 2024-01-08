module Main (main) where

import           Main.Utf8 (withUtf8)
import           Test.Tasty

import qualified Test.ChainProducerState (tests)
import qualified Test.Ouroboros.Network.BlockFetch (tests)
import qualified Test.Ouroboros.Network.Diffusion.Policies (tests)
import qualified Test.Ouroboros.Network.KeepAlive (tests)
import qualified Test.Ouroboros.Network.LedgerPeers (tests)
import qualified Test.Ouroboros.Network.MockNode (tests)
import qualified Test.Ouroboros.Network.NodeToClient.Version (tests)
import qualified Test.Ouroboros.Network.NodeToNode.Version (tests)
import qualified Test.Ouroboros.Network.PeerSelection (tests)
import qualified Test.Ouroboros.Network.PeerSelection.Json (tests)
import qualified Test.Ouroboros.Network.PeerSelection.KnownPeers
import qualified Test.Ouroboros.Network.PeerSelection.LocalRootPeers
import qualified Test.Ouroboros.Network.PeerSelection.MockEnvironment
import qualified Test.Ouroboros.Network.PeerSelection.PeerMetric
import qualified Test.Ouroboros.Network.PeerSelection.PublicRootPeers
import qualified Test.Ouroboros.Network.PeerSelection.RootPeersDNS
import qualified Test.Ouroboros.Network.PeerState (tests)
import qualified Test.Ouroboros.Network.Testnet (tests)
import qualified Test.Ouroboros.Network.TxSubmission (tests)
import qualified Test.Ouroboros.Network.Version (tests)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network:sim-tests"
    -- data structures
  [ Test.ChainProducerState.tests

    -- network logic
  , Test.Ouroboros.Network.Version.tests
  , Test.Ouroboros.Network.PeerState.tests
  , Test.Ouroboros.Network.BlockFetch.tests
  , Test.Ouroboros.Network.PeerSelection.tests
  , Test.Ouroboros.Network.PeerSelection.Json.tests
  , Test.Ouroboros.Network.PeerSelection.KnownPeers.tests
  , Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.PublicRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.MockEnvironment.tests
  , Test.Ouroboros.Network.PeerSelection.PeerMetric.tests
  , Test.Ouroboros.Network.PeerSelection.RootPeersDNS.tests
  , Test.Ouroboros.Network.KeepAlive.tests
  , Test.Ouroboros.Network.TxSubmission.tests
  , Test.Ouroboros.Network.NodeToNode.Version.tests
  , Test.Ouroboros.Network.NodeToClient.Version.tests
  , Test.Ouroboros.Network.Testnet.tests
  , Test.Ouroboros.Network.LedgerPeers.tests
  , Test.Ouroboros.Network.Diffusion.Policies.tests

    -- pseudo system-level
  , Test.Ouroboros.Network.MockNode.tests
  ]
