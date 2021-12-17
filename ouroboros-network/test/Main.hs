module Main (main) where

import           Test.Tasty

import qualified Ouroboros.Network.Protocol.BlockFetch.Test (tests)
import qualified Ouroboros.Network.Protocol.ChainSync.Test (tests)
import qualified Ouroboros.Network.Protocol.Handshake.Test (tests)
import qualified Ouroboros.Network.Protocol.KeepAlive.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test (tests)
import qualified Ouroboros.Network.Protocol.TipSample.Test (tests)
import qualified Ouroboros.Network.Protocol.TxSubmission.Test (tests)
import qualified Ouroboros.Network.Protocol.TxSubmission2.Test (tests)
import qualified Test.AnchoredFragment (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainGenerators (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.LedgerPeers (tests)
import qualified Test.Ouroboros.Network.BlockFetch (tests)
import qualified Test.Ouroboros.Network.Diffusion.Policies (tests)
import qualified Test.Ouroboros.Network.KeepAlive (tests)
import qualified Test.Ouroboros.Network.MockNode (tests)
import qualified Test.Ouroboros.Network.NodeToClient.Version (tests)
import qualified Test.Ouroboros.Network.NodeToNode.Version (tests)
import qualified Test.Ouroboros.Network.PeerSelection (tests)
import qualified Test.Ouroboros.Network.PeerSelection.Json (tests)
import qualified Test.Ouroboros.Network.PeerSelection.LocalRootPeers
import qualified Test.Ouroboros.Network.PeerSelection.MockEnvironment
import qualified Test.Ouroboros.Network.PeerSelection.RootPeersDNS
import qualified Test.Ouroboros.Network.TxSubmission (tests)
import qualified Test.PeerState (tests)
import qualified Test.Pipe (tests)
import qualified Test.Socket (tests)
import qualified Test.Version (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"

    -- data structures
  [ Test.ChainGenerators.tests
  , Test.Chain.tests
  , Test.AnchoredFragment.tests
  , Test.ChainProducerState.tests

    -- protocols
  , Ouroboros.Network.Protocol.ChainSync.Test.tests
  , Ouroboros.Network.Protocol.BlockFetch.Test.tests
  , Ouroboros.Network.Protocol.LocalStateQuery.Test.tests
  , Ouroboros.Network.Protocol.LocalTxMonitor.Test.tests
  , Ouroboros.Network.Protocol.LocalTxSubmission.Test.tests
  , Ouroboros.Network.Protocol.TxSubmission.Test.tests
  , Ouroboros.Network.Protocol.TxSubmission2.Test.tests
  , Ouroboros.Network.Protocol.Handshake.Test.tests
  , Ouroboros.Network.Protocol.KeepAlive.Test.tests
  , Ouroboros.Network.Protocol.TipSample.Test.tests

    -- network logic
  , Test.Version.tests
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.PeerState.tests
  , Test.Ouroboros.Network.BlockFetch.tests
  , Test.Ouroboros.Network.PeerSelection.tests
  , Test.Ouroboros.Network.PeerSelection.Json.tests
  , Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.MockEnvironment.tests
  , Test.Ouroboros.Network.PeerSelection.RootPeersDNS.tests
  , Test.Ouroboros.Network.KeepAlive.tests
  , Test.Ouroboros.Network.TxSubmission.tests
  , Test.Ouroboros.Network.NodeToNode.Version.tests
  , Test.Ouroboros.Network.NodeToClient.Version.tests
  , Test.LedgerPeers.tests
  , Test.Ouroboros.Network.Diffusion.Policies.tests

    -- pseudo system-level
  , Test.Ouroboros.Network.MockNode.tests
  ]
