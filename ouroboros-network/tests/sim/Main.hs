module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.ChainProducerState qualified (tests)

import Test.Ouroboros.Network.BlockFetch qualified (tests)
import Test.Ouroboros.Network.KeepAlive qualified (tests)
import Test.Ouroboros.Network.LedgerPeers qualified (tests)
import Test.Ouroboros.Network.MockNode qualified (tests)
import Test.Ouroboros.Network.PeerSelection.KnownPeers qualified
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers qualified
import Test.Ouroboros.Network.PeerSelection.PeerMetric qualified
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS qualified
import Test.Ouroboros.Network.TxSubmission qualified (tests)
import Test.Ouroboros.Network.TxSubmission.Mempool.Simple qualified (tests)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-sim-tests"
    -- data structures
  [ Test.ChainProducerState.tests

    -- network logic
  , Test.Ouroboros.Network.LedgerPeers.tests
  , Test.Ouroboros.Network.BlockFetch.tests
  , Test.Ouroboros.Network.KeepAlive.tests
  , Test.Ouroboros.Network.TxSubmission.tests
  , Test.Ouroboros.Network.TxSubmission.Mempool.Simple.tests
  , Test.Ouroboros.Network.PeerSelection.KnownPeers.tests
  , Test.Ouroboros.Network.PeerSelection.LocalRootPeers.tests
  , Test.Ouroboros.Network.PeerSelection.PeerMetric.tests
  , Test.Ouroboros.Network.PeerSelection.RootPeersDNS.tests

    -- pseudo system-level
  , Test.Ouroboros.Network.MockNode.tests
  ]
