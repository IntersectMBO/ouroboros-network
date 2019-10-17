module Main (main) where

import           Test.Tasty

import qualified Test.AnchoredFragment (tests)
import qualified Test.ChainGenerators (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainFragment (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Pipe (tests)
import qualified Test.Subscription (tests)
import qualified Test.PeerState (tests)
import qualified Test.Ouroboros.Network.MockNode (tests)
import qualified Test.Ouroboros.Network.BlockFetch (tests)
import qualified Ouroboros.Network.Protocol.ChainSync.Test (tests)
import qualified Ouroboros.Network.Protocol.BlockFetch.Test (tests)
import qualified Ouroboros.Network.Protocol.Handshake.Test (tests)
import qualified Ouroboros.Network.Protocol.TxSubmission.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test (tests)
import qualified Ouroboros.Network.PeerSelection.Examples ()
import qualified Test.Socket (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"

    -- data structures
  [ Test.ChainGenerators.tests
  , Test.Chain.tests
  , Test.ChainFragment.tests
  , Test.AnchoredFragment.tests
  , Test.ChainProducerState.tests

    -- protocols
  , Ouroboros.Network.Protocol.ChainSync.Test.tests
  , Ouroboros.Network.Protocol.BlockFetch.Test.tests
  , Ouroboros.Network.Protocol.LocalTxSubmission.Test.tests
  , Ouroboros.Network.Protocol.TxSubmission.Test.tests
  , Ouroboros.Network.Protocol.Handshake.Test.tests

    -- network logic
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.Subscription.tests
  , Test.PeerState.tests
  , Test.Ouroboros.Network.BlockFetch.tests

    -- pseudo system-level
  , Test.Ouroboros.Network.MockNode.tests
  ]
