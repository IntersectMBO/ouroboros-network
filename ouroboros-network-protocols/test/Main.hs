module Main (main) where

import           Test.Tasty

import qualified Ouroboros.Network.Protocol.BlockFetch.Test (tests)
import qualified Ouroboros.Network.Protocol.ChainSync.Test (tests)
import qualified Ouroboros.Network.Protocol.Handshake.Test (tests)
import qualified Ouroboros.Network.Protocol.KeepAlive.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Test (tests)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test (tests)
import qualified Ouroboros.Network.Protocol.PeerSharing.Test (tests)
import qualified Ouroboros.Network.Protocol.TxSubmission2.Test (tests)
import qualified Test.AnchoredFragment (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainGenerators (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-protocols"

    -- data structures
  [ Test.AnchoredFragment.tests
  , Test.Chain.tests
  , Test.ChainGenerators.tests

    -- protocols
  , Ouroboros.Network.Protocol.ChainSync.Test.tests
  , Ouroboros.Network.Protocol.BlockFetch.Test.tests
  , Ouroboros.Network.Protocol.LocalStateQuery.Test.tests
  , Ouroboros.Network.Protocol.LocalTxMonitor.Test.tests
  , Ouroboros.Network.Protocol.LocalTxSubmission.Test.tests
  , Ouroboros.Network.Protocol.TxSubmission2.Test.tests
  , Ouroboros.Network.Protocol.Handshake.Test.tests
  , Ouroboros.Network.Protocol.KeepAlive.Test.tests
  , Ouroboros.Network.Protocol.PeerSharing.Test.tests
  ]
