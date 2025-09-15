module Main (main) where

import Test.Tasty

import Ouroboros.Network.Protocol.BlockFetch.Test qualified (tests)
import Ouroboros.Network.Protocol.ChainSync.Test qualified (tests)
import Ouroboros.Network.Protocol.Handshake.Test qualified (tests)
import Ouroboros.Network.Protocol.KeepAlive.Test qualified (tests)
import Ouroboros.Network.Protocol.LocalStateQuery.Test qualified (tests)
import Ouroboros.Network.Protocol.LocalTxMonitor.Test qualified (tests)
import Ouroboros.Network.Protocol.LocalTxSubmission.Test qualified (tests)
import Ouroboros.Network.Protocol.PeerSharing.Test qualified (tests)
import Ouroboros.Network.Protocol.TxSubmission2.Test qualified (tests)
import Test.AnchoredFragment qualified (tests)
import Test.Chain qualified (tests)
import Test.ChainGenerators qualified (tests)
import Test.Data.CDDL qualified (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-protocols"

    -- data structures
  [ Test.AnchoredFragment.tests
  , Test.Chain.tests
  , Test.ChainGenerators.tests

  , Test.Data.CDDL.tests

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
