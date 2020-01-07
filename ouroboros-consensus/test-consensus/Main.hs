module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime.SlotLengths (tests)
import qualified Test.Consensus.BlockchainTime.WallClock (tests)
import qualified Test.Consensus.ChainSyncClient (tests)
import qualified Test.Consensus.Ledger.Byron (tests)
import qualified Test.Consensus.Ledger.Mock (tests)
import qualified Test.Consensus.Mempool (tests)
import qualified Test.Consensus.Node (tests)
import qualified Test.Consensus.Protocol.PBFT (tests)
import qualified Test.Consensus.ResourceRegistry (tests)
import qualified Test.ThreadNet.BFT (tests)
import qualified Test.ThreadNet.LeaderSchedule (tests)
import qualified Test.ThreadNet.PBFT (tests)
import qualified Test.ThreadNet.Praos (tests)
import qualified Test.ThreadNet.RealPBFT (tests)
import qualified Test.ThreadNet.Util.Tests (tests)

import qualified Test.Util.Split (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.SlotLengths.tests
  , Test.Consensus.BlockchainTime.WallClock.tests
  , Test.Consensus.ChainSyncClient.tests
  , Test.Consensus.Ledger.Byron.tests
  , Test.Consensus.Ledger.Mock.tests
  , Test.Consensus.Mempool.tests
  , Test.Consensus.Node.tests
  , Test.Consensus.Protocol.PBFT.tests
  , Test.Consensus.ResourceRegistry.tests
  , Test.ThreadNet.Util.Tests.tests
  , Test.ThreadNet.BFT.tests
  , Test.ThreadNet.LeaderSchedule.tests
  , Test.ThreadNet.PBFT.tests
  , Test.ThreadNet.Praos.tests
  , Test.ThreadNet.RealPBFT.tests
  , Test.Util.Split.tests
  ]
