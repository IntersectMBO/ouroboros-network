module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime.SlotLengths (tests)
import qualified Test.Consensus.BlockchainTime.WallClock (tests)
import qualified Test.Consensus.HardFork.History (tests)
import qualified Test.Consensus.HardFork.Summary (tests)
import qualified Test.Consensus.Mempool (tests)
import qualified Test.Consensus.MiniProtocol.ChainSync.Client (tests)
import qualified Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests)
import qualified Test.Consensus.Node (tests)
import qualified Test.Consensus.Protocol.PBFT (tests)
import qualified Test.Consensus.ResourceRegistry (tests)
import qualified Test.Consensus.Util.MonadSTM.RAWLock (tests)
import qualified Test.Consensus.Util.Versioned (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.SlotLengths.tests
  , Test.Consensus.BlockchainTime.WallClock.tests
  , Test.Consensus.MiniProtocol.ChainSync.Client.tests
  , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
  , Test.Consensus.Mempool.tests
  , Test.Consensus.Node.tests
  , Test.Consensus.Protocol.PBFT.tests
  , Test.Consensus.ResourceRegistry.tests
  , Test.Consensus.Util.MonadSTM.RAWLock.tests
  , Test.Consensus.Util.Versioned.tests
  , Test.Consensus.HardFork.Summary.tests
  , Test.Consensus.HardFork.History.tests
  ]
