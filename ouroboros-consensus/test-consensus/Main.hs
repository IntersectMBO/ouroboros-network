module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime (tests)
import qualified Test.Consensus.ChainSyncClient (tests)
import qualified Test.Consensus.Mempool (tests)
import qualified Test.Dynamic.BFT (tests)
import qualified Test.Dynamic.LeaderSchedule (tests)
import qualified Test.Dynamic.PBFT (tests)
import qualified Test.Dynamic.Praos (tests)
import qualified Test.Dynamic.RealPBFT (tests)
import qualified Test.Dynamic.Util.Tests (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.tests
  , Test.Consensus.ChainSyncClient.tests
  , Test.Consensus.Mempool.tests
  , Test.Dynamic.Util.Tests.tests
  , Test.Dynamic.BFT.tests
  , Test.Dynamic.LeaderSchedule.tests
  , Test.Dynamic.PBFT.tests
  , Test.Dynamic.Praos.tests
  , Test.Dynamic.RealPBFT.tests
  ]
