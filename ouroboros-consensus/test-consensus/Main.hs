module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime (tests)
import qualified Test.Dynamic.BFT (tests)
import qualified Test.Dynamic.LeaderSchedule (tests)
import qualified Test.Dynamic.PBFT (tests)
import qualified Test.Dynamic.Praos (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.tests
  , Test.Dynamic.BFT.tests
  , Test.Dynamic.LeaderSchedule.tests
  , Test.Dynamic.PBFT.tests
  , Test.Dynamic.Praos.tests
  ]
