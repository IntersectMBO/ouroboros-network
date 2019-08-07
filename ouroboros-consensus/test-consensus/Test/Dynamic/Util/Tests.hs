module Test.Dynamic.Util.Tests (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime (NumSlots (..))
import           Ouroboros.Consensus.Demo (defaultSecurityParam)
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
                     (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Test.Dynamic.Util.Tests" $
    [ testProperty "0 = determineForkLength roundRobinLeaderSchedule" $
          prop_roundRobin_forkLength defaultSecurityParam
    ]

-- | A round-robin schedule should reach consensus
prop_roundRobin_forkLength ::
    SecurityParam -> NumCoreNodes -> NumSlots -> Property
prop_roundRobin_forkLength k numCoreNodes numSlots =
  determineForkLength k schedule === NumBlocks 0
  where
    schedule = roundRobinLeaderSchedule numCoreNodes numSlots
