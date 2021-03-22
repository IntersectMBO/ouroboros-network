module Test.ThreadNet.Util.Tests (tests) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

tests :: TestTree
tests = testGroup "Test.ThreadNet.Util.Tests" $
    [ testProperty "0 = determineForkLength roundRobinLeaderSchedule" $
          prop_roundRobin_forkLength securityParam
    ]
  where
    securityParam = SecurityParam 5

-- | A round-robin schedule should reach consensus
prop_roundRobin_forkLength ::
    SecurityParam -> NumCoreNodes -> NumSlots -> Property
prop_roundRobin_forkLength k numCoreNodes numSlots =
  determineForkLength k nodeJoinPlan schedule === NumBlocks 0
  where
    nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
    schedule = roundRobinLeaderSchedule numCoreNodes numSlots
