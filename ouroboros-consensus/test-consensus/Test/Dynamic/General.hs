{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.General (
    prop_general
  , runTestNetwork
    -- * TestConfig
  , TestConfig (..)
  , genTestConfig
  , shrinkTestConfig
    -- * Re-exports
  , TestOutput (..)
  ) where

import           Control.Monad (join)
import qualified Data.Map as Map
import           Data.Word (Word64)
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (HasHeader)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol (LeaderSchedule (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

import           Test.Dynamic.Network
import           Test.Dynamic.TxGen
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.NoUnexpectedThunks ()
import           Test.Util.Range

{-------------------------------------------------------------------------------
  Configuring tests
-------------------------------------------------------------------------------}

data TestConfig = TestConfig
  { numCoreNodes :: !NumCoreNodes
  , numSlots     :: !NumSlots
  , nodeJoinPlan :: !NodeJoinPlan
  , nodeTopology :: !NodeTopology
  }
  deriving (Show)

genTestConfig :: NumCoreNodes -> NumSlots -> Gen TestConfig
genTestConfig numCoreNodes numSlots = do
    nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
    nodeTopology <- genNodeTopology numCoreNodes
    pure TestConfig{numCoreNodes, numSlots, nodeJoinPlan, nodeTopology}

-- | Shrink without changing the number of nodes or slots
shrinkTestConfig :: TestConfig -> [TestConfig]
shrinkTestConfig testConfig@TestConfig{nodeJoinPlan, nodeTopology} =
    tail $ -- drop the identity output
    [ testConfig{nodeJoinPlan = p', nodeTopology = top'}
    | p' <- nodeJoinPlan : shrinkNodeJoinPlan nodeJoinPlan
    , top' <- nodeTopology : shrinkNodeTopology nodeTopology
    ]

-- | Shrink, including the number of nodes and slots
shrinkTestConfigFreely :: TestConfig -> [TestConfig]
shrinkTestConfigFreely
  TestConfig{numCoreNodes, numSlots, nodeJoinPlan, nodeTopology} =
    tail $   -- drop the identity result
    [ TestConfig
        { numCoreNodes = n'
        , numSlots = t'
        , nodeJoinPlan = p'
        , nodeTopology = top'
        }
    | n' <- idAnd shrink numCoreNodes
    , t' <- idAnd shrink numSlots
    , let adjustedP = adjustedNodeJoinPlan n' t'
    , let adjustedTop = adjustedNodeTopology n'
    , p' <- idAnd shrinkNodeJoinPlan adjustedP
    , top' <- idAnd shrinkNodeTopology adjustedTop
    ]
  where
    idAnd :: forall a. (a -> [a]) -> a -> [a]
    idAnd f x = x : f x

    adjustedNodeJoinPlan (NumCoreNodes n') (NumSlots t') =
        NodeJoinPlan $
        -- scale by t' / t
        Map.map (\(SlotNo i) -> SlotNo $ (i * toEnum t') `div` toEnum t) $
        -- discard discarded nodes
        Map.filterWithKey (\(CoreNodeId nid) _ -> nid < n') $
        m
      where
        NumSlots t = numSlots
        NodeJoinPlan m = nodeJoinPlan

    adjustedNodeTopology (NumCoreNodes n') =
        NodeTopology $ Map.filterWithKey (\(CoreNodeId i) _ -> i < n') m
      where
        NodeTopology m = nodeTopology

instance Arbitrary TestConfig where
  arbitrary = join $ genTestConfig <$> arbitrary <*> arbitrary
  shrink = shrinkTestConfigFreely

{-------------------------------------------------------------------------------
  Running tests
-------------------------------------------------------------------------------}

-- | Thin wrapper around 'runNodeNetwork'
--
-- Provides a 'ResourceRegistry' and 'BlockchainTime', runs in the IO sim
-- monad.
--
runTestNetwork ::
  forall blk.
     ( RunNode blk
     , TxGen blk
     , TracingConstraints blk
     )
  => (CoreNodeId -> ProtocolInfo blk)
  -> TestConfig
  -> Seed
  -> TestOutput blk
runTestNetwork pInfo
  TestConfig{numCoreNodes, numSlots, nodeJoinPlan, nodeTopology}
  seed = runSimOrThrow $ do
    registry  <- unsafeNewRegistry
    testBtime <- newTestBlockchainTime registry numSlots slotLen
    runNodeNetwork
      registry
      testBtime
      numCoreNodes
      nodeJoinPlan
      nodeTopology
      pInfo
      (seedToChaCha seed)
      slotLen
  where
    slotLen :: DiffTime
    slotLen = 100000

{-------------------------------------------------------------------------------
  Test properties
-------------------------------------------------------------------------------}

-- | The properties always required
--
-- Includes:
--
-- * The competitive chains at the end of the simulation respect the expected
--   bound on fork length
-- * The nodes do not leak file handles
--
prop_general ::
     ( Condense blk
     , Eq blk
     , HasHeader blk
     )
  => SecurityParam
  -> TestConfig
  -> LeaderSchedule
  -> TestOutput blk
  -> Property
prop_general k TestConfig{numSlots, nodeJoinPlan, nodeTopology} schedule
  TestOutput{testOutputNodes} =
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("schedule: " <> condense schedule) $
    counterexample ("nodeChains: " <> unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeChains))) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    tabulate "consensus expected" [show isConsensusExcepected] $
    tabulate "shortestLength" [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    prop_all_common_prefix
        maxForkLength
        (Map.elems nodeChains) .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeInfo
      | (nid, nodeInfo) <- Map.toList nodeInfos ]
  where
    NumBlocks maxForkLength = determineForkLength k nodeJoinPlan schedule

    nodeChains = nodeOutputFinalChain <$> testOutputNodes
    nodeInfos  = nodeOutputNodeInfo   <$> testOutputNodes

    isConsensusExcepected :: Bool
    isConsensusExcepected = consensusExpected k nodeJoinPlan schedule

    fileHandleLeakCheck :: NodeId -> NodeInfo blk MockFS -> Property
    fileHandleLeakCheck nid nodeInfo = conjoin
        [ checkLeak "ImmutableDB" $ nodeInfoImmDbFs nodeInfo
        , checkLeak "VolatileDB"  $ nodeInfoVolDbFs nodeInfo
        , checkLeak "LedgerDB"    $ nodeInfoLgrDbFs nodeInfo
        ]
      where
        checkLeak dbName fs = counterexample
          ("Node " <> show nid <> "'s " <> dbName <> " is leaking file handles")
          (Mock.numOpenHandles fs === 0)

    -- in which quarter of the simulation does the last node join?
    lastJoinSlot :: Maybe Word64
    lastJoinSlot =
        fmap (\(SlotNo i, _) -> (4 * i) `div` toEnum t) $
        Map.maxView m
      where
        NumSlots t = numSlots
        NodeJoinPlan m = nodeJoinPlan
