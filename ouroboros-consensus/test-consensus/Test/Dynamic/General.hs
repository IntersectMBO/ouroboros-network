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

import           Control.Monad (guard, join)
import           Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (BlockNo (..), HasHeader, blockPoint)

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
     , RunNode blk
     )
  => SecurityParam
  -> TestConfig
  -> LeaderSchedule
  -> TestOutput blk
  -> Property
prop_general k TestConfig{numSlots, nodeJoinPlan, nodeTopology} schedule
  TestOutput{testOutputNodes, testOutputSlotBlockNos} =
    counterexample ("nodeChains: " <> unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeChains))) $
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    counterexample ("slot-node-depth: " <> condense slotNodeDepths) $
    counterexample ("schedule: " <> condense schedule) $
    counterexample ("schedule': " <> condense schedule') $
    counterexample ("consensus expected: " <> show isConsensusExcepected) $
    tabulate "consensus expected" [show isConsensusExcepected] $
    tabulate "shortestLength" [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    prop_all_common_prefix
        maxForkLength
        (Map.elems nodeChains) .&&.
    prop_all_growth .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeDBs
      | (nid, nodeDBs) <- Map.toList nodeOutputDBs ]
  where
    NumBlocks maxForkLength = determineForkLength k nodeJoinPlan schedule

    -- remove entries from @schedule@ if any of:
    --
    -- * the node just joined in this slot (unless it's the earliest slot in
    --   which any nodes joined)
    --
    -- * the node rejected its own new block (eg 'PBftExceededSignThreshold')
    --
    -- * the node forged an EBB
    --
    schedule' :: LeaderSchedule
    schedule' =
        Map.mapWithKey (filter . actuallyLead) `coerce` schedule
      where
        actuallyLead s cid = maybe False id $ do
            let nid = fromCoreNodeId cid
            let j   = nodeIdJoinSlot nodeJoinPlan nid

            guard $ j < s || (j == s && isFirstJoinSlot s)

            NodeOutput
              { nodeOutputForges
              , nodeOutputInvalids
              } <- Map.lookup nid testOutputNodes
            b <- Map.lookup s nodeOutputForges

            pure $ not $
              (||) (nodeIsEBB b) $
              Set.member (blockPoint b) nodeOutputInvalids

        isFirstJoinSlot s =
            let NodeJoinPlan m = nodeJoinPlan
            in
            Just s == (snd <$> Map.lookupMin m)

    nodeChains    = nodeOutputFinalChain <$> testOutputNodes
    nodeOutputDBs = nodeOutputNodeDBs    <$> testOutputNodes

    isConsensusExcepected :: Bool
    isConsensusExcepected = consensusExpected k nodeJoinPlan schedule

    fileHandleLeakCheck :: NodeId -> NodeDBs blk MockFS -> Property
    fileHandleLeakCheck nid nodeDBs = conjoin
        [ checkLeak "ImmutableDB" $ nodeDBsImm nodeDBs
        , checkLeak "VolatileDB"  $ nodeDBsVol nodeDBs
        , checkLeak "LedgerDB"    $ nodeDBsLgr nodeDBs
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

    -- check for Chain Growth violations if there are no Common Prefix
    -- violations
    --
    -- We consider all possible non-empty intervals, so the interval span
    -- @s@ varies but is always at least 1. We compute a different /speed
    -- coefficient/ @τ@ for each interval under the assumption that there are
    -- no message delays (ie @Δ = 0@). This is essentially a count of the
    -- active slots for that interval in the refined @schedule'@.
    prop_all_growth =
        (.||.) (not isConsensusExcepected) $
        conjoin $ zipWith f slotDepths (List.tails slotDepths)
      where
        f ::
             (SlotNo, BlockNo, BlockNo)
          -> [(SlotNo, BlockNo, BlockNo)]
          -> Property
        f (s1, _, max1) =
            conjoin .
            map (\(s2, min2, _) -> prop_growth (s1, max1) (s2, min2))

        prop_growth :: (SlotNo, BlockNo) -> (SlotNo, BlockNo) -> Property
        prop_growth (s1, b1) (s2, b2) =
            counterexample (condense (s1, s2, b1, b2, numActiveSlots)) $
            (.&&.)
                (counterexample "negative chain growth" $
                 property (b2 >= b1)) $
            counterexample "insufficient chain growth" $
            property (d >= toEnum numActiveSlots)
          where
            BlockNo d = b2 - b1
            numActiveSlots =
                Map.size $
                flip Map.filterWithKey (getLeaderSchedule schedule') $
                \slot ls -> s1 <= slot && slot < s2 && (not . null) ls

        slotDepths =
            [ case map snd bnos' of
                  [] -> (slot, 0, 0)
                  o  -> (slot, minimum o, maximum o)
            | (slot, bnos) <- slotNodeDepths
            , let bnos' = filter (joinedBefore slot . fst) bnos
            ]

        joinedBefore slot nid = nodeIdJoinSlot nodeJoinPlan nid < slot

    slotNodeDepths =
        Map.toAscList $
        fmap Map.toAscList $
        testOutputSlotBlockNos
