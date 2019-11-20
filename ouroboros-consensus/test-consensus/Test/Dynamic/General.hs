{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
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
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     pattern GenesisPoint, HasHeader, Point, blockPoint)

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

import           Test.Dynamic.Network
import           Test.Dynamic.TxGen
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
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
    -- nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots -- TODO #1257
    let nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
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
  forall blk.
     ( Condense blk
     , Eq blk
     , HasHeader blk
     , RunNode blk
     )
  => SecurityParam
  -> TestConfig
  -> Maybe LeaderSchedule
  -> TestOutput blk
  -> Property
prop_general k TestConfig{numSlots, nodeJoinPlan, nodeTopology} mbSchedule
  TestOutput{testOutputNodes, testOutputTipBlockNos} =
    counterexample ("nodeChains: " <> unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeChains))) $
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    counterexample ("slot-node-tipBlockNo: " <> condense tipBlockNos) $
    counterexample ("mbSchedule: " <> condense mbSchedule) $
    counterexample ("growth schedule: " <> condense growthSchedule) $
    counterexample ("actual leader schedule: " <> condense actualLeaderSchedule) $
    counterexample ("consensus expected: " <> show isConsensusExpected) $
    tabulate "consensus expected" [show isConsensusExpected] $
    tabulate "shortestLength" [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    prop_all_common_prefix
        maxForkLength
        (Map.elems nodeChains) .&&.
    prop_all_growth .&&.
    prop_no_unexpected_message_delays .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeDBs
      | (nid, nodeDBs) <- Map.toList nodeOutputDBs ]
  where
    schedule = case mbSchedule of
        Nothing    -> actualLeaderSchedule
        Just sched -> sched

    NumBlocks maxForkLength = determineForkLength k nodeJoinPlan schedule

    -- build a leader schedule which includes every node that forged unless:
    --
    -- * the node rejected its own new block (eg 'PBftExceededSignThreshold')
    --
    -- * the node forged an EBB
    --
    actualLeaderSchedule :: LeaderSchedule
    actualLeaderSchedule =
        foldl (<>) (emptyLeaderSchedule numSlots) $
        [ let NodeOutput
                { nodeOutputForges
                , nodeOutputInvalids
                } = no
          in
          LeaderSchedule $
          Map.mapMaybeWithKey
              (actuallyLead cid nodeOutputInvalids)
              nodeOutputForges
        | (cid, no) <- Map.toList testOutputNodes
        ]
      where
        actuallyLead ::
             NodeId
          -> Set (Point blk)
          -> SlotNo
          -> blk
          -> Maybe [CoreNodeId]
        actuallyLead nid invalids s b = do
            cid <- case nid of
                CoreId i  -> Just (CoreNodeId i)
                RelayId _ -> Nothing

            let j = nodeIdJoinSlot nodeJoinPlan nid
            guard $ j <= s

            guard $ not $
                isJust (nodeIsEBB b) || Set.member (blockPoint b) invalids

            pure [cid]

    -- Refine 'actualLeaderSchedule' to also ignore a leader if:
    --
    -- * the node just joined in this slot (unless it's the earliest slot in
    --   which any nodes joined)
    --
    growthSchedule :: LeaderSchedule
    growthSchedule =
        LeaderSchedule $ Map.mapWithKey (\s -> filter (keep s)) mlead
      where
        LeaderSchedule mlead = actualLeaderSchedule

        keep s cid =
             isFirstJoinSlot s
          || coreNodeIdJoinSlot nodeJoinPlan cid < s

        isFirstJoinSlot s =
            Just s == (snd <$> Map.lookupMin mjoin)
          where
            NodeJoinPlan mjoin = nodeJoinPlan

    nodeChains    = nodeOutputFinalChain <$> testOutputNodes
    nodeOutputDBs = nodeOutputNodeDBs    <$> testOutputNodes

    isConsensusExpected :: Bool
    isConsensusExpected = consensusExpected k nodeJoinPlan schedule

    fileHandleLeakCheck :: NodeId -> NodeDBs MockFS -> Property
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
    -- active slots for that interval in the refined @growthSchedule@.
    --
    -- The paper <https://eprint.iacr.org/2017/573/20171115:00183> defines
    -- Common Growth as follows.
    --
    -- * Chain Growth (CG); with parameters τ ∈ (0, 1], s ∈ N. Consider the
    --   chains C1, C2 possessed by two honest parties at the onset of two
    --   slots sl1, sl2 with sl2 at least s slots ahead of sl1. Then it holds
    --   that len(C2) − len(C1) ≥ τs. We call τ the speed coefficient.
    prop_all_growth =
        isConsensusExpected `implies`
            conjoin
                [ prop_growth (s1, max1) (s2, min2)
                | ((s1, _, max1), (s2, min2, _)) <- orderedPairs extrema
                ]
      where
        -- QuickCheck's @==>@ 'discard's the test if @p1@ fails; that's not
        -- what we want
        implies p1 p2 = not p1 .||. p2

        -- all pairs @(x, y)@ where @x@ precedes @y@ in the given list
        orderedPairs :: [a] -> [(a, a)]
        orderedPairs = \case
            []   -> []
            x:ys -> foldr ((:) . (,) x) (orderedPairs ys) ys

        prop_growth :: (SlotNo, BlockNo) -> (SlotNo, BlockNo) -> Property
        prop_growth (s1, b1) (s2, b2) =
            counterexample (condense (s1, s2, b1, b2, numActiveSlots)) $
            nonNegativeGrowth .&&.
            sufficientGrowth
          where
            nonNegativeGrowth =
                counterexample "negative chain growth" $
                    property (b2 >= b1)

            sufficientGrowth =
                counterexample "insufficient chain growth" $
                    property (d >= toEnum numActiveSlots)

            BlockNo d = b2 - b1
            numActiveSlots =
                Map.size $
                flip Map.filterWithKey (getLeaderSchedule growthSchedule) $
                \slot ls -> s1 <= slot && slot < s2 && (not . null) ls

        -- @(s, min, max)@ the minimum and maximum block number of the tip of a
        -- chain at the onset of slot @s@.
        extrema :: [(SlotNo, BlockNo, BlockNo)]
        extrema =
            [ case map snd bnos' of
                  [] -> (slot, 0, 0)
                  o  -> (slot, minimum o, maximum o)
            | (slot, bnos) <- tipBlockNos
            , let bnos' = filter (joinedBefore slot . fst) bnos
            ]

        joinedBefore slot nid = nodeIdJoinSlot nodeJoinPlan nid < slot

    -- swizzled 'testOutputTipBlockNos'
    tipBlockNos :: [(SlotNo, [(NodeId, BlockNo)])]
    tipBlockNos =
        Map.toAscList $
        fmap Map.toAscList $
        testOutputTipBlockNos

    -- In the paper <https://eprint.iacr.org/2017/573/20171115:00183>, a
    -- /message/ carries a chain from one party to another. When a party forges
    -- a block, it \"diffuses\" the chain with that block as its head by
    -- sending a message to each other party (actually, to itself too, but
    -- that's ultimately redundant). The adversary is able to delay each
    -- message differently, so some parties may receive it before others do.
    -- Once a party receives a message, the party can consider that chain for
    -- selection.
    --
    -- In the implementation, on the other hand, our messages are varied and
    -- much more granular than a whole chain. We therefore observe a delay
    -- analogous to the paper's /message/ /delay/ by comparing the slot in
    -- which a block is added to each node's ChainDB against the slot in which
    -- that block was forged.
    --
    -- Since our mock network currently introduces only negligible latency
    -- compared to the slot duration, we generally expect all messages to have
    -- no delay: they should arrive to all nodes during the same slot in which
    -- they were forged. However, some delays are expected, due to nodes
    -- joining late and also due to the practicality of the ChainSync and
    -- BlockFetch policies, which try to avoid /unnecessary/ header/block
    -- fetches. See the relevant comments below.
    --
    -- NOTE: This current property does not check for interminable message
    -- delay: i.e. for blocks that were never added to some ChainDBs. It only
    -- checks the slot difference once a message does arrive. This seems
    -- acceptable: if there are no Common Prefix or Chain Growth violations,
    -- then each message must have either arrived or ultimately been
    -- irrelevant.
    --
    prop_no_unexpected_message_delays :: HasCallStack => Property
    prop_no_unexpected_message_delays =
        conjoin $
        [ case p of
              GenesisPoint            -> error "impossible"
              BlockPoint sendSlot hsh ->
                  prop1 nid recvSlot sendSlot hsh bno
        | (nid, m)          <- Map.toList adds
        , (recvSlot, pbnos) <- Map.toList m
        , (p, bno)          <- Set.toList pbnos
        ]
      where
        -- INVARIANT: these AddBlock events are *not* for EBBs
        adds = nodeOutputAdds <$> testOutputNodes

        prop1 nid recvSlot sendSlot hsh bno =
            counterexample msg $
            delayOK || noDelay
          where
            msg =
                "Unexpected message delay " <>
                "(" <> "recipient: " <> condense nid <>
                "," <> "expected receive slot: "
                    <> condense firstPossibleReception <>
                "," <> "actual receive slot: " <> condense recvSlot <>
                "," <> "blockHash: " <> show hsh <>
                "," <> "blockNo: " <> condense (unBlockNo bno) <>
                ")"

            -- a node cannot receive a block until both exist
            firstPossibleReception =
                nodeIdJoinSlot nodeJoinPlan nid `max` sendSlot

            noDelay = recvSlot == firstPossibleReception

            delayOK = delayOK1 || delayOK2

            -- When a node leads in the same slot in which it joins the
            -- network, it immediately forges a single block on top of Genesis;
            -- this block then prevents it from fetching the network's current
            -- chain if that also consists of just one block.
            --
            -- NOTE This predicate is more general than that specific scenario,
            -- but we don't anticipate it wholly masking any interesting cases.
            delayOK1 = 1 == bno

            -- When a slot has multiple leaders, each node chooses one of the
            -- mutually-exclusive forged blocks and won't fetch any of the
            -- others until it's later compelled to switch to a chain
            -- containing one of them
            --
            -- TODO This predicate is more general than that specific scenario,
            -- and should be tightened accordingly. We currently anticipate
            -- that Issues #229 and #230 will handle that.
            delayOK2 = case Map.lookup sendSlot sched of
                Just (_:_:_) -> True
                _            -> False
              where
                LeaderSchedule sched = actualLeaderSchedule
