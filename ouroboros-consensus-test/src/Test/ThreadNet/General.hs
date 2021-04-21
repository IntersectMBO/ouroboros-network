{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Test.ThreadNet.General (
    PropGeneralArgs (..)
  , calcFinalIntersectionDepth
  , prop_general
  , prop_general_semisync
  , prop_inSync
  , runTestNetwork
    -- * TestConfig
  , TestConfig (..)
  , TestConfigB (..)
  , TestConfigMB (..)
  , truncateNodeJoinPlan
  , truncateNodeRestarts
  , truncateNodeTopology
    -- * Expected CannotForge
  , noExpectedCannotForges
    -- * Re-exports
  , ForgeEbbEnv (..)
  , TestOutput (..)
  , noCalcMessageDelay
  , plainTestNodeInitialization
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard)
import           Control.Tracer (nullTracer)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow, setCurrentTime)

import qualified Ouroboros.Network.MockChain.Chain as MockChain

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Block.Abstract as BA
import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (LedgerView)
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Test.ThreadNet.Network
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.Seed

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.HardFork.Future (Future)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.NoThunks ()
import           Test.Util.Range
import           Test.Util.Shrink (andId, dropId)
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time (dawnOfTime)

{-------------------------------------------------------------------------------
  Configuring tests
-------------------------------------------------------------------------------}

-- | Test configuration that does not depend on the block
--
-- The primary motivation for separating this type from 'TestConfigB' and
-- 'TestConfigMB' is so that the instance @'Arbitrary' 'TestConfig'@ can be
-- reused by multiple tests using different @blk@s: /as/ /of/ /yet/, no block
-- (each of which realizes a ledger-protocol combination) influences the
-- validity of these data.
data TestConfig = TestConfig
  { initSeed     :: Seed
  , nodeTopology :: NodeTopology
  , numCoreNodes :: NumCoreNodes
  , numSlots     :: NumSlots
    -- ^ TODO generate in function of @k@
  }
  deriving (Show)

truncateNodeJoinPlan ::
    NodeJoinPlan -> NumCoreNodes -> (NumSlots, NumSlots) -> NodeJoinPlan
truncateNodeJoinPlan
  (NodeJoinPlan m) (NumCoreNodes n') (NumSlots t, NumSlots t') =
    NodeJoinPlan $
    -- scale by t' / t
    Map.map (\(SlotNo i) -> SlotNo $ (i * t') `div` t) $
    -- discard discarded nodes
    Map.filterWithKey (\(CoreNodeId nid) _ -> nid < n') $
    m

truncateNodeTopology :: NodeTopology -> NumCoreNodes -> NodeTopology
truncateNodeTopology (NodeTopology m) (NumCoreNodes n') =
    NodeTopology $ Map.filterWithKey (\(CoreNodeId i) _ -> i < n') m

truncateNodeRestarts :: NodeRestarts -> NumSlots -> NodeRestarts
truncateNodeRestarts (NodeRestarts m) (NumSlots t) =
    NodeRestarts $ Map.filterWithKey (\(SlotNo s) _ -> s < t) m

instance Arbitrary TestConfig where
  arbitrary = do
      initSeed     <- arbitrary

      numCoreNodes <- arbitrary
      nodeTopology <- genNodeTopology numCoreNodes

      numSlots     <- arbitrary
      pure TestConfig
        { initSeed
        , nodeTopology
        , numCoreNodes
        , numSlots
        }

  shrink TestConfig
    { initSeed
    , nodeTopology
    , numCoreNodes
    , numSlots
    } =
      dropId $
      [ TestConfig
          { initSeed
          , nodeTopology = top'
          , numCoreNodes = n'
          , numSlots     = t'
          }
      | n'             <- andId shrink numCoreNodes
      , t'             <- andId shrink numSlots
      , let adjustedTop = truncateNodeTopology nodeTopology n'
      , top'           <- andId shrinkNodeTopology adjustedTop
      ]

{-------------------------------------------------------------------------------
  Configuring tests for a specific block type
-------------------------------------------------------------------------------}

-- | Test configuration that depends on the block (incl the ledger and\/or
-- protocol) but not on the monad
--
-- Some fields do not explicitly involve the @blk@ type, but their semantics
-- (at least their validity) does depend on the semantics of the underlying
-- ledger and\/or protocol. For example, 'nodeJoinPlan' is here instead of in
-- 'TestConfig' because different blocks can withstand different degrees of
-- absence/lateness. And 'epochSize' is here because eg the Byron ledger
-- assumes a fixed epoch size of @10k@. And so on.
data TestConfigB blk = TestConfigB
  { forgeEbbEnv  :: Maybe (ForgeEbbEnv blk)
  , future       :: Future
  , messageDelay :: CalcMessageDelay blk
  , nodeJoinPlan :: NodeJoinPlan
  , nodeRestarts :: NodeRestarts
  , txGenExtra   :: TxGenExtra blk
  , version      :: (NodeToNodeVersion, BlockNodeToNodeVersion blk)
  }

deriving instance (Show (TxGenExtra blk), Show (BlockNodeToNodeVersion blk))
                => Show (TestConfigB blk)

-- | Test configuration that depends on the block and the monad
--
-- The primary motivation for separating this type from 'TestConfigB' is so
-- that 'TestConfigB' can occur in contexts (such as in 'PropGeneralArgs') for
-- which the @m@ parameter is irrelevant and hence unknown.
data TestConfigMB m blk = TestConfigMB
  { nodeInfo :: CoreNodeId -> TestNodeInitialization m blk
  , mkRekeyM :: Maybe (m (RekeyM m blk))
    -- ^ 'runTestNetwork' immediately runs this action once in order to
    -- initialize an 'RekeyM' value that it then reuses throughout the test
  }

{-------------------------------------------------------------------------------
   Running tests
-------------------------------------------------------------------------------}

-- | Thin wrapper around 'runThreadNetwork'
--
runTestNetwork ::
  forall blk.
     ( RunNode blk
     , TxGen blk
     , TracingConstraints blk
     , HasCallStack
     )
  => TestConfig
  -> TestConfigB blk
  -> (forall m. IOLike m => TestConfigMB m blk)
  -> TestOutput blk
runTestNetwork TestConfig
  { numCoreNodes
  , numSlots
  , nodeTopology
  , initSeed
  } TestConfigB
  { forgeEbbEnv
  , future
  , messageDelay
  , nodeJoinPlan
  , nodeRestarts
  , txGenExtra
  , version = (networkVersion, blockVersion)
  }
    mkTestConfigMB
  = runSimOrThrow $ do
    setCurrentTime dawnOfTime
    let TestConfigMB
          { nodeInfo
          , mkRekeyM
          } = mkTestConfigMB
    let systemTime =
            BTime.defaultSystemTime
              (BTime.SystemStart dawnOfTime)
              nullTracer
    runThreadNetwork systemTime ThreadNetworkArgs
      { tnaForgeEbbEnv  = forgeEbbEnv
      , tnaFuture       = future
      , tnaJoinPlan     = nodeJoinPlan
      , tnaMessageDelay = messageDelay
      , tnaNodeInfo     = nodeInfo
      , tnaNumCoreNodes = numCoreNodes
      , tnaNumSlots     = numSlots
      , tnaSeed         = initSeed
      , tnaMkRekeyM     = mkRekeyM
      , tnaRestarts     = nodeRestarts
      , tnaTopology     = nodeTopology
      , tnaTxGenExtra   = txGenExtra
      , tnaVersion      = networkVersion
      , tnaBlockVersion = blockVersion
      }

{-------------------------------------------------------------------------------
  Test properties
-------------------------------------------------------------------------------}

-- | Data about a node rejecting a block as invalid
--
data BlockRejection blk = BlockRejection
  { brBlockHash :: !(HeaderHash blk)
  , brBlockSlot :: !SlotNo
  , brReason    :: !(ExtValidationError blk)
  , brRejector  :: !NodeId
  }
  deriving (Show)

data PropGeneralArgs blk = PropGeneralArgs
  { pgaBlockProperty       :: blk -> Property
    -- ^ test if the block is as expected
    --
    -- For example, it may fail if the block includes transactions that should
    -- have expired before/when the block was forged.
    --
  , pgaCountTxs            :: blk -> Word64
    -- ^ the number of transactions in the block
    --
  , pgaExpectedCannotForge :: SlotNo -> NodeId -> WrapCannotForge blk -> Bool
    -- ^ whether this 'CannotForge' was expected
    --
  , pgaFirstBlockNo        :: BlockNo
    -- ^ the block number of the first proper block on the chain
    --
    -- At time of writing this comment... For example, this is 1 for Byron
    -- tests and 0 for mock tests. The epoch boundary block (EBB) in slot 0
    -- specifies itself as having block number 0, which implies the genesis
    -- block is block number 0, and so the first proper block is number 1. For
    -- the mock tests, the first proper block is block number 0.
    --
  , pgaFixedMaxForkLength  :: Maybe NumBlocks
    -- ^ the maximum length of a unique suffix among the final chains
    --
    -- If not provided, it will be crudely estimated. For example, this
    -- estimation is known to be incorrect for PBFT; it does not anticipate
    -- 'Ouroboros.Consensus.Protocol.PBFT.PBftExceededSignThreshold'.
    --
  , pgaFixedSchedule       :: Maybe LeaderSchedule
    -- ^ the leader schedule of the nodes
    --
    -- If not provided, it will be recovered from the nodes' 'Tracer' data.
    --
  , pgaSecurityParam       :: SecurityParam
  , pgaTestConfig          :: TestConfig
  , pgaTestConfigB         :: TestConfigB blk
  }

-- | Expect no 'CannotForge's
noExpectedCannotForges :: SlotNo -> NodeId -> WrapCannotForge blk -> Bool
noExpectedCannotForges _ _ _ = False

-- | The properties always required
--
-- Assumes: /Synchrony/ ie (long) chains diffuse to all connected nodes before
-- the onset of the next slot.
--
-- Includes:
--
-- * The competitive chains at the end of the simulation respect the expected
--   bound on fork length
--
-- * The nodes do not leak file handles
--
-- * Blocks are exchanged without unexpected delays.
--
-- * The nodes' chains grow without unexpected delays.
--
-- * No nodes are unduly unable to lead (see 'pgaExpectedCannotForge').

-- * No blocks are rejected as invalid.
--
-- Those properties are currently checked under several assumptions. If the
-- nodes violate any of these assumptions, the tests will fail. The following
-- are the most primary assumptions.
--
-- Late Join Assumption: TODO! Only Ouroboros Genesis is designed to have nodes
-- join with "empty" (or even "stale") chains, so we do not strictly have a
-- spec for how the net should handle such a join under general circumstances.
-- (Note that we have not implemented Genesis yet.) For now, we make stop-gap
-- assumptions.
--
--   * Stop-Gap Assumption 0: Nodes join at the onset of a slot.
--
--   * Stop-Gap Assumption 1: A node joins with the "empty" chain (eg this
--     effectively includes the epoch 0 EBB for Byron).
--
--   * Stop-Gap Assumption 2: If a node leads a slot, then, at the slot onset,
--     its block production thread wins the race against the ChainSync and
--     BlockFetch client threads.
--
--   * Stop-Gap Assumption 3: If the old nodes (ie those that had joined in
--     previous slots) have a chain of greater than k blocks, then their
--     ChainSync client will throw ForkTooDeep :: ChainSyncClientException when
--     a new node joins. If any have such a chain, they all do, by the
--     Synchrony Assumption (see below). Thus all MiniProtocol clients on all
--     old nodes that share an edge with the new node in the topology will
--     disconnect from the new node until *exactly* the onset of the next slot.
--
-- Synchrony Assumption: Every (longest) chain diffuses to all other nodes
-- before the onset of the next slot.
--
--   * Corollary: Every node should have already selected one of the longest
--     chains in the net by the onset of each slot.
--
--   * Exception to the Corollary: A node cannot select such a chain if every
--     path between it and every recent leader is temporarily shutdown due to a
--     non-fatal MiniProtocol exception (such as ForkTooDeep discussed above).
--
-- Joining Leader Assumption: By Stop-Gap Assumption 2, a node that leads the
-- slot in which it joins forges a block atop the "empty" chain (ie origin or
-- else the epoch 0 EBB). Such a unary chain will be immediately discarded
-- unless there is no longer chain in the net.
--
--   * Remark: Under the current assumptions, that is the only way the selected
--     chains in the (P)BFT tests can diverge: if a node joins a net in which
--     the longest chain has only 1 block then it will introduce another
--     longest chain. As soon as any node forges a chain with 2 blocks, all
--     existing nodes will select that chain and will remain in consensus
--     thereafter. Once that happens, a node that joins in a later slot will
--     initially have the empty chain and might forge and briefly select its
--     own 1 block chain, but it also will have selected the single longest
--     chain by the end of the slot.
--
--   * Remark: Thus the (P)BFT tests only ever rollback to the "empty" chain,
--     and this will rollback at most 1 non-EBB block.
--
-- Online Assumption: Nodes are always online. In particular, a node always
-- forges a block at the onset of a slot it leads.
--
--   * Clarification: The test suite currently anticipates only two situations
--     in which a node will be unable to forge a valid block:
--     PBftExceededSignThreshold and PBftNotGenesisDelegate. If the node fails
--     to forge for any other reason, the tests will fail.
--
--   * Remark: Recall that a node leading the slot it joins usually wastes its
--     leadership by immediately forging a chain with 1 non-EBB block.
--
-- The above assumptions provide the (P)BFT tests enough information to predict
-- the net's behavior in each slot, except for identifiable cases in which that
-- behavior ultimately depends on how a node happens to select one among
-- multiple longest chains; see the PBFT reference simulator
-- "Test.ThreadNet.Ref.PBFT".
--
-- Specific tests make additional assumptions, eg the @Byron@ tests make
-- assumptions about delegation certificates, update proposals, etc.
prop_general ::
  forall blk.
     ( Condense blk
     , Condense (HeaderHash blk)
     , Eq blk
     , RunNode blk
     )
  => PropGeneralArgs blk
  -> TestOutput blk
  -> Property
prop_general = prop_general_internal Sync

-- | /Synchrony/ or /Semi-synchrony/
--
-- /Synchrony/ is characterized by every (relevant) message arriving during the
-- same slot in which it was sent. The Ouroboros research papers instead
-- characterize /semi-synchrony/ by a constant @Δ@ that bounds the number of
-- slots that it takes for any (relevant) message to arrive under " nominal "
-- circumstances (ie undersea cables have not been cut). Synchrony corresponds
-- to @Δ=1@ (ie " before the next slot ").
--
-- The net strictly cannot know @Δ@, but it can strive towards some value as an
-- objective eg.
data Synchronicity = SemiSync | Sync

-- | Like 'prop_general' but instead assuming /semi-synchrony/
--
-- For now, this simply disables a few 'Property's that depend on synchrony.
prop_general_semisync ::
  forall blk.
     ( Condense blk
     , Condense (HeaderHash blk)
     , Eq blk
     , RunNode blk
     )
  => PropGeneralArgs blk
  -> TestOutput blk
  -> Property
prop_general_semisync = prop_general_internal SemiSync

prop_general_internal ::
  forall blk.
     ( Condense blk
     , Condense (HeaderHash blk)
     , Eq blk
     , RunNode blk
     )
  => Synchronicity
  -> PropGeneralArgs blk
  -> TestOutput blk
  -> Property
prop_general_internal syncity pga testOutput =
    counterexample ("nodeChains: " <> nodeChainsString) $
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("nodeRestarts: " <> condense nodeRestarts) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    counterexample ("slot-node-tipBlockNo: " <> condense tipBlockNos) $
    counterexample ("mbSchedule: " <> condense mbSchedule) $
    counterexample ("growth schedule: " <> condense growthSchedule) $
    counterexample ("actual leader schedule: " <> condense actualLeaderSchedule) $
    counterexample ("consensus expected: " <> show isConsensusExpected) $
    counterexample ("maxForkLength: " <> show maxForkLength) $
    tabulateSync "consensus expected" [show isConsensusExpected] $
    tabulate "k" [show (maxRollbacks k)] $
    tabulate ("shortestLength (k = " <> show (maxRollbacks k) <> ")")
      [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    tabulate "involves >=1 re-delegation" [show hasNodeRekey] $
    tabulate "average #txs/block" [show (range averageNumTxs)] $
    tabulate "updates" [unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeUpdates))] $
    prop_no_BlockRejections .&&.
    prop_no_unexpected_CannotForges .&&.
    prop_no_invalid_blocks .&&.
    propSync
      ( prop_all_common_prefix maxForkLength (Map.elems nodeChains) .&&.
        prop_all_growth .&&.
        prop_no_unexpected_message_delays
      ) .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeDBs
      | (nid, nodeDBs) <- Map.toList nodeOutputDBs ]
  where
    tabulateSync  = case syncity of
        Sync     -> tabulate
        SemiSync -> \_ _ -> id
    propSync prop = case syncity of
        Sync     -> prop
        SemiSync -> property True

    _ = keepRedundantConstraint (Proxy @(Show (LedgerView (BlockProtocol blk))))

    PropGeneralArgs
      { pgaBlockProperty       = prop_valid_block
      , pgaCountTxs            = countTxs
      , pgaExpectedCannotForge = expectedCannotForge
      , pgaFirstBlockNo        = firstBlockNo
      , pgaFixedMaxForkLength  = mbMaxForkLength
      , pgaFixedSchedule       = mbSchedule
      , pgaSecurityParam       = k
      , pgaTestConfig
      , pgaTestConfigB
      } = pga
    TestConfig
      { numSlots
      , nodeTopology
      } = pgaTestConfig
    TestConfigB
      { nodeJoinPlan
      , nodeRestarts
      } = pgaTestConfigB
    TestOutput
      { testOutputNodes
      , testOutputTipBlockNos
      } = testOutput

    prop_no_BlockRejections =
        counterexample msg $
        null brs
      where
        msg =
            "There were unexpected block rejections: " <>
            unlines (map show brs)
        brs =
            [ BlockRejection
                { brBlockHash = h
                , brBlockSlot = s
                , brRejector  = nid
                , brReason    = err
                }
            | (nid, no) <- Map.toList testOutputNodes
            , let NodeOutput{nodeOutputInvalids} = no
            , (RealPoint s h, errs) <- Map.toList nodeOutputInvalids
            , err                   <- errs
            ]

    prop_no_unexpected_CannotForges =
        counterexample msg $
        Map.null cls
      where
        msg = "There were unexpected CannotForges: " <> show cls
        cls =
            Map.unionsWith (++) $
            [ Map.filter (not . null) $
              Map.mapWithKey (\s -> filter (not . ok s nid)) $
              nodeOutputCannotForges
            | (nid, no) <- Map.toList testOutputNodes
            , let NodeOutput{nodeOutputCannotForges} = no
            ]
        ok s nid cl =
            expectedCannotForge s nid (WrapCannotForge cl)

    schedule = case mbSchedule of
        Nothing    -> actualLeaderSchedule
        Just sched -> sched

    NumBlocks maxForkLength = case mbMaxForkLength of
      Nothing -> determineForkLength k nodeJoinPlan schedule
      Just fl -> fl

    -- build a leader schedule which includes every node that forged unless:
    --
    -- * the node rejected its own new block (eg 'PBftExceededSignThreshold')
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
              (actuallyLead cid (Map.keysSet nodeOutputInvalids))
              nodeOutputForges
        | (cid, no) <- Map.toList testOutputNodes
        ]
      where
        actuallyLead ::
             NodeId
          -> Set (RealPoint blk)
          -> SlotNo
          -> blk
          -> Maybe [CoreNodeId]
        actuallyLead nid invalids s b = do
            cid <- case nid of
                CoreId i  -> Just i
                RelayId _ -> Nothing

            let j = nodeIdJoinSlot nodeJoinPlan nid
            guard $ j <= s

            guard $ not $ Set.member (blockRealPoint b) invalids

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
    nodeUpdates   = nodeOutputUpdates    <$> testOutputNodes

    nodeChainsString :: String
    nodeChainsString =
        unlines $ ("" :) $
        map (\x -> "  " <> condense x) $
        Map.toList $ fmap MockChain.headTip nodeChains

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
        fmap (\(SlotNo i, _) -> (4 * i) `div` t) $
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

        prop_growth :: (SlotNo, WithOrigin BlockNo)
                    -> (SlotNo, WithOrigin BlockNo)
                    -> Property
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

            BlockNo d = case (b1, b2) of
                          (NotOrigin b1', NotOrigin b2') -> b2' - b1'
                          (Origin,        NotOrigin b2') -> b2' + 1
                          (Origin,        Origin)        -> 0
                          (NotOrigin _,   Origin)        -> error "prop_growth: negative growth"
            numActiveSlots =
                Map.size $
                flip Map.filterWithKey (getLeaderSchedule growthSchedule) $
                \slot ls -> s1 <= slot && slot < s2 && (not . null) ls

        -- @(s, min, max)@ the minimum and maximum block number of the tip of a
        -- chain at the onset of slot @s@.
        extrema :: [(SlotNo, WithOrigin BlockNo, WithOrigin BlockNo)]
        extrema =
            [ case map snd bnos' of
                  [] -> (slot, Origin, Origin)
                  o  -> (slot, minimum o, maximum o)
            | (slot, bnos) <- tipBlockNos
            , let bnos' = filter (joinedBefore slot . fst) bnos
            ]

        joinedBefore slot nid = nodeIdJoinSlot nodeJoinPlan nid < slot

    -- swizzled 'testOutputTipBlockNos'
    tipBlockNos :: [(SlotNo, [(NodeId, WithOrigin BlockNo)])]
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
              RealPoint sendSlot hsh ->
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
            delayOK1 = firstBlockNo == bno

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

    hasNodeRekey :: Bool
    hasNodeRekey =
        NodeRekey `Set.member` (foldMap . foldMap) Set.singleton m
      where
        NodeRestarts m = nodeRestarts

    -- Average number of txs/block
    averageNumTxs :: Double
    averageNumTxs =
          average
        . map (fromIntegral . countTxs)
        . concatMap MockChain.toOldestFirst
        $ Map.elems nodeChains
      where
        average :: [Double] -> Double
        average [] = 0
        average xs = sum xs / fromIntegral (length xs)

    -- The 'prop_valid_block' argument could, for example, check for no expired
    -- transactions.
    prop_no_invalid_blocks :: Property
    prop_no_invalid_blocks = conjoin $
        [ counterexample
            ("In slot " <> condense s <> ", node " <> condense nid) $
          counterexample ("forged an invalid block " <> condense blk) $
          prop_valid_block blk
        | (nid, NodeOutput{nodeOutputForges}) <- Map.toList testOutputNodes
          -- checking all forged blocks, even if they were never or only
          -- temporarily selected.
        , (s, blk) <- Map.toAscList nodeOutputForges
        ]

{-------------------------------------------------------------------------------
  Final chains properties
-------------------------------------------------------------------------------}

-- | What was the most number of /signed/ blocks needed to be dropped from a
-- final chain in order to reach the final chains' common prefix?
--
-- NOTE: This count excludes EBBs.
calcFinalIntersectionDepth :: forall blk. (BA.HasHeader blk)
                           => PropGeneralArgs blk
                           -> TestOutput blk
                           -> NumBlocks
calcFinalIntersectionDepth pga testOutput =
    NumBlocks $ unBlockNo $
    case (MockChain.headBlockNo commonPrefix, maxLength) of
      (BA.Origin,       BA.Origin)      -> 0
      (BA.Origin,       BA.NotOrigin b) -> 1 + b - pgaFirstBlockNo
      (BA.NotOrigin{},  BA.Origin)      -> error "impossible"
      (BA.NotOrigin cp, BA.NotOrigin b) ->
          assert (b >= cp) $   -- guaranteed by the foldl below
          b - cp
  where
    PropGeneralArgs{pgaFirstBlockNo} = pga
    TestOutput{testOutputNodes}      = testOutput

    -- length of longest chain
    maxLength    :: BA.WithOrigin BlockNo
    -- the common prefix
    commonPrefix :: MockChain.Chain blk
    (maxLength, commonPrefix) =
        case map prj $ Map.toList testOutputNodes of
          []   -> (BA.Origin, MockChain.Genesis)
          x:xs -> foldl combine x xs
      where
        prj (_nid, NodeOutput{nodeOutputFinalChain}) = (d, c)
          where
            d = MockChain.headBlockNo nodeOutputFinalChain
            c = nodeOutputFinalChain

        combine (dl, cl) (dr, cr) = (max dl dr, chainCommonPrefix cl cr)

-- | All final chains have the same block number
prop_inSync :: forall blk. (BA.HasHeader blk)
            => TestOutput blk -> Property
prop_inSync testOutput =
    counterexample (show lengths) $
    counterexample "the nodes' final chains have different block numbers" $
    property $
    case lengths of
      []   -> False
      l:ls -> all (== l) ls
  where
    TestOutput{testOutputNodes} = testOutput

    -- the length of each final chain
    lengths :: [BA.WithOrigin BlockNo]
    lengths =
        [ MockChain.headBlockNo nodeOutputFinalChain
        | (_nid, no) <- Map.toList testOutputNodes
        , let NodeOutput{nodeOutputFinalChain} = no
        ]
