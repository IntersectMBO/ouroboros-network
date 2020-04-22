{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Test.ThreadNet.General (
    PropGeneralArgs (..)
  , prop_general
  , runTestNetwork
    -- * TestConfig
  , Rekeying (..)
  , TestConfig (..)
  , TestConfigBlock (..)
  , truncateNodeJoinPlan
  , truncateNodeRestarts
  , truncateNodeTopology
    -- * Block rejections
  , BlockRejection (..)
    -- * Re-exports
  , ForgeEbbEnv (..)
  , TestOutput (..)
  , plainTestNodeInitialization
  ) where

import           Control.Monad (guard)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (BlockNo (..), HasHeader, HeaderHash,
                     SlotNo (..))
import qualified Ouroboros.Network.MockChain.Chain as MockChain
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract (LedgerView,
                     SecurityParam (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..))

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Test.ThreadNet.Network
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.NoUnexpectedThunks ()
import           Test.Util.Range
import           Test.Util.Shrink (andId, dropId)
import           Test.Util.Stream
import           Test.Util.Time

{-------------------------------------------------------------------------------
  Configuring tests
-------------------------------------------------------------------------------}

data TestConfig = TestConfig
  { numCoreNodes :: !NumCoreNodes
  , numSlots     :: !NumSlots
    -- ^ TODO generate in function of @k@
  , nodeJoinPlan :: !NodeJoinPlan
  , nodeRestarts :: !NodeRestarts
  , nodeTopology :: !NodeTopology
  , slotLength   :: !SlotLength
  , initSeed     :: !Seed
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
      numCoreNodes <- arbitrary
      numSlots     <- arbitrary
      nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
      nodeTopology <- genNodeTopology numCoreNodes
      slotLength   <- arbitrary
      initSeed     <- arbitrary
      pure TestConfig
        { numCoreNodes
        , numSlots
        , nodeJoinPlan
          -- TODO how to enrich despite variable schedules?
        , nodeRestarts = noRestarts
        , nodeTopology
        , slotLength
        , initSeed
        }

  shrink TestConfig
    { numCoreNodes
    , numSlots
    , nodeJoinPlan
    , nodeRestarts
    , nodeTopology
    , slotLength
    , initSeed
    } =
      dropId $
      [ TestConfig
          { numCoreNodes = n'
          , numSlots     = t'
          , nodeJoinPlan = p'
          , nodeRestarts = r'
          , nodeTopology = top'
          , slotLength   = len'
          , initSeed
          }
      | n'             <- andId shrink numCoreNodes
      , t'             <- andId shrink numSlots
      , let adjustedP   = truncateNodeJoinPlan nodeJoinPlan n' (numSlots, t')
      , let adjustedR   = truncateNodeRestarts nodeRestarts t'
      , let adjustedTop = truncateNodeTopology nodeTopology n'
      , p'             <- andId shrinkNodeJoinPlan adjustedP
      , r'             <- andId shrinkNodeRestarts adjustedR
      , top'           <- andId shrinkNodeTopology adjustedTop
      , len'           <- andId shrink slotLength
      ]

{-------------------------------------------------------------------------------
  Configuring tests for a specific block type
-------------------------------------------------------------------------------}

data TestConfigBlock blk = TestConfigBlock
  { forgeEbbEnv :: Maybe (ForgeEbbEnv blk)
  , nodeInfo    :: CoreNodeId -> TestNodeInitialization blk
  , rekeying    :: Maybe (Rekeying blk)
  , txGenExtra  :: TxGenExtra blk
  }

data Rekeying blk = forall opKey. Rekeying
  { rekeyOracle
      :: CoreNodeId -> SlotNo -> Maybe SlotNo
    -- ^ The first /nominal/ slot after the given slot, assuming the given core
    -- node cannot lead.
  , rekeyUpd ::
         ProtocolInfo blk
      -> EpochNo
      -> opKey
      -> Maybe (TestNodeInitialization blk)
     -- ^ new config and any corresponding delegation certificate transactions
  , rekeyFreshSKs :: Stream opKey
     -- ^ a stream that only repeats itself after an *effectively* *infinite*
     -- number of iterations and also never includes an operational key from
     -- the genesis configuration
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
     )
  => TestConfig
  -> EpochSize
  -- ^ Temporary: until we start testing the hard fork combinator, we just
  -- support a single 'EpochSize'. See also comments for 'tnaEpochInfo'.
  -> TestConfigBlock blk
  -> TestOutput blk
runTestNetwork
  TestConfig
    { numCoreNodes
    , numSlots
    , nodeJoinPlan
    , nodeRestarts
    , nodeTopology
    , slotLength
    , initSeed
    }
  epochSize
  TestConfigBlock{forgeEbbEnv, nodeInfo, rekeying, txGenExtra}
  = runSimOrThrow $ do
    let tna = ThreadNetworkArgs
          { tnaForgeEbbEnv    = forgeEbbEnv
          , tnaJoinPlan       = nodeJoinPlan
          , tnaNodeInfo       = nodeInfo
          , tnaNumCoreNodes   = numCoreNodes
          , tnaNumSlots       = numSlots
          , tnaRNG            = seedToChaCha initSeed
          , tnaRekeyM         = Nothing
          , tnaRestarts       = nodeRestarts
          , tnaSlotLength     = slotLength
          , tnaTopology       = nodeTopology
          , tnaEpochSize      = epochSize
          , tnaTxGenExtra     = txGenExtra
          }

    case rekeying of
      Nothing                                -> runThreadNetwork tna
      Just Rekeying{rekeyFreshSKs, rekeyOracle, rekeyUpd} -> do
        rekeyVar <- uncheckedNewTVarM rekeyFreshSKs
        runThreadNetwork tna
          { tnaRekeyM = Just $ \cid pInfo s mkEno -> case rekeyOracle cid s of
              Nothing -> pure $ plainTestNodeInitialization pInfo
              Just s' -> do
                x <- atomically $ do
                  x :< xs <- readTVar rekeyVar
                  x <$ writeTVar rekeyVar xs
                eno <- mkEno s'
                pure $ case rekeyUpd pInfo eno x of
                  Nothing  -> plainTestNodeInitialization pInfo
                  Just tni -> tni
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
  { pgaBlockProperty          :: blk -> Property
    -- ^ test if the block is as expected
    --
    -- For example, it may fail if the block includes transactions that should
    -- have expired before/when the block was forged.
    --
  , pgaCountTxs               :: blk -> Word64
    -- ^ the number of transactions in the block
    --
  , pgaExpectedBlockRejection :: BlockRejection blk -> Bool
    -- ^ whether this block rejection was expected
    --
  , pgaFirstBlockNo           :: BlockNo
    -- ^ the block number of the first proper block on the chain
    --
    -- At time of writing this comment... For example, this is 1 for Byron
    -- tests and 0 for mock tests. The epoch boundary block (EBB) in slot 0
    -- specifies itself as having block number 0, which implies the genesis
    -- block is block number 0, and so the first proper block is number 1. For
    -- the mock tests, the first proper block is block number 0.
    --
    -- TODO This implies the mock genesis block does not have a number?
    --
  , pgaFixedMaxForkLength     :: Maybe NumBlocks
    -- ^ the maximum length of a unique suffix among the final chains
    --
    -- If not provided, it will be crudely estimated. For example, this
    -- estimation is known to be incorrect for PBFT; it does not anticipate
    -- 'Ouroboros.Consensus.Protocol.PBFT.PBftExceededSignThreshold'.
    --
  , pgaFixedSchedule          :: Maybe LeaderSchedule
    -- ^ the leader schedule of the nodes
    --
    -- If not provided, it will be recovered from the nodes' 'Tracer' data.
    --
  , pgaSecurityParam          :: SecurityParam
  , pgaTestConfig             :: TestConfig
  }

-- | The properties always required
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
-- * No blocks are unduly rejected (see 'pgaExpectedBlockRejection').
--
prop_general ::
  forall blk.
     ( Condense blk
     , Eq blk
     , HasHeader blk
     , RunNode blk
     )
  => PropGeneralArgs blk
  -> TestOutput blk
  -> Property
prop_general pga testOutput =
    counterexample ("nodeChains: " <> unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeChains))) $
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("nodeRestarts: " <> condense nodeRestarts) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    counterexample ("slot-node-tipBlockNo: " <> condense tipBlockNos) $
    counterexample ("mbSchedule: " <> condense mbSchedule) $
    counterexample ("growth schedule: " <> condense growthSchedule) $
    counterexample ("actual leader schedule: " <> condense actualLeaderSchedule) $
    counterexample ("consensus expected: " <> show isConsensusExpected) $
    counterexample ("maxForkLength: " <> show maxForkLength) $
    tabulate "consensus expected" [show isConsensusExpected] $
    tabulate "k" [show (maxRollbacks k)] $
    tabulate ("shortestLength (k = " <> show (maxRollbacks k) <> ")")
      [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    tabulate "involves >=1 re-delegation" [show hasNodeRekey] $
    tabulate "average #txs/block" [show (range averageNumTxs)] $
    prop_no_unexpected_BlockRejections .&&.
    prop_no_invalid_blocks .&&.
    prop_all_common_prefix
        maxForkLength
        (Map.elems nodeChains) .&&.
    prop_all_growth .&&.
    prop_no_unexpected_message_delays .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeDBs
      | (nid, nodeDBs) <- Map.toList nodeOutputDBs ]
  where
    _ = keepRedundantConstraint (Proxy @(Show (LedgerView (BlockProtocol blk))))

    PropGeneralArgs
      { pgaBlockProperty          = prop_valid_block
      , pgaCountTxs               = countTxs
      , pgaExpectedBlockRejection = expectedBlockRejection
      , pgaFirstBlockNo           = firstBlockNo
      , pgaFixedMaxForkLength     = mbMaxForkLength
      , pgaFixedSchedule          = mbSchedule
      , pgaSecurityParam          = k
      , pgaTestConfig
      } = pga
    TestConfig
      { numSlots
      , nodeJoinPlan
      , nodeRestarts
      , nodeTopology
      } = pgaTestConfig
    TestOutput
      { testOutputNodes
      , testOutputTipBlockNos
      } = testOutput

    prop_no_unexpected_BlockRejections =
        counterexample msg $
        Map.null blocks
      where
        msg = "There were unexpected block rejections: " <> show blocks
        blocks =
            Map.unionsWith (++) $
            [ Map.filter (not . null) $
              Map.mapWithKey (\p -> filter (not . ok p nid)) $
              nodeOutputInvalids
            | (nid, no) <- Map.toList testOutputNodes
            , let NodeOutput{nodeOutputInvalids} = no
            ]
        ok (RealPoint s h) nid err =
          -- TODO The ExtValidationError data declaration imposes this case on
          -- us but should never exercise it.
          expectedBlockRejection BlockRejection
            { brBlockHash = h
            , brBlockSlot = s
            , brReason    = err
            , brRejector  = nid
            }

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
                          (At b1', At b2') -> b2' - b1'
                          (Origin, At b2') -> b2' + 1
                          (Origin, Origin) -> 0
                          (At _,   Origin) -> error "prop_growth: negative growth"
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
