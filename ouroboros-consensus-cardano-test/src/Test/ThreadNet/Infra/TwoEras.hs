{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

-- | Definitions used in ThreadNet tests that involve two eras.
module Test.ThreadNet.Infra.TwoEras (
    -- * Generators
    Partition (..)
  , genNonce
  , genPartition
  , genTestConfig
    -- * Era inspection
  , ReachesEra2 (..)
  , activeSlotCoeff
  , isFirstEraBlock
  , ledgerReachesEra2
  , mkMessageDelay
  , numFirstEraEpochs
  , partitionExclusiveUpperBound
  , secondEraOverlaySlots
  , shelleyEpochSize
    -- * Properties
  , label_ReachesEra2
  , label_hadActiveNonOverlaySlots
  , prop_ReachesEra2
  , tabulateFinalIntersectionDepth
  , tabulatePartitionDuration
  , tabulatePartitionPosition
  ) where

import           Control.Exception (assert)
import           Data.Functor ((<&>))
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Test.QuickCheck

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..))

import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History.Util as Util
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId

import           Data.SOP.Strict (NS (..))
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock (..),
                     OneEraBlock (..))

import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (unEpochSlots)

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.OverlaySchedule as SL

import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Network (CalcMessageDelay (..), NodeOutput (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import qualified Test.ThreadNet.Util.NodeTopology as Topo
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

-- | When and for how long the nodes are partitioned
--
-- The nodes are divided via message delays into two sub-networks by the parity
-- of their 'CoreNodeId'.
data Partition = Partition SlotNo NumSlots
  -- ^ the scheduled start slot and duration (which includes the start slot)
  deriving (Show)

partitionExclusiveUpperBound :: Partition -> SlotNo
partitionExclusiveUpperBound (Partition s (NumSlots dur)) =
    Util.addSlots dur s

genNonce :: Gen SL.Nonce
genNonce = frequency
      [ (1, pure SL.NeutralNonce)
      , (9, SL.mkNonceFromNumber <$> arbitrary)
      ]

-- | Generate a 'setupTestConfig' relevant to the case where the first era (eg
-- Byron) lasts for one epoch and the second era (eg Shelley) lasts for an
-- interesting number of slots.
genTestConfig :: SecurityParam -> (EpochSize, EpochSize) -> Gen TestConfig
genTestConfig k (EpochSize epochSize1, EpochSize epochSize2) = do
    initSeed <- arbitrary

    numSlots <- do
      let wiggle = min epochSize1 (2 * maxRollbacks k)

          approachSecondEra     =
              choose (0, wiggle)     <&> \t -> epochSize1 + t - wiggle
          reachSecondEra        =
              choose (1, epochSize2) <&> \t -> epochSize1 + t
          reachThirdEpochOfSecondEra =
              choose (1, epochSize2) <&> \t -> epochSize1 + 2 * epochSize2 + t

      fmap NumSlots $ frequency $
        [ (05, approachSecondEra)
        , (64, reachSecondEra)
        , (31, reachThirdEpochOfSecondEra)
        ]

    -- This test has more slots than most, so we de-emphasize the relatively
    -- expensive n=5 case. For example:
    --
    -- > 250 tests with 30% 2, 30% 3, 30% 4, and 10% 5 took 500s
    -- > 250 tests with 100% 5 took 1000s
    ncn <- frequency [(9, choose (2, 4)), (1, pure 5)]

    -- Ensure that each partition class is internally connected.
    nodeTopology <- do
      topo0   <- Topo.genNodeTopology $ NumCoreNodes ncn
      oddTopo <- do
        -- eg nodes 1 3 for ncn = 5
        topo <- Topo.genNodeTopology $ NumCoreNodes $ div ncn 2
        let rename (CoreNodeId i) = CoreNodeId (2 * i + 1)
        pure $ Topo.mapNodeTopology rename topo
      evenTopo <- do
        -- eg nodes 0 2 4 for ncn = 5
        topo <- Topo.genNodeTopology $ NumCoreNodes $ ncn - div ncn 2
        let rename (CoreNodeId i) = CoreNodeId (2 * i)
        pure $ Topo.mapNodeTopology rename topo
      pure $
        Topo.unionNodeTopology evenTopo $
        Topo.unionNodeTopology oddTopo $
        topo0

    pure TestConfig
      { initSeed
      , nodeTopology
      , numCoreNodes = NumCoreNodes ncn
      , numSlots
      }

-- | Generate 'setupPartition'
genPartition :: NumCoreNodes -> NumSlots -> SecurityParam -> Gen Partition
genPartition (NumCoreNodes n) (NumSlots t) (SecurityParam k) = do
    let ultimateSlot :: Word64
        ultimateSlot = assert (t > 0) $ t - 1

        crop :: Word64 -> Word64
        crop s = min ultimateSlot s

    -- Fundamental plan: all @MsgRollForward@ sent during the partition only
    -- arrive at the onset of slot after it. The partition begins at the onset
    -- of @firstSlotIn@ and ends at the onset of @firstSlotAfter@.

    -- FACT (A) The leader of @firstSlotAfter@ will forge before fully reacting
    -- to the @MsgRollForward@s that just arrived, due to a race in the test
    -- infrastructure that is consistently won by the forge. Thus the two
    -- class's unique extensions consist of the blocks forged in the slots from
    -- @firstSlotIn@ to @firstSlotAfter@ /inclusive/.

    -- @w@ is defined below this large comment to be how long the partition
    -- should last (this may be 'crop'ped farther below if the partition ends
    -- up near the end of the test)
    --
    -- Because of FACT (A), we limit the partition duration so that at least
    -- one of the two partition classes will forge /strictly/ /less/ /than/ @k@
    -- such blocks. While both classes would be able to rollback if we let them
    -- forge @k@ such blocks, they might (TODO "might" or "will"?) not able to
    -- /decide/ that they need to rollback, since the relevant ChainSync client
    -- might (TODO will?) not be able to forecast a ledger view far-enough into
    -- the future to validate the @k+1@st header from the other class after the
    -- partition ends. It will remain unaware that a longer chain exists and
    -- end up stuck on its own distinct chain. Hence it's a Common Prefix
    -- violation.
    --
    -- We therefore motivate an upper bound on the partition duration @w@ for a
    -- net with @n@ nodes by considering the two variables' parities.
    --
    -- o For @n = 2h@ nodes and @w = 2u@ slots, the classes respectively forge
    --   @u@ and @u+1@ blocks. (The @+1@th block is forged in
    --   @firstSlotAfter@.) The preceding paragraph requires @u < k@ requires
    --   @u <= k-1@ requires @w <= 2k-2@. So @w <= 2k-2@ when @w@ is even.
    --
    -- o For @n = 2h@ nodes and @w = 2u+1@ slots, the classes both forge @u+1@
    --   blocks. (The second @+1@th block is forged in @firstSlotAfter@.) The
    --   preceding paragraph requires @u+1 < k@ requires @u <= k-2@ requires @w
    --   <= 2k-3@. So @w <= 2k-3@ when @w@ is odd. Note that @w <= 2k-2@ is
    --   equivalent since @w@ is assumed odd.
    --
    -- o For @n = 2h+1@ nodes, the smaller class forges at most the number of
    --   blocks considered in the above cases for even @n@, so this case
    --   doesn't contribute anything novel to the upper bound. The smaller
    --   class has less than half of the nodes and so will violate Chain Growth
    --   /if/ /given/ /enough/ /time/ /alone/. But for Byron at least, the
    --   logic for avoiding a Common Prefix violation already establishes a
    --   sufficiently strict upper bound to preclude a Chain Growth violation,
    --   since Chain Growth isn't technically violated until @2k@ slots have
    --   passed (it's also relevant that the net forges at full round-robin
    --   speed until the partition starts). (TODO does that cover Shelley too?)
    --
    -- Thus @w@ can range from @0@ to @2k-2@ inclusive.
    w <- assert (k > 0) $ choose (0, 2 * k - 2)

    -- Each of the the following milestones happens at the listed slot in the
    -- absence of a partition.
    --
    -- o In slots 0 and 1 the proposal and the votes confirm it
    --
    -- o In slot 2k+1 the confirmation becomes stable
    --
    -- o In slot 2k+1+quorum the nodes endorse it
    --
    -- o In slot 4k+1+quorum the endorsement is stable
    --
    -- o In slot 10k the update is adopted.
    --
    -- We are most interested in what happens when the partition is near these
    -- milestones.
    mbFocus <- do
      let quorum = div n 2   -- in the right ballpark
      frequency $
        map (\(wgt, mbDur) -> (wgt, pure mbDur)) $
        filter (maybe True (< t) . snd)
          [ (5,  Nothing)
          , (1,  Just 0)
          , (1,  Just $ 2 * k + 1)
          , (1,  Just $ 2 * k + 1 + quorum)
          , (1,  Just $ 4 * k + 1)
          , (1,  Just $ 4 * k + 1 + quorum)
          , (20, assert (numFirstEraEpochs == (1 :: Int)) $
                 Just $ byronEpochSize (SecurityParam k))
          ]

    -- Position the partition so that it at least abuts the focus slot.
    firstSlotIn <- choose $
      case mbFocus of
        Nothing    -> (0,                            ultimateSlot    )
        Just focus -> (crop $ focus `monus` (w + 1), crop $ focus + 1)

    let -- Because of FACT (A), we require there to be at least one slot after
        -- the partition. This doesn't ensure full consensus, because the block
        -- forged in @firstSlotAfter@ may create a chain as long as that of the
        -- other partition (first era) or even multiple such chains (second
        -- era). But it does ensure that all final chains will be the same
        -- length.
        firstSlotAfter :: Word64
        firstSlotAfter = crop $ firstSlotIn + w

        dur :: Word64
        dur = Util.countSlots (SlotNo firstSlotAfter) (SlotNo firstSlotIn)

    pure $ Partition (SlotNo firstSlotIn) (NumSlots dur)

-- | Whether there was a block forged in a non-overlay slot in the second era.
--
-- This event evidences that the stake pools were correctly created and
-- delegated to.
label_hadActiveNonOverlaySlots ::
     TestOutput (HardForkBlock (era ': eras))
  -> Set SlotNo
  -> String
label_hadActiveNonOverlaySlots testOutput overlaySlots =
    show $ or $
    [ Set.notMember slot overlaySlots
    | (_nid, no) <- Map.toList testOutputNodes
    , let NodeOutput{nodeOutputForges} = no
    , (slot, blk) <- Map.toDescList nodeOutputForges
    , not $ isFirstEraBlock blk
    ]
  where
    TestOutput{testOutputNodes} = testOutput

-- | All OBFT overlay slots in the second era.
secondEraOverlaySlots ::
     NumSlots
  -> NumSlots
  -> SL.UnitInterval
  -> EpochSize
  -> Set SlotNo
secondEraOverlaySlots numSlots (NumSlots numFirstEraSlots) d secondEraEpochSize =
    Set.filter (< SlotNo t) $
    Set.unions $
    takeWhile (isJust . Set.lookupLT (SlotNo t)) $
    map overlayOffsets [0..]
  where
    NumSlots t = numSlots

    -- The overlay slots in the ith epoch of the second era.
    --
    -- TODO: This function conceptually should be simpler if we created a
    -- sufficiently accurate 'EpochInfo' and so didn't need the shift. But
    -- doing so (eg by constructing a HardFork @Summary@) is currently
    -- significantly more involved than this workaround.
    overlayOffsets :: Word64 -> Set SlotNo
    overlayOffsets i =
        -- Shift to account for the first era.
        Set.mapMonotonic (Util.addSlots numFirstEraSlots) .
        Set.fromList $
        -- Note: this is only correct if each epoch uses the same value for @d@
        SL.overlaySlots
          -- Suitable only for this narrow context
          (fixedEpochInfoFirst secondEraEpochSize (EpochNo i))
          -- notably contains setupD
          d
            -- NB 0 <-> the first epoch of the second era
          secondEraEpochSize

tabulatePartitionPosition ::
     NumSlots -> Partition -> Bool -> Property -> Property
tabulatePartitionPosition (NumSlots numFirstEraSlots) part transitions =
    tabulate "partition in or abuts era (First era, Second era)"
      [ show (inclFirstEra, inclSecondEra) ]
  where
    Partition (SlotNo firstSlotIn) (NumSlots dur) = part
    firstSlotAfter                                = firstSlotIn + dur

    inclFirstEra  =
        dur > 0 && (not transitions || firstSlotIn    <= numFirstEraSlots)
    inclSecondEra =
        dur > 0 && (transitions     && firstSlotAfter >= numFirstEraSlots)

tabulateFinalIntersectionDepth :: SecurityParam -> NumBlocks -> String -> Property -> Property
tabulateFinalIntersectionDepth k (NumBlocks finalIntersectionDepth) finalBlockEra =
    tabul "count"  (show          finalIntersectionDepth) .
    tabul "k frac" (approxFracK k finalIntersectionDepth) .
    tabul "k diff" (diffK       k finalIntersectionDepth)
  where
    lbl = "final intersection depth"
    tabul s x = tabulate
        (lbl <> ", " <> finalBlockEra <> ", " <> s)
        [x <> " blocks"]

tabulatePartitionDuration :: SecurityParam -> Partition -> Property -> Property
tabulatePartitionDuration k part =
    tabul "count"  (show          dur) .
    tabul "k frac" (approxFracK k dur)
  where
    tabul s x =
        tabulate ("partition duration, " <> s) [x <> " slots"]

    Partition _ (NumSlots dur) = part

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The active slot coefficient, @f@.
--
-- Some of these tests include epochs in the second era in which stakepools are
-- actually leading. In that case, the @k@, @d@, and @f@ parameters and the
-- length of any scheduled network partitions need to be balanced so that Common
-- Prefix violations (in particular, wedges) are extremely unlikely.
activeSlotCoeff :: Rational
activeSlotCoeff = 0.2   -- c.f. mainnet is more conservative, using 0.05

-- | The number of epochs in the first era in this test
--
-- All nodes join in slot 0, we generate the proposal in slot 0, we also
-- generate the votes in slot 0, and the nodes are endorsing the proposal as of
-- slot 0. Thus we expect that the first era will end after one epoch. Otherwise
-- it would indicate some sort of protocol failure.
numFirstEraEpochs :: Num a => a
numFirstEraEpochs = 1

{-------------------------------------------------------------------------------
  ReachesEra2 property
-------------------------------------------------------------------------------}

-- | Whether the test included second era blocks and (pre)reqs relevant to that
--
-- Note these fields are ordered alphabetically not semantically; see
-- 'label_ReachesEra2'.
data ReachesEra2 = ReachesEra2
  { rsEra1Slots  :: BoolProps.Prereq
    -- ^ enough slots in the first era to enable a block in the second era
  , rsPV         :: BoolProps.Prereq
    -- ^ sufficient protocol version to enable a block in the second era
  , rsEra2Blocks :: Bool
    -- ^ blocks from the second era included in final chains
  , rsEra2Slots  :: BoolProps.Requirement
    -- ^ enough slots in the second era to necessitate a block in the second era
  }
  deriving (Generic, Show)

instance BoolProps.CollectReqs ReachesEra2

-- | List the (pre)reqs in semantic order, followed by the observation
label_ReachesEra2 :: ReachesEra2 -> String
label_ReachesEra2 reachesEra2 =
    prepend "pv"     rsPV $
    prepend "slots1" rsEra1Slots $
    prepend "slots2" rsEra2Slots $
    "blocks2 " <> show rsEra2Blocks
  where
    -- incur a GHC warning if the number of fields changes
    ReachesEra2 _ _ _ _dummy = reachesEra2
    -- this pattern should bind each field by name
    ReachesEra2
      { rsEra1Slots
      , rsPV
      , rsEra2Blocks
      , rsEra2Slots
      } = reachesEra2

    prepend :: Show a => String -> a -> String -> String
    prepend s req x = s <> " " <> show req <> ", " <> x

-- | Is the update proposal adopted?
ledgerReachesEra2 :: ReachesEra2 -> Bool
ledgerReachesEra2 rs = BoolProps.checkReqs rs /= Just False

-- | Checks if the observation satisfies the (pre)reqs
prop_ReachesEra2 :: ReachesEra2 -> Property
prop_ReachesEra2 rs = case BoolProps.checkReqs rs of
    Nothing  -> property True
    Just req ->
        counterexample (show rs) $
        counterexample (msg req) $
        rsEra2Blocks == req
  where
    ReachesEra2{rsEra2Blocks} = rs

    msg :: Bool -> String
    msg req = if req
        then "the final chains should include at least one second era block"
        else "the final chains should not include any second era blocks"

{-------------------------------------------------------------------------------
  A short even-odd partition
-------------------------------------------------------------------------------}

-- | The temporary partition as a 'CalcMessageDelay'
--
-- Calculates the delays that implement 'setupPartition'.
mkMessageDelay :: Partition -> CalcMessageDelay blk
mkMessageDelay part = CalcMessageDelay $
    \(CoreNodeId i, CoreNodeId j) curSlot _hdr -> NumSlots $ if
      | curSlot <  firstSlotIn    -> 0
      | curSlot >= firstSlotAfter -> 0
      | mod i 2 == mod j 2        -> 0
      | otherwise                 -> unSlotNo $ firstSlotAfter - curSlot
  where
    Partition firstSlotIn _ = part
    firstSlotAfter          = partitionExclusiveUpperBound part

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

byronEpochSize :: SecurityParam -> Word64
byronEpochSize (SecurityParam k) =
    unEpochSlots $ kEpochSlots $ CC.Common.BlockCount k

shelleyEpochSize :: SecurityParam -> Word64
shelleyEpochSize k = unEpochSize $ Shelley.mkEpochSize k activeSlotCoeff

isFirstEraBlock :: HardForkBlock (era ': eras) -> Bool
isFirstEraBlock = \case
    HardForkBlock (OneEraBlock Z{}) -> True
    _                               -> False

-- | Render a number as a positive difference from @k@
--
-- PREREQUISITE: The number must not be greater than @k@.
diffK :: SecurityParam -> Word64 -> String
diffK (SecurityParam k) v =
    assert (k >= v) $
    "k - " <> show (k - v)

-- | Render a number as the nearest tenths of @k@
approxFracK :: SecurityParam -> Word64 -> String
approxFracK (SecurityParam k) v =
    "k * " <> show (fromIntegral tenths / 10 :: Double)
  where
    ratio  = toRational v / toRational k
    tenths = round (ratio * 10) :: Int

-- | <https://en.wikipedia.org/wiki/Monus>
monus :: (Num a, Ord a) => a -> a -> a
monus a b = if a <= b then 0 else a - b
