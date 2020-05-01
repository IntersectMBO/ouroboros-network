{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended for qualified import
--
-- > import Test.Util.WrappedClock (WrappedClock(..))
-- > import qualified Test.Util.WrappedClock as WrappedClock
module Test.Util.WrappedClock (
    WrappedClock(..)
  , NumSlots(..)
    -- * Wrap 'LogicalClock' API
  , new
  , getCurrentSlot
  , waitUntilDone
  , onSlotChange
  , onSlot
  , blockUntilSlot
    -- * Conversions
  , blockchainTime
  ) where

import           Data.Coerce
import           Data.Word
import           GHC.Stack
import           Test.QuickCheck (Arbitrary (..))
import qualified Test.QuickCheck as QC

import           Ouroboros.Consensus.BlockchainTime (SlotLength (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Cardano.Slotting.Slot

import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Test.Util.LogicalClock (LogicalClock)
import qualified Test.Util.LogicalClock as L

-- | Wrapper around 'LogicalClock' that pretends 'Tick' == 'SlotNo'
--
-- The only reason this type exists is to ease the transition from our innocent
-- youth in which we thought the current slot was always known to the harsh
-- reality where it turns out the current slot is ledger state dependent. We
-- should endeavour to reduce to the scope of the 'WrappedClock' as much as
-- possible (unwrapping it to use the nested 'LogicalClock'), and eventually
-- remove this type altogether. Indeed, if we want to test our test across the
-- hard fork we will have no choice (unless we pick the same slot length I
-- guess).
data WrappedClock m = WrappedClock {
      unwrapLogicalClock :: LogicalClock m
    }

{-------------------------------------------------------------------------------
  Wrap 'LogicalClock' API
-------------------------------------------------------------------------------}

-- | Number of slots
newtype NumSlots = NumSlots Word64
  deriving (Show)

new :: (IOLike m, HasCallStack)
    => ResourceRegistry m -> NumSlots -> SlotLength -> m (WrappedClock m)
new rr (NumSlots n) slotLen =
    WrappedClock <$> L.newWithDelay rr (L.NumTicks n) (getSlotLength slotLen)

getCurrentSlot :: MonadSTM m => WrappedClock m -> STM m SlotNo
getCurrentSlot (WrappedClock clock) = coerce <$> L.getCurrentTick clock

waitUntilDone :: WrappedClock m -> m ()
waitUntilDone (WrappedClock clock) = L.waitUntilDone clock

onSlotChange :: (IOLike m, HasCallStack)
             => ResourceRegistry m
             -> WrappedClock m
             -> String            -- ^ Label for the thread
             -> (SlotNo -> m ())  -- ^ Action to execute
             -> m (m ())
onSlotChange rr (WrappedClock clock) label action =
    L.onEachTick rr clock label (action . coerce)

blockUntilSlot :: IOLike m => WrappedClock m -> SlotNo -> m Bool
blockUntilSlot (WrappedClock clock) = L.blockUntilTick clock . coerce

onSlot :: (HasCallStack, IOLike m)
       => ResourceRegistry m
       -> WrappedClock m
       -> String
       -> SlotNo
       -> m ()
       -> m ()
onSlot rr (WrappedClock clock) label = L.onTick rr clock label . coerce

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

blockchainTime :: MonadSTM m => WrappedClock m -> BTime.BlockchainTime m
blockchainTime clock = BTime.BlockchainTime {
      BTime.getCurrentSlot = BTime.CurrentSlot <$> getCurrentSlot clock
    }

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> QC.choose (minNumSlots, 100)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)

minNumSlots :: Word64
minNumSlots = 1
