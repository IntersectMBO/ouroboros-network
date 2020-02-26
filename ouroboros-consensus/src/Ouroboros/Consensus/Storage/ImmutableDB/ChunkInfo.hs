{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- TODO: Temporary:
{-# OPTIONS -Wwarn #-}

module Ouroboros.Consensus.Storage.ImmutableDB.ChunkInfo (
    ChunkInfo -- Opaque
  , simpleChunkInfo
    -- * Chunk size
  , ChunkSize -- Opaque
  , getChunkSize
    -- * Layout: relative slots
  , RelativeSlot(..)
  , unsafeNextRelativeSlot
  , minRelativeSlot
  , maxRelativeSlot
  , relativeSlotForEBB
  , relativeSlotIsEBB
    -- * Layout: chunks
  , ChunkSlot(..)
  , chunkSlotForRegularBlock
  , chunkSlotForBoundaryBlock
  , chunkSlotForBlock
  , chunkSlotToSlotNo
  , chunkSlotIsEBB
    -- * Utility: working with EBBs
  , slotMightBeEBB
  , slotNoOfEBB
  , slotNoOfBlock
    -- * Emulate EpochInfo interface (TODO: temporary)
  --, epochInfoFirst
  --, epochInfoEpoch
  ) where

import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI

import           Ouroboros.Consensus.Block.EBB
import           Ouroboros.Consensus.Storage.ImmutableDB.Types (BlockOrEBB (..))

-- | Number of blocks per file in the immutable DB
--
-- Assumption: EBBs can /only/ appear in chunks of the /first/ 'ChunkSize' in
-- 'ChunkInfo', and moreover, that chunk size must be equal to the 'EpochSize'
-- of the corresponding epochs. In other words, those chunks must line up
-- exactly with epochs (no such requirement is present when EBBs don't appear).
--
-- This is an internal type, we export some convenience constructors only.
newtype ChunkInfo = ChunkInfo (EpochInfo Identity)
  deriving newtype (NoUnexpectedThunks, Show)

-- | Single uniform chunk size
simpleChunkInfo :: Word64 -> ChunkInfo
simpleChunkInfo = ChunkInfo . EI.fixedSizeEpochInfo . EpochSize

{-------------------------------------------------------------------------------
  Chunk size

  TODO: These 'EpochNo's are really 'ChunkNo's.
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- This is an opaque type! The reason is that the interpretation of the
-- chunk size is a bit confusing: the number of available slots within a single
-- chunk is the chunk size /plus one/, since we always make space for an EBB.
newtype ChunkSize = ChunkSize Word64
  deriving (Show)

getChunkSize :: ChunkInfo -> EpochNo -> ChunkSize
getChunkSize (ChunkInfo ei) = ChunkSize . unEpochSize
                            . runIdentity . EI.epochInfoSize ei

{-------------------------------------------------------------------------------
  Layout: Relative slots

  NOTE: Clients of the immutable DB do not care about the layout. We nonetheless
  define it here to try and encapsulate layout decisions as much as possible
  in this one module.
-------------------------------------------------------------------------------}

-- | A /relative/ slot within an 'EpochNo'
--
-- The constructor is marked as unsafe, as any code that uses relative slots
-- directly likely becomes dependent on specific layout choices made here.
newtype RelativeSlot = UnsafeRelativeSlot { unRelativeSlot :: Word64 }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (NoUnexpectedThunks)

-- | The next relative slot
--
-- This function is marked as unsafe because moving from one relative slot
-- to the next, and then using that new relative slot to index a chunk, would
-- expose details of the layout.
unsafeNextRelativeSlot :: RelativeSlot -> RelativeSlot
unsafeNextRelativeSlot (UnsafeRelativeSlot r) = UnsafeRelativeSlot (succ r)

-- | Return the last relative slot within the given chunk
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 1, so the last relative slot is equal to the chunk size.
maxRelativeSlot :: ChunkSize -> RelativeSlot
maxRelativeSlot (ChunkSize sz) = UnsafeRelativeSlot sz

-- | The first relative slot
--
-- Semantically, this is different from 'relativeSlotForEBB', because the latter
-- makes assumptions about where we store the EBB.
minRelativeSlot :: RelativeSlot
minRelativeSlot = UnsafeRelativeSlot 0

relativeSlotForEBB :: RelativeSlot
relativeSlotForEBB = minRelativeSlot

relativeSlotIsEBB :: RelativeSlot -> IsEBB
relativeSlotIsEBB (UnsafeRelativeSlot 0) = IsEBB
relativeSlotIsEBB _                      = IsNotEBB

{-------------------------------------------------------------------------------
  Layout: chunks
-------------------------------------------------------------------------------}

-- | The combination of an 'EpochNo' and a 'RelativeSlot' within the chunk.
--
-- This type can safely be exported, since unlike 'RelativeSlot', 'ChunkSlot'
-- does not expose any /within/ chunk layout.
--
-- TODO: (Like everywhere) This should be 'ChunkNo' not 'EpochNo'.
data ChunkSlot = ChunkSlot {
      chunkNo   :: !EpochNo
    , chunkSlot :: !RelativeSlot
    }
  deriving (Eq, Ord, Generic, NoUnexpectedThunks)

instance Show ChunkSlot where
  show (ChunkSlot (EpochNo e) (UnsafeRelativeSlot s)) = show (e, s)

chunkSlotForRegularBlock :: ChunkInfo -> SlotNo -> ChunkSlot
chunkSlotForRegularBlock ci (SlotNo absSlot) =
    let epoch        = epochInfoEpoch ci (SlotNo absSlot)
        SlotNo first = epochInfoFirst ci epoch
    in ChunkSlot epoch (UnsafeRelativeSlot (absSlot - first + 1))

chunkSlotForBoundaryBlock :: EpochNo -> ChunkSlot
chunkSlotForBoundaryBlock epoch =
    ChunkSlot epoch relativeSlotForEBB

chunkSlotForBlock :: ChunkInfo -> BlockOrEBB -> ChunkSlot
chunkSlotForBlock ci = \case
    Block s -> chunkSlotForRegularBlock ci s
    EBB   e -> chunkSlotForBoundaryBlock   e

-- | From relative to absolute slot
--
-- This can be used for EBBs and regular blocks, since they don't share a
-- relative slot
chunkSlotToSlotNo :: ChunkInfo -> ChunkSlot -> SlotNo
chunkSlotToSlotNo ci (ChunkSlot epoch (UnsafeRelativeSlot relSlot)) =
    let SlotNo first = epochInfoFirst ci epoch
    -- EBB and first block share the first slot
    in SlotNo $ if relSlot == 0 then first
                                else first + relSlot - 1

-- | Is the given 'ChunkSlot' reserved for an EBB?
chunkSlotIsEBB :: ChunkSlot -> Maybe EpochNo
chunkSlotIsEBB (ChunkSlot epoch relSlot) =
    case relativeSlotIsEBB relSlot of
      IsEBB    -> Just epoch
      IsNotEBB -> Nothing

{-------------------------------------------------------------------------------
  Utility: working with EBBs
-------------------------------------------------------------------------------}

-- | Might the specified 'Slot' be the slot of an EBB?
slotMightBeEBB :: ChunkInfo -> SlotNo -> Maybe EpochNo
slotMightBeEBB ci slot
    -- 'chunkSlotForRegularBlock' always assumes the given 'SlotNo' refers to a
    -- regular block and will return 1 when given an EBB.
  | relSlot == UnsafeRelativeSlot 1 = Just epoch
  | otherwise                       = Nothing
  where
    ChunkSlot epoch relSlot = chunkSlotForRegularBlock ci slot

slotNoOfEBB :: ChunkInfo -> EpochNo -> SlotNo
slotNoOfEBB ci = chunkSlotToSlotNo ci . chunkSlotForBoundaryBlock

slotNoOfBlock :: ChunkInfo -> BlockOrEBB -> SlotNo
slotNoOfBlock ci = \case
    Block s -> s
    EBB   e -> slotNoOfEBB ci e

{-------------------------------------------------------------------------------
  TODO: Temporary:
  Emulate the EpochInfo interface
-------------------------------------------------------------------------------}

epochInfoFirst :: ChunkInfo -> EpochNo -> SlotNo
epochInfoFirst (ChunkInfo ei) = runIdentity . EI.epochInfoFirst ei

epochInfoEpoch :: ChunkInfo -> SlotNo -> EpochNo
epochInfoEpoch (ChunkInfo ei) = runIdentity . EI.epochInfoEpoch ei
