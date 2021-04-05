{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Layout of individual chunks on disk
--
-- This module is not re-exported from the public Chunks API, since it's only
-- relevant internally in the immutable DB.
module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout (
    -- * Relative slots
    NextRelativeSlot (..)
  , firstBlockOrEBB
  , maxRelativeSlot
  , nextRelativeSlot
  , nthBlockOrEBB
  , relativeSlotIsEBB
  , unsafeNextRelativeSlot
    -- ** Opaque
  , RelativeSlot
    -- * Chunks
  , chunkIndexOfSlot
    -- * Slots within a chunk
  , ChunkSlot (..)
  , pattern ChunkSlot
    -- ** Translation /to/ 'ChunkSlot'
  , chunkSlotForBlockOrEBB
  , chunkSlotForBoundaryBlock
  , chunkSlotForRegularBlock
  , chunkSlotForRelativeSlot
  , chunkSlotForTip
  , chunkSlotForUnknownBlock
    -- ** Translation /from/ 'ChunkSlot'
  , chunkSlotToBlockOrEBB
  , chunkSlotToSlot
    -- ** Support for EBBs
  , slotMightBeEBB
  , slotNoOfBlockOrEBB
  , slotNoOfEBB
  ) where

import           Control.Monad
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.Storage.ImmutableDB.API (Tip (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
                     (BlockOrEBB (..))

-- Most types in the Chunks interface are opaque in the public API, since their
-- interpretation is subject to layout decisions. In this module we /make/ those
-- layout decisions, however, and so here we need access to the internal types.
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal

{-------------------------------------------------------------------------------
  Relative slots
-------------------------------------------------------------------------------}

-- | The last relative slot within a chunk of the given size
maxRelativeSlot :: ChunkInfo -> ChunkNo -> RelativeSlot
maxRelativeSlot ci chunk =
    mkRelativeSlot ci chunk (maxRelativeIndex size)
  where
    size = getChunkSize ci chunk

-- | Is this relative slot reserved for an EBB?
relativeSlotIsEBB :: RelativeSlot -> IsEBB
relativeSlotIsEBB RelativeSlot{..}
  | relativeSlotIndex == 0
  , chunkCanContainEBB relativeSlotChunkSize
  = IsEBB
  | otherwise
  = IsNotEBB

-- | The @n@'th relative slot for an arbitrary block
--
-- NOTE: Offset @0@ refers to an EBB only if the 'ChunkSize' supports it.
nthBlockOrEBB :: (HasCallStack, Integral a)
              => ChunkInfo -> ChunkNo -> a -> RelativeSlot
nthBlockOrEBB ci chunk = mkRelativeSlot ci chunk . fromIntegral

-- | The first relative slot
--
-- NOTE: This refers to an EBB only if the 'ChunkSize' supports it.
firstBlockOrEBB :: ChunkInfo -> ChunkNo -> RelativeSlot
firstBlockOrEBB ci chunk = mkRelativeSlot ci chunk 0

-- | Result of 'nextRelativeSlot'
data NextRelativeSlot =
    -- | There is a next negative slot
    NextRelativeSlot RelativeSlot

    -- | We reached the end of the chunk
  | NoMoreRelativeSlots

-- | Next relative slot
nextRelativeSlot :: HasCallStack => RelativeSlot -> NextRelativeSlot
nextRelativeSlot s@RelativeSlot{..} =
    -- Assert that the /current/ value is within bounds
    assertWithinBounds relativeSlotIndex relativeSlotChunkSize $
      if relativeSlotIndex == maxRelativeIndex relativeSlotChunkSize
        then NoMoreRelativeSlots
        else NextRelativeSlot $ s { relativeSlotIndex = succ relativeSlotIndex }

-- | Variation on 'nextRelativeSlot' where the caller /knows/ that there must
-- be a next slot
--
-- Throws an assertion failure (if assertions are enabled) if there is no
-- next slot.
unsafeNextRelativeSlot :: HasCallStack => RelativeSlot -> RelativeSlot
unsafeNextRelativeSlot s@RelativeSlot{..} =
    assertWithinBounds (succ relativeSlotIndex) relativeSlotChunkSize $
      s { relativeSlotIndex = succ relativeSlotIndex }

{-------------------------------------------------------------------------------
  Chucks
-------------------------------------------------------------------------------}

chunkIndexOfSlot :: ChunkInfo -> SlotNo -> ChunkNo
chunkIndexOfSlot (UniformChunkSize ChunkSize{..}) (SlotNo slot) = ChunkNo $
    slot `div` numRegularBlocks

{-------------------------------------------------------------------------------
  Slot within an epoch
-------------------------------------------------------------------------------}

-- | Uniquely identify a block within the immutable DB
--
-- Constructor marked as 'Unsafe'; construction should normally happen inside
-- this module only (though see the 'ChunkSlot' pattern synonym).
data ChunkSlot = UnsafeChunkSlot
  { chunkIndex    :: !ChunkNo
  , chunkRelative :: !RelativeSlot
  } deriving (Eq, Generic, NoThunks)

-- | We provide a manual 'Ord' instance because 'RelativeSlot' does not
-- (and cannot) define one. By comparing the 'chunkIndex' before the index here,
-- we establish the precondition to 'compareRelativeSlot'.
instance Ord ChunkSlot where
  compare a b = mconcat [
      compare             (chunkIndex    a) (chunkIndex    b)
    , compareRelativeSlot (chunkRelative a) (chunkRelative b)
    ]

{-# COMPLETE ChunkSlot #-}
pattern ChunkSlot :: ChunkNo -> RelativeSlot -> ChunkSlot
pattern ChunkSlot index relative <- UnsafeChunkSlot index relative

instance Show ChunkSlot where
  show (ChunkSlot e s) = show (unChunkNo e, relativeSlotIndex s)

{-------------------------------------------------------------------------------
  Translation /to/ 'ChunkSlot'
-------------------------------------------------------------------------------}

-- | Chunk slot for an unknown block
--
-- This returns /two/ 'ChunkSlot's: one in case the block could be an EBB,
-- and one in case the block is a regular block. In addition, it also returns
-- the 'ChunkNo' that both of these 'ChunkSlot's must necessarily share.
chunkSlotForUnknownBlock :: HasCallStack
                         => ChunkInfo
                         -> SlotNo
                         -> (ChunkNo, Maybe ChunkSlot, ChunkSlot)
chunkSlotForUnknownBlock ci slot = (
      (case mIfBoundary of
         Nothing         -> id
         Just ifBoundary -> assertSameChunk (chunkIndex ifBoundary)
                                            (chunkIndex ifRegular)) $
        chunkIndex ifRegular
    , mIfBoundary
    , ifRegular
    )
  where
    ifRegular   = chunkSlotForRegularBlock  ci slot
    mIfBoundary = chunkSlotForBoundaryBlock ci <$> slotMightBeEBB ci slot

-- | Chunk slot for a regular block (i.e., not an EBB)
chunkSlotForRegularBlock :: ChunkInfo -> SlotNo -> ChunkSlot
chunkSlotForRegularBlock (UniformChunkSize sz@ChunkSize{..}) (SlotNo slot) =
    UnsafeChunkSlot {
        chunkIndex    = ChunkNo chunk
      , chunkRelative = RelativeSlot (ChunkNo chunk) sz $
                          if chunkCanContainEBB
                            then withinChunk + 1
                            else withinChunk
      }
  where
    (chunk, withinChunk) = slot `divMod` numRegularBlocks

-- | Chunk slot for EBB
chunkSlotForBoundaryBlock :: HasCallStack => ChunkInfo -> EpochNo -> ChunkSlot
chunkSlotForBoundaryBlock ci epoch =
    assertChunkCanContainEBB chunk size $
      UnsafeChunkSlot chunk $ firstBlockOrEBB ci chunk
  where
    chunk = unsafeEpochNoToChunkNo epoch
    size  = getChunkSize ci chunk

-- | Chunk slot for 'BlockOrEBB'
chunkSlotForBlockOrEBB :: ChunkInfo -> BlockOrEBB -> ChunkSlot
chunkSlotForBlockOrEBB ci = \case
    Block slot  -> chunkSlotForRegularBlock  ci slot
    EBB   epoch -> chunkSlotForBoundaryBlock ci epoch

-- | Chunk slot for 'Tip'
chunkSlotForTip :: ChunkInfo -> Tip blk -> ChunkSlot
chunkSlotForTip ci Tip { tipSlotNo, tipIsEBB } = case tipIsEBB of
    IsNotEBB -> chunkSlotForRegularBlock ci tipSlotNo
    IsEBB    -> assertChunkCanContainEBB chunkIndex relativeSlotChunkSize $
                 UnsafeChunkSlot chunkIndex $ firstBlockOrEBB ci chunkIndex
  where
    UnsafeChunkSlot{..} = chunkSlotForRegularBlock ci tipSlotNo
    RelativeSlot{..}    = chunkRelative

chunkSlotForRelativeSlot :: ChunkNo -> RelativeSlot -> ChunkSlot
chunkSlotForRelativeSlot chunk relSlot =
    assertSameChunk (relativeSlotChunkNo relSlot) chunk $
      UnsafeChunkSlot chunk relSlot

{-------------------------------------------------------------------------------
  Translation /from/ 'ChunkSlot'

  Reminder:

  * EBB shares its slot number with its successor
  * EBB shares its block number with its predecessor
-------------------------------------------------------------------------------}

-- | From relative to absolute slot
--
-- This can be used for EBBs and regular blocks, since they don't share a
-- relative slot.
chunkSlotToSlot :: ChunkInfo -> ChunkSlot -> SlotNo
chunkSlotToSlot (UniformChunkSize ChunkSize{..}) UnsafeChunkSlot{..} = SlotNo $
      chunk * numRegularBlocks
    + case (chunkCanContainEBB, relativeSlotIndex) of
        (_    , 0) -> 0
        (True , n) -> n - 1
        (False, n) -> n
  where
    ChunkNo chunk    = chunkIndex
    RelativeSlot{..} = chunkRelative

chunkSlotToBlockOrEBB :: ChunkInfo -> ChunkSlot -> BlockOrEBB
chunkSlotToBlockOrEBB chunkInfo chunkSlot@(ChunkSlot chunk relSlot) =
    case relativeSlotIsEBB relSlot of
      IsEBB    -> EBB   $ unsafeChunkNoToEpochNo chunk
      IsNotEBB -> Block $ chunkSlotToSlot chunkInfo chunkSlot

{-------------------------------------------------------------------------------
  Support for EBBs
-------------------------------------------------------------------------------}

slotNoOfEBB :: HasCallStack => ChunkInfo -> EpochNo -> SlotNo
slotNoOfEBB ci = chunkSlotToSlot ci . chunkSlotForBoundaryBlock ci

slotMightBeEBB :: ChunkInfo -> SlotNo -> Maybe EpochNo
slotMightBeEBB ci slot = do
    guard $ chunkCanContainEBB relativeSlotChunkSize && relativeSlotIndex == 1
    return $ unsafeChunkNoToEpochNo chunkIndex
  where
    UnsafeChunkSlot{..} = chunkSlotForRegularBlock ci slot
    RelativeSlot{..}    = chunkRelative

slotNoOfBlockOrEBB :: ChunkInfo -> BlockOrEBB -> SlotNo
slotNoOfBlockOrEBB _  (Block slot)  = slot
slotNoOfBlockOrEBB ci (EBB   epoch) = slotNoOfEBB ci epoch
