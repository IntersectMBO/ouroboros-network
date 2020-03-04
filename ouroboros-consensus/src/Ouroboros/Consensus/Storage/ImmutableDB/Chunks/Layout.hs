{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Layout of individual chunks on disk
--
-- This module is not re-exported from the public Chunks API, since it's only
-- relevant internally in the immutable DB.
module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout (
    -- * Relative slots
    RelativeSlot -- Opaque
  , maxRelativeSlot
  , relativeSlotIsEBB
  , nthRelativeSlot
  , firstRelativeSlot
  , NextRelativeSlot(..)
  , nextRelativeSlot
  , unsafeNextRelativeSlot
    -- * Chunks
  , chunkIndexOfSlot
    -- * Slots within a chunk
  , ChunkSlot(..)
  , pattern ChunkSlot
    -- ** Translation /to/ 'ChunkSlot'
  , chunkSlotForUnknownBlock
  , chunkSlotForRegularBlock
  , chunkSlotForBoundaryBlock
  , chunkSlotForBlockOrEBB
    -- ** Translation /from/ 'ChunkSlot'
  , chunkSlotToSlot
  , chunkSlotToBlockOrEBB
    -- ** Support for EBBs
  , slotNoOfEBB
  , slotMightBeEBB
  , slotNoOfBlockOrEBB
  ) where

import           Control.Monad
import           Data.Functor.Identity
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)
import qualified Cardano.Slotting.EpochInfo as EI
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block.EBB
import           Ouroboros.Consensus.Storage.ImmutableDB.Types (BlockOrEBB (..))

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
relativeSlotIsEBB MkRelativeSlot{..}
  | relativeSlotIndex == 0 = IsEBB
  | otherwise              = IsNotEBB

-- | The @n@'th relative slot
--
-- NOTE: @relativeSlotIsEBB (nthRelativeSlot chunk size 0)@.
nthRelativeSlot :: (HasCallStack, Integral a)
                => ChunkInfo -> ChunkNo -> a -> RelativeSlot
nthRelativeSlot ci chunk = mkRelativeSlot ci chunk . fromIntegral

-- | The first relative slot
--
-- NOTE: @relativeSlotIsEBB (firstRelativeSlot chunk size)@
firstRelativeSlot :: ChunkInfo -> ChunkNo -> RelativeSlot
firstRelativeSlot ci chunk = mkRelativeSlot ci chunk 0

-- | Result of 'nextRelativeSlot'
data NextRelativeSlot =
    -- | There is a next negative slot
    NextRelativeSlot RelativeSlot

    -- | We reached the end of the chunk
    --
    -- We record the 'ChunkSize' for ease of reference
  | NoMoreRelativeSlots ChunkSize

-- | Next relative slot
nextRelativeSlot :: HasCallStack => RelativeSlot -> NextRelativeSlot
nextRelativeSlot s@MkRelativeSlot{..} =
    -- Assert that the /current/ value is within bounds
    assertWithinBounds relativeSlotIndex relativeSlotChunkSize $
      if relativeSlotIndex == maxRelativeIndex relativeSlotChunkSize
        then NoMoreRelativeSlots relativeSlotChunkSize
        else NextRelativeSlot $ s { relativeSlotIndex = succ relativeSlotIndex }

-- | Variation on 'nextRelativeSlot' where the caller /knows/ that there must
-- be a next slot
--
-- Throws an assertion failure (if assertions are enabled) if there is no
-- next slot.
unsafeNextRelativeSlot :: HasCallStack => RelativeSlot -> RelativeSlot
unsafeNextRelativeSlot s@MkRelativeSlot{..} =
    assertWithinBounds (succ relativeSlotIndex) relativeSlotChunkSize $
      s { relativeSlotIndex = succ relativeSlotIndex }

{-------------------------------------------------------------------------------
  Chucks
-------------------------------------------------------------------------------}

chunkIndexOfSlot :: ChunkInfo -> SlotNo -> ChunkNo
chunkIndexOfSlot ci = unsafeEpochNoToChunkNo . epochInfoEpoch ci

{-------------------------------------------------------------------------------
  Slot within an epoch
-------------------------------------------------------------------------------}

-- | Uniquely identity a block within the immutable DB
--
-- Constructor marked as 'Unsafe'; construction should normally happen inside
-- this module only (though see the 'ChunkSlot' pattern synonym).
data ChunkSlot = UnsafeChunkSlot
  { chunkIndex    :: !ChunkNo
  , chunkRelative :: !RelativeSlot
  } deriving (Eq, Generic, NoUnexpectedThunks)

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
chunkSlotForRegularBlock ci (SlotNo absSlot) =
    UnsafeChunkSlot{..}
  where
    epochIndex    = epochInfoEpoch ci (SlotNo absSlot)
    chunkIndex    = unsafeEpochNoToChunkNo epochIndex
    SlotNo first  = epochInfoFirst ci epochIndex
    chunkRelative = mkRelativeSlot ci chunkIndex (absSlot - first + 1)

-- | Chunk slot for EBB
chunkSlotForBoundaryBlock :: ChunkInfo -> EpochNo -> ChunkSlot
chunkSlotForBoundaryBlock ci epochIndex =
    UnsafeChunkSlot chunkIndex $ firstRelativeSlot ci chunkIndex
  where
    chunkIndex = unsafeEpochNoToChunkNo epochIndex

-- | Chunk slot for 'BlockOrEBB'
chunkSlotForBlockOrEBB :: ChunkInfo -> BlockOrEBB -> ChunkSlot
chunkSlotForBlockOrEBB ci = \case
    Block slot  -> chunkSlotForRegularBlock  ci slot
    EBB   epoch -> chunkSlotForBoundaryBlock ci epoch

{-------------------------------------------------------------------------------
  Translation /from/ 'ChunkSlot'
-------------------------------------------------------------------------------}

-- | From relative to absolute slot
--
-- This can be used for EBBs and regular blocks, since they don't share a
-- relative slot.
chunkSlotToSlot :: ChunkInfo -> ChunkSlot -> SlotNo
chunkSlotToSlot chunkInfo (ChunkSlot chunk relSlot) =
    -- TODO: Use of unsafe idioms in these definitons aren't very important,
    -- as these will change entirely once we give a proper implementation of
    -- 'ChunkInfo'.
    let SlotNo first = epochInfoFirst chunkInfo (unsafeChunkNoToEpochNo chunk)
    -- EBB and first block share the first slot
    in SlotNo $ if relativeSlotIndex relSlot == 0
                  then first
                  else first + relativeSlotIndex relSlot - 1

chunkSlotToBlockOrEBB :: ChunkInfo -> ChunkSlot -> BlockOrEBB
chunkSlotToBlockOrEBB chunkInfo chunkSlot@(ChunkSlot chunk relSlot) =
    case relativeSlotIsEBB relSlot of
      IsEBB    -> EBB   $ unsafeChunkNoToEpochNo chunk
      IsNotEBB -> Block $ chunkSlotToSlot chunkInfo chunkSlot

{-------------------------------------------------------------------------------
  Support for EBBs
-------------------------------------------------------------------------------}

slotNoOfEBB :: ChunkInfo -> EpochNo -> SlotNo
slotNoOfEBB ci = chunkSlotToSlot ci . chunkSlotForBoundaryBlock ci

slotMightBeEBB :: ChunkInfo -> SlotNo -> Maybe EpochNo
slotMightBeEBB ci slot = do
    guard $ relativeSlotIndex (chunkRelative ifRegular) == 1
    return $ unsafeChunkNoToEpochNo $ chunkIndex ifRegular
  where
    ifRegular = chunkSlotForRegularBlock ci slot

slotNoOfBlockOrEBB :: ChunkInfo -> BlockOrEBB -> SlotNo
slotNoOfBlockOrEBB _  (Block slot)  = slot
slotNoOfBlockOrEBB ci (EBB   epoch) = slotNoOfEBB ci epoch

{-------------------------------------------------------------------------------
  TODO: Temporary: emulate the EpochInfo interface

  These are not exported.
-------------------------------------------------------------------------------}

epochInfoFirst :: ChunkInfo -> EpochNo -> SlotNo
epochInfoFirst ci = runIdentity . EI.epochInfoFirst (unwrapEpochInfo ci)

epochInfoEpoch :: ChunkInfo -> SlotNo -> EpochNo
epochInfoEpoch ci = runIdentity . EI.epochInfoEpoch (unwrapEpochInfo ci)
