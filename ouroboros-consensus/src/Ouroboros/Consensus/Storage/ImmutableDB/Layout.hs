{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Immutable DB per-epoch layout
--
-- The chain consists of slots ('SlotNo'). Each slot may be occupied by at
-- most one block. In the 'ImmutableDB', we don't store the chain in one big
-- file, but group blocks per epoch ('EpochNo'), which are then written to an
-- epoch file. Within each epoch, the blocks are given 'RelativeSlot's. The
-- combination of an 'EpochNo' and a 'RelativeSlot' is an 'EpochSlot'. The
-- 'ImmutableDB' will need to be able to convert 'SlotNo's to 'EpochSlot's and
-- vice versa.
--
-- Additionally, each epoch may store an Epoch Boundary Block (EBB). This EBB
-- logically lives between the last slot of an epoch and the first slot of the
-- next epoch. In the 'ImmutableDB', these are stored at the beginning of each
-- epoch file, namely at relative slot 0.
--
-- For example:
--
-- > Epochs:         <──────── 0 ────────> <────── 1 ──────>
-- > Epoch size:               4                   3
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > 'RelativeSlot':   0   1   2   3   4     0   1   2   3
-- > 'SlotNo':        EBB  0   1   2   3    EBB  4   5   6
--
-- Note that the epoch size does not include the (optional) EBB.

module Ouroboros.Consensus.Storage.ImmutableDB.Layout (
    -- * Working with relative slots
    RelativeSlot(..)
  , EpochSlot(..)
  , maxRelativeSlot
    -- * Derived information from 'EpochInfo'
  , epochInfoBlockRelative
  , epochToChunkSlot
  , epochInfoAbsolute
  ) where

import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB.ChunkSize

{-------------------------------------------------------------------------------
  Working with relative slots
-------------------------------------------------------------------------------}

-- | A /relative/ slot within an 'EpochNo'.
newtype RelativeSlot = RelativeSlot { unRelativeSlot :: Word64 }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, NoUnexpectedThunks)

-- | The combination of an 'EpochNo' and a 'RelativeSlot' within the epoch.
--
-- TODO: Rename to 'ChunkSlot'
data EpochSlot = EpochSlot
  { _epoch        :: !EpochNo -- TODO: Rename to '_chunk', make 'Word64'
  , _relativeSlot :: !RelativeSlot
  } deriving (Eq, Ord, Generic, NoUnexpectedThunks)

instance Show EpochSlot where
  show (EpochSlot (EpochNo e) (RelativeSlot s)) = show (e, s)

-- | Return the last relative slot within the given epoch size.
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 0, so the last relative slot is equal to the epoch size.
maxRelativeSlot :: ChunkSize -> RelativeSlot
maxRelativeSlot (ChunkSize sz) = RelativeSlot sz

{-------------------------------------------------------------------------------
  Derived information from 'EpochInfo'

  These functions are defined here rather than in 'EpochInfo' since they
  hardcode assumptions about the internal layout in the immutable DB.
-------------------------------------------------------------------------------}

-- | Relative slot for a regular block
--
-- Should NOT be used for EBBs.
--
-- TODO: Rename to 'slotToChunkSlot'
epochInfoBlockRelative :: ChunkSize -> SlotNo -> EpochSlot
epochInfoBlockRelative (ChunkSize sz) (SlotNo absSlot) =
    let (chunk, relSlot) = absSlot `divMod` sz
    in EpochSlot (EpochNo chunk) (RelativeSlot (relSlot + 1))

-- | Relative slot for a boundary block
epochToChunkSlot :: EpochNo -> EpochSlot
epochToChunkSlot epoch = EpochSlot epoch 0

-- | From relative to absolute slot
--
-- This can be used for EBBs and regular blocks, since they don't share a
-- relative slot
--
-- TODO: Rename to 'slotFromChunkSlot'
epochInfoAbsolute :: ChunkSize -> EpochSlot -> SlotNo
epochInfoAbsolute (ChunkSize sz)
                  (EpochSlot (EpochNo epoch) (RelativeSlot relSlot)) =
    let first = sz * epoch in
    -- EBB and first block share the first slot
    SlotNo $ if relSlot == 0 then first
                             else first + relSlot - 1
