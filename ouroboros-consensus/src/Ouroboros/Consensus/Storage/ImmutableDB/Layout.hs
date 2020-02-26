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
    -- * Derived information from 'ChunkInfo'
  , epochInfoBlockRelative
  , epochInfoAbsolute
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Storage.Common

import           Ouroboros.Consensus.Storage.ImmutableDB.ChunkInfo

{-------------------------------------------------------------------------------
  Working with relative slots
-------------------------------------------------------------------------------}

-- | The combination of an 'EpochNo' and a 'RelativeSlot' within the epoch.
data EpochSlot = EpochSlot
  { _epoch        :: !EpochNo
  , _relativeSlot :: !RelativeSlot
  } deriving (Eq, Ord, Generic, NoUnexpectedThunks)

instance Show EpochSlot where
  show (EpochSlot (EpochNo e) (RelativeSlot s)) = show (e, s)

{-------------------------------------------------------------------------------
  Derived information from 'ChunkInfo'

  These functions are defined here rather than in 'ChunkInfo' since they
  hardcode assumptions about the internal layout in the immutable DB.
-------------------------------------------------------------------------------}

-- | Relative slot for a regular block
--
-- Should NOT be used for EBBs.
epochInfoBlockRelative :: ChunkInfo -> SlotNo -> EpochSlot
epochInfoBlockRelative chunkInfo (SlotNo absSlot) =
    let epoch        = epochInfoEpoch chunkInfo (SlotNo absSlot)
        SlotNo first = epochInfoFirst chunkInfo epoch
    in EpochSlot epoch (RelativeSlot (absSlot - first + 1))

-- | From relative to absolute slot
--
-- This can be used for EBBs and regular blocks, since they don't share a
-- relative slot
epochInfoAbsolute :: ChunkInfo -> EpochSlot -> SlotNo
epochInfoAbsolute chunkInfo (EpochSlot epoch (RelativeSlot relSlot)) =
    let SlotNo first = epochInfoFirst chunkInfo epoch
    -- EBB and first block share the first slot
    in SlotNo $ if relSlot == 0 then first
                                else first + relSlot - 1
