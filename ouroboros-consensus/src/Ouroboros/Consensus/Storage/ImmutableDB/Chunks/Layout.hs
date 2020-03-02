{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Layout of individual chunks on disk
--
-- This module is not re-exported from the public Chunks API, since it's only
-- relevant internally in the immutable DB.
module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout (
    RelativeSlot(..) -- TODO: Opaque
  , maxRelativeSlot
  , relativeSlotIsEBB
  , nthRelativeSlot
  , firstRelativeSlot
  , nextRelativeSlot
  ) where

import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.EBB

-- Most types in the Chunks interface are opaque in the public API, since their
-- interpretation is subject to layout decisions. In this module we /make/ those
-- layout decisions, however, and so here we need access to the internal types.
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal

-- | A /relative/ slot within a chunk
--
-- TODO: This should be opaque.
newtype RelativeSlot = RelativeSlot { unRelativeSlot :: Word64 }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (NoUnexpectedThunks)

-- | The last relative slot within a chunk of the given size
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 1, so the last relative slot is equal to the chunk size.
maxRelativeSlot :: ChunkSize -> RelativeSlot
maxRelativeSlot (ChunkSize sz) = RelativeSlot sz

-- | Is this relative slot reserved for an EBB?
relativeSlotIsEBB :: RelativeSlot -> IsEBB
relativeSlotIsEBB (RelativeSlot s) = if s == 0 then IsEBB else IsNotEBB

-- | The @n@'th relative slot
--
-- NOTE: @relativeSlotIsEBB (nthRelativeSlot 0)@.
nthRelativeSlot :: Integral a => a -> RelativeSlot
nthRelativeSlot = RelativeSlot . fromIntegral

-- | The first relative slot
--
-- NOTE: @relativeSlotIsEBB firstRelativeSlot@
firstRelativeSlot :: RelativeSlot
firstRelativeSlot = RelativeSlot 0

-- | Next relative slot
--
-- TODO: We should record the ChunkSize along with the RelativeSlot, so that
-- we can do a bounds check here.
nextRelativeSlot :: RelativeSlot -> RelativeSlot
nextRelativeSlot (RelativeSlot s) = RelativeSlot (succ s)
