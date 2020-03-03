{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo(..)
  , simpleChunkInfo
    -- * Queries
  , ChunkSize(..)
  , getChunkSize
    -- * Layout
  , RelativeSlot(..)
  ) where

import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI
import           Cardano.Slotting.Slot

-- TODO: Temporary definition
data ChunkInfo = WrapEpochInfo {
      getSimpleChunkInfo :: EpochSize
    , unwrapEpochInfo    :: EpochInfo Identity
    }
  deriving stock (Generic)
  deriving anyclass (NoUnexpectedThunks)

-- TODO: Temporary definition
instance Show ChunkInfo where
  show ci = "(simpleChunkInfo " ++ show (getSimpleChunkInfo ci) ++ ")"

-- | Simple chunk config with a single chunk size
--
-- This intentionally takes 'EpochSize' (number of slots) rather than
-- 'ChunkSize': the translation from 'EpochSize' to 'ChunkSize' (number of
-- available entries in a chunk) should not be done by client code.
simpleChunkInfo :: EpochSize -> ChunkInfo
simpleChunkInfo sz = WrapEpochInfo sz $ EI.fixedSizeEpochInfo sz

{-------------------------------------------------------------------------------
  Queries

  TODO: EpochNo here should be replaced by ChunkNo
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- 'ChunkSize' is an opaque type in the public API, as its interpretation is
-- confusing: a chunk of @ChunkSize n@ can actually contain @n + 1@ blocks:
-- @n@ regular blocks and one EBB.
newtype ChunkSize = ChunkSize Word64
  deriving newtype (Show)

getChunkSize :: ChunkInfo -> EpochNo -> ChunkSize
getChunkSize ci = ChunkSize . unEpochSize . runIdentity . EI.epochInfoSize (unwrapEpochInfo ci)

{-------------------------------------------------------------------------------
  Layout

  These are defined in the @Internal@ module so that most code can safely
  import from "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout" without
  worrying that it's making assumptions that it shouldn't. All bets are off for
  modules that import "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal".
-------------------------------------------------------------------------------}

-- | A /relative/ slot within a chunk
newtype RelativeSlot = RelativeSlot { unRelativeSlot :: Word64 }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (NoUnexpectedThunks)
