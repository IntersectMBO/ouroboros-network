{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo(..)
  , simpleChunkInfo
    -- * Queries
  , getChunkSize
    -- * Emulation of the EpochInfo interface
  , epochInfoFirst
  , epochInfoEpoch
  ) where

import           Data.Functor.Identity
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
-- TODO: This should not use 'EpochSize'.
simpleChunkInfo :: EpochSize -> ChunkInfo
simpleChunkInfo sz = WrapEpochInfo sz $ EI.fixedSizeEpochInfo sz

{-------------------------------------------------------------------------------
  Queries

  TODO: EpochNo here should be replaced by ChunkNo
-------------------------------------------------------------------------------}

-- TODO: EpochSize should become ChunkSize
getChunkSize :: ChunkInfo -> EpochNo -> EpochSize
getChunkSize = epochInfoSize

{-------------------------------------------------------------------------------
  TODO: Temporary: emulate the EpochInfo interface
-------------------------------------------------------------------------------}

epochInfoSize :: ChunkInfo -> EpochNo -> EpochSize
epochInfoSize ci = runIdentity . EI.epochInfoSize (unwrapEpochInfo ci)

epochInfoFirst :: ChunkInfo -> EpochNo -> SlotNo
epochInfoFirst ci = runIdentity . EI.epochInfoFirst (unwrapEpochInfo ci)

epochInfoEpoch :: ChunkInfo -> SlotNo -> EpochNo
epochInfoEpoch ci = runIdentity . EI.epochInfoEpoch (unwrapEpochInfo ci)
