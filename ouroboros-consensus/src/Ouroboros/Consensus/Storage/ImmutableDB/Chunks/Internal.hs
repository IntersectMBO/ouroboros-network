{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo(..)
  , simpleChunkInfo
    -- * Emulation of the EpochInfo interface
  , epochInfoSize
  , epochInfoFirst
  , epochInfoEpoch
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI
import           Cardano.Slotting.Slot

-- TODO: Temporary definition
data ChunkInfo m = WrapEpochInfo {
      getSimpleChunkInfo :: EpochSize
    , unwrapEpochInfo    :: EpochInfo m
    }
  deriving stock (Generic)
  deriving anyclass (NoUnexpectedThunks)

-- TODO: Temporary definition
instance Show (ChunkInfo m) where
  show ci = "(simpleChunkInfo " ++ show (getSimpleChunkInfo ci) ++ ")"

-- | Simple chunk config with a single chunk size
--
-- TODO: This should not use 'EpochSize'.
simpleChunkInfo :: Monad m => EpochSize -> ChunkInfo m
simpleChunkInfo sz = WrapEpochInfo sz $ EI.fixedSizeEpochInfo sz

{-------------------------------------------------------------------------------
  TODO: Temporary: emulate the EpochInfo interface
-------------------------------------------------------------------------------}

epochInfoSize :: ChunkInfo m -> EpochNo -> m EpochSize
epochInfoSize = EI.epochInfoSize . unwrapEpochInfo

epochInfoFirst :: ChunkInfo m -> EpochNo -> m SlotNo
epochInfoFirst = EI.epochInfoFirst . unwrapEpochInfo

epochInfoEpoch :: ChunkInfo m -> SlotNo -> m EpochNo
epochInfoEpoch = EI.epochInfoEpoch . unwrapEpochInfo
