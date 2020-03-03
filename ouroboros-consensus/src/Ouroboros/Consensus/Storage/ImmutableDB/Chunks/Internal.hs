{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo(..)
  , simpleChunkInfo
    -- * Chunk number
  , ChunkNo(..)
  , firstChunkNo
  , chunkNoToInt
  , chunkNoFromInt
  , nextChunkNo
  , prevChunkNo
  , countChunks
  , chunksBetween
  , unsafeEpochNoToChunkNo
  , unsafeChunkNoToEpochNo
    -- * Chunk size
  , ChunkSize(..)
  , getChunkSize
    -- * Layout
  , RelativeSlot(..)
  ) where

import           Control.Monad
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
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- 'ChunkSize' is an opaque type in the public API, as its interpretation is
-- confusing: a chunk of @ChunkSize n@ can actually contain @n + 1@ blocks:
-- @n@ regular blocks and one EBB.
newtype ChunkSize = ChunkSize { unChunkSize :: Word64 }
  deriving newtype (Show)

-- | Chunk number
newtype ChunkNo = ChunkNo { unChunkNo :: Word64 }
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, NoUnexpectedThunks)

-- | First chunk
firstChunkNo :: ChunkNo
firstChunkNo = ChunkNo 0

-- | Convert 'ChunkNo' to 'Int'
--
-- This is primarily useful for the immutable DB, which uses an 'IntPSQ'.
chunkNoToInt :: ChunkNo -> Int
chunkNoToInt (ChunkNo n) = fromIntegral n

-- | Convert 'Int' to 'ChunkNo'
--
-- See 'chunkNoToInt' for motivation.
chunkNoFromInt :: Int -> ChunkNo
chunkNoFromInt n = ChunkNo (fromIntegral n)

nextChunkNo :: ChunkNo -> ChunkNo
nextChunkNo (ChunkNo n) = ChunkNo (n + 1)

prevChunkNo :: ChunkNo -> Maybe ChunkNo
prevChunkNo (ChunkNo n) = guard (n > 0) >> return (ChunkNo $ n - 1)

-- | Count number of chunks between two indices
--
-- > countChunks x              x  == 0
-- > countChunks x (nextChunkNo x) == 1
countChunks :: ChunkNo -> ChunkNo -> Word64
countChunks (ChunkNo a) (ChunkNo b) = if a >= b then a - b else b - a

-- | Enumerate all chunks
--
-- > chunksBetween x              x  == [x]
-- > chunksBetween x (nextChunkNo x) == [x, nextChunkNo x]
chunksBetween :: ChunkNo -> ChunkNo -> [ChunkNo]
chunksBetween (ChunkNo a) (ChunkNo b) = map ChunkNo $
                                          if a >= b then [a .. b] else [b .. a]

-- | Translate 'EpochNo' to 'ChunkNo'
--
-- This should /ONLY/ be used to translate the 'EpochNo' of an EBB, since the
-- invariant says EBBs can only exist in the first period of the DB, where the
-- chunk size must equal the epoch size. See 'ChunkInfo' for details.
unsafeEpochNoToChunkNo :: EpochNo -> ChunkNo
unsafeEpochNoToChunkNo (EpochNo n) = ChunkNo n

-- | Translate 'ChunkNo' to 'EpochNo'
--
-- This should /ONLY/ be used for chunks that contain EBBs.
-- See 'unsafeEpochNoToChunkNo' and 'ChunkInfo' for details.
unsafeChunkNoToEpochNo :: ChunkNo -> EpochNo
unsafeChunkNoToEpochNo (ChunkNo n) = EpochNo n

getChunkSize :: ChunkInfo -> ChunkNo -> ChunkSize
getChunkSize ci = ChunkSize
                . unEpochSize
                . runIdentity
                . EI.epochInfoSize (unwrapEpochInfo ci)
                . EpochNo
                . unChunkNo

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
