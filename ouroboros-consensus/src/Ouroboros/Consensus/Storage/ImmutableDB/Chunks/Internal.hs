{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo(..)
  , simpleChunkInfo
  , singleChunkInfo
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
  , maxRelativeIndex
  , mkRelativeSlot
  , assertRelativeSlotInChunk
  , compareRelativeSlot
    -- * Assertions
  , ChunkAssertionFailure
  , assertSameChunk
  , assertWithinBounds
  ) where

import           Control.Exception (Exception, throw)
import           Control.Monad
import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Util.RedundantConstraints

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

-- | 'ChunkInfo' for a single 'ChunkSize'
--
-- See also 'simpleChunkInfo'.
--
-- TODO: This definition should change once we modify 'ChunkSize' to record
-- whether or not EBBs are present.
singleChunkInfo :: ChunkSize -> ChunkInfo
singleChunkInfo (ChunkSize n) = simpleChunkInfo (EpochSize n)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- 'ChunkSize' is an opaque type in the public API, as its interpretation is
-- confusing: a chunk of @ChunkSize n@ can actually contain @n + 1@ blocks:
-- @n@ regular blocks and one EBB.
--
-- TODO: Replace by the definition below.
newtype ChunkSize = ChunkSize { unChunkSize :: Word64 }
  deriving stock   (Show, Eq, Generic)
  deriving newtype (NoUnexpectedThunks)

{-
-- | Size of a chunk
--
-- The total number of slots available in a chunk is equal to 'numRegularBlocks'
-- if @not@ 'chunkCanContainEBB', and 'numRegularBlocks' @+ 1@ otherwise.
data ChunkSize = ChunkSize {
      -- | Does this chunk also accomodate an EBB?
      chunkCanContainEBB :: Bool

      -- | The number of regular blocks in this chunk
    , numRegularBlocks   :: Word64
    }
  deriving stock (Show)
-}

-- | Chunk number
newtype ChunkNo = ChunkNo { unChunkNo :: Word64 }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Show, NoUnexpectedThunks)

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
data RelativeSlot = MkRelativeSlot {
    -- | The chunk index of the chunk this slot is in
    --
    -- Recorded primarily to be able to define a semi-sensible 'Ord' instance.
    relativeSlotChunkNo   :: !ChunkNo

    -- | The size of the chunk that this slot is in
    --
    -- We record this for bounds checking as well as to be able to answer
    -- questions such as 'relativeSlotIsEBB'.
  , relativeSlotChunkSize :: !ChunkSize

    -- | The index within the chunk
  , relativeSlotIndex     :: !Word64
  }
  deriving stock    (Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Maximum relative index within a chunk
--
-- Relative slot 0 is reserved for the EBB and regular relative slots start at
-- 1, so the last relative slot is equal to the chunk size.
maxRelativeIndex :: ChunkSize -> Word64
maxRelativeIndex (ChunkSize n) = n

-- | Smart constructor for 'RelativeSlot'
mkRelativeSlot :: HasCallStack => ChunkInfo -> ChunkNo -> Word64 -> RelativeSlot
mkRelativeSlot chunkInfo chunk index =
    assertWithinBounds index size $
    MkRelativeSlot {
        relativeSlotChunkNo   = chunk
      , relativeSlotChunkSize = size
      , relativeSlotIndex     = index
      }
  where
    size = getChunkSize chunkInfo chunk

instance Eq RelativeSlot where
  a == b
    | relativeSlotChunkNo a /= relativeSlotChunkNo b = False
    | otherwise =
        -- If the 'ChunkNo's are the same, then the 'ChunkSize's /must/ also be
        assertSameChunk (relativeSlotChunkNo a) (relativeSlotChunkNo b) $
          relativeSlotIndex a == relativeSlotIndex b

-- | 'RelativeSlot' is partially ordered, not totally ordered
--
-- It makes no sense to compare 'RelativeSlots' from different chunks. Doing so
-- will result in an assertion failure.
compareRelativeSlot :: HasCallStack => RelativeSlot -> RelativeSlot -> Ordering
compareRelativeSlot a b =
    assertSameChunk (relativeSlotChunkNo a) (relativeSlotChunkNo b) $
      compare (relativeSlotIndex a) (relativeSlotIndex b)

assertRelativeSlotInChunk :: HasCallStack => ChunkNo -> RelativeSlot -> Word64
assertRelativeSlotInChunk chunk relSlot =
    assertSameChunk (relativeSlotChunkNo relSlot) chunk $
      relativeSlotIndex relSlot

{-------------------------------------------------------------------------------
  Assert failures

  We insist on keeping the HasCallStack constraint here, because if we make
  that constraint depend on CPP, we will get redundant constraint warnings for
  any functions that (transitively) call these functions.
-------------------------------------------------------------------------------}

data ChunkAssertionFailure =
    NotSameChunk ChunkNo ChunkNo CallStack
  | NotWithinBounds Word64 ChunkSize CallStack
  deriving (Show)

instance Exception ChunkAssertionFailure

assertSameChunk :: HasCallStack => ChunkNo -> ChunkNo -> a -> a
#if ENABLE_ASSERTIONS
assertSameChunk a b
  | a == b    = id
  | otherwise = throw $ NotSameChunk a b callStack
#else
assertSameChunk _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)

assertWithinBounds :: HasCallStack => Word64 -> ChunkSize -> a -> a
#if ENABLE_ASSERTIONS
assertWithinBounds ix sz
  | ix <= maxRelativeIndex sz = id
  | otherwise                 = throw $ NotWithinBounds ix sz callStack
#else
assertWithinBounds _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)
