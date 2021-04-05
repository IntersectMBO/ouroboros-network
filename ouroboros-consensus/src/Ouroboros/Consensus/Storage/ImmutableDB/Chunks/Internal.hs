{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo (..)
  , chunkInfoSupportsEBBs
  , simpleChunkInfo
  , singleChunkInfo
    -- * Chunk number
  , ChunkNo (..)
  , chunkNoFromInt
  , chunkNoToInt
  , chunksBetween
  , countChunks
  , firstChunkNo
  , nextChunkNo
  , prevChunkNo
  , unsafeChunkNoToEpochNo
  , unsafeEpochNoToChunkNo
    -- * Chunk size
  , ChunkSize (..)
  , getChunkSize
    -- * Layout
  , RelativeSlot (..)
  , assertRelativeSlotInChunk
  , compareRelativeSlot
  , maxRelativeIndex
  , mkRelativeSlot
    -- * Assertions
  , ChunkAssertionFailure
  , assertChunkCanContainEBB
  , assertSameChunk
  , assertWithinBounds
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.RedundantConstraints

-- | Size of the chunks of the immutable DB
--
-- This is the key data structure that drives all layout functions.
--
-- TODO: Add support for non-uniform 'ChunkInfo'
-- <https://github.com/input-output-hk/ouroboros-network/issues/1754>
data ChunkInfo =
    -- | A single, uniform, chunk size
    --
    -- If EBBs are present, the chunk size must line up precisely with the
    -- epoch size (that is, the number of regular blocks in the chunk must equal
    -- the number of regular blocks in an epoch).
    --
    UniformChunkSize !ChunkSize
  deriving stock    (Show, Generic)
  deriving anyclass (NoThunks)

-- | Simple chunk config with a single chunk size
--
-- This intentionally takes 'EpochSize' (number of slots) rather than
-- 'ChunkSize': the translation from 'EpochSize' to 'ChunkSize' (number of
-- available entries in a chunk) should not be done by client code.
simpleChunkInfo :: EpochSize -> ChunkInfo
simpleChunkInfo (EpochSize sz) = UniformChunkSize (ChunkSize True sz)

-- | 'ChunkInfo' for a single 'ChunkSize'
--
-- See also 'simpleChunkInfo'.
singleChunkInfo :: ChunkSize -> ChunkInfo
singleChunkInfo = UniformChunkSize

-- | Can we store EBBs in the chunks described by this 'ChunkInfo'?
--
-- This is only used for tests. This API will need to change (and the tests will
-- become more complicated) once we support non-uniform 'ChunkInfo'.
chunkInfoSupportsEBBs :: ChunkInfo -> Bool
chunkInfoSupportsEBBs (UniformChunkSize chunkSize) =
    chunkCanContainEBB chunkSize

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- The total number of slots available in a chunk is equal to 'numRegularBlocks'
-- if @not@ 'chunkCanContainEBB', and 'numRegularBlocks' @+ 1@ otherwise.
data ChunkSize = ChunkSize {
      -- | Does this chunk also accomodate an EBB?
      chunkCanContainEBB :: !Bool

      -- | The number of regular blocks in this chunk
    , numRegularBlocks   :: !Word64
    }
  deriving stock    (Show, Generic)
  deriving anyclass (NoThunks)

-- | Chunk number
newtype ChunkNo = ChunkNo { unChunkNo :: Word64 }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Show, NoThunks)

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
getChunkSize chunkInfo _chunk =
    case chunkInfo of
      UniformChunkSize sz -> sz

{-------------------------------------------------------------------------------
  Layout

  These are defined in the @Internal@ module so that most code can safely
  import from "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout" without
  worrying that it's making assumptions that it shouldn't. All bets are off for
  modules that import "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal".
-------------------------------------------------------------------------------}

-- | A /relative/ slot within a chunk
data RelativeSlot = RelativeSlot {
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
  deriving anyclass (NoThunks)

-- | Maximum relative index within a chunk
maxRelativeIndex :: ChunkSize -> Word64
maxRelativeIndex ChunkSize{..}
  | chunkCanContainEBB = numRegularBlocks
  | otherwise          = numRegularBlocks - 1

-- | Smart constructor for 'RelativeSlot'
mkRelativeSlot :: HasCallStack => ChunkInfo -> ChunkNo -> Word64 -> RelativeSlot
mkRelativeSlot chunkInfo chunk index =
    assertWithinBounds index size $
    RelativeSlot {
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
    NotSameChunk ChunkNo ChunkNo PrettyCallStack
  | NotWithinBounds Word64 ChunkSize PrettyCallStack
  | ChunkCannotContainEBBs ChunkNo PrettyCallStack
  deriving (Show)

instance Exception ChunkAssertionFailure

assertSameChunk :: HasCallStack => ChunkNo -> ChunkNo -> a -> a
#if ENABLE_ASSERTIONS
assertSameChunk a b
  | a == b    = id
  | otherwise = throw $ NotSameChunk a b prettyCallStack
#else
assertSameChunk _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)

assertWithinBounds :: HasCallStack => Word64 -> ChunkSize -> a -> a
#if ENABLE_ASSERTIONS
assertWithinBounds ix sz
  | ix <= maxRelativeIndex sz = id
  | otherwise                 = throw $ NotWithinBounds ix sz prettyCallStack
#else
assertWithinBounds _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)

assertChunkCanContainEBB :: HasCallStack => ChunkNo -> ChunkSize -> a -> a
#if ENABLE_ASSERTIONS
assertChunkCanContainEBB chunk size
  | chunkCanContainEBB size = id
  | otherwise               = throw $ ChunkCannotContainEBBs chunk prettyCallStack
#else
assertChunkCanContainEBB _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)
