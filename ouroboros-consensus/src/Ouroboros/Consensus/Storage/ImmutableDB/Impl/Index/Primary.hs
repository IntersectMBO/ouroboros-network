{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Primary Index
--
-- Intended for qualified import
-- > import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as PrimaryIndex
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary (
    -- * SecondaryOffset
    SecondaryOffset
    -- * PrimaryIndex
  , PrimaryIndex (..)
  , appendOffsets
  , backfill
  , backfillChunk
  , containsSlot
  , currentVersionNumber
  , filledSlots
  , firstFilledSlot
  , getLastSlot
  , isFilledSlot
  , lastFilledSlot
  , lastOffset
  , load
  , nextFilledSlot
  , offsetOfSlot
  , open
  , readFirstFilledSlot
  , readOffset
  , readOffsets
  , secondaryOffsetSize
  , sizeOfSlot
  , slots
  , truncateToSlot
  , truncateToSlotFS
  , unfinalise
  , write
    -- * Exported for testing purposes
  , mk
  , toSecondaryOffsets
  ) where

import           Control.Exception (assert)
import           Control.Monad
import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity (Identity (..))
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Foreign.Storable (sizeOf)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block (StandardHash)
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types (AbsOffset (..),
                     AllowExisting (..), OpenMode (..), SeekMode (..))

import           Ouroboros.Consensus.Storage.ImmutableDB.API
                     (ImmutableDBError (..), UnexpectedFailure (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
                     (fsPathPrimaryIndexFile, runGet)

{------------------------------------------------------------------------------
  SecondaryOffset
------------------------------------------------------------------------------}

-- | An offset in the secondary index file.
--
-- We need 4 bytes ('Word32') because the secondary index file can grow to
-- +1MiB.
type SecondaryOffset = Word32

getSecondaryOffset :: Get SecondaryOffset
getSecondaryOffset = Get.getWord32be

putSecondaryOffset :: SecondaryOffset -> Put
putSecondaryOffset = Put.putWord32be

-- | The size of each entry in the primary index file, i.e., the size of a
-- 'SecondaryOffset'.
secondaryOffsetSize :: Word64
secondaryOffsetSize = fromIntegral $ sizeOf (error "sizeOf" :: SecondaryOffset)
{-# INLINE secondaryOffsetSize #-}

{------------------------------------------------------------------------------
  PrimaryIndex
------------------------------------------------------------------------------}

-- | In-memory representation of the primary index file.
--
-- The primary index maps relative slots to offsets in the secondary index
-- file. The first offset is always 0, as the first entry in the secondary
-- index file will always start at offset 0. The second offset will be equal
-- to the size of a secondary index entry, unless the slot is empty, in which
-- case it will be 0. In general, an offset will either be a repetition of the
-- offset before it, to indicate the slot is empty, or the offset before it +
-- the fixed size of a secondary index entry, in case the slot is filled.
--
-- The size of a secondary index entry can be computed by subtracting the
-- offset corresponding to the respective slot from the offset corresponding
-- to the slot after it.
--
-- For example, if slots 0, 1 and 4 are filled, we'd have the following
-- offsets in the primary index file:
--
-- > slot:       0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ x │ y │ y │ y │ z │
-- >         └───┴───┴───┴───┴───┴───┘
--
-- We use @x, y, z@ in the example above, but in practice these will be
-- multiples of the (fixed) size of an entry in secondary index.
--
-- TODO As all entries have the same size, we could use a bitvector instead,
-- see #1234.
--
-- The serialisation of a primary index file starts with
-- @currentVersionNumber@ followed by all its offset.
data PrimaryIndex = MkPrimaryIndex {
      -- | The 'ChunkNo' of the chunk this index is associated with
      primaryIndexChunkNo :: !ChunkNo

      -- | The entries in the index proper
    , primaryIndexOffsets :: !(Vector SecondaryOffset)
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoThunks)

assertInPrimaryIndex :: HasCallStack => PrimaryIndex -> RelativeSlot -> Word64
assertInPrimaryIndex = assertRelativeSlotInChunk . primaryIndexChunkNo

-- | Smart constructor: checks that the offsets are non-decreasing, there is
-- at least one offset, and that the first offset is 0.
mk :: ChunkNo -> [SecondaryOffset] -> Maybe PrimaryIndex
mk chunk offsets@(0:_)
    | and $ zipWith (<=) offsets (drop 1 offsets)
    = Just $ MkPrimaryIndex chunk $ V.fromList offsets
mk _ _ = Nothing

-- | Return the 'SecondaryOffset's in the 'PrimaryIndex'.
toSecondaryOffsets :: PrimaryIndex -> [SecondaryOffset]
toSecondaryOffsets = V.toList . primaryIndexOffsets

-- | Version number of the index format
currentVersionNumber :: Word8
currentVersionNumber = 1

-- | Count the number of (filled or unfilled) slots currently in the index
slots :: PrimaryIndex -> Word64
slots (MkPrimaryIndex _ offsets) = fromIntegral $ V.length offsets - 1

-- | Read the 'SecondaryOffset' corresponding to the given relative slot in
-- the primary index. Return 'Nothing' when the slot is empty.
readOffset
  :: forall blk m h.
     (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk)
  => Proxy blk
  -> HasFS m h
  -> ChunkNo
  -> RelativeSlot
  -> m (Maybe SecondaryOffset)
readOffset pb hasFS chunk slot = runIdentity <$>
    readOffsets pb hasFS chunk (Identity slot)

-- | Same as 'readOffset', but for multiple offsets.
--
-- NOTE: only use this for a few offsets, as we will seek (@pread@) for each
-- offset. Use 'load' if you want to read the whole primary index.
readOffsets
  :: forall blk m h t.
     ( HasCallStack
     , MonadThrow m
     , Traversable t
     , StandardHash blk
     , Typeable blk
     )
  => Proxy blk
  -> HasFS m h
  -> ChunkNo
  -> t RelativeSlot
  -> m (t (Maybe SecondaryOffset))
       -- ^ The offset in the secondary index file corresponding to the given
       -- slot. 'Nothing' when the slot is empty.
readOffsets pb hasFS@HasFS { hGetSize } chunk toRead =
    withFile hasFS primaryIndexFile ReadMode $ \pHnd -> do
      size <- hGetSize pHnd
      forM toRead $ \relSlot -> do
        let slot   = assertRelativeSlotInChunk chunk relSlot
        let offset = AbsOffset $
              fromIntegral (sizeOf currentVersionNumber) +
              slot * secondaryOffsetSize
        if unAbsOffset offset + nbBytes > size then
          -- Don't try reading if the file doesn't contain enough bytes
          return Nothing
        else do
          (secondaryOffset, nextSecondaryOffset) <-
            runGet pb primaryIndexFile get =<<
            hGetExactlyAt hasFS pHnd nbBytes offset
          return $ if nextSecondaryOffset - secondaryOffset > 0
            then Just secondaryOffset
            else Nothing
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk
    nbBytes          = secondaryOffsetSize * 2

    get :: Get (SecondaryOffset, SecondaryOffset)
    get = (,) <$> getSecondaryOffset <*> getSecondaryOffset

-- | Return the first filled slot in the primary index file, or 'Nothing' in
-- case there are no filled slots.
--
-- PRECONDITION: the index file must exist and contain at least the version
-- number and offset 0.
--
-- May throw 'InvalidPrimaryIndexException'.
readFirstFilledSlot
  :: forall blk m h.
     (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk)
  => Proxy blk
  -> HasFS m h
  -> ChunkInfo
  -> ChunkNo
  -> m (Maybe RelativeSlot)
readFirstFilledSlot pb hasFS@HasFS { hSeek, hGetSome } chunkInfo chunk =
    withFile hasFS primaryIndexFile ReadMode $ \pHnd -> do
      hSeek pHnd AbsoluteSeek skip
      go pHnd $ NextRelativeSlot (firstBlockOrEBB chunkInfo chunk)
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk

    -- | Skip the version number and the first offset, which is always 0.
    skip = fromIntegral (sizeOf currentVersionNumber)
         + fromIntegral secondaryOffsetSize

    -- | Read offset per offset until we find a non-zero one. In the
    -- Byron-era, the first slot is always filled with an EBB, so we only need
    -- to read one 4-byte offset. In the Shelley era, approximately one in ten
    -- slots is filled, so on average we need to read 5 4-byte offsets. The OS
    -- will buffer this anyway.
    go :: HasCallStack => Handle h -> NextRelativeSlot -> m (Maybe RelativeSlot)
    go pHnd nextRelative = getNextOffset pHnd >>= \mOffset ->
      case (nextRelative, mOffset) of
        (_, Nothing) ->
          -- Reached end of file, no filled slot
          return Nothing
        (NoMoreRelativeSlots, Just _) ->
          throwIO $ UnexpectedFailure $
            InvalidFileError
              @blk
              primaryIndexFile
              "Index file too large"
              prettyCallStack
        (NextRelativeSlot slot, Just offset)
          | offset == 0 -> go pHnd (nextRelativeSlot slot)
          | otherwise   -> return $ Just slot

    -- | We don't know in advance if there are bytes left to read, so it could
    -- be that 'hGetSome' returns 0 bytes, in which case we reached EOF and
    -- return 'Nothing'.
    --
    -- NOTE: when using 'hGetSome' directly, we can get partial reads, which
    -- we should handle appropriately.
    getNextOffset :: Handle h -> m (Maybe SecondaryOffset)
    getNextOffset pHnd = goGet secondaryOffsetSize mempty
      where
        goGet :: Word64 -> Lazy.ByteString -> m (Maybe SecondaryOffset)
        goGet remaining acc = do
          bs <- hGetSome pHnd remaining
          let acc' = acc <> Lazy.fromStrict bs
          case fromIntegral (Strict.length bs) of
            0 -> return Nothing
            n | n < remaining  -- Partial read, read some more
              -> goGet (remaining - n) acc'
              | otherwise      -- All bytes read, 'Get' the offset
              -> assert (n == remaining) $ Just <$>
                 runGet pb primaryIndexFile getSecondaryOffset acc'

-- | Load a primary index file in memory.
load
  :: forall blk m h.
     (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk)
  => Proxy blk
  -> HasFS m h
  -> ChunkNo
  -> m PrimaryIndex
load pb hasFS chunk =
    withFile hasFS primaryIndexFile ReadMode $ \pHnd ->
      hGetAll hasFS pHnd >>= runGet pb primaryIndexFile get
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk

    -- TODO incremental?
    get :: Get PrimaryIndex
    get = Get.getWord8 >>= \versionNumber ->
      if versionNumber == currentVersionNumber
        then MkPrimaryIndex chunk . V.fromList <$> go
        else fail $ "unknown version number: " <> show versionNumber
      where
        go = do
          isEmpty <- Get.isEmpty
          if isEmpty then return []
          else (:) <$> getSecondaryOffset <*> go

-- | Write a primary index to a file.
--
-- Property: for @hasFS@, @err@, @chunk@
--
-- > 'write' hasFS chunk primaryIndex
-- > primaryIndex' <- 'load' hasFS err chunk
--
-- Then it must be that:
--
-- > primaryIndex === primaryIndex'
--
write
  :: (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ChunkNo
  -> PrimaryIndex
  -> m ()
write hasFS@HasFS { hTruncate } chunk (MkPrimaryIndex _ offsets) =
    withFile hasFS primaryIndexFile (AppendMode AllowExisting) $ \pHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      hTruncate pHnd 0
      void $ hPut hasFS pHnd $ Put.execPut $
        -- The version number
        Put.putWord8 currentVersionNumber <>
        -- Hopefully the intermediary list is fused away
        foldMap putSecondaryOffset (V.toList offsets)
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk

-- | Truncate the primary index so that the given 'RelativeSlot' will be the
-- last slot (filled or not) in the primary index, unless the primary index
-- didn't contain the 'RelativeSlot' in the first place.
truncateToSlot :: ChunkInfo -> RelativeSlot -> PrimaryIndex -> PrimaryIndex
truncateToSlot chunkInfo relSlot primary@(MkPrimaryIndex _ offsets) =
    case getLastSlot chunkInfo primary of
      Just lastSlot | compareRelativeSlot lastSlot relSlot == GT ->
        primary { primaryIndexOffsets = V.take (fromIntegral slot + 2) offsets }
      _otherwise ->
        primary
  where
    slot = assertInPrimaryIndex primary relSlot

-- | On-disk variant of 'truncateToSlot'. The truncation is done without
-- reading the primary index from disk.
truncateToSlotFS
  :: (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ChunkNo
  -> RelativeSlot
  -> m ()
truncateToSlotFS hasFS@HasFS { hTruncate, hGetSize } chunk relSlot =
    withFile hasFS primaryIndexFile (AppendMode AllowExisting) $ \pHnd -> do
      size <- hGetSize pHnd
      when (offset < size) $ hTruncate pHnd offset
  where
    slot             = assertRelativeSlotInChunk chunk relSlot
    primaryIndexFile = fsPathPrimaryIndexFile chunk
    offset           = fromIntegral (sizeOf currentVersionNumber)
                     + (slot + 2) * secondaryOffsetSize

-- | Remove all trailing empty slots that were added during the
-- finalisation/backfilling of the primary index.
--
-- POSTCONDITION: the last slot of the primary index file will be filled,
-- unless the index itself is empty.
unfinalise
  :: (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk)
  => Proxy blk
  -> HasFS m h
  -> ChunkInfo
  -> ChunkNo
  -> m ()
unfinalise pb hasFS chunkInfo chunk = do
    -- TODO optimise so that we only need to open the file once
    primaryIndex <- load pb hasFS chunk
    case lastFilledSlot chunkInfo primaryIndex of
      Nothing   -> return ()
      Just slot -> truncateToSlotFS hasFS chunk slot

-- | Open a primary index file for the given chunk and return a handle to it.
--
-- The file is opened with the given 'AllowExisting' value. When given
-- 'MustBeNew', the version number is written to the file.
open
  :: (HasCallStack, MonadCatch m)
  => HasFS m h
  -> ChunkNo
  -> AllowExisting
  -> m (Handle h)
open hasFS@HasFS { hOpen, hClose } chunk allowExisting = do
    -- TODO we rely on the fact that if the file exists, it already contains
    -- the version number and the first offset. What if that is not the case?
    pHnd <- hOpen primaryIndexFile (AppendMode allowExisting)
    flip onException (hClose pHnd) $ do
      case allowExisting of
        AllowExisting -> return ()
        -- If the file is new, write the version number and the first offset,
        -- i.e. 0.
        MustBeNew     -> void $ hPut hasFS pHnd $ Put.execPut $
          Put.putWord8 currentVersionNumber <>
          putSecondaryOffset 0
      return pHnd
  where
    primaryIndexFile = fsPathPrimaryIndexFile chunk

-- | Append the given 'SecondaryOffset' to the end of the file (passed as a
-- handle).
appendOffsets
  :: (Monad m, Foldable f, HasCallStack)
  => HasFS m h
  -> Handle h
  -> f SecondaryOffset
  -> m ()
appendOffsets hasFS pHnd offsets =
    void $ hPut hasFS pHnd $ Put.execPut $ foldMap putSecondaryOffset offsets

-- | Return the last 'SecondaryOffset' in the primary index file.
lastOffset :: PrimaryIndex -> SecondaryOffset
lastOffset (MkPrimaryIndex _ offsets)
  | V.null offsets = 0
  | otherwise = offsets ! (V.length offsets - 1)

-- | Return the last slot of the primary index (empty or not).
--
-- Returns 'Nothing' if the index is empty.
getLastSlot :: ChunkInfo -> PrimaryIndex -> Maybe RelativeSlot
getLastSlot chunkInfo (MkPrimaryIndex chunk offsets) = do
    guard $ V.length offsets >= 2
    return $ nthBlockOrEBB chunkInfo chunk (V.length offsets - 2)

-- | Check whether the given slot is within the primary index.
containsSlot :: PrimaryIndex -> RelativeSlot -> Bool
containsSlot primary@(MkPrimaryIndex _ offsets) relSlot =
    slot < fromIntegral (V.length offsets) - 1
  where
    slot = assertInPrimaryIndex primary relSlot

-- | Return the offset for the given slot.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
offsetOfSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> SecondaryOffset
offsetOfSlot primary@(MkPrimaryIndex _ offsets) relSlot =
    offsets ! fromIntegral slot
  where
    slot = assertInPrimaryIndex primary relSlot

-- | Return the size of the given slot according to the primary index.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
sizeOfSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> Word32
sizeOfSlot primary@(MkPrimaryIndex _ offsets) relSlot =
    offsetAfter - offsetAt
  where
    slot        = assertInPrimaryIndex primary relSlot
    i           = fromIntegral slot
    offsetAt    = offsets ! i
    offsetAfter = offsets ! (i + 1)

-- | Return 'True' when the given slot is filled.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
isFilledSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> Bool
isFilledSlot primary slot = sizeOfSlot primary slot /= 0

-- | Find the next filled (length > zero) slot after the given slot in the
-- primary index. If there is none, return 'Nothing'.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
--
-- Example: given the primary index below and slot 1:
--
-- > slot:       0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ x │ y │ y │ y │ z │
-- >         └───┴───┴───┴───┴───┴───┘
--
-- Return slot 4.
nextFilledSlot :: ChunkInfo -> PrimaryIndex -> RelativeSlot -> Maybe RelativeSlot
nextFilledSlot chunkInfo primary@(MkPrimaryIndex chunk offsets) relSlot =
    go (fromIntegral slot + 1)
  where
    slot = assertInPrimaryIndex primary relSlot

    len :: Int
    len = V.length offsets

    go :: Int -> Maybe RelativeSlot
    go i
      | i + 1 >= len
      = Nothing
      | offsets ! i == offsets ! (i + 1)
      = go (i + 1)
      | otherwise
      = Just (nthBlockOrEBB chunkInfo chunk i)

-- | Find the first filled (length > zero) slot in the primary index. If there
-- is none, return 'Nothing'.
--
-- Example: given the primary index below:
--
-- > slot:       0   1
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 0 │ x │
-- >         └───┴───┴───┘
--
-- Return slot 1.
firstFilledSlot :: ChunkInfo -> PrimaryIndex -> Maybe RelativeSlot
firstFilledSlot chunkInfo (MkPrimaryIndex chunk offsets) = go 1
  where
    len :: Int
    len = V.length offsets

    go :: Int -> Maybe RelativeSlot
    go i
      | i >= len
      = Nothing
      | offsets ! i == 0
      = go (i + 1)
      | otherwise
      = Just (nthBlockOrEBB chunkInfo chunk (i - 1))

-- | Return a list of all the filled (length > zero) slots in the primary
-- index.
filledSlots :: ChunkInfo -> PrimaryIndex -> [RelativeSlot]
filledSlots chunkInfo primary = go (firstFilledSlot chunkInfo primary)
  where
    go Nothing     = []
    go (Just slot) = slot : go (nextFilledSlot chunkInfo primary slot)

-- | Return the last filled slot in the primary index.
lastFilledSlot :: HasCallStack => ChunkInfo -> PrimaryIndex -> Maybe RelativeSlot
lastFilledSlot chunkInfo (MkPrimaryIndex chunk offsets) =
    go (V.length offsets - 1)
  where
    go :: Int -> Maybe RelativeSlot
    go i
      | i < 1
      = Nothing
      | offsets ! i == offsets ! (i - 1)
      = go (i - 1)
      | otherwise
      = Just (nthBlockOrEBB chunkInfo chunk (i - 1))

-- | Return the slots to backfill the primary index file with.
--
-- A situation may arise in which we \"skip\" some relative slots, and we
-- write into the DB, for example, every other relative slot. In this case, we
-- need to backfill the primary index file with offsets for the skipped
-- relative slots. Similarly, before we start a new chunk, we must backfill
-- the primary index file of the current chunk to indicate that the remaining
-- slots in the chunk are empty.
--
-- For example, say we have written to relative slots 0 and 1. We have the
-- following primary index file:
--
-- > slot:       0   1
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ x │ y │
-- >         └───┴───┴───┘
--
-- Now we want to write to relative slot 4, skipping 2 and 3. We first have to
-- backfill the primary index by repeating the last offset for the two missing
-- slots:
--
-- > slot:       0   1   2   3
-- >         ┌───┬───┬───┬───┬───┐
-- > offset: │ 0 │ x │ y │ y │ y │
-- >         └───┴───┴───┴───┴───┘
--
-- After backfilling (writing the offset @y@ twice), we can write the next
-- offset:
--
-- > slot:       0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ x │ y │ y │ y │ z │
-- >         └───┴───┴───┴───┴───┴───┘
--
-- For the example above, the output of this function would thus be: @[y, y]@.
--
-- We use @x, y, z@ in the examples above, but in practice these will be
-- multiples of the (fixed) size of an entry in secondary index.
backfill
  :: RelativeSlot     -- ^ The slot to write to (>= next expected slot)
  -> RelativeSlot     -- ^ The next expected slot to write to
  -> SecondaryOffset  -- ^ The last 'SecondaryOffset' written to
  -> [SecondaryOffset]
backfill slot nextExpected offset =
    replicate (fromIntegral gap) offset
  where
    gap = relativeSlotIndex slot
        - relativeSlotIndex nextExpected

-- | Return the slots to backfill the primary index file with when padding it
-- to the chunk size.
--
-- See 'backfill' for more details.
backfillChunk
  :: ChunkInfo
  -> ChunkNo
  -> NextRelativeSlot
  -> SecondaryOffset
  -> [SecondaryOffset]
backfillChunk _ _ NoMoreRelativeSlots _ =
    []
backfillChunk chunkInfo chunk (NextRelativeSlot nextExpected) offset =
    replicate (fromIntegral gap) offset
  where
    finalSlot = maxRelativeSlot chunkInfo chunk
    gap       = relativeSlotIndex finalSlot
              - relativeSlotIndex nextExpected
              + 1 -- fill all slots /including/ 'finalSlot'

{------------------------------------------------------------------------------
  Helper for debugging
------------------------------------------------------------------------------}

(!) :: (HasCallStack, V.Unbox a) => Vector a -> Int -> a
v ! i
  | 0 <= i, i < V.length v
  = V.unsafeIndex v i
  | otherwise
  = error $
    "Index " <> show i <> " out of bounds (0, " <> show (V.length v - 1) <> ")"
{-# INLINE (!) #-}
