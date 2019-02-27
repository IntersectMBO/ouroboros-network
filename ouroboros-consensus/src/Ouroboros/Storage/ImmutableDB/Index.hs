{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Ouroboros.Storage.ImmutableDB.Index
  ( Index
  , indexSlots
  , indexEntrySizeBytes
  , indexExpectedFileSize
  , loadIndex
  , writeIndex
  , writeSlotOffsets
  , isValidIndex
  , indexFromByteString
  , indexFromSlotOffsets
  , indexToSlotOffsets
  , lastSlotOffset
  , containsSlot
  , offsetOfSlot
  , sizeOfSlot
  , isFilledSlot
  , nextFilledSlot
  , firstFilledSlot
  , filledSlots
  , lastFilledSlot
  , isPrefixOf
  , extendWithTrailingUnfilledSlotsFrom
  ) where

import           Control.Exception (assert)
import           Control.Monad (void)
import           Control.Monad.Class.MonadThrow

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)

import           GHC.Stack (HasCallStack)

import           System.IO (IOMode (..))

import           Ouroboros.Consensus.Util (lastMaybe)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types (FsPath)
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util


{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- | In-memory representation of the index file.
newtype Index = MkIndex { getIndex :: V.Vector SlotOffset }
  deriving (Show, Eq)

-- | Return the number of slots in the index (the number of offsets - 1).
indexSlots :: Index -> EpochSize
indexSlots (MkIndex offsets) = fromIntegral $ V.length offsets - 1

-- | The size of each entry in the index file, namely 8 bytes for the offset
-- (represented as a Word64).
indexEntrySizeBytes :: Int
indexEntrySizeBytes = 8
{-# INLINE indexEntrySizeBytes #-}

-- | Return the expected size (number of bytes) the given 'Index' requires
-- when writing it to a file.
indexExpectedFileSize :: Index -> Int
indexExpectedFileSize (MkIndex offsets) = V.length offsets * indexEntrySizeBytes

-- | Loads an index file in memory.
--
-- Returns trailing invalid data that could not be read as @'Maybe'
-- 'ByteString'@.
loadIndex :: (HasCallStack, MonadThrow m)
          => HasFS m h
          -> FsPath
          -> Epoch
          -> m (Index, Maybe ByteString)
loadIndex hasFS dbFolder epoch = do
    let indexFile = dbFolder <> renderFile "index" epoch
    (indexContents, junk) <- withFile hasFS indexFile ReadMode $ \hnd -> do
      bs <- BL.toStrict . BS.toLazyByteString <$> readAll hasFS hnd
      let trailingJunkBytes = BS.length bs `rem` indexEntrySizeBytes
      return $ BS.splitAt (BS.length bs - trailingJunkBytes) bs
    return ( indexFromByteString indexContents
           , if BS.null junk then Nothing else Just junk)

-- | Write an index to an index file.
--
-- Property: for @hasFS@, @dbFolder@, @epoch@, and @offsets@:
--
-- > 'writeIndex' hasFS dbFolder epoch index
-- > (index', mbJunk) <- loadIndex hasFS dbFolder epoch
--
-- Then it must be that:
--
-- > index === index' .&&. isNothing mbJunk
writeIndex :: MonadThrow m
           => HasFS m h
           -> FsPath
           -> Epoch
           -> Index
           -> m ()
writeIndex hasFS@HasFS{..} dbFolder epoch (MkIndex offsets) = do
    let indexFile = dbFolder <> renderFile "index" epoch
    withFile hasFS indexFile AppendMode $ \iHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      void $ hTruncate iHnd 0
      void $ hPut iHnd $ V.foldl'
        (\acc offset -> acc <> encodeIndexEntry offset) mempty offsets
  -- TODO efficient enough?

-- | Write a non-empty list of 'SlotOffset's to an index file.
--
-- Property: for @hasFS@, @dbFolder@, @epoch@, and @offsets@:
--
-- > 'writeSlotOffsets' hasFS dbFolder epoch offsets
-- > index <- loadIndex hasFS dbFolder epoch
--
-- Then it must be that:
--
-- > indexToSlotOffsets index === offsets
writeSlotOffsets :: MonadThrow m
                 => HasFS m h
                 -> FsPath
                 -> Epoch
                 -> NonEmpty SlotOffset
                 -> m ()
writeSlotOffsets hasFS@HasFS{..} dbFolder epoch sos = do
    let indexFile = dbFolder <> renderFile "index" epoch
    withFile hasFS indexFile WriteMode $ \iHnd ->
      void $ hPut iHnd (foldMap encodeIndexEntry (NE.reverse sos))
  -- TODO efficient enough?


-- | Check if the index is valid.
--
-- A valid index is non-empty and begins with 0. The index should be
-- monotonically increasing.
isValidIndex :: Index -> Bool
isValidIndex (MkIndex offsets) = case offsets V.!? 0 of
  Just 0 -> let os = V.toList offsets
            -- TODO if we do this check in a 'hot path', we should optimise it
            in and $ zipWith (<=) os (drop 1 os)
  _ -> False

-- | Create an 'Index' from the given 'BS.ByteString'.
indexFromByteString :: BS.ByteString -> Index
indexFromByteString bs = MkIndex $ V.generate vectorSize mkEntry
  where
    bytes = BS.length bs
    -- Example: we have stored 4 slots in our index, which will have resulted
    -- in 6 offsets. 6 * sizeof(Word64) = 6 * 8 bytes = 48 bytes. So to go
    -- from 48 bytes, we have to divide by sizeof(Word64) (=
    -- 'indexEntrySizeBytes').
    vectorSize = bytes `quot` indexEntrySizeBytes
    mkEntry ix = decodeIndexEntryAt (ix * indexEntrySizeBytes) bs

-- | Create an 'Index' from the given non-empty list of 'SlotOffset's.
--
-- The 'SlotOffset's must occur in reverse order: the greatest offset should
-- come first in the list. Thus, the list must be monotonically decreasing.
indexFromSlotOffsets :: NonEmpty SlotOffset -> Index
indexFromSlotOffsets = MkIndex . V.fromList . reverse . NE.toList

-- | Convert an 'Index' into a non-empty list of 'SlotOffset's.
--
-- The 'SlotOffset's will occur in reverse order: the greatest offset comes
-- first in the list. Thus, the list will be monotonically decreasing.
indexToSlotOffsets :: Index -> NonEmpty SlotOffset
indexToSlotOffsets (MkIndex offsets)
  | Just sos <- NE.nonEmpty $ V.toList $ V.reverse offsets
  = sos
  | otherwise
  = 0 NE.:| []

-- | Return the last 'SlotOffset' in the index file.
lastSlotOffset :: Index -> SlotOffset
lastSlotOffset (MkIndex offsets)
  | V.null offsets = 0
  | otherwise = offsets V.! (V.length offsets - 1)

-- | Check whether the given slot is within the index.
containsSlot :: Index -> RelativeSlot -> Bool
containsSlot (MkIndex offsets) (RelativeSlot slot) =
  slot < fromIntegral (V.length offsets) - 1

-- | Return the offset for the given slot is filled.
--
-- Precondition: the given slot must be within the index ('containsSlot').
offsetOfSlot :: Index -> RelativeSlot -> SlotOffset
offsetOfSlot (MkIndex offsets) (RelativeSlot slot) =
  offsets V.! fromIntegral slot

-- | Return the size of the given slot according to the index.
--
-- Precondition: the given slot must be within the index ('containsSlot').
sizeOfSlot :: Index -> RelativeSlot -> Word64
sizeOfSlot (MkIndex offsets) (RelativeSlot slot) =
  let i           = fromIntegral slot
      offsetAt    = offsets V.! i
      offsetAfter = offsets V.! (i + 1)
  in offsetAfter - offsetAt

-- | Return 'True' when the given slot is filled.
--
-- Precondition: the given slot must be within the index ('containsSlot').
isFilledSlot :: Index -> RelativeSlot -> Bool
isFilledSlot index slot = sizeOfSlot index slot /= 0

-- | Find the next filled (length > zero) slot after the given slot in the
-- index file. If there is none, return 'Nothing'.
--
-- Precondition: the given slot must be within the index ('containsSlot').
--
-- Example: given the index below and slot 1:
--
-- > slot:     0   1   2   3   4    5
-- >         ┌───┬───┬───┬───┬───┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┘
--
-- Return slot 4.
nextFilledSlot :: Index -> RelativeSlot -> Maybe RelativeSlot
nextFilledSlot (MkIndex offsets) (RelativeSlot slot) =
    go (fromIntegral slot + 1)
  where
    len = V.length offsets
    go i
      | i + 1 >= len
      = Nothing
      | offsets V.! i == offsets V.! (i + 1)
      = go (i + 1)
      | otherwise
      = Just (fromIntegral i)

-- | Find the first filled (length > zero) slot in the index. If there is
-- none, return 'Nothing'.
--
-- Example: given the index below:
--
-- > slot:     0   1   2
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 0 │ 4 │
-- >         └───┴───┴───┘
--
-- Return slot 1.
firstFilledSlot :: Index -> Maybe RelativeSlot
firstFilledSlot (MkIndex offsets) = go 1
  where
    len = V.length offsets
    go i
      | i >= len
      = Nothing
      | offsets V.! i == 0
      = go (i + 1)
      | otherwise
      = Just (fromIntegral (i - 1))

-- | Return a list of all the filled (length > zero) slots in the index.
filledSlots :: Index -> [RelativeSlot]
filledSlots index = go (firstFilledSlot index)
  where
    go Nothing     = []
    go (Just slot) = slot : go (nextFilledSlot index slot)

-- | Return the last filled slot in the index.
lastFilledSlot :: Index -> Maybe RelativeSlot
lastFilledSlot = lastMaybe . filledSlots
-- TODO optimise

-- | Check if the first 'Index' is a prefix of the second 'Index'.
isPrefixOf :: Index -> Index -> Bool
isPrefixOf (MkIndex pre) (MkIndex offsets)
    | V.length pre > V.length offsets
    = False
    | otherwise
    = V.and $ V.zipWith (==) pre offsets

-- | Add trailing unfilled slots from the second index to the end of the
-- first. The boolean return value indicates whether the second index
-- contained trailing filled slots after the (possibly 0) trailing unfilled
-- slots.
--
-- Precondition: the first index is non-empty and a prefix of the second
-- index.
--
-- Example: given the indices below:
--
-- > ┌───┬───┐
-- > │ 0 │ 1 │
-- > └───┴───┘
--
-- > ┌───┬───┬───┬───┐
-- > │ 0 │ 1 │ 1 │ 2 │
-- > └───┴───┴───┴───┘
--
-- Return the following index:
--
-- > ┌───┬───┬───┐
-- > │ 0 │ 1 │ 1 │
-- > └───┴───┴───┘
--
-- and 'True'.
extendWithTrailingUnfilledSlotsFrom
  :: Index
  -> Index
  -> (Index, Bool)
extendWithTrailingUnfilledSlotsFrom (MkIndex validOffsets) (MkIndex withTrailingUnfilledSlots) =
    assert (not (V.null validOffsets)) $
    assert (validOffsets == prefix)    $
    ( MkIndex (validOffsets <> trailingUnfilledSlots)
    , containedTrailingFilledSlots )
  where
    (prefix, trailingSlots) =
      V.splitAt (V.length validOffsets) withTrailingUnfilledSlots
    trailingUnfilledSlots
      | V.null trailingSlots
      = V.empty
      | otherwise
      = V.takeWhile (== V.last validOffsets) trailingSlots
    containedTrailingFilledSlots =
      V.length trailingUnfilledSlots /= V.length trailingSlots
