{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Ouroboros.Storage.ImmutableDB.Index where

import           Control.Monad (void)
import           Control.Monad.Catch (MonadMask)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)

import           System.IO (IOMode (..))

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
  deriving (Show)

-- | Return the number of slots in the index (the number of offsets - 1).
indexSlots :: Index -> EpochSize
indexSlots (MkIndex offsets) = fromIntegral $ V.length offsets - 1

-- | The size of each entry in the index file, namely 8 bytes for the offset
-- (represented as a Word64).
indexEntrySizeBytes :: Int
indexEntrySizeBytes = 8
{-# INLINE indexEntrySizeBytes #-}

-- | Loads an index file in memory.
loadIndex :: MonadMask m
          => HasFS m
          -> FsPath
          -> Epoch
          -> m Index
loadIndex hasFS dbFolder epoch = do
    let indexFile = dbFolder <> renderFile "index" epoch
    indexContents <- withFile hasFS indexFile ReadMode $ \hnd ->
      BL.toStrict . BS.toLazyByteString <$> readAll hasFS hnd
    return $ indexFromByteString indexContents

-- | Write a non-empty list of 'SlotOffset's to a file.
--
-- Property: for @dbFolder@, @epoch@, and @offsets@:
--
-- > 'writeSlotOffsets' dbFolder epoch offsets
-- > index <- loadIndex dbFolder epoch
--
-- Then it must be that:
--
-- > indexToSlotOffsets index === offsets
writeSlotOffsets :: MonadMask m
                 => HasFS m
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

-- | Return the 'SlotOffset' of the last slot in the index file.
lastSlotOffset :: Index -> SlotOffset
lastSlotOffset (MkIndex offsets)
  | V.null offsets = 0
  | otherwise = offsets V.! (V.length offsets - 1)

-- | Check whether the given slot is within the index.
containsSlot :: Index -> RelativeSlot -> Bool
containsSlot (MkIndex offsets) (RelativeSlot slot) =
  fromIntegral slot < V.length offsets - 1

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
