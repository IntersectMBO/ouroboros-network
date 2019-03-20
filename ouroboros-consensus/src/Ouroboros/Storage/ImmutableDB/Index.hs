{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Ouroboros.Storage.ImmutableDB.Index
  ( Index
  , getEBBHash
  , indexSlots
  , indexEntrySizeBytes
  , loadIndex
  , writeIndex
  , writeSlotOffsets
  , indexFromSlotOffsets
  , indexToSlotOffsets
  , lastSlotOffset
  , lastSlot
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
  , truncateToSlots
  ) where

import           Codec.Serialise (Serialise (..), deserialiseOrFail, serialise)
import           Control.Exception (assert)
import           Control.Monad (void, when)
import           Control.Monad.Class.MonadThrow

import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)

import           GHC.Stack (HasCallStack, callStack)

import           System.IO (IOMode (..))

import           Ouroboros.Storage.FS.API (HasFS (..), withFile)
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes
                     (RelativeSlot (..))

import           Ouroboros.Storage.ImmutableDB.Types (EpochNo, EpochSize,
                     ImmutableDBError, SlotOffset,
                     UnexpectedError (DeserialisationError, InvalidFileError))
import           Ouroboros.Storage.ImmutableDB.Util (readAll, renderFile,
                     throwUnexpectedError)
import           Ouroboros.Storage.Util (decodeIndexEntryAt, encodeIndexEntry)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))


{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- | In-memory representation of the index file.
data Index hash = MkIndex
  { getOffsets :: !(V.Vector SlotOffset)
  , getEBBHash :: !(Maybe hash)
    -- ^ Return the hash of the EBB, if the index stores one.
  } deriving (Eq, Show)


-- | Return the number of slots in the index (the number of offsets - 1).
--
-- Note that the index will typically contain a slot for the EBB, so for an
-- for an epoch with 10 regular slots, this will function will return 11.
indexSlots :: Index hash -> EpochSize
indexSlots (MkIndex offsets _) = fromIntegral $ V.length offsets - 1

-- | The size of each entry in the index file, namely 8 bytes for the offset
-- (represented as a Word64).
indexEntrySizeBytes :: Int
indexEntrySizeBytes = 8
{-# INLINE indexEntrySizeBytes #-}

-- | Loads an index file in memory.
--
-- Returns trailing invalid data that could not be read as @'Maybe'
-- 'ByteString'@.
loadIndex :: (HasCallStack, MonadThrow m, Serialise hash)
          => HasFS m h
          -> ErrorHandling ImmutableDBError m
          -> EpochNo
          -> EpochSize -- ^ The number of slots expected in the index,
                       -- including the EBB: the size of the epoch + 1.
          -> m (Index hash)
loadIndex hasFS err epoch indexSize = do
    let indexFile       = renderFile "index" epoch
        expectedOffsets = fromIntegral indexSize + 1
        expectedBytes   = fromIntegral $
          expectedOffsets * indexEntrySizeBytes

    withFile hasFS indexFile ReadMode $ \hnd -> do
      bl <- BS.toLazyByteString <$> readAll hasFS hnd
      let (offsetsBL, ebbHashBL) = BL.splitAt expectedBytes bl
      when (BL.length offsetsBL /= expectedBytes) $
        throwUnexpectedError err $ InvalidFileError indexFile callStack
      let offsetsBS  = BL.toStrict offsetsBL
          offsets    = V.generate expectedOffsets mkEntry
          mkEntry ix = decodeIndexEntryAt (ix * indexEntrySizeBytes) offsetsBS
      case deserialiseOrFail ebbHashBL of
        Right ebbHash -> return $ MkIndex offsets ebbHash
        Left  df      -> throwUnexpectedError err $
          DeserialisationError df callStack

-- | Write an index to an index file.
--
-- Property: for @hasFS@, @err@, @epoch@
--
-- > 'writeIndex' hasFS epoch index
-- > index' <- loadIndex hasFS err epoch (indexSlots index)
--
-- Then it must be that:
--
-- > index === index'
--
writeIndex :: (MonadThrow m, Serialise hash)
           => HasFS m h
           -> EpochNo
           -> Index hash
           -> m ()
writeIndex hasFS@HasFS{..} epoch (MkIndex offsets ebbHash) = do
    let indexFile = renderFile "index" epoch
    withFile hasFS indexFile AppendMode $ \iHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      void $ hTruncate iHnd 0
      -- TODO efficient enough?
      void $ hPut iHnd $ V.foldl'
        (\acc offset -> acc <> encodeIndexEntry offset) mempty offsets
      void $ hPut iHnd $ BS.lazyByteString $ serialise ebbHash

-- | Write a non-empty list of 'SlotOffset's to an index file.
writeSlotOffsets :: (MonadThrow m, Serialise hash)
                 => HasFS m h
                 -> EpochNo
                 -> NonEmpty SlotOffset
                 -> Maybe hash
                 -> m ()
writeSlotOffsets hasFS@HasFS{..} epoch sos ebbHash = do
    let indexFile = renderFile "index" epoch
    withFile hasFS indexFile AppendMode $ \iHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      void $ hTruncate iHnd 0
      -- TODO efficient enough?
      void $ hPut iHnd (foldMap encodeIndexEntry (NE.reverse sos))
      void $ hPut iHnd $ BS.lazyByteString $ serialise ebbHash

-- | Create an 'Index' from the given non-empty list of 'SlotOffset's.
--
-- The 'SlotOffset's must occur in reverse order: the greatest offset should
-- come first in the list. Thus, the list must be monotonically decreasing.
indexFromSlotOffsets :: NonEmpty SlotOffset -> Maybe hash -> Index hash
indexFromSlotOffsets = MkIndex . V.fromList . reverse . NE.toList

-- | Convert an 'Index' into a non-empty list of 'SlotOffset's.
--
-- The 'SlotOffset's will occur in reverse order: the greatest offset comes
-- first in the list. Thus, the list will be monotonically decreasing.
indexToSlotOffsets :: Index hash -> NonEmpty SlotOffset
indexToSlotOffsets (MkIndex offsets _)
  | Just sos <- NE.nonEmpty $ V.toList $ V.reverse offsets
  = sos
  | otherwise
  = 0 NE.:| []

-- | Return the last 'SlotOffset' in the index file.
lastSlotOffset :: Index hash -> SlotOffset
lastSlotOffset (MkIndex offsets _)
  | V.null offsets = 0
  | otherwise = offsets V.! (V.length offsets - 1)

-- | Return the last slot of the index (empty or not).
lastSlot :: Index hash -> RelativeSlot
lastSlot (MkIndex offsets _) = fromIntegral (V.length offsets - 2)

-- | Check whether the given slot is within the index.
containsSlot :: Index hash -> RelativeSlot -> Bool
containsSlot (MkIndex offsets _) (RelativeSlot slot) =
  slot < fromIntegral (V.length offsets) - 1

-- | Return the offset for the given slot is filled.
--
-- Precondition: the given slot must be within the index ('containsSlot').
offsetOfSlot :: Index hash -> RelativeSlot -> SlotOffset
offsetOfSlot (MkIndex offsets _) (RelativeSlot slot) =
  offsets V.! fromIntegral slot

-- | Return the size of the given slot according to the index.
--
-- Precondition: the given slot must be within the index ('containsSlot').
sizeOfSlot :: Index hash -> RelativeSlot -> Word64
sizeOfSlot (MkIndex offsets _) (RelativeSlot slot) =
  let i           = fromIntegral slot
      offsetAt    = offsets V.! i
      offsetAfter = offsets V.! (i + 1)
  in offsetAfter - offsetAt

-- | Return 'True' when the given slot is filled.
--
-- Precondition: the given slot must be within the index ('containsSlot').
isFilledSlot :: Index hash -> RelativeSlot -> Bool
isFilledSlot index slot = sizeOfSlot index slot /= 0

-- | Find the next filled (length > zero) slot after the given slot in the
-- index file. If there is none, return 'Nothing'.
--
-- Precondition: the given slot must be within the index ('containsSlot').
--
-- Example: given the index below and slot 1:
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┘
--
-- Return slot 4.
nextFilledSlot :: Index hash -> RelativeSlot -> Maybe RelativeSlot
nextFilledSlot (MkIndex offsets _) (RelativeSlot slot) =
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
-- > slot:     0   1
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 0 │ 4 │
-- >         └───┴───┴───┘
--
-- Return slot 1.
firstFilledSlot :: Index hash -> Maybe RelativeSlot
firstFilledSlot (MkIndex offsets _) = go 1
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
filledSlots :: Index hash -> [RelativeSlot]
filledSlots index = go (firstFilledSlot index)
  where
    go Nothing     = []
    go (Just slot) = slot : go (nextFilledSlot index slot)

-- | Return the last filled slot in the index.
lastFilledSlot :: Index hash -> Maybe RelativeSlot
lastFilledSlot (MkIndex offsets _) = go (V.length offsets - 1)
  where
    go 0 = Nothing
    go i
      | offsets V.! i == offsets V.! (i - 1)
      = go (i - 1)
      | otherwise
      = Just (fromIntegral i - 1)

-- | Check if the first 'Index' is a prefix of the second 'Index'.
--
-- The value of 'getEBBHash' is ignored.
isPrefixOf :: Index hash -> Index hash -> Bool
isPrefixOf (MkIndex pre _) (MkIndex offsets _)
    | V.length pre > V.length offsets
    = False
    | otherwise
    = V.and $ V.zipWith (==) pre offsets

-- | Add trailing unfilled slots from the second index to the end of the
-- first. The EBB hash stored in the first index is ignored, the resulting
-- index will have the 'getEBBHash' value of the second index.
--
-- Precondition: the first index is non-empty and a prefix of the second
-- index.
--
-- Example: given the indices below (ignoring the hashes):
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
extendWithTrailingUnfilledSlotsFrom
  :: Index hash
  -> Index hash
  -> Index hash
extendWithTrailingUnfilledSlotsFrom index1 index2 =
    assert (not (V.null validOffsets)) $
    assert (validOffsets == prefix)    $
    MkIndex (validOffsets <> trailingUnfilledSlots) ebbHash
  where
    MkIndex validOffsets              _       = index1
    MkIndex withTrailingUnfilledSlots ebbHash = index2

    (prefix, trailingSlots) =
      V.splitAt (V.length validOffsets) withTrailingUnfilledSlots
    trailingUnfilledSlots
      | V.null trailingSlots
      = V.empty
      | otherwise
      = V.takeWhile (== V.last validOffsets) trailingSlots

-- | Truncate the index to the given number of slots (the number of offsets -
-- 1). No-op if the index is <= the given number of slots. The value of
-- 'getEBBHash' is retained.
truncateToSlots :: EpochSize -> Index hash -> Index hash
truncateToSlots slots index@(MkIndex offsets ebbHash)
  | indexSlots index <= slots
  = index
  | otherwise
  = MkIndex (V.take (fromIntegral slots + 1) offsets) ebbHash
