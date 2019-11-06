{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
-- | SlotOffsets
--
-- Intended for qualified import
-- > import qualified Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets as SlotOffsets
module Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets
  ( SlotOffsets (..)
  , isFirst
  , last
  , append
  , empty
  , toNonEmptyList
  , toList
  , fromNonEmptyList
  , fromList
  , drop
  , write
  , reconstruct
  ) where

import           Prelude hiding (drop, last)

import           Codec.CBOR.Encoding (Encoding)
import           Control.Exception (assert)
import           Control.Monad (void)
import           Control.Monad.Class.MonadThrow
import           GHC.Generics (Generic)

import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word64)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Storage.FS.API (HasFS (..), hPut, hPutAll, withFile)
import           Ouroboros.Storage.FS.API.Types (AllowExisting (..),
                     OpenMode (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.Util (encodeIndexEntry)

import           Ouroboros.Storage.ImmutableDB.Impl.Util (indexBackfill,
                     renderFile, serialiseHash)
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Types (CurrentEBB (..))

{------------------------------------------------------------------------------
  SlotOffsets
------------------------------------------------------------------------------}

-- | The 'SlotOffset's must increase from left to right, in other words, each
-- offset is greater than or equal to all offsets before it (to the left).
data SlotOffsets
    = First
    | Snoc !SlotOffsets !SlotOffset
    deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Return 'True' if this is the first slot offset, i.e. 0. No other offsets
-- have been snoc'ed to it yet.
isFirst :: SlotOffsets -> Bool
isFirst First      = True
isFirst (Snoc _ _) = False

-- | Return the last (right-most) 'SlotOffset'.
last :: SlotOffsets -> SlotOffset
last First       = 0
last (Snoc _ so) = so

-- | Append the given 'SlotOffset's to the end of the given 'SlotOffsets'.
append :: Foldable t => SlotOffsets -> t SlotOffset -> SlotOffsets
append = foldl' Snoc

-- | Empty 'SlotOffsets', only offset 0.
empty :: SlotOffsets
empty = First

-- | Convert 'SlotOffsets' into a non-empty list of 'SlotOffset's.
--
-- The offsets in the non-empty list will decrease from left to right.
--
-- > forall sos. fromNonEmptyList (toNonEmptyList sos) == sos
toNonEmptyList :: SlotOffsets -> NonEmpty SlotOffset
toNonEmptyList = go
  where
    go First         = 0 NE.:| []
    go (Snoc sos so) = so NE.<| go sos

-- | Convert 'SlotOffsets' into a list of 'SlotOffset's.
--
-- The offsets in the list will decrease from left to right.
--
-- > forall sos. fromList (toList sos) == sos
toList :: SlotOffsets -> [SlotOffset]
toList = NE.toList . toNonEmptyList

-- | Convert a non-empty list of 'SlotOffset's to 'SlotOffsets'.
--
-- PRECONDITION: the last element in the list must be 0.
fromNonEmptyList :: NonEmpty SlotOffset -> SlotOffsets
fromNonEmptyList = go
  where
    go xs = case NE.uncons xs of
      (0, Nothing)  -> First
      (n, Nothing)  -> error $ "last offset was " <> show n <> " instead of 0"
      (n, Just xs') -> go xs' `Snoc` n

-- | Convert a list of 'SlotOffset's to 'SlotOffsets'.
--
-- WARNING: this is a /partial function/. It will throw an 'error' when the
-- list is empty.
--
-- PRECONDITION: the last element in the list must be 0.
fromList :: [SlotOffset] -> SlotOffsets
fromList = fromNonEmptyList . NE.fromList

-- | Drop the last offsets from 'SlotOffsets'.
drop :: Int -> SlotOffsets -> SlotOffsets
drop = go
  where
    go n sos
      | n > 0, Snoc sos' _ <- sos
      = go (n - 1) sos'
      | otherwise
      = sos

-- | Write a non-empty list of 'SlotOffset's to an index file.
write
  :: MonadThrow m
  => (hash -> Encoding)
  -> HasFS m h
  -> EpochNo
  -> SlotOffsets
  -> CurrentEBB hash
  -> m ()
write hashEncoder hasFS@HasFS{..} epoch sos ebbHash = do
    let indexFile = renderFile "index" epoch
    withFile hasFS indexFile (AppendMode AllowExisting) $ \iHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      void $ hTruncate iHnd 0
      -- TODO efficient enough?
      void $ hPut hasFS iHnd (foldMap encodeIndexEntry (reverse (toList sos)))
      void $ hPutAll hasFS iHnd $ serialiseHash hashEncoder ebbHash

-- | Given a list of increasing 'SlotOffset's together with the 'Word' (blob
-- size) and 'RelativeSlot' corresponding to the offset, reconstruct a
-- non-empty list of (decreasing) slot offsets, i.e. 'SlotOffsets'.
--
-- The input list (typically returned by 'EpochFileParser') is assumed to be
-- valid: __strictly__ monotonically increasing offsets as well as
-- __strictly__ monotonically increasing relative slots.
--
-- The 'RelativeSlot's are used to detect empty/unfilled slots that will
-- result in repeated offsets in the output, indicating that the size of the
-- slot is 0.
reconstruct :: [(SlotOffset, (Word64, RelativeSlot))] -> SlotOffsets
reconstruct = go 0 [] 0
  where
    go :: SlotOffset
       -> [SlotOffset]
       -> RelativeSlot
       -> [(SlotOffset, (Word64, RelativeSlot))]
       -> SlotOffsets
    go offsetAfterLast offsets expectedRelSlot ((offset, (len, relSlot)):olrs') =
      assert (offsetAfterLast == offset) $
      assert (relSlot >= expectedRelSlot) $
      let backfill = indexBackfill relSlot expectedRelSlot offset
      in go (offset + fromIntegral len) (offset : backfill <> offsets)
            (succ relSlot) olrs'
    go offsetAfterLast offsets _lastRelSlot [] =
      fromNonEmptyList $ offsetAfterLast NE.:| offsets
