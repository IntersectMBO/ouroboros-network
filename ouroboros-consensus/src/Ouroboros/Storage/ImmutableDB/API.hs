{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Ouroboros.Storage.ImmutableDB.API
  ( ImmutableDB(..)
  , withDB
  , getCurrentEpoch
  , Iterator(..)
  , withIterator
  , IteratorResult(..)
  , iteratorToList
  , blobProducer

  , module Ouroboros.Storage.ImmutableDB.Types
  ) where

import           Control.Monad.Catch (MonadMask, bracket)

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Function (on)

import           GHC.Stack (HasCallStack)

import           Pipes (Producer, lift, yield)

import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadMask m)
       => m (ImmutableDB m)
          -- ^ How to open the database
       -> (ImmutableDB m -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

-- | API for the 'ImmutableDB'.
--
-- See "Ouroboros.Storage.ImmutableDB.Impl" for an overview of the immutable
-- database.
data ImmutableDB m = ImmutableDB
  { -- | Close the database.
    --
    -- Idempotent.
    --
    -- __Note__: Use 'withDB' instead of this function.
    closeDB
      :: HasCallStack => m ()
      -- TODO remove this operation from the public API, replace it with a
      -- reopen + validate function that can be used in case of an error.
      --
      -- The idea is that the 'ImmutableDB' handle/record can be stored in a
      -- Reader (instead of State), so in case of an error, we can reopen the
      -- same database (after validation) instead of having to create a new
      -- 'ImmutableDB' record for the same on-disk database.
      --
      -- We should still expose this field in an internal record so it can be
      -- used by 'withDB'.

    -- | Return 'True' when the database is open.
  , isOpen
      :: HasCallStack => m Bool

    -- | Return the next free 'RelativeSlot' in the current epoch as an
    -- 'EpochSlot'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getNextEpochSlot
      :: HasCallStack => m EpochSlot

    -- | Get the binary blob stored at the given 'EpochSlot'.
    --
    -- Returns 'Nothing' if no blob was stored at the given (valid) slot.
    --
    -- Throws a 'ReadFutureSlotError' if the requested slot is in the future,
    -- i.e >= 'getNextEpochSlot'.
    --
    -- Throws a 'SlotGreaterThanEpochSizeError' if the requested (relative)
    -- slot was exceeded the size of the corresponding epoch.
    --
    -- Throws a 'MissingFileError' if the epoch or index file corresponding
    -- with the requested epoch was missing from disk.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getBinaryBlob
      :: HasCallStack
      => EpochSlot -> m (Maybe ByteString)

    -- | Appends a binary blob at the given relative slot in the current epoch
    -- file.
    --
    -- After appending the last available blob to the current epoch, the user
    -- must first manually call 'startNewEpoch'.
    --
    -- Throws an 'AppendToSlotInThePastError' if the given relative slot is
    -- before the one returned by 'getNextEpochSlot'.
    --
    -- Throws a 'SlotGreaterThanEpochSizeError' if the relative slot exceeds
    -- the size of the current epoch.
    --
    -- If a 'FileSystemError' is encountered while appending, the epoch and
    -- index files are truncated to the last valid state, the state before the
    -- write.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO the given binary blob may not be empty.
  , appendBinaryBlob
      :: HasCallStack
      => RelativeSlot -> Builder -> m ()

    -- | Close the current epoch and start a new epoch (the epoch number is
    -- incremented by 1). The new epoch number is returned.
    --
    -- The size of the new epoch must be passed.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , startNewEpoch
      :: HasCallStack => EpochSize -> m Epoch
      -- TODO For additional safety in concurrent usage: let the user pass the
      -- new 'Epoch' to start, which must be 'getCurrentEpoch' + 1, so no
      -- skipping allowed.

    -- | When given a start (first argument) and a stop (second argument)
    -- position (both inclusive bounds), return an 'Iterator' to efficiently
    -- stream binary blocks out of the database.
    --
    -- Use 'iteratorNext' to stream from the iterator.
    --
    -- Relative slots (and even whole epochs) that do not store a blob are
    -- skipped by the iterator.
    --
    -- Throws an 'InvalidIteratorRangeError' if the start of the range is
    -- greater than the end of the range.
    --
    -- Throws a 'ReadFutureSlotError' if the start or end 'EpochSlot' are in
    -- the future.
    --
    -- Throws a 'SlotGreaterThanEpochSizeError' if the relative slot of the
    -- start or end 'EpochSlot' exceeds the corresponding epoch size.
    --
    -- The iterator is automatically closed when exhausted, and can be
    -- prematurely closed with 'iteratorClose'.
  , streamBinaryBlobs
      :: HasCallStack
      => EpochSlot
      -> EpochSlot
      -> m (Iterator m)

    -- | Throw 'ImmutableDB' errors
  , immutableDBErr :: ErrorHandling ImmutableDBError m
  }

-- | An 'Iterator' is a handle which can be used to efficiently stream binary
-- blobs across multiple epochs. Slots not containing a blob are skipped.
data Iterator m = Iterator
  { -- | Steps an 'Iterator', yielding either an 'ImmutableDBError' or an
    -- 'IteratorResult'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO NOTE(adn): This works under the assumption that the user is the
    -- exclusive owner of the iterator.
    --
    -- When the iterator is exhausted ('IteratorExhausted'), the iterator is
    -- automatically closed with 'iteratorClose'.
    iteratorNext  :: HasCallStack => m IteratorResult

    -- | Dispose of the 'Iterator' by closing any open handles.
    --
    -- Idempotent operation.
  , iteratorClose :: HasCallStack => m ()

    -- | A identifier for the 'Iterator' that is unique for @m@.
    --
    -- This used for the 'Eq' instance, which is needed for testing.
    --
    -- TODO how can we avoid this abstraction leak?
  , iteratorID    :: IteratorID
  }

-- | Create an iterator from the given 'ImmutableDB' using 'streamBinaryBlob'
-- and the given @start@ and @end@ 'EpochSlot's. Perform the given action
-- using the iterator, and close the iterator using its 'iteratorClose'
-- function, in case of success or when an exception was raised.
withIterator :: (HasCallStack, MonadMask m)
             => ImmutableDB m
                -- ^ The database
             -> EpochSlot -- ^ Start streaming from here (inclusive)
             -> EpochSlot -- ^ End streaming here (inclusive)
             -> (Iterator m -> m a)
                -- ^ Action to perform using the iterator
             -> m a
withIterator db start end =
    bracket (streamBinaryBlobs db start end) iteratorClose


-- | Equality based on 'iteratorID'
instance Eq (Iterator m) where
  (==) = (==) `on` iteratorID


-- | Return the currently opened epoch that can be appended to.
--
-- Throws a 'ClosedDBError' if the database is closed.
getCurrentEpoch :: (HasCallStack, Functor m) => ImmutableDB m -> m Epoch
getCurrentEpoch db = _epoch <$> getNextEpochSlot db

-- | The result of stepping an 'Iterator'.
data IteratorResult
  = IteratorExhausted
  | IteratorResult EpochSlot ByteString
  deriving (Show, Eq)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the blobs produced by the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m) => Iterator m -> m [ByteString]
iteratorToList it = go mempty
  where
    go acc = do
      next <- iteratorNext it
      case next of
        IteratorResult _ x -> go (x : acc)
        IteratorExhausted  -> return $ reverse acc

-- | A 'Producer' that streams binary blobs from the database in the
-- given range.
blobProducer :: (Monad m, HasCallStack)
             => ImmutableDB m
             -> EpochSlot   -- ^ When to start streaming (inclusive).
             -> EpochSlot   -- ^ When to stop streaming (inclusive).
             -> Producer (EpochSlot, ByteString) m ()
blobProducer db start end = do
    it <- lift $ streamBinaryBlobs db start end
    let loop = do
          res <- lift $ iteratorNext it
          case res of
              IteratorExhausted -> lift $ iteratorClose it
              IteratorResult epochSlot blob -> yield (epochSlot, blob) *> loop
    loop
