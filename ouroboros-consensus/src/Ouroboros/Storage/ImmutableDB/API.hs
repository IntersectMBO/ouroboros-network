{-# LANGUAGE RankNTypes #-}
module Ouroboros.Storage.ImmutableDB.API
  ( ImmutableDB(..)
  , withDB
  , Iterator(..)
  , withIterator
  , IteratorResult(..)
  , iteratorToList
  , blobProducer

  , module Ouroboros.Storage.ImmutableDB.Types
  ) where

import           Control.Monad.Class.MonadThrow

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Function (on)

import           GHC.Stack (HasCallStack)

import           Pipes (Producer, lift, yield)

import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

-- | Open the database using the given function, perform the given action
-- using the database. Close the database using its 'closeDB' function
-- afterwards.
--
-- The second return value of the database opener (and the second argument of
-- the action), is the last slot at which a blob is stored in the database or
-- 'Nothing' in case the database is empty.
withDB :: (HasCallStack, MonadThrow m)
       => m (ImmutableDB m, Maybe Slot)
          -- ^ How to open the database
       -> (ImmutableDB m -> Maybe Slot -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB action = bracket openDB (\(db, _) -> closeDB db) (uncurry action)

-- | API for the 'ImmutableDB'.
--
-- The 'ImmutableDB' stores binary blobs in 'Slot's, i.e. the blocks of a
-- chain.
--
-- The database is append-only, so you cannot append a blob to a slot in the
-- past, nor can you remove a blob. You can, however, skip slots, e.g., append
-- to slot 0 and then to slot 5, but afterwards, you can no longer append to
-- slots 1-4. You can only store at most one blob in each slot. It is not
-- allowed to store an empty blob in a slot (we would need a way to
-- distinguish an empty blob from an empty slot).
--
-- To find out the next slot that can be appended to, use 'getNextSlot'.
--
-- The blob stored in a slot can be queried with 'getBinaryBlob'. Blobs can be
-- efficiently streamed using 'Iterator's, see 'streamBinaryBlobs'.
--
-- The database can be explicitly closed, but can also be automatically closed
-- in case of an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError'. Use
-- 'reopen' to reopen the database.
data ImmutableDB m = ImmutableDB
  { -- | Close the database.
    --
    -- Idempotent.
    --
    -- __Note__: Use 'withDB' instead of this function.
    closeDB
      :: HasCallStack => m ()
      -- TODO remove this operation from the public API and expose it using an
      -- internal record so it can be used by 'withDB'.

    -- | Return 'True' when the database is open.
  , isOpen
      :: HasCallStack => m Bool

    -- | When the database was closed, manually or because of an
    -- 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError' during a write
    -- operation, recover using the given 'ValidationPolicy' and reopen it at
    -- the most recent epoch.
    --
    -- Returns the 'Slot' at which the last valid blob is stored in the
    -- reopened database or 'Nothing' in case of an empty database.
    --
    -- Optionally truncate the database to the given slot ('TruncateFrom')
    -- before reopening it.
    --
    -- When validation fails, an error will be thrown. Use
    -- 'extractTruncateFrom' to try to extract a 'TruncateFrom' from the
    -- error. When one could be extracted, use it to reopen the database
    -- again, it should now succeed unless more file-system errors are thrown.
    -- Note that this will truncate the database to some slot in the past.
    --
    -- The database is always truncated to its last filled slot. An open
    -- database can never be opened at some empty slot.
    --
    -- When the database is still open, this is basically a no-op, unless a
    -- 'TruncateFrom' is given.
  , reopen
      :: HasCallStack
      => ValidationPolicy -> Maybe TruncateFrom -> m (Maybe Slot)

    -- | Return the next free 'Slot'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getNextSlot
      :: HasCallStack => m Slot

    -- | Get the binary blob stored at the given 'Slot'.
    --
    -- Returns 'Nothing' if no blob was stored at the given slot.
    --
    -- Throws a 'ReadFutureSlotError' if the requested slot is in the future,
    -- i.e @>= 'getNextSlot'@.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getBinaryBlob
      :: HasCallStack => Slot -> m (Maybe ByteString)

    -- | Appends a binary blob at the given slot.
    --
    -- Throws an 'AppendToSlotInThePastError' if the given slot is before the
    -- one returned by 'getNextSlot'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO the given binary blob may not be empty.
  , appendBinaryBlob
      :: HasCallStack => Slot -> Builder -> m ()

    -- | Return an 'Iterator' to efficiently stream binary blocks out of the
    -- database.
    --
    -- Optionally, a start position (first argument) and/or a stop position
    -- (second argument) can be given that will be used to determine from
    -- which 'Slot' streaming will start and/or stop (both inclusive bounds).
    --
    -- When no start position is given, streaming wil start from the first
    -- blob in the database. When no stop position is given, streaming will
    -- stop at the last blob currently stored in the database. This means that
    -- appends happening while streaming will not be visible to the iterator.
    --
    -- Use 'iteratorNext' to get a blob from the iterator and to advance it.
    --
    -- Slots that do not store a blob are skipped by the iterator.
    --
    -- Throws an 'InvalidIteratorRangeError' if the start of the range is
    -- greater than the end of the range.
    --
    -- Throws a 'ReadFutureSlotError' if the start or end 'EpochSlot' are in
    -- the future.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- The iterator is automatically closed when exhausted, and can be
    -- prematurely closed with 'iteratorClose'.
  , streamBinaryBlobs
      :: HasCallStack => Maybe Slot -> Maybe Slot -> m (Iterator m)

    -- | Throw 'ImmutableDB' errors
  , immutableDBErr :: ErrorHandling ImmutableDBError m
  }

-- | An 'Iterator' is a handle which can be used to efficiently stream binary
-- blobs. Slots not containing a blob are skipped.
data Iterator m = Iterator
  { -- | Steps an 'Iterator' yielding an 'IteratorResult'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO NOTE(adn): This works under the assumption that the user is the
    -- exclusive owner of the iterator.
    --
    -- The iterator is automatically closed when exhausted
    -- ('IteratorExhausted'), and can be prematurely closed with
    -- 'iteratorClose'.
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

-- | Create an iterator from the given 'ImmutableDB' using 'streamBinaryBlobs'
-- and the given @start@ and @end@ @Maybe 'Slot'@s. Perform the given action
-- using the iterator, and close the iterator using its 'iteratorClose'
-- function, in case of success or when an exception was thrown.
withIterator :: (HasCallStack, MonadThrow m)
             => ImmutableDB m
                -- ^ The database
             -> Maybe Slot -- ^ Start streaming from here (inclusive)
             -> Maybe Slot -- ^ End streaming here (inclusive)
             -> (Iterator m -> m a)
                -- ^ Action to perform using the iterator
             -> m a
withIterator db start end =
    bracket (streamBinaryBlobs db start end) iteratorClose


-- | Equality based on 'iteratorID'
instance Eq (Iterator m) where
  (==) = (==) `on` iteratorID

-- | The result of stepping an 'Iterator'.
data IteratorResult
  = IteratorExhausted
  | IteratorResult Slot ByteString
  deriving (Show, Eq)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the blobs produced by the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m) => Iterator m -> m [ByteString]
iteratorToList it = go
  where
    go = do
      next <- iteratorNext it
      case next of
        IteratorResult _ x -> (x:) <$> go
        IteratorExhausted  -> return []

-- | A 'Producer' that streams binary blobs from the database in the given
-- range.
blobProducer :: (Monad m, HasCallStack)
             => ImmutableDB m
             -> Maybe Slot   -- ^ When to start streaming (inclusive).
             -> Maybe Slot   -- ^ When to stop streaming (inclusive).
             -> Producer (Slot, ByteString) m ()
blobProducer db start end = do
    it <- lift $ streamBinaryBlobs db start end
    let loop = do
          res <- lift $ iteratorNext it
          case res of
              IteratorExhausted        -> lift $ iteratorClose it
              IteratorResult slot blob -> yield (slot, blob) *> loop
    loop
