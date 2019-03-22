{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
module Ouroboros.Storage.ImmutableDB.API
  ( ImmutableDB (..)
  , withDB
  , Iterator (..)
  , withIterator
  , IteratorResult (..)
  , iteratorToList
  , blobProducer

  , module Ouroboros.Storage.ImmutableDB.Types
  ) where

import           Control.Monad.Class.MonadThrow

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Function (on)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Pipes (Producer, lift, yield)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.ImmutableDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

-- | Open the database using the given function, perform the given action
-- using the database. Close the database using its 'closeDB' function
-- afterwards.
withDB :: (HasCallStack, MonadThrow m)
       => m (ImmutableDB hash m)
          -- ^ How to open the database
       -> (ImmutableDB hash m -> m a)
          -- ^ Action to perform using the database
       -> m a
withDB openDB = bracket openDB closeDB

-- | API for the 'ImmutableDB'.
--
-- The 'ImmutableDB' stores binary blobs in 'SlotNo's, i.e. the blocks of a
-- chain.
--
-- The database is append-only, so you cannot append a blob to a slot in the
-- past. You can, however, skip slots, e.g., append to slot 0 and then to slot
-- 5, but afterwards, you can no longer append to slots 1-4. You can only
-- store at most one blob in each slot. It is not allowed to store an empty
-- blob in a slot (we would need a way to distinguish an empty blob from an
-- empty slot).
--
-- The blob stored in a slot can be queried with 'getBinaryBlob'. Blobs can be
-- efficiently streamed using 'Iterator's, see 'streamBinaryBlobs'.
--
-- An Epoch Boundary Block (EBB) can be appended to the start of each epoch
-- using 'appendEBB'.
--
-- The 'Tip' of the database can be queried with 'getTip'. This tip will
-- always point to a filled slot or an EBB that is present.
--
-- It is possible to delete blobs from the database using 'deleteAfter'.
--
-- The database can be explicitly closed, but can also be automatically closed
-- in case of an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError'. Use
-- 'reopen' to reopen the database.
data ImmutableDB hash m = ImmutableDB
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
    -- 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError' during an
    -- operation, recover using the given 'ValidationPolicy' and reopen it at
    -- the most recent epoch.
    --
    -- During validation, the database will be truncated to the last valid
    -- block or EBB stored in it. The tip of the database will never point to
    -- an unfilled slot or missing EBB.
    --
    -- Throws an 'OpenDBError' if the database is open.
  , reopen
      :: HasCallStack => ValidationPolicy -> m ()

    -- | Delete everything in the database after 'Tip'.
    --
    -- Trailing empty slots are also deleted so that the new tip of the
    -- database points at a filled slot (or EBB). If the given tip is not
    -- before the current tip, but newer, then nothing will be deleted.
    --
    -- Consequently, the result of calling 'getTip' after this call completed
    -- will return a tip <= the given tip.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , deleteAfter
      :: HasCallStack => ImmTip -> m ()

    -- | Return the tip of the database.
    --
    -- The tip of the database will never point to an unfilled slot or missing
    -- EBB.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getTip
      :: HasCallStack => m ImmTip

    -- | Get the binary blob stored at the given 'SlotNo'.
    --
    -- Returns 'Nothing' if no blob was stored at the given slot.
    --
    -- Throws a 'ReadFutureSlotError' if the requested slot is in the future,
    -- i.e > the result of 'getTip'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getBinaryBlob
      :: HasCallStack => SlotNo -> m (Maybe ByteString)

    -- | Get the EBB (Epoch Boundary Block) and its hash of the given epoch.
    --
    -- Returns 'Nothing' if no EEB was stored for the given epoch.
    --
    -- Throws a 'ReadFutureEBBError' if the requested EBB is in the future.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getEBB
      :: HasCallStack => EpochNo -> m (Maybe (hash, ByteString))

    -- | Appends a binary blob at the given slot.
    --
    -- Throws an 'AppendToSlotInThePastError' if the given slot is <= the
    -- result of 'getTip'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO the given binary blob may not be empty.
  , appendBinaryBlob
      :: HasCallStack => SlotNo -> Builder -> m ()

    -- | Appends a binary blob as the EBB of the given epoch.
    --
    -- The EEB can only be added before regular blobs are appended to the
    -- current epoch.
    --
    -- Throws an 'AppendToEBBInThePastError' if the given epoch is before the
    -- current or if the blobs have already been appended to the current
    -- epoch.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO the given binary blob may not be empty.
  , appendEBB
      :: HasCallStack => EpochNo -> hash -> Builder -> m ()

    -- | Return an 'Iterator' to efficiently stream binary blocks out of the
    -- database.
    --
    -- Optionally, a start position (first argument) and/or a stop position
    -- (second argument) can be given that will be used to determine from
    -- which 'SlotNo' streaming will start and/or stop (both inclusive bounds).
    --
    -- The @hash@ argument is used to distinguish the EBB from the regular
    -- block stored at the same slot. Note that the database only stores the
    -- hash of the EBB explicitly, so if the given hash matches the EBB's, we
    -- know the bound refers to the EBB. If it doesn't match, we assume it
    -- matches the regular block's hash, but we don't verify this. In order to
    -- to verify this, we would either have to store the hash of each block,
    -- or parse the contents of the block.
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
    -- Throws a 'ReadFutureSlotError' if the start or end 'SlotNo' are in the
    -- future.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- The iterator is automatically closed when exhausted, and can be
    -- prematurely closed with 'iteratorClose'.
  , streamBinaryBlobs
      :: HasCallStack
      => Maybe (SlotNo, hash)
      -> Maybe (SlotNo, hash)
      -> m (Iterator hash m)
      -- TODO inclusive + exclusive bounds

    -- | Throw 'ImmutableDB' errors
  , immutableDBErr :: ErrorHandling ImmutableDBError m
  }

-- | An 'Iterator' is a handle which can be used to efficiently stream binary
-- blobs. Slots not containing a blob and missing EBBs are skipped.
data Iterator hash m = Iterator
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
    iteratorNext  :: HasCallStack => m (IteratorResult hash)

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


-- | Create an iterator from the given 'ImmutableDB' using
-- 'streamBinaryBlobs'. Perform the given action using the iterator, and close
-- the iterator using its 'iteratorClose' function, in case of success or when
-- an exception was thrown.
withIterator :: (HasCallStack, MonadThrow m)
             => ImmutableDB hash m        -- ^ The database
             -> Maybe (SlotNo, hash)      -- ^ Start streaming from here
             -> Maybe (SlotNo, hash)      -- ^ End streaming here
             -> (Iterator hash m -> m a)  -- ^ Action to perform on the iterator
             -> m a
withIterator db start end =
    bracket (streamBinaryBlobs db start end) iteratorClose


-- | Equality based on 'iteratorID'
instance Eq (Iterator hash m) where
  (==) = (==) `on` iteratorID

-- | The result of stepping an 'Iterator'.
data IteratorResult hash
  = IteratorExhausted
  | IteratorResult    SlotNo       ByteString
  | IteratorEBB       EpochNo hash ByteString
  deriving (Show, Eq, Generic)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the 'IteratorResult's (excluding the final 'IteratorExhausted') produced by
-- the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m)
               => Iterator hash m -> m [IteratorResult hash]
iteratorToList it = go
  where
    go = do
      next <- iteratorNext it
      case next of
        IteratorExhausted -> return []
        _                 -> (next:) <$> go

-- | A 'Producer' that streams binary blobs from the database in the given
-- range.
blobProducer :: (Monad m, HasCallStack)
             => ImmutableDB hash m
             -> Maybe (SlotNo, hash)   -- ^ When to start streaming (inclusive).
             -> Maybe (SlotNo, hash)   -- ^ When to stop streaming (inclusive).
             -> Producer (Either (SlotNo, ByteString)
                                 (EpochNo, hash, ByteString))
                         m ()
blobProducer db start end = do
    it <- lift $ streamBinaryBlobs db start end
    let loop = do
          res <- lift $ iteratorNext it
          case res of
            IteratorExhausted           -> lift $ iteratorClose it
            IteratorResult    slot blob -> yield (Left  (slot,  blob))       *> loop
            IteratorEBB epoch hash blob -> yield (Right (epoch, hash, blob)) *> loop
    loop
