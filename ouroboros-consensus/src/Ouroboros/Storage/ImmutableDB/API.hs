{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Ouroboros.Storage.ImmutableDB.API
  ( ImmutableDB(..)
  , withDB
  , getCurrentEpoch
  , Iterator(..)

  , IteratorResult(..)
  , iteratorToList
  , blobProducer

  , module Ouroboros.Storage.ImmutableDB.Types
  ) where

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Except (MonadError, ExceptT(ExceptT), runExceptT,
                             throwError)

import Data.Function (on)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)

import GHC.Stack (HasCallStack)

import Pipes (Producer, yield, lift)

import Ouroboros.Storage.FS.Class.Types
import Ouroboros.Storage.ImmutableDB.Types


-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB :: (HasCallStack, MonadMask m)
       => m (Either ImmutableDBError (ImmutableDB m))
          -- ^ How to open the database
       -> (ImmutableDB m -> m (Either ImmutableDBError a))
          -- ^ Action to perform using the database
       -> m (Either ImmutableDBError a)
withDB openDB action = runExceptT $
    bracket (ExceptT openDB) (ExceptT . closeDB) (ExceptT . action)

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
      :: HasCallStack => m (Either ImmutableDBError ())
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
      :: HasCallStack => m (Either ImmutableDBError EpochSlot)

    -- | Return the folder in which the database was opened.
  , getDBFolder :: FsPath

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
      => EpochSlot -> m (Either ImmutableDBError (Maybe ByteString))

    -- | Appends a binary blob at the given relative slot in the current epoch
    -- file.
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
      => RelativeSlot -> Builder -> m (Either ImmutableDBError ())

    -- | Close the current epoch and start a new epoch (the epoch number is
    -- incremented by 1). The new epoch number is returned.
    --
    -- The size of the new epoch must be passed.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , startNewEpoch
      :: HasCallStack => EpochSize -> m (Either ImmutableDBError Epoch)
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
      -> m (Either ImmutableDBError (Iterator m))
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
    iteratorNext :: HasCallStack => m (Either ImmutableDBError IteratorResult)

    -- | Dispose of the 'Iterator' by closing any open handles.
    --
    -- Idempotent operation.
  , iteratorClose :: HasCallStack => m (Either ImmutableDBError ())

    -- | A identifier for the 'Iterator' that is unique for @m@.
    --
    -- This used for the 'Eq' instance, which is needed for testing.
    --
    -- TODO how can we avoid this abstraction leak?
  , iteratorID  :: IteratorID
  }

-- | Equality based on 'iteratorID'
instance Eq (Iterator m) where
  (==) = (==) `on` iteratorID


-- | Return the currently opened epoch that can be appended to.
--
-- Throws a 'ClosedDBError' if the database is closed.
getCurrentEpoch :: (HasCallStack, Functor m)
                => ImmutableDB m
                -> m (Either ImmutableDBError Epoch)
getCurrentEpoch db = fmap _epoch <$> getNextEpochSlot db

-- | The result of stepping an 'Iterator'.
data IteratorResult
  = IteratorExhausted
  | IteratorResult EpochSlot ByteString
  deriving (Show, Eq)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the blobs produced by the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m)
               => Iterator m
               -> m (Either ImmutableDBError [ByteString])
iteratorToList it = runExceptT $ go mempty
  where
    go acc = do
      next <- ExceptT $ iteratorNext it
      case next of
        IteratorResult _ x -> go (x : acc)
        IteratorExhausted  -> return $ reverse acc

-- | A 'Producer' that streams binary blobs from the database in the
-- given range.
blobProducer :: (HasCallStack, MonadError ImmutableDBError m)
             => ImmutableDB m
             -> EpochSlot   -- ^ When to start streaming (inclusive).
             -> EpochSlot   -- ^ When to stop streaming (inclusive).
             -> Producer (EpochSlot, ByteString) m ()
blobProducer db start end = do
    it <- liftErr $ streamBinaryBlobs db start end
    let loop = do
          res <- liftErr $ iteratorNext it
          case res of
              IteratorExhausted -> liftErr $ iteratorClose it
              IteratorResult epochSlot blob -> yield (epochSlot, blob) *> loop
    loop
  where
    liftErr action = do
      res <- lift action
      case res of
        Left  e -> throwError e
        Right r -> return r

