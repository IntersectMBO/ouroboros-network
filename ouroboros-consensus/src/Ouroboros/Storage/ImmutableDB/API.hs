{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE RankNTypes        #-}
module Ouroboros.Storage.ImmutableDB.API
  ( ImmutableDB (..)
  , withDB
  , Iterator (..)
  , IteratorResult (..)
  , iteratorToList

  , module Ouroboros.Storage.ImmutableDB.Types
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..),
                     ThunkInfo (..))

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString)
import           Data.Function (on)

import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow

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

    -- | Return the tip of the database.
    --
    -- The tip of the database will never point to an unfilled slot or missing
    -- EBB.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getTip
      :: HasCallStack => m (ImmTipWithHash hash)

    -- | Get the block as a 'ByteString' and its header hash stored at the given
    -- 'SlotNo'.
    --
    -- Returns 'Nothing' if no blob was stored at the given slot.
    --
    -- Throws a 'ReadFutureSlotError' if the requested slot is in the future,
    -- i.e > the result of 'getTip'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getBlock
      :: HasCallStack => SlotNo -> m (Maybe (hash, ByteString))

    -- | TODO
  , getBlockHeader
      :: HasCallStack => SlotNo -> m (Maybe (hash, ByteString))

    -- | TODO
  , getBlockHash
      :: HasCallStack => SlotNo -> m (Maybe hash)

    -- | Get the EBB (Epoch Boundary Block) as a 'ByteString' and its header
    -- hash of the given epoch.
    --
    -- Returns 'Nothing' if no EEB was stored for the given epoch.
    --
    -- Throws a 'ReadFutureEBBError' if the requested EBB is in the future.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , getEBB
      :: HasCallStack => EpochNo -> m (Maybe (hash, ByteString))

    -- | TODO
  , getEBBHeader
      :: HasCallStack => EpochNo -> m (Maybe (hash, ByteString))

    -- | TODO
  , getEBBHash
      :: HasCallStack => EpochNo -> m (Maybe hash)

    -- | Appends a block at the given slot.
    --
    -- Throws an 'AppendToSlotInThePastError' if the given slot is <= the
    -- result of 'getTip'.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO the given binary blob may not be empty.
  , appendBlock
      :: HasCallStack => SlotNo -> hash -> BinaryInfo Builder -> m ()

    -- | Appends a block as the EBB of the given epoch.
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
      :: HasCallStack => EpochNo -> hash -> BinaryInfo Builder -> m ()

    -- | Return an 'Iterator' to efficiently stream binary blocks out of the
    -- database.
    --
    -- Optionally, a start position (first argument) and/or a stop position
    -- (second argument) can be given that will be used to determine which
    -- range of blocks should be streamed.
    --
    -- The start and stop position are of type @(SlotNo, hash)@. Both are
    -- inclusive bounds. The 'SlotNo' can refer to an EBB, in which case the
    -- @hash@ is used to distinguish it from the regular block in the same
    -- slot. When there is no block (or EBB) in the given slot with the given
    -- hash, a 'WrongBoundError' is returned.
    --
    -- When no start position is given, streaming wil start from the first
    -- block in the database. When no stop position is given, streaming will
    -- stop at the tip of the database at the time of opening the iterator.
    -- This means that appends happening while streaming will not be visible
    -- to the iterator.
    --
    -- Slots that do not store a block are skipped by the iterator.
    --
    -- Throws an 'InvalidIteratorRangeError' if the start of the range is
    -- greater than the end of the range.
    --
    -- NOTE: 'WrongBoundError' is returned, but 'InvalidIteratorRangeError' is
    -- thrown. This is because the former is expected to occur during normal
    -- operation: the user doesn't know upfront if those blocks can be
    -- streamed or not. Checking it beforehand would be expensive and
    -- inefficient, and 'streamBlocks' is the best place to do it anyway. The
    -- latter exception indicates incorrect usage and should not happen during
    -- normal operation.
    --
    -- Throws a 'ReadFutureSlotError' if the start or end 'SlotNo' are in the
    -- future.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- The iterator is automatically closed when exhausted, and can be
    -- prematurely closed with 'iteratorClose'.
  , streamBlocks
      :: HasCallStack
      => Maybe (SlotNo, hash)
      -> Maybe (SlotNo, hash)
      -> m (Either (WrongBoundError hash)
                   (Iterator hash m ByteString))

    -- | Same as 'streamBlocks', but only the headers are streamed, not the
    -- whole blocks.
  , streamHeaders
      :: HasCallStack
      => Maybe (SlotNo, hash)
      -> Maybe (SlotNo, hash)
      -> m (Either (WrongBoundError hash)
                   (Iterator hash m ByteString))

    -- | Throw 'ImmutableDB' errors
  , immutableDBErr :: ErrorHandling ImmutableDBError m
  }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "ImmutableDB" (ImmutableDB hash m)

-- | An 'Iterator' is a handle which can be used to efficiently stream binary
-- blobs. Slots not containing a blob and missing EBBs are skipped.
data Iterator hash m a = Iterator
  { -- | Steps an 'Iterator' yielding an 'IteratorResult'.
    --
    -- After returning the blob as an 'IteratorResult', the iterator is
    -- advanced to the next non-empty slot or non-empty EBB.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    --
    -- TODO NOTE(adn): This works under the assumption that the user is the
    -- exclusive owner of the iterator.
    --
    -- The iterator is automatically closed when exhausted
    -- ('IteratorExhausted'), and can be prematurely closed with
    -- 'iteratorClose'.
    iteratorNext    :: HasCallStack => m (IteratorResult hash a)

    -- | Read the blob the 'Iterator' is currently pointing.
    --
    -- This operation is idempotent.
    --
    -- The next time 'iteratorNext' is called, the same 'IteratorResult' will
    -- be returned.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
  , iteratorPeek    :: HasCallStack => m (IteratorResult hash a)

    -- | Return 'True' if there is a next blob to return. Return 'False' if
    -- not.
    --
    -- This operation is idempotent.
  , iteratorHasNext :: HasCallStack => m Bool

    -- | Dispose of the 'Iterator' by closing any open handles.
    --
    -- Idempotent operation.
  , iteratorClose   :: HasCallStack => m ()

    -- | A identifier for the 'Iterator' that is unique for @m@.
    --
    -- This used for the 'Eq' instance, which is needed for testing.
    --
    -- TODO how can we avoid this abstraction leak?
  , iteratorID      :: IteratorID
  }

-- | This only contains actions, we don't check anything
instance NoUnexpectedThunks (Iterator hash m a) where
  showTypeOf _ = "Iterator"
  whnfNoUnexpectedThunks _ctxt _itr = return NoUnexpectedThunks

instance Functor m => Functor (Iterator hash m) where
  fmap f itr = Iterator{
        iteratorNext    = fmap f <$> iteratorNext itr
      , iteratorPeek    = fmap f <$> iteratorPeek itr
      , iteratorHasNext = iteratorHasNext itr
      , iteratorClose   = iteratorClose itr
      , iteratorID      = DerivedIteratorID $ iteratorID itr
      }

-- | Equality based on 'iteratorID'
instance Eq (Iterator hash m a) where
  (==) = (==) `on` iteratorID

instance Ord (Iterator hash m a) where
  compare = compare `on` iteratorID

-- | The result of stepping an 'Iterator'.
data IteratorResult hash a
  = IteratorExhausted
  | IteratorResult    SlotNo  hash a
  | IteratorEBB       EpochNo hash a
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the 'IteratorResult's (excluding the final 'IteratorExhausted') produced by
-- the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m)
               => Iterator hash m a -> m [IteratorResult hash a]
iteratorToList it = go
  where
    go = do
      next <- iteratorNext it
      case next of
        IteratorExhausted -> return []
        _                 -> (next:) <$> go
