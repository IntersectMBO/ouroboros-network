{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
module Ouroboros.Consensus.Storage.ImmutableDB.API (
    -- * API
    ImmutableDB (..)
    -- * Iterator API
  , Iterator (..)
  , IteratorResult (..)
  , iteratorToList
  , traverseIterator
    -- * Types
  , CompareTip (..)
  , Tip (..)
  , blockToTip
  , tipToAnchor
  , tipToPoint
  , tipToRealPoint
    -- * Errors
  , ApiMisuse (..)
  , ImmutableDBError (..)
  , MissingBlock (..)
  , UnexpectedFailure (..)
  , missingBlockPoint
  , throwApiMisuse
  , throwUnexpectedFailure
    -- * Wrappers that preserve 'HasCallStack'
  , appendBlock
  , closeDB
  , getBlockComponent
  , getTip
  , stream
    -- * Derived functionality
  , getKnownBlockComponent
  , getTipAnchor
  , getTipPoint
  , getTipSlot
  , hasBlock
  , streamAfterKnownPoint
  , streamAfterPoint
  , streamAll
  , withDB
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Monad.Except (ExceptT (..), lift, runExceptT,
                     throwError)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Either (isRight)
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))

import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, FsPath)
import           Ouroboros.Consensus.Storage.FS.CRC (CRC)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | API for the 'ImmutableDB'.
--
-- The 'ImmutableDB' stores blocks in 'SlotNo's. Nevertheless, lookups use
-- 'RealPoint', primarily because Epoch Boundary Blocks (EBBs) have the same
-- 'SlotNo' as the regular block after them (unless that slot is empty), so that
-- we have to use the hash of the block to distinguish the two (hence
-- 'RealPoint'). But also to avoid reading the wrong block, i.e., when we expect
-- a block with a different hash.
--
-- The database is append-only, so you cannot append a block to a slot in the
-- past. You can, however, skip slots, e.g., append to slot 0 and then to slot
-- 5, but afterwards, you can no longer append to slots 1-4. You can only store
-- at most one block in each slot, except for EBBs, which are stored separately,
-- at the start of each epoch/chunk.
--
-- The block stored in a slot can be queried with 'getBlockComponent'. Block
-- components can also be streamed using 'Iterator's, see 'stream'.
--
-- The 'Tip' of the database can be queried with 'getTip'. This tip will
-- always point to a filled slot or an EBB that is present.
--
-- The database can be explicitly closed, but can also be automatically closed
-- in case of an 'UnexpectedFailure'.
data ImmutableDB m blk = ImmutableDB {
      -- | Close the database.
      --
      -- Idempotent.
      --
      -- __Note__: Use 'withDB' instead of this function.
      closeDB_ :: HasCallStack => m ()

      -- | Return the tip of the database.
      --
      -- The tip of the database will never point to an unfilled slot or missing
      -- EBB.
      --
      -- Throws a 'ClosedDBError' if the database is closed.
    , getTip_ :: HasCallStack => STM m (WithOrigin (Tip blk))

      -- | Get the block component of the block with the given 'Point'.
      --
      -- The hash of the point is used to distinguish a potential EBB from the
      -- regular block in the same slot.
      --
      -- Returns a 'MissingBlockError' if no block was stored with the given
      -- 'Point', either because the slot was empty or because the block stored
      -- with that slot had a different hash.
      --
      -- Throws a 'ClosedDBError' if the database is closed.
    , getBlockComponent_ ::
           forall b. HasCallStack
        => BlockComponent blk b -> RealPoint blk -> m (Either (MissingBlock blk) b)

      -- | Appends a block to the ImmutableDB.
      --
      -- Throws an 'AppendBlockNotNewerThanTipError' if the given slot is <= the
      -- result of 'getTip'.
      --
      -- Throws a 'ClosedDBError' if the database is closed.
    , appendBlock_
        :: HasCallStack => blk -> m ()

      -- | Return an 'Iterator' to efficiently stream blocks from the
      -- ImmutableDB.
      --
      -- Throws an 'InvalidIteratorRangeError' if the start of the range is
      -- greater than the end of the range.
      --
      -- NOTE: 'MissingBlock' is returned, but 'InvalidIteratorRangeError' is
      -- thrown. This is because the former is expected to occur during normal
      -- operation: a node serving blocks might get requests to stream blocks
      -- that are not in the database. The latter exception indicates incorrect
      -- usage and should not happen during normal operation.
      --
      -- Throws a 'ClosedDBError' if the database is closed.
      --
      -- The iterator is automatically closed when exhausted, and can be
      -- prematurely closed with 'iteratorClose'.
    , stream_
        :: forall b. HasCallStack
        => ResourceRegistry m
        -> BlockComponent blk b
        -> StreamFrom blk
        -> StreamTo   blk
        -> m (Either (MissingBlock blk) (Iterator m blk b))
    }
  deriving NoThunks via OnlyCheckWhnfNamed "ImmutableDB" (ImmutableDB m blk)

{-------------------------------------------------------------------------------
  Iterator API
-------------------------------------------------------------------------------}

-- | An 'Iterator' is a handle which can be used to efficiently stream block
-- components from the ImmutableDB.
data Iterator m blk b = Iterator {
      -- | Steps an 'Iterator' yielding an 'IteratorResult'.
      --
      -- After returning the block component as an 'IteratorResult', the
      -- iterator is advanced to the next non-empty slot or non-empty EBB.
      --
      -- Throws a 'ClosedDBError' if the database is closed.
      --
      -- The iterator is automatically closed when exhausted
      -- ('IteratorExhausted'), and can be prematurely closed with
      -- 'iteratorClose'.
      iteratorNext    :: HasCallStack => m (IteratorResult b)

      -- | Return the point of the next block to stream, if there is one. Return
      -- 'Nothing' if not.
      --
      -- This operation is idempotent.
    , iteratorHasNext :: HasCallStack => STM m (Maybe (RealPoint blk))

      -- | Dispose of the 'Iterator' by closing any open handles.
      --
      -- Idempotent operation.
    , iteratorClose   :: HasCallStack => m ()
    }
  deriving (Functor)
  deriving NoThunks via OnlyCheckWhnfNamed "Iterator" (Iterator m blk b)

-- | Variant of 'traverse' instantiated to @'Iterator' m blk m@ that executes
-- the monadic function when calling 'iteratorNext'.
traverseIterator
  :: Monad m
  => (b -> m b')
  -> Iterator m blk b
  -> Iterator m blk b'
traverseIterator f itr = Iterator{
      iteratorNext    = iteratorNext    itr >>= traverse f
    , iteratorHasNext = iteratorHasNext itr
    , iteratorClose   = iteratorClose   itr
    }

-- | The result of stepping an 'Iterator'.
data IteratorResult b
  = IteratorExhausted
  | IteratorResult b
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

-- | Consume an 'Iterator' by stepping until it is exhausted. A list of all
-- the 'IteratorResult's (excluding the final 'IteratorExhausted') produced by
-- the 'Iterator' is returned.
iteratorToList :: (HasCallStack, Monad m)
               => Iterator m blk b -> m [b]
iteratorToList it = go []
  where
    go acc = do
      next <- iteratorNext it
      case next of
        IteratorExhausted  -> return $ reverse acc
        IteratorResult res -> go (res:acc)

-- | An iterator that is immediately exhausted.
emptyIterator :: MonadSTM m => Iterator m blk b
emptyIterator = Iterator {
      iteratorNext    = return IteratorExhausted
    , iteratorHasNext = return Nothing
    , iteratorClose   = return ()
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Information about the tip of the ImmutableDB.
data Tip blk = Tip {
      tipSlotNo  :: !SlotNo
    , tipIsEBB   :: !IsEBB
    , tipBlockNo :: !BlockNo
    , tipHash    :: !(HeaderHash blk)
    }
  deriving (Generic)

deriving instance StandardHash blk => Eq       (Tip blk)
deriving instance StandardHash blk => Show     (Tip blk)
deriving instance StandardHash blk => NoThunks (Tip blk)

tipToRealPoint :: Tip blk -> RealPoint blk
tipToRealPoint Tip { tipSlotNo, tipHash } = RealPoint tipSlotNo tipHash

tipToPoint :: WithOrigin (Tip blk) -> Point blk
tipToPoint = \case
    Origin        -> GenesisPoint
    NotOrigin tip -> realPointToPoint $ tipToRealPoint tip

tipToAnchor :: WithOrigin (Tip blk) -> AF.Anchor blk
tipToAnchor = \case
    Origin ->
      AF.AnchorGenesis
    NotOrigin (Tip { tipSlotNo, tipHash, tipBlockNo }) ->
      AF.Anchor tipSlotNo tipHash tipBlockNo

blockToTip :: (HasHeader blk, GetHeader blk) => blk -> Tip blk
blockToTip blk = Tip {
      tipSlotNo  = blockSlot    blk
    , tipIsEBB   = blockToIsEBB blk
    , tipBlockNo = blockNo      blk
    , tipHash    = blockHash    blk
    }

-- | newtype with an 'Ord' instance that only uses 'tipSlotNo' and 'tipIsEBB'
-- and ignores the other fields.
newtype CompareTip blk = CompareTip { getCompareTip :: Tip blk }

instance Eq (CompareTip blk) where
  a == b = compare a b == EQ

instance Ord (CompareTip blk) where
  compare = mconcat [
        compare      `on` tipSlotNo . getCompareTip
      , compareIsEBB `on` tipIsEBB  . getCompareTip
      ]
    where
      -- When a block and an EBB share a slot number, the EBB is "older".
      compareIsEBB :: IsEBB -> IsEBB -> Ordering
      compareIsEBB IsEBB    IsNotEBB = LT
      compareIsEBB IsNotEBB IsEBB    = GT
      compareIsEBB _        _        = EQ

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors that might arise when working with this database.
data ImmutableDBError blk =
    ApiMisuse (ApiMisuse blk) PrettyCallStack
    -- ^ An error thrown because of incorrect usage of the immutable database
    -- by the user.
  | UnexpectedFailure (UnexpectedFailure blk)
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
  deriving (Generic, Show)

instance (StandardHash blk, Typeable blk)
      => Exception (ImmutableDBError blk) where
  displayException = \case
      ApiMisuse {} ->
        "ImmutableDB incorrectly used, indicative of a bug"
      UnexpectedFailure (FileSystemError fse) ->
        displayException fse
      UnexpectedFailure {} ->
        "The ImmutableDB got corrupted, full validation will be enabled for the next startup"

data ApiMisuse blk =
    -- | When trying to append a new block, it was not newer than the current
    -- tip, i.e., the slot was older than or equal to the current tip's slot.
    --
    -- The 'RealPoint' corresponds to the new block and the 'Point' to the
    -- current tip.
    AppendBlockNotNewerThanTipError (RealPoint blk) (Point blk)

    -- | When the chosen iterator range was invalid, i.e. the @start@ (first
    -- parameter) came after the @end@ (second parameter).
  | InvalidIteratorRangeError (StreamFrom blk) (StreamTo blk)

    -- | When performing an operation on a closed DB that is only allowed when
    -- the database is open.
  | ClosedDBError

    -- | When performing an operation on an open DB that is only allowed when
    -- the database is closed.
  | OpenDBError

deriving instance (StandardHash blk, Typeable blk) => Show (ApiMisuse blk)

throwApiMisuse ::
     (MonadThrow m, HasCallStack, StandardHash blk, Typeable blk)
  => ApiMisuse blk -> m a
throwApiMisuse e = throwIO $ ApiMisuse e prettyCallStack

data UnexpectedFailure blk =
    -- | An IO operation on the file-system threw an error.
    FileSystemError FsError -- An FsError already stores the callstack

    -- | When loading an epoch or index file, its contents did not pass
    -- validation.
  | InvalidFileError FsPath String PrettyCallStack

    -- | A missing epoch or index file.
  | MissingFileError FsPath PrettyCallStack

    -- | There was a checksum mismatch when reading the block with the given
    -- point. The first 'CRC' is the expected one, the second one the actual
    -- one.
  | ChecksumMismatchError (RealPoint blk) CRC CRC FsPath PrettyCallStack

    -- | A block failed to parse
  | ParseError FsPath (RealPoint blk) CBOR.DeserialiseFailure

    -- | When parsing a block we got some trailing data
  | TrailingDataError FsPath (RealPoint blk) Lazy.ByteString

    -- | Block missing
    --
    -- This exception gets thrown when a block that we /know/ it should be in
    -- the ImmutableDB, nonetheless was not found.
  | MissingBlockError (MissingBlock blk)

    -- | A (parsed) block did not pass the integrity check.
    --
    -- This exception gets thrown when a block doesn't pass the integrity check
    -- done for 'GetVerifiedBlock'.
    --
    -- NOTE: we do not check the integrity of a block when it is added to the
    -- ImmutableDB. While this exception typically means the block has been
    -- corrupted, it could also mean the block didn't pass the check at the time
    -- it was added.
  | CorruptBlockError (RealPoint blk)

deriving instance (StandardHash blk, Typeable blk) => Show (UnexpectedFailure blk)

throwUnexpectedFailure ::
     (StandardHash blk, Typeable blk, MonadThrow m)
  => UnexpectedFailure blk -> m a
throwUnexpectedFailure = throwIO . UnexpectedFailure

-- | This type can be part of an exception, but also returned as part of an
-- 'Either', because it can be expected in some cases.
data MissingBlock blk
    -- | There is no block in the slot of the given point.
  = EmptySlot (RealPoint blk)
    -- | The block and/or EBB in the slot of the given point have a different
    -- hash.
  | WrongHash (RealPoint blk) (NonEmpty (HeaderHash blk))
    -- | The requested point is in the future, i.e., its slot is greater than
    -- that of the tip. We record the tip as the second argument.
  | NewerThanTip (RealPoint blk) (Point blk)
  deriving (Eq, Show, Generic)

-- | Return the 'RealPoint' of the block that was missing.
missingBlockPoint :: MissingBlock blk -> RealPoint blk
missingBlockPoint (EmptySlot pt)      = pt
missingBlockPoint (WrongHash pt _)    = pt
missingBlockPoint (NewerThanTip pt _) = pt

{-------------------------------------------------------------------------------
  Wrappers that preserve 'HasCallStack'

  @ghc@ really should do this for us :-/
-------------------------------------------------------------------------------}

closeDB ::
     HasCallStack
  => ImmutableDB m blk
  -> m ()
closeDB = closeDB_

getTip ::
     HasCallStack
  => ImmutableDB m blk
  -> STM m (WithOrigin (Tip blk))
getTip = getTip_

getBlockComponent ::
     HasCallStack
  => ImmutableDB m blk
  -> BlockComponent blk b -> RealPoint blk -> m (Either (MissingBlock blk) b)
getBlockComponent = getBlockComponent_

appendBlock ::
     HasCallStack
  => ImmutableDB m blk
  -> blk -> m ()
appendBlock = appendBlock_

stream ::
     HasCallStack
  => ImmutableDB m blk
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> StreamFrom blk
  -> StreamTo   blk
  -> m (Either (MissingBlock blk) (Iterator m blk b))
stream = stream_

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | Open the database using the given function, perform the given action
-- using the database, and closes the database using its 'closeDB' function,
-- in case of success or when an exception was raised.
withDB ::
     (HasCallStack, MonadThrow m)
  => m (ImmutableDB m blk)
     -- ^ How to open the database
  -> (ImmutableDB m blk -> m a)
     -- ^ Action to perform using the database
  -> m a
withDB openDB = bracket openDB closeDB

getKnownBlockComponent ::
     (MonadThrow m, HasHeader blk)
  => ImmutableDB m blk
  -> BlockComponent blk b
  -> RealPoint blk
  -> m b
getKnownBlockComponent db blockComponent pt =
    getBlockComponent db blockComponent pt >>= \case
      Left missing -> throwUnexpectedFailure $ MissingBlockError missing
      Right b      -> return b

-- | Open an iterator with the given point as lower exclusive bound and the
-- current tip as the inclusive upper bound.
--
-- Returns a 'MissingBlock' when the point is not in the ImmutableDB.
streamAfterPoint ::
     (MonadSTM m, HasHeader blk, HasCallStack)
  => ImmutableDB m blk
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> Point blk
  -> m (Either (MissingBlock blk) (Iterator m blk b))
streamAfterPoint db registry blockComponent fromPt = runExceptT $ do
    tipPt <- lift $ atomically $ getTipPoint db
    case (pointToWithOriginRealPoint fromPt,
          pointToWithOriginRealPoint tipPt) of

      (Origin, Origin) ->
        -- Nothing to stream
        return emptyIterator

      (NotOrigin fromPt', Origin) ->
        -- Asked to stream after a block while the ImmutableDB is empty
        throwError $ NewerThanTip fromPt' GenesisPoint

      (NotOrigin fromPt', NotOrigin _) | pointSlot fromPt > pointSlot tipPt ->
        -- Lower bound is newer than the tip, nothing to stream
        throwError $ NewerThanTip fromPt' tipPt

      (NotOrigin fromPt', NotOrigin tipPt') | fromPt' == tipPt' ->
        -- Nothing to stream after the tip
        return emptyIterator

      (_, NotOrigin tipPt') ->
        -- Stream from the given point to the current tip (not genesis)
        ExceptT $ stream
          db
          registry
          blockComponent
          (StreamFromExclusive fromPt)
          (StreamToInclusive tipPt')

-- | Variant of 'streamAfterPoint' that throws a 'MissingBlockError' when the
-- point is not in the ImmutableDB (or genesis).
streamAfterKnownPoint ::
     (MonadSTM m, MonadThrow m, HasHeader blk, HasCallStack)
  => ImmutableDB m blk
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> Point blk
  -> m (Iterator m blk b)
streamAfterKnownPoint db registry blockComponent fromPt =
    streamAfterPoint db registry blockComponent fromPt >>=
      either (throwUnexpectedFailure . MissingBlockError) return

streamAll ::
     (MonadSTM m, MonadThrow m, HasHeader blk, HasCallStack)
  => ImmutableDB m blk
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> m (Iterator m blk b)
streamAll db registry blockComponent =
    streamAfterKnownPoint db registry blockComponent GenesisPoint

hasBlock ::
     (MonadSTM m, HasCallStack)
  => ImmutableDB m blk
  -> RealPoint blk
  -> m Bool
hasBlock db pt = isRight <$> getBlockComponent db (pure ()) pt

getTipPoint ::
     (MonadSTM m, HasCallStack)
  => ImmutableDB m blk -> STM m (Point blk)
getTipPoint = fmap tipToPoint . getTip

getTipAnchor ::
     (MonadSTM m, HasCallStack)
  => ImmutableDB m blk -> STM m (AF.Anchor blk)
getTipAnchor = fmap tipToAnchor . getTip

getTipSlot ::
     (MonadSTM m, HasCallStack)
  => ImmutableDB m blk -> STM m (WithOrigin SlotNo)
getTipSlot = fmap (fmap tipSlotNo) . getTip
