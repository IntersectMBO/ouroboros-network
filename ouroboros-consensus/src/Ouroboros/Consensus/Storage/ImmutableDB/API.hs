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
  , traverseIterator
  , iteratorToList
    -- * Types
  , Tip (..)
  , tipToRealPoint
  , tipToPoint
  , tipToAnchor
  , blockToTip
  , CompareTip (..)
    -- * Wrappers that preserve 'HasCallStack'
  , closeDB
  , getTip
  , getBlockComponent
  , appendBlock
  , stream
    -- * Derived functionality
  , withDB
  , getKnownBlockComponent
  , streamAfterKnownPoint
  , streamAfterPoint
  , streamAll
  , hasBlock
  , getTipPoint
  , getTipAnchor
  , getTipSlot
  ) where

import           Control.Monad.Except (ExceptT (..), lift, runExceptT,
                     throwError)
import           Data.Either (isRight)
import           Data.Function (on)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.Common

import           Ouroboros.Consensus.Storage.ImmutableDB.Error

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
-- in case of an 'UnexpectedError'.
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
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "ImmutableDB" (ImmutableDB m blk)

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
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "Iterator" (Iterator m blk b)

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

deriving instance StandardHash blk => Eq                 (Tip blk)
deriving instance StandardHash blk => Show               (Tip blk)
deriving instance StandardHash blk => NoUnexpectedThunks (Tip blk)

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
      Left missing -> throwUnexpectedError $ MissingBlockError missing
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
      either (throwUnexpectedError . MissingBlockError) return

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
