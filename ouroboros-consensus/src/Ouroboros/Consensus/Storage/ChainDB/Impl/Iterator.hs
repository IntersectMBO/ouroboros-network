{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Iterators
module Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator
  ( stream
  , closeAllIterators
    -- * Exported for testing purposes
  , IteratorEnv (..)
  , newIterator
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.Except (ExceptT (..), catchError, lift,
                     runExceptT, throwError)
import           Control.Tracer
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Slotting.Block (BlockNo)

import           Ouroboros.Network.Block (pattern BlockPoint, ChainHash (..),
                     pattern GenesisPoint, HasHeader, HeaderHash, Point,
                     SlotNo, StandardHash, pointHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     ChainDB, ChainDbError (..), Iterator (..),
                     IteratorResult (..), StreamFrom (..), StreamTo (..),
                     UnknownRange (..), getPoint, validBounds)

import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB as VolDB

-- | Stream blocks
--
-- = Start & end point
--
-- The start point can either be in the ImmutableDB (on our chain) or in the
-- VolatileDB (on our chain or on a recent fork). We first check whether it is
-- in the VolatileDB, if not, we check if it is in the ImmutableDB (see
-- \"Garbage collection\" for why this order is important). Similarly for the
-- end point.
--
-- If a bound can't be found in the ChainDB, an 'UnknownRange' error is
-- returned.
--
-- When the bounds are nonsensical, e.g.,
-- > StreamFromExclusive (Point { pointSlot = SlotNo 3 , .. }
-- > StreamToInclusive   (Point { pointSlot = SlotNo 3 , .. }
-- An 'InvalidIteratorRange' exception is thrown.
--
-- = Paths of blocks
--
-- To stream blocks from the ImmutableDB we can simply use the iterators
-- offered by the ImmutableDB.
--
-- To stream blocks from the VolatileDB we have to construct a path of block
-- hashes backwards through the VolatileDB, starting from the end point using
-- 'getPredecessor' until we get to the start point, genesis, or we get to a
-- block that is not in the VolatileDB. Then, for each hash in the path, we
-- can ask the VolatileDB for the corresponding block.
--
-- If the path through the VolatileDB is incomplete, we will first have to
-- stream blocks from the ImmutableDB and then switch to the path through the
-- VolatileDB. We only allow the tip of the ImmutableDB to be the switchover
-- point between the two DBs. In other words, the incomplete path through the
-- VolatileDB must fit onto the tip of the ImmutableDB. This must be true at
-- the time of initialising the iterator, but does not have to hold during the
-- whole lifetime of the iterator. If it doesn't fit on it, it means the path
-- forked off more than @k@ blocks in the past and blocks belonging to it are
-- more likely to go missing because of garbage-collection (see the next
-- paragraph). In that case, we return 'ForkTooOld'.
--
-- = Garbage collection
--
-- We have to be careful about the following: as our chain grows, blocks from
-- our chain will be copied to the ImmutableDB in the background. After a
-- while, old blocks will be garbage-collected from the VolatileDB. Blocks
-- that were part of the current chain will be in the ImmutableDB, but blocks
-- that only lived on forks will be gone forever.
--
-- This means that blocks that were part of the VolatileDB when the iterator
-- was initialised might no longer be part of the VolatileDB when we come to
-- the point that the iterator will try to read them. When this is noticed, we
-- will try to open an iterator from the ImmutableDB to obtain the blocks that
-- have moved over. However, this will only work if they were and are part of
-- the current chain, otherwise they will have been deleted from the
-- VolatileDB without being copied to the ImmutableDB.
--
-- This iterator is opened with an open upper bound and will be used to stream
-- blocks until the path has been fully streamed, the iterator is exhausted,
-- or a block doesn't match the expected hash. In the latter two cases, we
-- switch back to the VolatileDB. If the block is missing from the VolatileDB,
-- we will switch back to streaming from the ImmutableDB. If that fails, we
-- switch back to the VolatileDB. To avoid eternally switching between the two
-- DBs, we only switch back to the VolatileDB if the stream from the
-- ImmutableDB has made progress, i.e. streamed at least one block with the
-- expected hash. If no block was streamed from the ImmutableDB, not even the
-- first one, we know for sure that that block isn't part of the VolatileDB
-- (the reason we switch to the ImmutableDB) and isn't part of the ImmutableDB
-- (no block was streamed). In that case, we return 'IteratorBlockGCed' and
-- stop the stream.
--
-- Note that the open upper bound doesn't allow us to include blocks in the
-- stream that are copied to the ImmutableDB after opening this iterator, as
-- the bound of the iterator is fixed upon initialisation. These newly added
-- blocks will be included in the stream because we will repeatedly open new
-- ImmutableDB iterators (as long as we make progress).
--
-- = Bounds checking
--
-- The VolatileDB is hash-based instead of point-based. While the bounds of a
-- stream are /point/s, we can simply check whether the hashes of the bounds
-- match the hashes stored in the points.
--
-- The ImmutableDB is slot-based instead of point-based, which means that
-- before we know whether a block in the ImmutableDB matches a given point, we
-- must first read the block's hash corresponding to the point's slot from the
-- (cached) on-disk indices, after which we can then verify whether it matches
-- the hash of the point. This is important for the start and end bounds (both
-- points) of a stream in case they are in the ImmutableDB (i.e., their slots
-- are <= the tip of the ImmutableDB): we must first read the hashes
-- corresponding to the bounds from the (cached) on-disk indices to be sure
-- the range is valid. Note that these reads happen before the first call to
-- 'iteratorNext'.
--
-- Note that when streaming to an /exclusive/ bound, the block corresponding
-- to that bound ('Point') must exist in the ChainDB.
--
-- The ImmutableDB will keep the on-disk indices of a chunk of blocks in
-- memory after the first read so that the next lookup doesn't have to read
-- from disk. When both bounds are in the same chunk, which will typically be
-- the case, only checking the first bound will require disk reads, the second
-- will be cached.
--
-- = Costs
--
-- Opening an iterator has some costs:
--
-- * When blocks have to be streamed from the ImmutableDB: as discussed in
--   \"Bounds checking\", the hashes corresponding to the bounds have to be
--   read from the (cached) on-disk indices.
--
-- * When blocks have to be streamed both from the ImmutableDB and the
--   VolatileDB, only the hash of the block corresponding to the lower bound
--   will have to be read from the ImmutableDB upfront, as described in the
--   previous bullet point. Note that the hash of the block corresponding to
--   the upper bound does not have to be read from disk, since it will be in
--   the VolatileDB, which means that we know its hash already from the
--   in-memory index.
--
-- * When an exclusive lower bound requires streaming from the ImmutableDB,
--   the block corresponding to that lower bound is read from disk (if the
--   passed 'BlockComponent' requires reading from disk) because the
--   ImmutableDB doesn't support exclusive bounds. There is no such cost
--   attached to an exclusive upper bound. See #548.
--
-- In summary:
--
-- * Only streaming from the VolatileDB: 0 (cached) reads from disk upfront.
-- * Only streaming from the ImmutableDB: 2 (cached) reads from disk upfront +
--   1 block read from disk upfront in case of an exclusive lower bound.
-- * Streaming from both the ImmutableDB and the VolatileDB: 1 (cached) read
--   from disk upfront + 1 block read from disk upfront in case of an
--   exclusive lower bound.
--
-- Additionally, when we notice during streaming that a block is no longer in
-- the VolatileDB, we try to see whether it can be streamed from the
-- ImmutableDB instead. Opening such an iterator costs 2 (cached) reads from
-- disk upfront and 1 block read from disk upfront as it uses an exclusive
-- lower bound. This can happen multiple times.
stream
  :: forall m blk b. (IOLike m, HasHeader blk, HasCallStack)
  => ChainDbHandle m blk
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> StreamFrom blk
  -> StreamTo blk
  -> m (Either (UnknownRange blk) (Iterator m blk b))
stream h registry blockComponent from to = getEnv h $ \cdb ->
    newIterator (fromChainDbEnv cdb) getItEnv registry blockComponent from to
  where
    getItEnv :: forall r. (IteratorEnv m blk -> m r) -> m r
    getItEnv f = getEnv h (f . fromChainDbEnv)

{-------------------------------------------------------------------------------
  Iterator environment
-------------------------------------------------------------------------------}

-- | Environment containing everything needed to implement iterators.
--
-- The main purpose of bundling these things in a separate record is to make
-- it easier to test this code: no need to set up a whole ChainDB, just
-- provide this record.
data IteratorEnv m blk = IteratorEnv
  { itImmDB           :: ImmDB m blk
  , itVolDB           :: VolDB m blk
  , itIterators       :: StrictTVar m (Map IteratorKey (m ()))
  , itNextIteratorKey :: StrictTVar m IteratorKey
  , itTracer          :: Tracer m (TraceIteratorEvent blk)
  }

-- | Obtain an 'IteratorEnv' from a 'ChainDbEnv'.
fromChainDbEnv :: ChainDbEnv m blk -> IteratorEnv m blk
fromChainDbEnv CDB{..} = IteratorEnv
  { itImmDB           = cdbImmDB
  , itVolDB           = cdbVolDB
  , itIterators       = cdbIterators
  , itNextIteratorKey = cdbNextIteratorKey
  , itTracer          = contramap TraceIteratorEvent cdbTracer
  }

-- | See 'streamBlocks'.
newIterator
  :: forall m blk b. (IOLike m, HasHeader blk, HasCallStack)
  => IteratorEnv m blk
  -> (forall r. (IteratorEnv m blk -> m r) -> m r)
     -- ^ Function with which the operations on the returned iterator should
     -- obtain their 'IteratorEnv'. This function should check whether the
     -- ChainDB is still open or throw an exception otherwise. This makes sure
     -- that when we call 'iteratorNext', we first check whether the ChainDB
     -- is still open.
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> StreamFrom blk
  -> StreamTo blk
  -> m (Either (UnknownRange blk) (Iterator m blk b))
newIterator itEnv@IteratorEnv{..} getItEnv registry blockComponent from to = do
    unless (validBounds from to) $
      throwM $ InvalidIteratorRange from to
    res <- runExceptT start
    case res of
      Left e -> trace $ UnknownRangeRequested e
      _      -> return ()
    return res
  where
    trace = traceWith itTracer

    endPoint :: RealPoint blk
    endPoint = case to of
      StreamToInclusive pt -> pt
      StreamToExclusive pt -> pt

    -- | Use the tip of the ImmutableDB to determine whether to look directly
    -- in the ImmutableDB (the range is <= the tip) or first try the
    -- VolatileDB (in the other cases).
    start :: HasCallStack
          => ExceptT (UnknownRange blk) m (Iterator m blk b)
    start = lift (ImmDB.getTipInfo itImmDB) >>= \case
      Origin                                       -> findPathInVolDB
      At (tipSlot, tipHash, tipIsEBB, _tipBlockNo) ->
        case realPointSlot endPoint `compare` tipSlot of
          -- The end point is < the tip of the ImmutableDB
          LT -> streamFromImmDB

          EQ | realPointHash endPoint == tipHash
                -- The end point == the tip of the ImmutableDB
             -> streamFromImmDB

             -- The end point /= the tip of the ImmutableDB.
             --
             -- The end point can be a regular block or EBB. So can the tip of
             -- the ImmutableDB. We distinguish the following for cases where
             -- each block and EBB has the same slot number, and a block or
             -- EBB /not/ on the current chain is indicated with a '.
             --
             -- 1. ImmutableDB: .. :> EBB :> B
             --    end point: B'
             --    desired outcome: ForkTooOld
             --
             -- 2. ImmutableDB: .. :> EBB :> B
             --    end point: EBB'
             --    desired outcome: ForkTooOld
             --
             -- 3. ImmutableDB: .. :> EBB :> B
             --    end point: EBB
             --    desired outcome: stream from ImmutableDB
             --
             -- 4. ImmutableDB: .. :> EBB
             --    end point: B
             --    desired outcome: find path in the VolatileDB
             --
             -- 5. ImmutableDB: .. :> EBB
             --    end point: B'
             --    desired outcome: ForkTooOld
             --
             -- 6. ImmutableDB: .. :> EBB
             --    end point: EBB'
             --    desired outcome: ForkTooOld
             --
             -- We don't know upfront whether the given end point refers to a
             -- block or EBB nor whether it is part of the current chain or
             -- not. This means we don't know yet with which case we are
             -- dealing. The only thing we know for sure, is whether the
             -- ImmutableDB tip ends with a regular block (1-3) or an EBB
             -- (4-6).

             | IsNotEBB <- tipIsEBB  -- Cases 1-3
             -> streamFromImmDB `catchError`
                -- We also use 'streamFromImmDB' to check whether the block or
                -- EBB is in the ImmutableDB. If that's not the case,
                -- 'streamFromImmDB' will return 'MissingBlock'. Instead of
                -- returning that, we should return 'ForkTooOld', which is
                -- more correct.
                const (throwError $ ForkTooOld from)
             | otherwise  -- Cases 4-6
             -> findPathInVolDB

          -- The end point is > the tip of the ImmutableDB
          GT -> findPathInVolDB

    -- | PRECONDITION: the upper bound >= the tip of the ImmutableDB.
    -- Greater or /equal/, because of EBBs :(
    findPathInVolDB :: HasCallStack
                    => ExceptT (UnknownRange blk) m (Iterator m blk b)
    findPathInVolDB = do
      path <- lift $ atomically $ VolDB.computePathSTM itVolDB from to
      case path of
        VolDB.NotInVolDB        _hash           -> throwError $ ForkTooOld from
        VolDB.PartiallyInVolDB  predHash hashes -> streamFromBoth predHash hashes
        VolDB.CompletelyInVolDB hashes          -> case NE.nonEmpty hashes of
          Just hashes' -> lift $ streamFromVolDB hashes'
          Nothing      -> lift $ emptyIterator

    streamFromVolDB :: NonEmpty (HeaderHash blk) -> m (Iterator m blk b)
    streamFromVolDB hashes = do
      trace $ StreamFromVolDB from to (NE.toList hashes)
      createIterator $ InVolDB from hashes

    streamFromImmDB :: ExceptT (UnknownRange blk) m (Iterator m blk b)
    streamFromImmDB = do
      lift $ trace $ StreamFromImmDB from to
      streamFromImmDBHelper to

    streamFromImmDBHelper
      :: StreamTo blk
      -> ExceptT (UnknownRange blk) m (Iterator m blk b)
    streamFromImmDBHelper to' = do
        -- 'ImmDB.stream' will check the hash of the block at the
        -- start and end bounds.
        immIt <- ExceptT $ ImmDB.stream itImmDB registry
          ((,) <$> getPoint <*> blockComponent) from to'
        lift $ createIterator $ InImmDB from immIt (StreamTo to')

    -- | If we have to stream from both the ImmutableDB and the VolatileDB, we
    -- only allow the (current) tip of the ImmutableDB to be the switchover
    -- point between the two DBs. If not, this would mean we have to stream a
    -- fork that forks off more than @k@ blocks in the past, in which case the
    -- risk of blocks going missing due to GC increases. So we refuse such a
    -- stream.
    streamFromBoth :: HasCallStack
                   => HeaderHash blk
                   -> [HeaderHash blk]
                   -> ExceptT (UnknownRange blk) m (Iterator m blk b)
    streamFromBoth predHash hashes = do
        lift $ trace $ StreamFromBoth from to hashes
        lift (toPoint <$> ImmDB.getTipInfo itImmDB) >>= \tip ->
          case pointToWithOriginRealPoint tip of
            -- The ImmutableDB is empty
            Origin -> throwError $ ForkTooOld from
            -- The incomplete path fits onto the tip of the ImmutableDB.
            At pt@(RealPoint _ tipHash)
              | tipHash == predHash
              -> case NE.nonEmpty hashes of
                   Just hashes' -> startStream pt hashes'
                   -- The path is actually empty, but the exclusive upper bound
                   -- was in the VolatileDB. Since there's nothing to stream,
                   -- just return an empty iterator.
                   Nothing      -> lift emptyIterator
              -- The incomplete path doesn't fit onto the tip of the ImmutableDB.
              -- Note that since we have constructed the incomplete path through
              -- the VolatileDB, blocks might have moved from the VolatileDB to
              -- the ImmutableDB so that the tip of the ImmutableDB has changed.
              -- Either the path used to fit onto the tip but the tip has changed,
              -- or the path simply never fitted onto the tip.
              | otherwise  -> case dropWhile (/= tipHash) hashes of
                -- The current tip is not in the path, this means that the path
                -- never fitted onto the tip of the ImmutableDB. We refuse this
                -- stream.
                []                    -> throwError $ ForkTooOld from
                -- The current tip is in the path, with some hashes after it, this
                -- means that some blocks in our path have moved from the
                -- VolatileDB to the ImmutableDB. We can shift the switchover
                -- point to the current tip.
                _tipHash:hash:hashes' -> startStream pt (hash NE.:| hashes')
                -- The current tip is the end of the path, this means we can
                -- actually stream everything from just the ImmutableDB. It
                -- could be that the exclusive end bound was not part of the
                -- ImmutableDB, so stream to the current tip of the ImmutableDB
                -- (inclusive) to avoid trying to stream (exclusive) to a block
                -- that's not in the ImmutableDB.
                [_tipHash]            -> streamFromImmDBHelper (StreamToInclusive pt)
      where
        startStream :: RealPoint blk -- ^ Tip of the imm DB
                    -> NonEmpty (HeaderHash blk)
                    -> ExceptT (UnknownRange blk) m (Iterator m blk b)
        startStream immTip hashes' = do
          let immEnd = SwitchToVolDBFrom (StreamToInclusive immTip) hashes'
          immIt <- ExceptT $
            ImmDB.stream itImmDB registry ((,) <$> getPoint <*> blockComponent)
              from (StreamToInclusive immTip)
          lift $ createIterator $ InImmDB from immIt immEnd

        toPoint :: WithOrigin (SlotNo, HeaderHash blk, IsEBB, BlockNo) -> Point blk
        toPoint Origin                            = GenesisPoint
        toPoint (At (slot, hash, _isEBB, _block)) = BlockPoint slot hash

    makeIterator :: Bool  -- ^ Register the iterator in 'cdbIterators'?
                 -> IteratorState m blk b
                 -> m (Iterator m blk b)
    makeIterator register itState = do
      iteratorKey <- makeNewIteratorKey
      varItState  <- newTVarM itState
      when register $ atomically $ modifyTVar itIterators $
        -- Note that we don't use 'itEnv' here, because that would mean that
        -- invoking the function only works when the database is open, which
        -- probably won't be the case.
        Map.insert iteratorKey (implIteratorClose varItState iteratorKey itEnv)
      return Iterator {
          iteratorNext  = getItEnv $
            implIteratorNext  registry varItState blockComponent
        , iteratorClose = getItEnv $
            implIteratorClose          varItState iteratorKey
        }

    emptyIterator :: m (Iterator m blk b)
    emptyIterator = makeIterator False Closed

    -- | This is 'makeIterator' +  it in 'cdbIterators'.
    createIterator :: IteratorState m blk b -> m (Iterator m blk b)
    createIterator = makeIterator True

    makeNewIteratorKey :: m IteratorKey
    makeNewIteratorKey = atomically $ do
      newIteratorKey <- readTVar itNextIteratorKey
      modifyTVar itNextIteratorKey succ
      return newIteratorKey

-- | Close the iterator and remove it from the map of iterators ('itIterators'
-- and thus 'cdbIterators').
implIteratorClose
  :: IOLike m
  => StrictTVar m (IteratorState m blk b)
  -> IteratorKey
  -> IteratorEnv m blk
  -> m ()
implIteratorClose varItState itrKey IteratorEnv{..} = do
    mbImmIt <- atomically $ do
      modifyTVar itIterators (Map.delete itrKey)
      mbImmIt <- iteratorStateImmIt <$> readTVar varItState
      writeTVar varItState Closed
      return mbImmIt
    mapM_ (ImmDB.iteratorClose itImmDB) mbImmIt

-- | Possible states of an iterator.
--
-- When streaming solely from the ImmutableDB ('InImmDB' where 'InImmDBEnd' is
-- /not/ 'SwitchToVolDBFrom'): we will remain in this state until we are done,
-- and end up in 'Closed'.
--
-- When streaming solely from the VolatileDB ('InVolDB'): when
-- 'VolDB.getBlock' returns 'Nothing', i.e. the block is missing from the
-- VolatileDB and might have moved to the ImmutableDB: we switch to the
-- 'InImmDBRetry' state, unless we just come from that state, in that case,
-- return 'IteratorBlockGCed' and close the iterator.
--
-- When streaming from the ImmutableDB with a planned switchover to the
-- VolatileDB ('InImmDB' where 'InImmDBEnd' is 'SwitchToVolDBFrom') and we
-- have reached the end of the ImmutableDB iterator (exhausted or upper bound
-- is reached): we switch to the 'InVolDB' state.
--
-- In the 'InImmDBRetry' state, we distinguish two cases:
--
-- 1. We have just switched to it because a block was missing from the
--    VolatileDB. We have an iterator that could stream this block from the
--    ImmutableDB (if it was indeed moved to the ImmutableDB). If the streamed
--    block matches the expected hash, we continue. If not, or if the iterator
--    is immediately exhausted, then the block is missing and we return
--    'IteratorBlockGCed' and close the iterator.
--
-- 2. We have successfully streamed one or more blocks from the ImmutableDB
--    that were previously part of the VolatileDB. When we now encounter a
--    block of which the hash does not match the expected hash or when the
--    iterator is exhausted, we switch back to the 'InVolDB' state.
--
data IteratorState m blk b
  = InImmDB
      !(StreamFrom blk)
      !(ImmDB.Iterator (HeaderHash blk) m (Point blk, b))
      !(InImmDBEnd blk)
    -- ^ Streaming from the ImmutableDB.
    --
    -- Invariant: an ImmutableDB iterator opened using the 'StreamFrom'
    -- parameter as lower bound will yield the same next block as the iterator
    -- stored as parameter. There is one difference, which is exactly the
    -- reason for keeping track of this 'StreamFrom': if the latter iterator
    -- (the parameter) is exhausted and blocks have been appended to the end
    -- of the ImmutableDB since it was originally opened, the new iterator can
    -- include them in its stream.
    --
    -- Invariant: the iterator is not exhausted.
  | InVolDB
      !(StreamFrom blk)
      !(NonEmpty (HeaderHash blk))
    -- ^ Streaming from the VolatileDB.
    --
    -- The (non-empty) list of hashes is the path to follow through the
    -- VolatileDB.
    --
    -- Invariant: if the blocks corresponding to the hashes have been moved to
    -- the ImmutableDB, it should be possible to stream these blocks from the
    -- ImmutableDB by starting an iterator using the 'StreamFrom' parameter.
    -- Note that the hashes of these blocks still have to be checked against
    -- the hashes in the path, because the blocks might not have been part of
    -- the current chain, in which case they will not be in the ImmutableDB.
  | InImmDBRetry
      !(StreamFrom blk)
      !(ImmDB.Iterator (HeaderHash blk) m (Point blk, b))
      !(NonEmpty (HeaderHash blk))
    -- ^ When streaming blocks (a list of hashes) from the VolatileDB, we
    -- noticed a block was missing from the VolatileDB. It may have moved to
    -- the ImmutableDB since we initialised the iterator (and built the path),
    -- so we'll try if we can stream it from the ImmutableDB.
    --
    -- Invariants: invariants of 'InImmDB' + invariant of 'InVolDB'.

  | Closed
  deriving (Generic)

instance (Typeable blk, StandardHash blk)
      => NoUnexpectedThunks (IteratorState m blk b)
  -- use generic instance

-- | Extract the ImmutableDB Iterator from the 'IteratorState'.
iteratorStateImmIt
  :: IteratorState m blk b
  -> Maybe (ImmDB.Iterator (HeaderHash blk) m (Point blk, b))
iteratorStateImmIt = \case
    Closed                 -> Nothing
    InImmDB      _ immIt _ -> Just immIt
    InImmDBRetry _ immIt _ -> Just immIt
    InVolDB {}             -> Nothing

-- | Determines if/when to stop streaming from the ImmutableDB and what to do
-- afterwards.
data InImmDBEnd blk
  = StreamAll
    -- ^ Don't stop streaming until the iterator is exhausted.
  | StreamTo          !(StreamTo blk)
    -- ^ Stream to the upper bound.
  | SwitchToVolDBFrom !(StreamTo blk)  !(NonEmpty (HeaderHash blk))
    -- ^ Stream to the upper bound. Afterwards, start streaming the path (the
    -- second parameter) from the VolatileDB.
  deriving (Generic, NoUnexpectedThunks)

implIteratorNext :: forall m blk b. (IOLike m, HasHeader blk)
                 => ResourceRegistry m
                 -> StrictTVar m (IteratorState m blk b)
                 -> BlockComponent (ChainDB m blk) b
                 -> IteratorEnv m blk
                 -> m (IteratorResult blk b)
implIteratorNext registry varItState blockComponent IteratorEnv{..} =
    atomically (readTVar varItState) >>= \case
      Closed ->
        return IteratorExhausted
      InImmDB continueAfter immIt immEnd ->
        nextInImmDB continueAfter immIt immEnd
      InImmDBRetry continueAfter immIt immHashes ->
        nextInImmDBRetry (Just continueAfter) immIt immHashes
      InVolDB continueAfter volHashes ->
        nextInVolDB continueAfter volHashes
  where
    trace = traceWith itTracer

    -- | Read the next block while in the 'InVolDB' state.
    nextInVolDB :: StreamFrom blk
                   -- ^ In case the block corresponding to the first hash in
                   -- the path is missing from the VolatileDB, we can use this
                   -- lower bound to try to stream it from the ImmutableDB (if
                   -- the block indeed has been moved there).
                -> NonEmpty (HeaderHash blk)
                -> m (IteratorResult blk b)
    nextInVolDB continueFrom (hash NE.:| hashes) =
      VolDB.getBlockComponent itVolDB ((,) <$> getPoint <*> blockComponent) hash >>= \case
        -- Block is missing
        Nothing -> do
            trace $ BlockMissingFromVolDB hash
            -- Try if we can stream a block from the ImmutableDB that was
            -- previously in the VolatileDB. This will only work if the block
            -- was part of the current chain, otherwise it will not have been
            -- copied to the ImmutableDB.
            --
            -- This call cannot throw a 'ReadFutureSlotError' or a
            -- 'ReadFutureEBBError' because if the block is missing, it /must/
            -- have been garbage-collected, which means that its slot was
            -- older than the slot of the tip of the ImmutableDB.
            immTip <- ImmDB.getPointAtTip itImmDB
            case pointToWithOriginRealPoint immTip of
              Origin ->
                -- The block was in the volatile DB, but isn't anymore. This can
                -- only happen due to GC. It's not guaranteed that GC will have
                -- moved /that/ block to the imm DB (so it might have just
                -- disappeared altogether), /but/ after GC the imm DB cannot be
                -- empty (because GC will only be triggered after some newly
                -- immutable blocks have been copied to the imm DB).
                error "nextInVolDB: impossible"
              At tip ->
                ImmDB.stream itImmDB registry
                  ((,) <$> getPoint <*> blockComponent)
                  continueFrom
                  (StreamToInclusive tip) >>= \case
                    -- The block was not found in the ImmutableDB, it must have
                    -- been garbage-collected
                    Left  _ -> do
                      trace $ BlockGCedFromVolDB hash
                      return $ IteratorBlockGCed hash
                    Right immIt ->
                      nextInImmDBRetry Nothing immIt (hash NE.:| hashes)

        -- Block is there
        Just (pt, b) | Just hashes' <- NE.nonEmpty hashes -> do
          let continueFrom' = StreamFromExclusive pt
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes')
          return $ IteratorResult b
        -- No more hashes, so we can stop
        Just (_pt, b) -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult b

    -- | Read the next block while in the 'InImmDB' state.
    nextInImmDB :: StreamFrom blk
                -> ImmDB.Iterator (HeaderHash blk) m (Point blk, b)
                -> InImmDBEnd blk
                -> m (IteratorResult blk b)
    nextInImmDB continueFrom immIt immEnd =
      selectResult immIt >>= \case
        NotDone (pt, b) -> do
          let continueFrom' = StreamFromExclusive pt
          atomically $ writeTVar varItState (InImmDB continueFrom' immIt immEnd)
          return $ IteratorResult b
        -- True indicates that this is the last element in the stream
        DoneAfter (pt, b) | SwitchToVolDBFrom _ hashes <- immEnd -> do
          let continueFrom' = StreamFromExclusive pt
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes)
          return $ IteratorResult b
        DoneAfter (_pt, b) -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult b
        Done | SwitchToVolDBFrom _ hashes <- immEnd ->
          nextInVolDB continueFrom hashes
        Done -> do
          -- No need to switch to the VolatileDB, so we can stop
          atomically $ writeTVar varItState Closed
          return IteratorExhausted

    -- | Read the next block while in the 'InImmDBRetry' state.
    --
    -- We try to stream blocks that we suspect are now in the ImmutableDB
    -- because they are no longer in the VolatileDB. We don't know this for
    -- sure, so we must check whether they match the expected hashes.
    nextInImmDBRetry
      :: Maybe (StreamFrom blk)
         -- ^ 'Nothing' iff the iterator was just opened and nothing has been
         -- streamed from it yet. This is used to avoid switching right back
         -- to the VolatileDB if we came from there.
       -> ImmDB.Iterator (HeaderHash blk) m (Point blk, b)
       -> NonEmpty (HeaderHash blk)
       -> m (IteratorResult blk b)
    nextInImmDBRetry mbContinueFrom immIt (hash NE.:| hashes) =
      selectResult immIt >>= \case
        NotDone (pt, b) | pointHash pt == BlockHash hash -> do
          trace $ BlockWasCopiedToImmDB hash
          let continueFrom' = StreamFromExclusive pt
          case NE.nonEmpty hashes of
            Nothing      -> do
              atomically $ writeTVar varItState Closed
              ImmDB.iteratorClose itImmDB immIt
            Just hashes' ->
              atomically $ writeTVar varItState $
                InImmDBRetry continueFrom' immIt hashes'
          return $ IteratorResult b

        DoneAfter (pt, b) | pointHash pt == BlockHash hash -> do
          -- 'DoneAfter': 'selectResult' will have closed the ImmDB iterator
          -- already
          trace $ BlockWasCopiedToImmDB hash
          let continueFrom' = StreamFromExclusive pt
          case NE.nonEmpty hashes of
            Nothing      -> atomically $ writeTVar varItState Closed
            Just hashes' -> do
              atomically $ writeTVar varItState $ InVolDB continueFrom' hashes'
              trace SwitchBackToVolDB
          return $ IteratorResult b

        -- Hash mismatch or 'Done'. Close the ImmDB Iterator (idempotent).
        _ -> ImmDB.iteratorClose itImmDB immIt *> case mbContinueFrom of
          -- We just switched to this state and the iterator was just opened.
          -- The block must be GC'ed, since we opened the iterator because it
          -- was missing from the VolatileDB and now it is not in the
          -- ImmutableDB either.
          Nothing -> do
            atomically $ writeTVar varItState Closed
            trace $ BlockGCedFromVolDB hash
            return $ IteratorBlockGCed hash

          -- We have already streamed something from the iterator. We can try
          -- looking in the VolatileDB again. If we hadn't streamed something
          -- yet, switching to the VolatileDB would be pointless as we just
          -- came from there.
          Just continueFrom -> do
            trace SwitchBackToVolDB
            nextInVolDB continueFrom (hash NE.:| hashes)

    -- | Given an ImmutableDB iterator, try to stream a value from it and
    -- convert it to a 'Done'. See the documentation of 'Done' for more
    -- details.
    --
    -- Note that this function closes the iterator when necessary, i.e., when
    -- the return value is 'Done' or 'DoneAfter'.
    selectResult :: ImmDB.Iterator (HeaderHash blk) m (Point blk, b)
                 -> m (Done (Point blk, b))
    selectResult immIt = do
        itRes   <-            ImmDB.iteratorNext    itImmDB immIt
        hasNext <- isJust <$> ImmDB.iteratorHasNext itImmDB immIt
        case itRes of
          ImmDB.IteratorResult blk -> select blk hasNext
          ImmDB.IteratorExhausted  -> return Done
      where
        select blk hasNext
          | hasNext
          = return $ NotDone blk
          | otherwise
          = ImmDB.iteratorClose itImmDB immIt $> DoneAfter blk

-- | Auxiliary data type used for 'selectResult' in 'implIteratorNext'.
data Done blk
  = Done
    -- ^ We're done with the iterator, either it is exhausted or we reached
    -- its upper bound.
  | DoneAfter blk
    -- ^ We're done with the iterator, but have to return this last block. We
    -- must have reached its upper /inclusive/ bound.
  | NotDone     blk
    -- ^ We're not done yet with the iterator and have to return this block.


-- | Close all open 'Iterator's.
--
-- This /can/ be called when the ChainDB is already closed.
closeAllIterators
  :: IOLike m
  => ChainDbEnv m blk
  -> m ()
closeAllIterators CDB{..} = do
    iteratorClosers <- atomically $ Map.elems <$> readTVar cdbIterators
    -- Note that each closer removes its entry from the 'cdbIterators' map.
    sequence_ iteratorClosers
