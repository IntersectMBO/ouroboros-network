{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Iterators
module Ouroboros.Storage.ChainDB.Impl.Iterator
  ( streamBlocks
  , closeAllIterators
    -- * Exported for testing purposes
  , IteratorEnv (..)
  , newIterator
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.Except (ExceptT (..), lift, runExceptT,
                     throwError)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Control.Tracer

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint, HasHeader, HeaderHash, Point,
                     atSlot, blockHash, blockPoint, castPoint, pointSlot,
                     withHash)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     Iterator (..), IteratorId (..), IteratorResult (..),
                     StreamFrom (..), StreamTo (..), UnknownRange (..),
                     validBounds)

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.Impl.Types
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.Impl.VolDB as VolDB

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
-- must first read the block at the point's slot to obtain its hash, after
-- which we can then verify whether it matches the hash of the point. This is
-- important for the start and end bounds (both points) of a stream in case
-- they are in the ImmutableDB (i.e., their slots are <= the tip of the
-- ImmutableDB): we must first read the blocks corresponding to the bounds to
-- be sure the range is valid. Note that these reads happen before the first
-- call to 'iteratorNext' (which will trigger a second read of the first
-- block).
--
-- Note that when streaming to an /exclusive/ bound, the block corresponding
-- to that bound ('Point') must exist in the ChainDB.
--
-- = Costs
--
-- Opening an iterator has some costs:
--
-- * When blocks have to be streamed from the ImmutableDB: as discussed in
--   \"Bounds checking\", the blocks corresponding to the bounds have to be
--   read from disk.
--
-- * When blocks have to be streamed both from the ImmutableDB and the
--   VolatileDB, only the block corresponding to the lower bound will have to
--   be read from the ImmutableDB upfront, as described in the previous bullet
--   point. Note that the block corresponding to the upper bound does not have
--   to be read from disk, since it will be in the VolatileDB, which means
--   that we know its hash already from the in-memory index.
--
-- In summary:
--
-- * Only streaming from the VolatileDB: 0 blocks read upfront.
-- * Only streaming from the ImmutableDB: 2 blocks read upfront.
-- * Streaming from both the ImmutableDB and the VolatileDB: 1 block read
--   upfront.
--
-- Additionally, when we notice during streaming that a block is no longer in
-- the VolatileDB, we try to see whether it can be streamed from the
-- ImmutableDB instead. Opening such an iterator (with an exclusive bound) has
-- the cost of reading (but not parsing) one extra block from disk, in
-- addition to the block(s) we are actually interested in. This can happen
-- multiple times. See #548.
streamBlocks
  :: forall m blk.
     ( MonadCatch m
     , MonadSTM   m
     , MonadThrow (STM m)
     , HasHeader blk
     , HasCallStack
     )
  => ChainDbHandle m blk
  -> StreamFrom      blk
  -> StreamTo        blk
  -> m (Either (UnknownRange blk) (Iterator m blk))
streamBlocks h from to = getEnv h $ \cdb ->
    newIterator (fromChainDbEnv cdb) getItEnv from to
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
  { itImmDB          :: ImmDB m blk
  , itVolDB          :: VolDB m blk
  , itGetImmDBTip    :: m (Point blk)
    -- ^ This should preferably be cheap.
  , itIterators      :: TVar m (Map IteratorId (m ()))
  , itNextIteratorId :: TVar m IteratorId
  , itTracer         :: Tracer m (TraceIteratorEvent blk)
  }

-- | Obtain an 'IteratorEnv' from a 'ChainDbEnv'.
fromChainDbEnv :: MonadSTM m
               => ChainDbEnv m blk -> IteratorEnv m blk
fromChainDbEnv CDB{..} = IteratorEnv
  { itImmDB          = cdbImmDB
  , itVolDB          = cdbVolDB
    -- See the invariant of 'cdbChain'.
  , itGetImmDBTip    = castPoint . AF.anchorPoint <$>
                       atomically (readTVar cdbChain)
  , itIterators      = cdbIterators
  , itNextIteratorId = cdbNextIteratorId
  , itTracer         = contramap TraceIteratorEvent cdbTracer
  }

-- | See 'streamBlocks'.
newIterator
  :: forall m blk.
     ( MonadCatch m
     , MonadSTM   m
     , MonadThrow (STM m)
     , HasHeader blk
     , HasCallStack
     )
  => IteratorEnv   m blk
  -> (forall r. (IteratorEnv m blk -> m r) -> m r)
     -- ^ Function with which the operations on the returned iterator should
     -- obtain their 'IteratorEnv'. This function should check whether the
     -- ChainDB is still open or throw an exception otherwise. This makes sure
     -- that when we call 'iteratorNext', we first check whether the ChainDB
     -- is still open.
  -> StreamFrom      blk
  -> StreamTo        blk
  -> m (Either (UnknownRange blk) (Iterator m blk))
newIterator itEnv@IteratorEnv{..} getItEnv from to = do
    unless (validBounds from to) $
      throwM $ InvalidIteratorRange from to
    res <- runExceptT start
    case res of
      Left e -> trace $ UnknownRangeRequested e
      _      -> return ()
    return res
  where
    trace = traceWith itTracer

    endPoint :: Point blk
    endPoint = case to of
      StreamToInclusive pt -> pt
      StreamToExclusive pt -> pt

    -- | Use the tip of the ImmutableDB to determine whether to look directly
    -- in the ImmutableDB (the range is <= the tip) or first try the
    -- VolatileDB (in the other cases).
    start :: HasCallStack => ExceptT (UnknownRange blk) m (Iterator m blk)
    start = lift itGetImmDBTip >>= \tip -> case tip of
      GenesisPoint -> findPathInVolDB
      BlockPoint { atSlot = tipSlot, withHash = tipHash } ->
        case atSlot endPoint `compare` tipSlot of
          -- The end point is < the tip of the ImmutableDB
          LT -> streamFromImmDB
          EQ | withHash endPoint == tipHash
                -- The end point == the tip of the ImmutableDB
             -> streamFromImmDB
             | otherwise
                -- The end point /= the tip of the ImmutableDB
             -> throwError $ ForkTooOld from
          -- The end point is > the tip of the ImmutableDB
          GT -> findPathInVolDB

    -- | PRECONDITION: the upper bound > the tip of the ImmutableDB
    findPathInVolDB :: HasCallStack
                    => ExceptT (UnknownRange blk) m (Iterator m blk)
    findPathInVolDB = do
      path <- lift $ atomically $ VolDB.computePathSTM itVolDB from to
      case path of
        VolDB.NotInVolDB        _hash           -> throwError $ ForkTooOld from
        VolDB.PartiallyInVolDB  predHash hashes -> streamFromBoth predHash hashes
        VolDB.CompletelyInVolDB hashes          -> case NE.nonEmpty hashes of
          Just hashes' -> lift $ streamFromVolDB hashes'
          Nothing      -> lift $ emptyIterator

    streamFromVolDB :: NonEmpty (HeaderHash blk) -> m (Iterator m blk)
    streamFromVolDB hashes = do
      trace $ StreamFromVolDB from to (NE.toList hashes)
      createIterator $ InVolDB from hashes

    streamFromImmDB :: HasCallStack
                    => ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDB = do
      lift $ trace $ StreamFromImmDB from to
      streamFromImmDBHelper True

    streamFromImmDBHelper :: HasCallStack
                          => Bool -- ^ Check the hash of the upper bound
                          -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromImmDBHelper checkUpperBound = do
        -- First check whether the block in the ImmDB at the end bound has the
        -- correct hash.
        when checkUpperBound $ do
          slotNoAtTip <- lift $ ImmDB.getSlotNoAtTip itImmDB
          when (pointSlot endPoint > slotNoAtTip) $
            throwError $ MissingBlock endPoint
          lift (ImmDB.getBlockWithPoint itImmDB endPoint) >>= \case
            Just _  -> return ()
            Nothing -> throwError $ MissingBlock endPoint
        -- 'ImmDB.streamBlocksFrom' will check the hash of the block at the
        -- start bound.
        immIt <- ExceptT $ ImmDB.streamBlocksFrom itImmDB from
        lift $ createIterator $ InImmDB from immIt (StreamTo to)

    -- | If we have to stream from both the ImmutableDB and the VolatileDB, we
    -- only allow the (current) tip of the ImmutableDB to be the switchover
    -- point between the two DBs. If not, this would mean we have to stream a
    -- fork that forks off more than @k@ blocks in the past, in which case the
    -- risk of blocks going missing due to GC increases. So we refuse such a
    -- stream.
    streamFromBoth :: HasCallStack
                   => HeaderHash blk
                   -> [HeaderHash blk]
                   -> ExceptT (UnknownRange blk) m (Iterator m blk)
    streamFromBoth predHash hashes = do
        lift $ trace $ StreamFromBoth from to hashes
        lift itGetImmDBTip >>= \case
          -- The ImmutableDB is empty
          GenesisPoint -> throwError $ ForkTooOld from
          -- The incomplete path fits onto the tip of the ImmutableDB.
          pt@BlockPoint { withHash = tipHash }
            | tipHash == predHash
            -> case NE.nonEmpty hashes of
                 Just hashes' -> stream pt hashes'
                 -- The path is actually empty, but the exclusive upper bound was
                 -- in the VolatileDB. Just stream from the ImmutableDB without
                 -- checking the upper bound (which might not be in the
                 -- ImmutableDB)
                 Nothing      -> streamFromImmDBHelper False
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
              _tipHash:hash:hashes' -> stream pt (hash NE.:| hashes')
              -- The current tip is the end of the path, this means we can
              -- actually stream everything from just the ImmutableDB. No need
              -- to check the hash at the upper bound again.
              [_tipHash]            -> streamFromImmDBHelper False
      where
        stream pt hashes' = do
          let immEnd = SwitchToVolDBFrom (StreamToInclusive pt) hashes'
          immIt <- ExceptT $ ImmDB.streamBlocksFrom itImmDB from
          lift $ createIterator $ InImmDB from immIt immEnd

    makeIterator :: Bool  -- ^ Register the iterator in 'cdbIterators'?
                 -> IteratorState m blk
                 -> m (Iterator m blk)
    makeIterator register itState = do
      iteratorId <- makeNewIteratorId
      varItState <- newTVarM itState
      when register $ atomically $ modifyTVar' itIterators $
        -- Note that we don't use 'itEnv' here, because that would mean that
        -- invoking the function only works when the database is open, which
        -- probably won't be the case.
        Map.insert iteratorId (implIteratorClose varItState iteratorId itEnv)
      return Iterator {
          iteratorNext  = getItEnv $ implIteratorNext  varItState
        , iteratorClose = getItEnv $ implIteratorClose varItState iteratorId
        , iteratorId    = iteratorId
        }

    emptyIterator :: m (Iterator m blk)
    emptyIterator = makeIterator False Closed

    -- | This is 'makeIterator' +  it in 'cdbIterators'.
    createIterator :: IteratorState m blk -> m (Iterator m blk)
    createIterator = makeIterator True

    makeNewIteratorId :: m IteratorId
    makeNewIteratorId = atomically $ do
      newIteratorId <- readTVar itNextIteratorId
      modifyTVar' itNextIteratorId succ
      return newIteratorId

-- | Close the iterator and remove it from the map of iterators ('itIterators'
-- and thus 'cdbIterators').
implIteratorClose
  :: (MonadSTM m, MonadCatch m, HasHeader blk)
  => TVar m (IteratorState m blk)
  -> IteratorId
  -> IteratorEnv m blk
  -> m ()
implIteratorClose varItState itrId IteratorEnv{..} = do
    mbImmIt <- atomically $ do
      modifyTVar' itIterators (Map.delete itrId)
      mbImmIt <- iteratorStateImmIt <$> readTVar varItState
      writeTVar varItState Closed
      return mbImmIt
    mapM_ (ImmDB.iteratorClose cdbImmDB) mbImmIt

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
data IteratorState m blk
  = InImmDB
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (InImmDBEnd blk)
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
      (StreamFrom blk)
      (NonEmpty (HeaderHash blk))
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
      (StreamFrom blk)
      (ImmDB.Iterator (HeaderHash blk) m blk)
      (NonEmpty (HeaderHash blk))
    -- ^ When streaming blocks (a list of hashes) from the VolatileDB, we
    -- noticed a block was missing from the VolatileDB. It may have moved to
    -- the ImmutableDB since we initialised the iterator (and built the path),
    -- so we'll try if we can stream it from the ImmutableDB.
    --
    -- Invariants: invariants of 'InImmDB' + invariant of 'InVolDB'.

  | Closed

-- | Extract the ImmutableDB Iterator from the 'IteratorState'.
iteratorStateImmIt :: IteratorState m blk
                   -> Maybe (ImmDB.Iterator (HeaderHash blk) m blk)
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
  | StreamTo          (StreamTo blk)
    -- ^ Stream to the upper bound.
  | SwitchToVolDBFrom (StreamTo blk)  (NonEmpty (HeaderHash blk))
    -- ^ Stream to the upper bound. Afterwards, start streaming the path (the
    -- second parameter) from the VolatileDB.

implIteratorNext :: forall m blk.
                    ( MonadCatch m
                    , MonadSTM   m
                    , HasHeader blk
                    )
                 => TVar m (IteratorState m blk)
                 -> IteratorEnv m blk
                 -> m (IteratorResult blk)
implIteratorNext varItState IteratorEnv{..} =
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
                -> m (IteratorResult blk)
    nextInVolDB continueFrom (hash NE.:| hashes) =
      VolDB.getBlock itVolDB hash >>= \case
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
            immIt <- ImmDB.streamBlocksFromUnchecked itImmDB continueFrom
            nextInImmDBRetry Nothing immIt (hash NE.:| hashes)

        -- Block is there
        Just blk | Just hashes' <- NE.nonEmpty hashes -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes')
          return $ IteratorResult blk
        -- No more hashes, so we can stop
        Just blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk

    -- | Read the next block while in the 'InImmDB' state.
    nextInImmDB :: StreamFrom blk
                -> ImmDB.Iterator (HeaderHash blk) m blk
                -> InImmDBEnd blk
                -> m (IteratorResult blk)
    nextInImmDB continueFrom immIt immEnd = do
      immRes <- selectResult immEnd <$> ImmDB.iteratorNext    cdbImmDB immIt
                                    <*> ImmDB.iteratorHasNext cdbImmDB immIt
      case immRes of
        NotDone blk -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InImmDB continueFrom' immIt immEnd)
          return $ IteratorResult blk
        -- True indicates that this is the last element in the stream
        DoneAfter blk | SwitchToVolDBFrom _ hashes <- immEnd -> do
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState (InVolDB continueFrom' hashes)
          return $ IteratorResult blk
        DoneAfter blk -> do
          atomically $ writeTVar varItState Closed
          return $ IteratorResult blk
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
    nextInImmDBRetry :: Maybe (StreamFrom blk)
                        -- ^ 'Nothing' iff the iterator was just opened and
                        -- nothing has been streamed from it yet. This is used
                        -- to avoid switching right back to the VolatileDB if
                        -- we came from there.
                     -> ImmDB.Iterator (HeaderHash blk) m blk
                     -> NonEmpty (HeaderHash blk)
                     -> m (IteratorResult blk)
    nextInImmDBRetry mbContinueFrom immIt (hash NE.:| hashes) =
      selectResult StreamAll <$> ImmDB.iteratorNext    cdbImmDB immIt
                             <*> ImmDB.iteratorHasNext cdbImmDB immIt >>= \case
        NotDone blk | blockHash blk == hash -> do
          trace $ BlockWasCopiedToImmDB hash
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          atomically $ writeTVar varItState $ case NE.nonEmpty hashes of
            Nothing      -> Closed
            Just hashes' -> InImmDBRetry continueFrom' immIt hashes'
          return $ IteratorResult blk

        DoneAfter blk | blockHash blk == hash -> do
          trace $ BlockWasCopiedToImmDB hash
          let continueFrom' = StreamFromExclusive (blockPoint blk)
          case NE.nonEmpty hashes of
            Nothing      -> atomically $ writeTVar varItState Closed
            Just hashes' -> do
              atomically $ writeTVar varItState $ InVolDB continueFrom' hashes'
              trace SwitchBackToVolDB
          return $ IteratorResult blk

        -- Hash mismatch or 'Done'
        _ -> case mbContinueFrom of
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

    -- | Return a 'Done' based on the 'InImmDBEnd'. See the documentation of
    -- 'Done'. The 'Bool' argument should be the result of
    -- 'ImmDB.iteratorHasNext' and indicates whether the iterator is able to
    -- stream more blocks ('True') or whether it is exhausted ('False') after
    -- returning the last result.
    --
    -- We're doing this because we're streaming from the ImmutableDB with an
    -- open upper bound, because the ImmutableDB doesn't support streaming to
    -- an exclusive upper bound.
    selectResult :: InImmDBEnd blk
                 -> ImmDB.IteratorResult (HeaderHash blk) blk
                 -> Bool  -- ^ has the iterator a next element
                 -> Done blk
    selectResult immEnd itRes hasNext = case itRes of
        ImmDB.IteratorResult _ blk -> select blk
        ImmDB.IteratorEBB  _ _ blk -> select blk
        ImmDB.IteratorExhausted    -> Done
      where
        select blk = case immEnd of
          StreamAll
            | hasNext             -> NotDone   blk
            | otherwise           -> DoneAfter blk
          StreamTo          to'   -> checkUpperBound blk to'
          SwitchToVolDBFrom to' _ -> checkUpperBound blk to'
        checkUpperBound blk = \case
          StreamToExclusive pt
            | pt == blockPoint blk -> Done
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk
          StreamToInclusive pt
            | pt == blockPoint blk -> DoneAfter blk
            | hasNext              -> NotDone   blk
            | otherwise            -> DoneAfter blk

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
    -- We know the iterator is not exhausted, but this does not mean that the
    -- next block returned by it will be included in the stream as it might
    -- correspond to the exclusive upper bound.


-- | Close all open 'Iterator's.
--
-- This /can/ be called when the ChainDB is already closed.
closeAllIterators
  :: MonadSTM m
  => ChainDbEnv m blk
  -> m ()
closeAllIterators CDB{..} = do
    iteratorClosers <- atomically $ Map.elems <$> readTVar cdbIterators
    -- Note that each closer removes its entry from the 'cdbIterators' map.
    sequence_ iteratorClosers
