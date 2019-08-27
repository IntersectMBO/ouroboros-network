{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Readers
module Ouroboros.Storage.ChainDB.Impl.Reader
  ( newHeaderReader
  , newBlockReader
  , switchFork
  , closeAllReaders
  ) where

import           Control.Exception (assert)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow

import           Control.Tracer (contramap, traceWith)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainUpdate (..), HasHeader,
                     HeaderHash, Point, SlotNo, blockPoint, castPoint,
                     genesisPoint, pointSlot)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (GetHeader (..), headerPoint)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..), Reader (..),
                     ReaderId)

import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Storage.ChainDB.Impl.Types


-- | Check if the ChainDB is open. If not, throw a 'ClosedDBError'. Next,
-- check whether the reader with the given 'ReaderId' still exists. If not,
-- throw a 'ClosedReaderError'.
--
-- Otherwise, execute the given function on the 'ChainDbEnv' and 'ReaderState'
-- 'StrictTVar'.
getReader
  :: forall m blk r.
     ( MonadSTM m
     , MonadThrow (STM m)
     , HasHeader blk
     )
  => ChainDbHandle m blk
  -> ReaderId
  -> (ChainDbEnv m blk -> StrictTVar m (ReaderState m blk) -> m r)
  -> m r
getReader (CDBHandle varState) readerId f = do
    (env, varRdr) <- atomically $ readTVar varState >>= \case
      ChainDbClosed _env -> throwM $ ClosedDBError @blk
      -- See the docstring of 'ChainDbReopening'
      ChainDbReopening   -> error "ChainDB used while reopening"
      ChainDbOpen    env ->
        Map.lookup readerId <$> readTVar (cdbReaders env) >>= \case
          Nothing  -> throwM $ ClosedReaderError @blk readerId
          Just varRdr -> return (env, varRdr)
    f env varRdr

-- | Variant 'of 'getReader' for functions taking one argument.
getReader1
  :: forall m blk a r.
     ( MonadSTM m
     , MonadThrow (STM m)
     , HasHeader blk
     )
  => ChainDbHandle m blk
  -> ReaderId
  -> (ChainDbEnv m blk -> StrictTVar m (ReaderState m blk) -> a -> m r)
  -> a -> m r
getReader1 h rdrId f a =
    getReader h rdrId (\env varReader -> f env varReader a)

newReader
  :: forall m blk.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , MonadThrow (STM m)
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbEnv    m blk
  -> ChainDbHandle m blk
     -- ^ We need the handle to pass it on to the 'Reader', so that each time
     -- a function is called on the 'Reader', we check whether the ChainDB is
     -- actually open.
  -> ResourceRegistry m
  -> m (Reader m blk (BlockOrHeader blk))
newReader cdb@CDB{..} h registry = do
    tipPoint <- atomically $ Query.getTipPoint cdb
    readerState <- if tipPoint == genesisPoint
      then return $ ReaderInMem rollState
      else do
        -- TODO avoid opening an iterator immediately when creating a reader,
        -- because the user will probably request an intersection with the
        -- recent chain. OR should we not optimise for the best case?
        immIt <- ImmDB.streamBlocksAfter cdbImmDB registry genesisPoint
        return $ ReaderInImmDB rollState immIt

    (reader, readerId) <- atomically $ do
      readerId <- readTVar cdbNextReaderId
      modifyTVar cdbNextReaderId succ

      varReader <- newTVar readerState
      modifyTVar cdbReaders (Map.insert readerId varReader)
      let reader = makeNewBlockOrHeaderReader h readerId registry
      return (reader, readerId)
    traceWith cdbTracer $ TraceReaderEvent $ NewReader readerId
    return reader
  where
    rollState = RollBackTo genesisPoint

newHeaderReader
  :: forall m blk.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , MonadThrow (STM m)
     , GetHeader blk
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbHandle m blk
  -> ResourceRegistry m
  -> m (Reader m blk (Header blk))
newHeaderReader h registry = getEnv h $ \cdb ->
    turnIntoHeaderReader <$> newReader cdb h registry
  where
    turnIntoHeaderReader :: Reader m blk (BlockOrHeader blk)
                         -> Reader m blk (Header blk)
    turnIntoHeaderReader Reader {..} = Reader
      { readerInstruction         = fmap (fmap toHeader) <$> readerInstruction
      , readerInstructionBlocking = fmap toHeader        <$> readerInstructionBlocking
      , ..
      }

    toHeader :: BlockOrHeader blk -> Header blk
    toHeader (Left  hdr) = hdr
    toHeader (Right blk) = getHeader blk

newBlockReader
  :: forall m blk.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , MonadThrow (STM m)
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbHandle m blk
  -> ResourceRegistry m
  -> m (Reader m blk blk)
newBlockReader h registry = getEnv h $ \cdb ->
    turnIntoBlockReader cdb <$> newReader cdb h registry
  where
    turnIntoBlockReader :: ChainDbEnv m blk
                        -> Reader m blk (BlockOrHeader blk)
                        -> Reader m blk blk
    turnIntoBlockReader CDB{..} Reader {..} = Reader
        { readerInstruction =
            readerInstruction >>= traverse (traverse toBlock)
        , readerInstructionBlocking =
            readerInstructionBlocking >>= traverse toBlock
        , ..
        }
      where
        toBlock :: BlockOrHeader blk -> m blk
        toBlock (Left  hdr) = Query.getAnyKnownBlock cdbImmDB cdbVolDB $ headerPoint hdr
        toBlock (Right blk) = return blk

makeNewBlockOrHeaderReader
  :: forall m blk.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , MonadThrow (STM m)
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbHandle m blk
  -> ReaderId
  -> ResourceRegistry m
  -> Reader m blk (BlockOrHeader blk)
makeNewBlockOrHeaderReader h readerId registry = Reader {..}
  where
    readerInstruction         = getReader  h readerId $ instructionHelper registry id Just
    readerInstructionBlocking = getReader  h readerId $ instructionHelper registry blockUntilJust id
    readerForward             = getReader1 h readerId $ forward           registry
    readerClose               = getEnv     h          $ close readerId

close
  :: forall m blk.
     (MonadSTM m, MonadCatch m, HasHeader blk)
  => ReaderId
  -> ChainDbEnv m blk
  -> m ()
close readerId CDB{..} = do
    mbReaderState <- atomically $ do
      readers <- readTVar cdbReaders
      let (mbReader, readers') = delete readerId readers
      writeTVar cdbReaders readers'
      traverse readTVar mbReader
    -- If the ReaderId is not present in the map, the Reader must have been
    -- closed already.
    mapM_ (closeReaderState cdbImmDB) mbReaderState
  where
    -- | Delete the entry corresponding to the given key from the map. If it
    -- existed, return it, otherwise, return 'Nothing'. The updated map is of
    -- course also returned.
    delete :: forall k a. Ord k => k -> Map k a -> (Maybe a, Map k a)
    delete = Map.updateLookupWithKey (\_ _ -> Nothing)

-- | Close the given 'ReaderState' by closing any 'ImmDB.Iterator' it might
-- contain.
closeReaderState :: (MonadCatch m, HasHeader blk)
                 => ImmDB.ImmDB m blk -> ReaderState m blk -> m ()
closeReaderState immDB = \case
     ReaderInMem _         -> return ()
     -- IMPORTANT: the main reason we're closing readers: to close this open
     -- iterator, which contains a reference to a file handle.
     ReaderInImmDB _ immIt -> ImmDB.iteratorClose immDB immIt


type BlockOrHeader blk = Either (Header blk) blk

-- | Helper for 'readerInstruction' and 'readerInstructionBlocking'.
--
-- The result type @r@ will be instantiated to:
--
-- * @Maybe (ChainUpdate blk (BlockOrHeader blk))@ in case of
--   'readerInstruction'.
-- * @ChainUpdate blk (BlockOrHeader blk)@ in case of
--   'readerInstructionBlocking'.
--
-- The returned 'ChainUpdate' contain a 'BlockOrHeader' because depending on
-- the state, we have to read the whole block anyway (in 'ReaderInImmDB') or
-- only have the header in memory (in 'ReaderInMem'). The header or block
-- reader can then perform the appropriate conversion or lookup itself. This
-- way, we avoid having to read the same block twice or reading the whole
-- block while we only needed its header.
--
-- When in the 'ReaderInImmDB' state, we never have to block, as we can just
-- stream the next block from the ImmutableDB.
--
-- When in the 'ReaderInMem' state, we may have to block when we have reached
-- the end of the current chain.
instructionHelper
  :: forall m blk r.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ResourceRegistry m
  -> (STM m (Maybe (ChainUpdate blk (BlockOrHeader blk))) -> STM m r)
     -- ^ How to turn a transaction that may or may not result in a new
     -- 'ChainUpdate' in one that returns the right return type: use
     -- 'blockUntilJust' to block or 'id' to just return the @Maybe@.
  -> (ChainUpdate blk (BlockOrHeader blk) -> r)
     -- ^ How to turn a (pure) 'ChainUpdate' in the right return type: use
     -- 'id' or 'Just'.
  -> ChainDbEnv m blk
  -> StrictTVar m (ReaderState m blk)
  -> m r
instructionHelper registry fromMaybeSTM fromPure CDB{..} varReader = do
    -- In one transaction: check in which state we are, if in the
    -- @ReaderInMem@ state, just call 'instructionSTM', otherwise,
    -- return the contents of the 'ReaderInImmDB' state.
    inImmDBOrRes <- atomically $ do
      curChain <- readTVar cdbChain
      readTVar varReader >>= \case
        -- Just return the contents of the state and end the transaction
        ReaderInImmDB rollState immIt -> return $ Left (rollState, Just immIt)
        ReaderInMem   rollState
          | AF.withinFragmentBounds
            (castPoint (readerRollStatePoint rollState)) curChain
            -- The point is still in the current chain fragment
          -> fmap Right $ fromMaybeSTM $ fmap (fmap Left) <$>
               instructionSTM
                 rollState
                 curChain
                 (writeTVar varReader . ReaderInMem)
          | otherwise
            -- The point is no longer on the fragment. Blocks must have moved
            -- (off the fragment) to the ImmutableDB. Note that 'switchFork'
            -- will try to keep the point on the fragment in case we switch to
            -- a fork.
          -> return $ Left (rollState, Nothing)
    case inImmDBOrRes of
      -- We were able to obtain the result inside the transaction as we were
      -- in the 'ReaderInMem' state.
      Right res               -> return res
      -- We were in the 'ReaderInImmDB' state or we need to switch to it.
      Left (rollState, mbImmIt) -> case rollState of
        RollForwardFrom pt -> do
          immIt <- case mbImmIt of
            Just immIt -> return immIt
            -- We were in the 'ReaderInMem' state but have to switch to the
            -- 'ReaderInImmDB' state.
            -- COST: the block at @pt@ is read.
            Nothing    -> do
              trace $ ReaderNoLongerInMem rollState
              ImmDB.streamBlocksAfter cdbImmDB registry pt
          rollForwardImmDB immIt pt
        RollBackTo      pt -> do
          case mbImmIt of
            Just immIt -> ImmDB.iteratorClose cdbImmDB immIt
            Nothing    -> trace $ ReaderNoLongerInMem rollState
          -- COST: the block at @pt@ is read.
          immIt' <- ImmDB.streamBlocksAfter cdbImmDB registry pt
          let readerState' = ReaderInImmDB (RollForwardFrom pt) immIt'
          atomically $ writeTVar varReader readerState'
          return $ fromPure $ RollBack pt
  where
    trace = traceWith (contramap TraceReaderEvent cdbTracer)

    nextBlock :: ImmDB.Iterator (HeaderHash blk) m blk -> m (Maybe blk)
    nextBlock immIt = ImmDB.iteratorNext cdbImmDB immIt <&> \case
      ImmDB.IteratorResult _ blk -> Just blk
      ImmDB.IteratorEBB  _ _ blk -> Just blk
      ImmDB.IteratorExhausted    -> Nothing

    rollForwardImmDB :: ImmDB.Iterator (HeaderHash blk) m blk -> Point blk
                     -> m r
    rollForwardImmDB immIt pt = nextBlock immIt >>= \case
      Just blk -> do
        let pt'          = blockPoint blk
            readerState' = ReaderInImmDB (RollForwardFrom pt') immIt
        atomically $ writeTVar varReader readerState'
        return $ fromPure $ AddBlock $ Right blk
      Nothing  -> do
        -- Even though an iterator is automatically closed internally when
        -- exhausted, we close it again (idempotent), but this time to
        -- unregister the associated clean-up action.
        ImmDB.iteratorClose cdbImmDB immIt
        -- The iterator is exhausted: we've reached the end of the
        -- ImmutableDB, or actually what was the end of the ImmutableDB at the
        -- time of opening the iterator. We must now check whether that is
        -- still the end (blocks might have been added to the ImmutableDB in
        -- the meantime).
        slotNoAtImmDBTip <- ImmDB.getSlotNoAtTip cdbImmDB
        case pointSlot pt `compare` slotNoAtImmDBTip of
          -- The ImmutableDB somehow rolled back
          GT -> error "reader streamed beyond tip of the ImmutableDB"

          -- The tip is still the same, so switch to the in-memory chain
          EQ -> do
            trace $ ReaderSwitchToMem pt slotNoAtImmDBTip
            atomically $ fromMaybeSTM $ do
              curChain <- readTVar cdbChain
              fmap (fmap Left) <$> instructionSTM
                (RollForwardFrom pt)
                curChain
                (writeTVar varReader . ReaderInMem)

          -- The tip of the ImmutableDB has progressed since we opened the
          -- iterator
          LT -> do
            trace $ ReaderNewImmIterator pt slotNoAtImmDBTip
            -- COST: the block at @pt@ is read.
            immIt' <- ImmDB.streamBlocksAfter cdbImmDB registry pt
            -- Try again with the new iterator
            rollForwardImmDB immIt' pt

-- | 'readerInstruction' for when the reader is in the 'ReaderInMem' state.
instructionSTM
  :: forall m blk.
     ( MonadSTM m
     , HasHeader (Header blk)
     )
  => ReaderRollState blk
     -- ^ The current 'ReaderRollState' of the reader
  -> AnchoredFragment (Header blk)
     -- ^ The current chain fragment
  -> (ReaderRollState blk -> STM m ())
     -- ^ How to save the updated 'ReaderRollState'
  -> STM m (Maybe (ChainUpdate blk (Header blk)))
instructionSTM rollState curChain saveRollState =
    assert (invariant curChain) $ case rollState of
      RollForwardFrom pt ->
        case AF.successorBlock (castPoint pt) curChain of
          -- There is no successor block because the reader is at the head
          Nothing  -> return Nothing
          Just hdr -> do
            saveRollState $ RollForwardFrom $ headerPoint hdr
            return $ Just $ AddBlock hdr
      RollBackTo      pt -> do
        saveRollState $ RollForwardFrom pt
        return $ Just $ RollBack pt
  where
    invariant =
      AF.withinFragmentBounds (castPoint (readerRollStatePoint rollState))

forward
  :: forall m blk.
     ( MonadSTM  m
     , MonadMask m
     , MonadFork m
     , HasHeader blk
     , HasHeader (Header blk)
     , HasCallStack
     )
  => ResourceRegistry m
  -> ChainDbEnv m blk
  -> StrictTVar m (ReaderState m blk)
  -> [Point blk]
  -> m (Maybe (Point blk))
forward registry CDB{..} varReader = \pts -> do
    -- The current state of the reader doesn't matter, the given @pts@ could
    -- be in the current chain fragment or in the ImmutableDB.

    -- NOTE: we don't use 'Query.getCurrentChain', which only returns the last
    -- @k@ headers, because we want to see the headers that have not yet been
    -- written to the ImmutableDB too.
    curChain         <- atomically $ readTVar cdbChain
    slotNoAtImmDBTip <- ImmDB.getSlotNoAtTip cdbImmDB
    findFirstPointOnChain curChain slotNoAtImmDBTip pts
  where
    findFirstPointOnChain
      :: HasCallStack
      => AnchoredFragment (Header blk)
      -> WithOrigin SlotNo
      -> [Point blk]
      -> m (Maybe (Point blk))
    findFirstPointOnChain curChain slotNoAtImmDBTip = \case
      []     -> return Nothing
      pt:pts
        | AF.withinFragmentBounds (castPoint pt) curChain
        -> do
          -- It's in the in-memory chain fragment.
          updateState $ ReaderInMem $ RollBackTo pt
          return $ Just pt

        | pointSlot pt > slotNoAtImmDBTip
          -- The point is after the tip of the ImmutableDB, so don't look in
          -- the ImmutableDB and skip the point.
        -> findFirstPointOnChain curChain slotNoAtImmDBTip pts

        | otherwise
        -> do
          inImmDB <- if pt == genesisPoint
            then return True -- Genesis is always "in" the ImmutableDB
            else isJust <$> ImmDB.getBlockWithPoint cdbImmDB pt
          if inImmDB
            then do
              -- TODO combine block checking with opening the iterator to
              -- avoid reading the same block twice
              immIt <- ImmDB.streamBlocksAfter cdbImmDB registry pt
              updateState $ ReaderInImmDB (RollBackTo pt) immIt
              return $ Just pt
            else findFirstPointOnChain curChain slotNoAtImmDBTip pts

    -- | Update the state of the reader to the given state. If the current
    -- state is 'ReaderInImmDB', close the ImmutableDB iterator to avoid
    -- leaking the file handles.
    updateState :: ReaderState m blk -> m ()
    updateState newReaderState = do
      mbImmIt <- atomically $ do
        mbImmIt <- readTVar varReader <&> \case
          ReaderInImmDB _ immIt -> Just immIt
          ReaderInMem   _       -> Nothing
        writeTVar varReader newReaderState
        return mbImmIt
      mapM_ (ImmDB.iteratorClose cdbImmDB) mbImmIt

-- | Update the given 'ReaderState' to account for switching the current
-- chain to the given fork (which might just be an extension of the
-- current chain).
--
-- PRECONDITION: the intersection point must be within the fragment bounds
-- of the new chain
switchFork
  :: forall m blk.
     HasHeader (Header blk)
  => Point blk  -- ^ Intersection point between old and new chain
  -> AnchoredFragment (Header blk)  -- ^ The new chain
  -> ReaderState m blk -> ReaderState m blk
switchFork ipoint newChain readerState =
    assert (AF.withinFragmentBounds (castPoint ipoint) newChain) $
      case readerState of
        -- If the reader is still reading from the ImmutableDB, switching to a
        -- fork won't affect it at all
        ReaderInImmDB {} -> readerState
        ReaderInMem   rollState
          | pointSlot (readerRollStatePoint rollState) > pointSlot ipoint
            -- If the reader point is more recent than the intersection point,
            -- we have to roll back the reader to the intersection point.
          -> ReaderInMem $ RollBackTo ipoint
          | otherwise
            -- We can keep rolling forward. Note that this does not mean the
            -- reader point is still on the current fragment, as headers older
            -- than @k@ might have been moved from the fragment to the
            -- ImmutableDB. This will be noticed when the next instruction is
            -- requested; we'll switch to the 'ReaderInImmDB' state.
          -> readerState

-- | Close all open 'Reader's.
closeAllReaders
  :: (MonadSTM m, MonadCatch m, HasHeader blk)
  => ChainDbEnv m blk
  -> m ()
closeAllReaders CDB{..} = do
    readerStates <- atomically $ do
      readerStates <- readTVar cdbReaders >>= traverse readTVar . Map.elems
      writeTVar cdbReaders Map.empty
      return readerStates
    mapM_ (closeReaderState cdbImmDB) readerStates
