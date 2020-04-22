{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Readers
module Ouroboros.Consensus.Storage.ChainDB.Impl.Reader
  ( newReader
  , switchFork
  , closeAllReaders
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Exception (assert)
import           Control.Monad (join)
import           Control.Tracer (contramap, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack, callStack)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainUpdate (..), HasHeader,
                     HeaderHash, Point, SlotNo, blockSlot, castPoint,
                     genesisPoint, pointHash, pointSlot)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     ChainDB, ChainDbError (..), Reader (..), getPoint)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

-- | Check if the ChainDB is open. If not, throw a 'ClosedDBError'. Next,
-- check whether the reader with the given 'ReaderKey' still exists. If not,
-- throw a 'ClosedReaderError'.
--
-- Otherwise, execute the given function on the 'ChainDbEnv'.
getReader
  :: forall m blk r. (IOLike m, HasCallStack)
  => ChainDbHandle m blk
  -> ReaderKey
  -> (ChainDbEnv m blk -> m r)
  -> m r
getReader (CDBHandle varState) readerKey f = do
    env <- atomically $ readTVar varState >>= \case
      ChainDbClosed   -> throwM $ ClosedDBError callStack
      ChainDbOpen env -> do
        readerOpen <- Map.member readerKey <$> readTVar (cdbReaders env)
        if readerOpen
          then return env
          else throwM ClosedReaderError
    f env

-- | Variant 'of 'getReader' for functions taking one argument.
getReader1
  :: forall m blk a r. IOLike m
  => ChainDbHandle m blk
  -> ReaderKey
  -> (ChainDbEnv m blk -> a -> m r)
  -> a -> m r
getReader1 h readerKey f a = getReader h readerKey (\env -> f env a)

{-------------------------------------------------------------------------------
  Reader
-------------------------------------------------------------------------------}

newReader
  :: forall m blk b.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbHandle m blk
  -> (Header blk -> Encoding)
     -- ^ Needed to serialise a deserialised header that we already had in
     -- memory.
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> m (Reader m blk b)
newReader h encodeHeader registry blockComponent = getEnv h $ \CDB{..} -> do
    -- The following operations don't need to be done in a single transaction
    readerKey  <- atomically $ updateTVar cdbNextReaderKey $ \r -> (succ r, r)
    varReader <- newTVarM ReaderInit
    let readerHandle = mkReaderHandle cdbImmDB varReader
    atomically $ modifyTVar cdbReaders $ Map.insert readerKey readerHandle
    let reader = makeNewReader h encodeHeader readerKey varReader registry
          blockComponent
    traceWith cdbTracer $ TraceReaderEvent NewReader
    return reader
  where
    mkReaderHandle
      :: ImmDB m blk
      -> StrictTVar m (ReaderState m blk b)
      -> ReaderHandle m blk
    mkReaderHandle cdbImmDB varReader = ReaderHandle
      { rhClose      = do
          -- This is only called by 'closeAllReaders'. We just release the
          -- resources. We don't check whether the Reader is still open.
          -- We don't have to remove the reader from the 'cdbReaders',
          -- 'closeAllReaders' will empty that map already.
          readerState <- atomically $ readTVar varReader
          closeReaderState cdbImmDB readerState
      , rhSwitchFork = \ipoint newChain -> modifyTVar varReader $
          switchFork ipoint newChain
      }

makeNewReader
  :: forall m blk b.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ChainDbHandle m blk
  -> (Header blk -> Encoding)
  -> ReaderKey
  -> StrictTVar m (ReaderState m blk b)
  -> ResourceRegistry m
  -> BlockComponent (ChainDB m blk) b
  -> Reader m blk b
makeNewReader h encodeHeader readerKey varReader registry blockComponent = Reader {..}
  where
    readerInstruction :: m (Maybe (ChainUpdate blk b))
    readerInstruction = getReader h readerKey $
      instructionHelper registry varReader blockComponent encodeHeader id

    readerInstructionBlocking :: m (ChainUpdate blk b)
    readerInstructionBlocking = fmap runIdentity $
      getReader h readerKey $
      instructionHelper registry varReader blockComponent encodeHeader
        (fmap Identity . blockUntilJust)

    readerForward :: [Point blk] -> m (Maybe (Point blk))
    readerForward = getReader1 h readerKey $
      forward registry varReader blockComponent

    readerClose :: m ()
    readerClose = getEnv h $ close readerKey varReader

-- | Implementation of 'readerClose'.
--
-- To be called using 'getEnv' to make sure the ChainDB is still open.
--
-- Idempotent: the reader doesn't have to be open.
--
-- Unlike 'closeAllReaders', this is meant to be called by the user of the
-- ChainDB.Reader.
close
  :: forall m blk b. IOLike m
  => ReaderKey
  -> StrictTVar m (ReaderState m blk b)
  -> ChainDbEnv m blk
  -> m ()
close readerKey varReader CDB { cdbReaders, cdbImmDB } = do
    -- If the ReaderKey is not present in the map, the Reader must have been
    -- closed already.
    atomically $ modifyTVar cdbReaders $ Map.delete readerKey
    readerState <- atomically $ readTVar varReader
    closeReaderState cdbImmDB readerState

-- | Close the given 'ReaderState' by closing any 'ImmDB.Iterator' it might
-- contain.
closeReaderState
  :: MonadCatch m
  => ImmDB.ImmDB m blk -> ReaderState m blk b -> m ()
closeReaderState immDB = \case
     ReaderInit            -> return ()
     ReaderInMem _         -> return ()
     -- IMPORTANT: the main reason we're closing readers: to close this open
     -- iterator, which contains a reference to a file handle.
     ReaderInImmDB _ immIt -> ImmDB.iteratorClose immDB immIt

-- | Helper for 'readerInstruction' and 'readerInstructionBlocking'.
--
-- The type @f@ will be instantiated to:
--
-- * 'Maybe' in case of 'readerInstruction'.
-- * 'Identity' in case of 'readerInstructionBlocking'.
--
-- The returned 'ChainUpdate' contains a 'b', as defined by 'BlockComponent'.
--
-- When in the 'ReaderInImmDB' state, we never have to block, as we can just
-- stream the next block/header from the ImmutableDB.
--
-- When in the 'ReaderInMem' state, we may have to block when we have reached
-- the end of the current chain.
instructionHelper
  :: forall m blk b f.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     , Traversable f, Applicative f
     )
  => ResourceRegistry m
  -> StrictTVar m (ReaderState m blk b)
  -> BlockComponent (ChainDB m blk) b
  -> (Header blk -> Encoding)
  -> (    STM m (Maybe (ChainUpdate blk (Header blk)))
       -> STM m (f     (ChainUpdate blk (Header blk))))
     -- ^ How to turn a transaction that may or may not result in a new
     -- 'ChainUpdate' in one that returns the right return type: use @fmap
     -- Identity . 'blockUntilJust'@ to block or 'id' to just return the
     -- @Maybe@.
  -> ChainDbEnv m blk
  -> m (f (ChainUpdate blk b))
instructionHelper registry varReader blockComponent encodeHeader fromMaybeSTM CDB{..} = do
    -- In one transaction: check in which state we are, if in the
    -- @ReaderInMem@ state, just call 'instructionSTM', otherwise,
    -- return the contents of the 'ReaderInImmDB' state.
    inImmDBOrRes <- atomically $ do
      curChain <- readTVar cdbChain
      readTVar varReader >>= \case
        -- Just return the contents of the state and end the transaction in
        -- these two cases.
        ReaderInit
          -> return $ Left (RollBackTo genesisPoint, Nothing)
        ReaderInImmDB rollState immIt
          -> return $ Left (rollState, Just immIt)

        ReaderInMem   rollState
          | AF.withinFragmentBounds
            (castPoint (readerRollStatePoint rollState)) curChain
            -- The point is still in the current chain fragment
          -> fmap Right $ fromMaybeSTM $
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
      -- in the 'ReaderInMem' state. We only got a header, which we must first
      -- convert to the right block component.
      Right fupdate -> headerUpdateToBlockComponentUpdate fupdate
      -- We were in the 'ReaderInImmDB' state or we need to switch to it.
      Left (rollState, mbImmIt) -> do
        immIt <- case mbImmIt of
          Just immIt -> return immIt
          -- We were in the 'ReaderInMem' state but have to switch to the
          -- 'ReaderInImmDB' state.
          Nothing    -> do
            trace $ ReaderNoLongerInMem rollState
            ImmDB.streamAfterKnownBlock cdbImmDB registry
              ((,) <$> getPoint <*> blockComponent)
              (readerRollStatePoint rollState)
        case rollState of
          RollForwardFrom pt -> rollForwardImmDB immIt pt
          RollBackTo      pt -> do
            let readerState' = ReaderInImmDB (RollForwardFrom pt) immIt
            atomically $ writeTVar varReader readerState'
            return $ pure $ RollBack pt
  where
    trace = traceWith (contramap TraceReaderEvent cdbTracer)

    headerUpdateToBlockComponentUpdate
      :: f (ChainUpdate blk (Header blk)) -> m (f (ChainUpdate blk b))
    headerUpdateToBlockComponentUpdate =
      traverse (traverse (`getBlockComponentFromHeader` blockComponent))

    -- | We only got the header for the in-memory chain fragment, so depending
    -- on the 'BlockComponent' that's requested, we might have to read the
    -- whole block.
    getBlockComponentFromHeader
      :: forall b'. Header blk -> BlockComponent (ChainDB m blk) b' -> m b'
    getBlockComponentFromHeader hdr = \case
        GetBlock      -> getBlockComponent GetBlock
        GetRawBlock   -> getBlockComponent GetRawBlock
        GetHeader     -> return $ return hdr
        GetRawHeader  -> return $ toLazyByteString $ encodeHeader hdr
        GetHash       -> return $ headerHash hdr
        GetSlot       -> return $ blockSlot hdr
        GetIsEBB      -> return $ cdbIsEBB hdr
        GetBlockSize  -> getBlockComponent GetBlockSize
        -- We could look up the header size in the index of the VolatileDB,
        -- but getting the serialisation is cheap because we keep the
        -- serialisation in memory as an annotation, and the following way is
        -- less stateful
        GetHeaderSize -> return $
          fromIntegral $ Lazy.length $ toLazyByteString $ encodeHeader hdr
        GetPure a     -> return a
        GetApply f bc ->
          getBlockComponentFromHeader hdr f <*>
          getBlockComponentFromHeader hdr bc
      where
        -- | Use the 'ImmDB' and 'VolDB' to read the 'BlockComponent' from
        -- disk (or memory).
        getBlockComponent :: forall c. BlockComponent (ChainDB m blk) c -> m c
        getBlockComponent bc =
          Query.getAnyKnownBlockComponent cdbImmDB cdbVolDB bc (headerRealPoint hdr)

    next
      :: ImmDB.Iterator (HeaderHash blk) m (Point blk, b)
      -> m (Maybe (Point blk, b))
    next immIt = ImmDB.iteratorNext cdbImmDB immIt <&> \case
      ImmDB.IteratorResult  b -> Just b
      ImmDB.IteratorExhausted -> Nothing

    rollForwardImmDB
      :: ImmDB.Iterator (HeaderHash blk) m (Point blk, b)
      -> Point blk
      -> m (f (ChainUpdate blk b))
    rollForwardImmDB immIt pt = next immIt >>= \case
      Just (pt', b) -> do
        let readerState' = ReaderInImmDB (RollForwardFrom pt') immIt
        atomically $ writeTVar varReader readerState'
        return $ pure $ AddBlock b
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
        pointAtImmDBTip <- ImmDB.getPointAtTip cdbImmDB
        let slotNoAtImmDBTip = pointSlot pointAtImmDBTip
        case pointSlot pt `compare` slotNoAtImmDBTip of
          -- The ImmutableDB somehow rolled back
          GT -> error "reader streamed beyond tip of the ImmutableDB"

          -- The tip is still the same, so switch to the in-memory chain
          EQ | pt == pointAtImmDBTip
             -> do
            trace $ ReaderSwitchToMem pt slotNoAtImmDBTip
            fupdate <- atomically $ fromMaybeSTM $ do
              curChain <- readTVar cdbChain
              instructionSTM
                (RollForwardFrom pt)
                curChain
                (writeTVar varReader . ReaderInMem)
            -- We only got the header, we must first convert it to the right
            -- block component.
            headerUpdateToBlockComponentUpdate fupdate

          -- Two possibilities:
          --
          -- 1. (EQ): the tip changed, but the slot number is the same. This
          --    is only possible when an EBB was at the tip and the regular
          --    block in the same slot was appended to the ImmutableDB.
          --
          -- 2. (LT): the tip of the ImmutableDB has progressed since we
          --    opened the iterator.
          _  -> do
            trace $ ReaderNewImmIterator pt slotNoAtImmDBTip
            immIt' <- ImmDB.streamAfterKnownBlock cdbImmDB registry
              ((,) <$> getPoint <*> blockComponent) pt
            -- Try again with the new iterator
            rollForwardImmDB immIt' pt

-- | 'readerInstruction' for when the reader is in the 'ReaderInMem' state.
instructionSTM
  :: forall stm blk. (MonadSTMTx stm, HasHeader (Header blk))
  => ReaderRollState blk
     -- ^ The current 'ReaderRollState' of the reader
  -> AnchoredFragment (Header blk)
     -- ^ The current chain fragment
  -> (ReaderRollState blk -> stm ())
     -- ^ How to save the updated 'ReaderRollState'
  -> stm (Maybe (ChainUpdate blk (Header blk)))
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
  :: forall m blk b.
     ( IOLike m
     , HasCallStack
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ResourceRegistry m
  -> StrictTVar m (ReaderState m blk b)
  -> BlockComponent (ChainDB m blk) b
  -> ChainDbEnv m blk
  -> [Point blk]
  -> m (Maybe (Point blk))
forward registry varReader blockComponent CDB{..} = \pts -> do
    -- NOTE: we don't use 'Query.getCurrentChain', which only returns the last
    -- @k@ headers, because we want to see the headers that have not yet been
    -- written to the ImmutableDB too.
    (curChain, readerState) <- atomically $
      (,) <$> readTVar cdbChain <*> readTVar varReader
    slotNoAtImmDBTip <- ImmDB.getSlotNoAtTip cdbImmDB
    findFirstPointOnChain curChain readerState slotNoAtImmDBTip pts
  where
    findFirstPointOnChain
      :: HasCallStack
      => AnchoredFragment (Header blk)
      -> ReaderState m blk b
      -> WithOrigin SlotNo
      -> [Point blk]
      -> m (Maybe (Point blk))
    findFirstPointOnChain curChain readerState slotNoAtImmDBTip = \case
      []     -> return Nothing
      pt:pts
        | AF.withinFragmentBounds (castPoint pt) curChain
        -> do
          -- It's in the in-memory chain fragment.
          updateState $ ReaderInMem $ RollBackTo pt
          return $ Just pt

        | otherwise
        -- Not in the in-memory chain fragment, so older than @k@, hence it
        -- should be in the ImmutableDB. If not, then the point is not on our
        -- chain.
        --
        -- We try to avoid IO (in the ImmutableDB) as much as possible by
        -- checking whether the requested point corresponds to the current
        -- state of the reader.
        -> case readerState of
            ReaderInit
              | pt == genesisPoint
              -- The 'ReaderInit' state is equivalent to @'RollBackTo'
              -- 'genesisPoint'@, so the state doesn't have to change when
              -- requesting a rollback to genesis.
              -> return $ Just pt

            ReaderInImmDB rollState immIt
              | rollState == RollBackTo pt
              -- If we already have to roll back to the given point in the
              -- ImmutableDB, the state doesn't have to change, saving us from
              -- checking whether the point is in the ImmutableDB (cached disk
              -- reads), closing, and opening the same ImmutableDB iterator.
              -> return $ Just pt

              | rollState == RollForwardFrom pt
              -- If we're already rolling forward from the given point in the
              -- ImmutableDB, we can reuse the open ImmutableDB iterator,
              -- saving the same costs as in the comment above. We do have to
              -- update the state from 'RollForwardFrom' to 'RollBackTo'.
              -> do
                atomically $ writeTVar varReader $
                  ReaderInImmDB (RollBackTo pt) immIt
                return $ Just pt

            _otherwise -> case pointToWithOriginRealPoint pt of
              -- Genesis is always "in" the ImmutableDB
              Origin -> do
                updateState ReaderInit
                return $ Just pt

              At pt' -> do
                inImmDB <- ImmDB.hasBlock cdbImmDB pt'
                if inImmDB then do
                  immIt <- ImmDB.streamAfterKnownBlock cdbImmDB registry
                    ((,) <$> getPoint <*> blockComponent) pt
                  updateState $ ReaderInImmDB (RollBackTo pt) immIt
                  return $ Just pt
                else
                  -- The point is not in the current chain, try the next point
                  findFirstPointOnChain curChain readerState slotNoAtImmDBTip pts

    -- | Update the state of the reader to the given state. If the current
    -- state is 'ReaderInImmDB', close the ImmutableDB iterator to avoid
    -- leaking the file handles.
    updateState :: ReaderState m blk b -> m ()
    updateState newReaderState = join $ atomically $
      updateTVar varReader $ \readerState ->
        (newReaderState, ) $ case readerState of
          -- Return a continuation (that we'll 'join') that closes the
          -- previous iterator.
          ReaderInImmDB _ immIt -> ImmDB.iteratorClose cdbImmDB immIt
          ReaderInit            -> return ()
          ReaderInMem   _       -> return ()

-- | Update the given 'ReaderState' to account for switching the current
-- chain to the given fork (which might just be an extension of the
-- current chain).
--
-- PRECONDITION: the intersection point must be within the fragment bounds
-- of the new chain
switchFork
  :: forall m blk b. (HasHeader blk, HasHeader (Header blk))
  => Point blk  -- ^ Intersection point between old and new chain
  -> AnchoredFragment (Header blk)  -- ^ The new chain
  -> ReaderState m blk b -> ReaderState m blk b
switchFork ipoint newChain readerState =
    assert (AF.withinFragmentBounds (castPoint ipoint) newChain) $
      case readerState of
        -- If the reader is still in the initial state, switching to a fork
        -- won't affect it.
        ReaderInit       -> readerState
        -- If the reader is still reading from the ImmutableDB, switching to a
        -- fork won't affect it.
        ReaderInImmDB {} -> readerState
        ReaderInMem   rollState ->
            case pointSlot readerPoint `compare` pointSlot ipoint of
              -- If the reader point is more recent than the intersection point,
              -- we have to roll back the reader to the intersection point.
              GT -> ReaderInMem $ RollBackTo ipoint

              -- The reader point and the intersection point are in the same
              -- slot. We have to be careful here, because one (or both) of them
              -- could be an EBB.
              EQ
                | pointHash readerPoint == pointHash ipoint
                  -- The same point, so no rollback needed.
                -> readerState
                | Just pointAfterRollStatePoint <- headerPoint <$>
                    AF.successorBlock (castPoint readerPoint) newChain
                , pointAfterRollStatePoint == ipoint
                  -- The point after the reader point is the intersection
                  -- point. It must be that the reader point is an EBB and
                  -- that the intersection point is a regular block in the
                  -- same slot. As the reader point is older than the
                  -- intersection point, no rollback is needed.
                -> readerState
                | otherwise
                  -- Either the intersection point is the EBB before the
                  -- reader point (referring to the regular block in the same
                  -- slot), in which case we need to roll back, as the
                  -- intersection point is older than the reader point. Or,
                  -- we're dealing with two blocks (could be two EBBs) in the
                  -- same slot with a different hash, in which case we'll have
                  -- to rollback to the intersection point.
                -> ReaderInMem $ RollBackTo ipoint

              -- The reader point is older than the intersection point, so we
              -- can keep rolling forward. Note that this does not mean the
              -- reader point is still on the current fragment, as headers older
              -- than @k@ might have been moved from the fragment to the
              -- ImmutableDB. This will be noticed when the next instruction is
              -- requested; we'll switch to the 'ReaderInImmDB' state.
              LT -> readerState
          where
            readerPoint = readerRollStatePoint rollState

-- | Close all open block and header 'Reader's.
closeAllReaders :: IOLike m => ChainDbEnv m blk -> m ()
closeAllReaders CDB{..} = do
    readerHandles <- atomically $ do
      readerHandles  <- Map.elems <$> readTVar cdbReaders
      writeTVar cdbReaders  Map.empty
      return readerHandles
    mapM_ rhClose readerHandles
