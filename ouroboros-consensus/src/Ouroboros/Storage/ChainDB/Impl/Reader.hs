{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Readers
module Ouroboros.Storage.ChainDB.Impl.Reader
  ( newReader
  , switchFork
  , closeAllReaders
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Exception (assert)
import           Control.Monad (sequence_)
import           Data.Functor ((<&>))
import           Data.Functor.Identity (Identity (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer (contramap, traceWith)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainUpdate (..), HasHeader,
                     HeaderHash, Point, Serialised (..), SlotNo, blockSlot,
                     castPoint, genesisPoint, pointHash, pointSlot)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (GetHeader (..), headerHash,
                     headerPoint)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (blockUntilJust)

import           Ouroboros.Storage.ChainDB.API (BlockOrHeader (..),
                     ChainDbError (..), Deserialisable (..), Reader (..),
                     ReaderId, deserialisablePoint)

import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Storage.ChainDB.Impl.Types

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

getReadersVar
  :: ChainDbEnv m blk
  -> BlockOrHeader blk b
  -> StrictTVar m (Map ReaderId (StrictTVar m (ReaderState m blk b)))
getReadersVar env = \case
    Block  -> cdbBlockReaders  env
    Header -> cdbHeaderReaders env

getReaderVar
  :: IOLike m
  => ChainDbEnv m blk
  -> BlockOrHeader blk b
  -> ReaderId
  -> STM m (Maybe (StrictTVar m (ReaderState m blk b)))
getReaderVar env blockOrHeader readerId =
  fmap (Map.lookup readerId) $ readTVar $ getReadersVar env blockOrHeader

-- | Check if the ChainDB is open. If not, throw a 'ClosedDBError'. Next,
-- check whether the reader with the given 'ReaderId' still exists. If not,
-- throw a 'ClosedReaderError'.
--
-- Otherwise, execute the given function on the 'ChainDbEnv' and 'ReaderState'
-- 'StrictTVar'.
getReader
  :: forall m blk b r. IOLike m
  => ChainDbHandle m blk
  -> ReaderId
  -> BlockOrHeader blk b
  -> (ChainDbEnv m blk -> StrictTVar m (ReaderState m blk b) -> m r)
  -> m r
getReader (CDBHandle varState) readerId blockOrHeader f = do
    (env, varRdr) <- atomically $ readTVar varState >>= \case
      ChainDbClosed _env -> throwM ClosedDBError
      -- See the docstring of 'ChainDbReopening'
      ChainDbReopening   -> error "ChainDB used while reopening"
      ChainDbOpen    env -> getReaderVar env blockOrHeader readerId >>= \case
        Nothing     -> throwM $ ClosedReaderError readerId
        Just varRdr -> return (env, varRdr)
    f env varRdr

-- | Variant 'of 'getReader' for functions taking one argument.
getReader1
  :: forall m blk b a r. IOLike m
  => ChainDbHandle m blk
  -> ReaderId
  -> BlockOrHeader blk b
  -> (ChainDbEnv m blk -> StrictTVar m (ReaderState m blk b) -> a -> m r)
  -> a -> m r
getReader1 h rdrId blockOrHeader f a =
    getReader h rdrId blockOrHeader (\env varReader -> f env varReader a)

{-------------------------------------------------------------------------------
  Reader
-------------------------------------------------------------------------------}

newReader
  :: forall m blk b.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     , HeaderHash blk ~ HeaderHash b
     )
  => ChainDbHandle m blk
  -> (Header blk -> Encoding)
     -- ^ Needed to serialise a deserialised header that we already had in
     -- memory.
  -> BlockOrHeader blk b
  -> ResourceRegistry m
  -> m (Reader m blk (Deserialisable m blk b))
newReader h encodeHeader blockOrHeader registry = getEnv h $ \cdb@CDB{..} -> do
    readerId  <- atomically $ updateTVar cdbNextReaderId $ \r -> (succ r, r)
    varReader <- newTVarM ReaderInit
    let readersVar = getReadersVar cdb blockOrHeader
    atomically $ modifyTVar readersVar $ Map.insert readerId varReader
    let reader = makeNewReader h encodeHeader readerId blockOrHeader registry
    traceWith cdbTracer $ TraceReaderEvent $ NewReader readerId
    return reader

makeNewReader
  :: forall m blk b.
     ( IOLike m
     , HasHeader blk
     , HasHeader (Header blk)
     , HeaderHash blk ~ HeaderHash b
     )
  => ChainDbHandle m blk
  -> (Header blk -> Encoding)
  -> ReaderId
  -> BlockOrHeader blk b
  -> ResourceRegistry m
  -> Reader m blk (Deserialisable m blk b)
makeNewReader h encodeHeader readerId blockOrHeader registry = Reader {..}
  where
    readerInstruction :: m (Maybe (ChainUpdate blk (Deserialisable m blk b)))
    readerInstruction = getReader h readerId blockOrHeader $
      instructionHelper registry encodeHeader blockOrHeader id

    readerInstructionBlocking :: m (ChainUpdate blk (Deserialisable m blk b))
    readerInstructionBlocking = fmap runIdentity $
      getReader h readerId blockOrHeader $
      instructionHelper registry encodeHeader blockOrHeader (fmap Identity . blockUntilJust)

    readerForward :: [Point blk] -> m (Maybe (Point blk))
    readerForward = getReader1 h readerId blockOrHeader $
      forward registry blockOrHeader

    readerClose :: m ()
    readerClose = getEnv h $ close blockOrHeader readerId

close
  :: forall m blk b. IOLike m
  => BlockOrHeader blk b -> ReaderId -> ChainDbEnv m blk -> m ()
close blockOrHeader readerId cdb@CDB { cdbImmDB } = do
    mbReaderState <- atomically $ do
      readers <- readTVar varReaders
      let (mbReader, readers') = delete readerId readers
      writeTVar varReaders readers'
      traverse readTVar mbReader
    -- If the ReaderId is not present in the map, the Reader must have been
    -- closed already.
    mapM_ (closeReaderState cdbImmDB) mbReaderState
  where
    varReaders = getReadersVar cdb blockOrHeader

    -- | Delete the entry corresponding to the given key from the map. If it
    -- existed, return it, otherwise, return 'Nothing'. The updated map is of
    -- course also returned.
    delete :: forall k a. Ord k => k -> Map k a -> (Maybe a, Map k a)
    delete = Map.updateLookupWithKey (\_ _ -> Nothing)

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
-- The returned 'ChainUpdate' contains a 'b', which can either be a @blk@ or a
-- @'Header' blk@.
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
     , HeaderHash blk ~ HeaderHash b
     , Traversable f, Applicative f
     )
  => ResourceRegistry m
  -> (Header blk -> Encoding)
  -> BlockOrHeader blk b
  -> (    STM m (Maybe (ChainUpdate blk (Header blk)))
       -> STM m (f     (ChainUpdate blk (Header blk))))
     -- ^ How to turn a transaction that may or may not result in a new
     -- 'ChainUpdate' in one that returns the right return type: use @fmap
     -- Identity . 'blockUntilJust'@ to block or 'id' to just return the
     -- @Maybe@.
  -> ChainDbEnv m blk
  -> StrictTVar m (ReaderState m blk b)
  -> m (f (ChainUpdate blk (Deserialisable m blk b)))
instructionHelper registry encodeHeader blockOrHeader fromMaybeSTM CDB{..} varReader = do
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
      -- in the 'ReaderInMem' state.
      Right fupdate -> case blockOrHeader of
        -- We only got the header, so we have to read the whole block if
        -- that's requested.
        Block  -> traverse (traverse toDeserialisableBlock)  fupdate
        Header -> traverse (traverse toDeserialisableHeader) fupdate
      -- We were in the 'ReaderInImmDB' state or we need to switch to it.
      Left (rollState, mbImmIt) -> case rollState of
        RollForwardFrom pt -> do
          immIt <- case mbImmIt of
            Just immIt -> return immIt
            -- We were in the 'ReaderInMem' state but have to switch to the
            -- 'ReaderInImmDB' state.
            Nothing    -> do
              trace $ ReaderNoLongerInMem rollState
              ImmDB.streamAfter cdbImmDB registry blockOrHeader pt
          rollForwardImmDB immIt pt
        RollBackTo      pt -> do
          case mbImmIt of
            Just immIt -> ImmDB.iteratorClose cdbImmDB immIt
            Nothing    -> trace $ ReaderNoLongerInMem rollState
          immIt' <- ImmDB.streamAfter cdbImmDB registry blockOrHeader pt
          let readerState' = ReaderInImmDB (RollForwardFrom pt) immIt'
          atomically $ writeTVar varReader readerState'
          return $ pure $ RollBack pt
  where
    trace = traceWith (contramap TraceReaderEvent cdbTracer)

    toDeserialisableHeader :: Header blk -> m (Deserialisable m blk (Header blk))
    toDeserialisableHeader hdr = return Deserialisable
      { serialised         = Serialised $ toLazyByteString (encodeHeader hdr)
      , deserialisableSlot = blockSlot hdr
      , deserialisableHash = headerHash hdr
      , deserialise        = return hdr
      }

    toDeserialisableBlock :: Header blk -> m (Deserialisable m blk blk)
    toDeserialisableBlock hdr = Query.getAnyKnownDeserialisableBlockOrHeader
      cdbImmDB cdbVolDB Block (headerPoint hdr)

    next
      :: ImmDB.Iterator (HeaderHash blk) m (Deserialisable m blk b)
      -> m (Maybe (Deserialisable m blk b))
    next immIt = ImmDB.iteratorNext cdbImmDB immIt <&> \case
      ImmDB.IteratorResult _ _ b -> Just b
      ImmDB.IteratorEBB    _ _ b -> Just b
      ImmDB.IteratorExhausted    -> Nothing

    rollForwardImmDB
      :: ImmDB.Iterator (HeaderHash blk) m (Deserialisable m blk b)
      -> Point blk
      -> m (f (ChainUpdate blk (Deserialisable m blk b)))
    rollForwardImmDB immIt pt = next immIt >>= \case
      Just b -> do
        let pt' :: Point blk
            pt' = castPoint (deserialisablePoint b)
            readerState' = ReaderInImmDB (RollForwardFrom pt') immIt
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
            case blockOrHeader of
              Header -> traverse (traverse toDeserialisableHeader) fupdate
              Block  -> traverse (traverse toDeserialisableBlock)  fupdate

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
            immIt' <- ImmDB.streamAfter cdbImmDB registry blockOrHeader pt
            -- Try again with the new iterator
            rollForwardImmDB immIt' pt

-- | 'readerInstruction' for when the reader is in the 'ReaderInMem' state.
instructionSTM
  :: forall m blk. (IOLike m, HasHeader (Header blk))
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
  :: forall m blk b.
     ( IOLike m
     , HasCallStack
     , HasHeader blk
     , HasHeader (Header blk)
     )
  => ResourceRegistry m
  -> BlockOrHeader blk b
  -> ChainDbEnv m blk
  -> StrictTVar m (ReaderState m blk b)
  -> [Point blk]
  -> m (Maybe (Point blk))
forward registry blockOrHeader CDB{..} varReader = \pts -> do
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

        | otherwise
        -> do
          inImmDB <- if pt == genesisPoint
            then return True -- Genesis is always "in" the ImmutableDB
            else ImmDB.hasBlock cdbImmDB pt
          if inImmDB
            then do
              immIt <- ImmDB.streamAfter cdbImmDB registry blockOrHeader pt
              updateState $ ReaderInImmDB (RollBackTo pt) immIt
              return $ Just pt
            else findFirstPointOnChain curChain slotNoAtImmDBTip pts

    -- | Update the state of the reader to the given state. If the current
    -- state is 'ReaderInImmDB', close the ImmutableDB iterator to avoid
    -- leaking the file handles.
    updateState :: ReaderState m blk b -> m ()
    updateState newReaderState = do
      mbCloseImmIt <- atomically $ do
        mbCloseImmIt <- readTVar varReader <&> \case
          ReaderInImmDB _ immIt -> Just (ImmDB.iteratorClose cdbImmDB immIt)
          ReaderInit            -> Nothing
          ReaderInMem   _       -> Nothing
        writeTVar varReader newReaderState
        return mbCloseImmIt
      sequence_ mbCloseImmIt

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
    (blockReaderStates, headerReaderStates) <- atomically $ do
      blockReaderStates  <- readTVar cdbBlockReaders  >>= traverse readTVar . Map.elems
      headerReaderStates <- readTVar cdbHeaderReaders >>= traverse readTVar . Map.elems
      writeTVar cdbBlockReaders  Map.empty
      writeTVar cdbHeaderReaders Map.empty
      return (blockReaderStates, headerReaderStates)
    mapM_ (closeReaderState cdbImmDB) blockReaderStates
    mapM_ (closeReaderState cdbImmDB) headerReaderStates
