{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator
  ( streamImpl
  , getSlotInfo
  , CurrentChunkInfo (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Monad.Except
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (find)
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isNothing)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks, forceElemsToWHNF)
import           Cardano.Slotting.Slot

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceKey,
                     ResourceRegistry, allocate, release, unsafeRelease)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.CRC

import           Ouroboros.Consensus.Storage.ImmutableDB.API hiding
                     (getBlockComponent)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..), BlockSize (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}

-- | Internal handle to an iterator
data IteratorHandle hash m = forall h. IteratorHandle
  { itHasFS   :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , itIndex   :: !(Index m hash h)
    -- ^ Bundled Index instance allows to hide type parameters
  , itState   :: !(StrictTVar m (IteratorStateOrExhausted hash m h))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , itEnd     :: !ChunkSlot
    -- ^ The end of the iterator: the last 'ChunkSlot' it should return.
  , itEndHash :: !hash
    -- ^ The @hash@ of the last block the iterator should return.
  }

data IteratorStateOrExhausted hash m h =
    IteratorStateOpen !(IteratorState hash m h)
  | IteratorStateExhausted
  deriving (Generic, NoUnexpectedThunks)

-- Existential type; we can't use generics
instance ( forall a. NoUnexpectedThunks (StrictTVar m a)
         , NoUnexpectedThunks hash
         ) => NoUnexpectedThunks (IteratorHandle hash m) where
  showTypeOf _ = "IteratorHandle"
  whnfNoUnexpectedThunks ctxt IteratorHandle{..} =
      allNoUnexpectedThunks [
          noUnexpectedThunks ctxt itHasFS
        , noUnexpectedThunks ctxt itIndex
        , noUnexpectedThunks ctxt itState
        , noUnexpectedThunks ctxt itEnd
        , noUnexpectedThunks ctxt itEndHash
        ]

data IteratorState hash m h = IteratorState
  { itChunk        :: !ChunkNo
    -- ^ The current chunk the iterator is streaming from.
  , itChunkHandle  :: !(Handle h)
    -- ^ A handle to the chunk file corresponding with 'itChunk'.
  , itChunkKey     :: !(ResourceKey m)
    -- ^ The 'ResourceKey' corresponding to the 'itChunkHandle'. We use it to
    -- release the handle from the 'ResourceRegistry'.
    --
    -- NOTE: if we only close the handle but don't release the resource, the
    -- registry will still hold on to the (closed) handle/resource.
  , itChunkEntries :: !(NonEmpty (WithBlockSize (Secondary.Entry hash)))
    -- ^ The entries from the secondary index corresponding to the current
    -- chunk. The first entry in the list is the next one to stream.
    --
    -- Invariant: all the entries in this list must be included in the stream.
    -- In other words, entries corresponding to blocks after the end bound are
    -- not included in this list.
  }
  deriving (Generic, NoUnexpectedThunks)

-- | Auxiliary data type that combines the 'currentChunk' and
-- 'currentChunkOffset' fields from 'OpenState'. This is used to avoid passing
-- the whole state around, and moreover, it avoids issues with existential @h@
-- type parameter.
data CurrentChunkInfo = CurrentChunkInfo !ChunkNo !BlockOffset

streamImpl
  :: forall m hash b. (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
  => ImmutableDBEnv m hash
  -> ResourceRegistry m
  -> BlockComponent (ImmutableDB hash m) b
  -> Maybe (SlotNo, hash)
     -- ^ When to start streaming (inclusive).
  -> Maybe (SlotNo, hash)
     -- ^ When to stop streaming (inclusive).
  -> m (Either (WrongBoundError hash)
               (Iterator hash m b))
streamImpl dbEnv registry blockComponent mbStart mbEnd =
    withOpenState dbEnv $ \hasFS OpenState{..} -> runExceptT $ do
      lift $ either throwM return =<<
        validateIteratorRange chunkInfo (forgetTipInfo <$> currentTip)
          mbStart mbEnd

      case currentTip of
        Origin ->
          -- If any of the two bounds were specified, 'validateIteratorRange'
          -- would have thrown a 'ReadFutureSlotError'.
          assert (isNothing mbStart && isNothing mbEnd) $ return mkEmptyIterator
        At tip -> do
          WithHash endHash endChunkSlot <- fillInEndBound   currentIndex tip mbEnd
          (secondaryOffset, start)      <- fillInStartBound currentIndex     mbStart

          lift $ do
            -- 'validateIteratorRange' will catch nearly all invalid ranges,
            -- except for one: streaming from the regular block to the EBB in
            -- the same slot. The EBB comes before the regular block, so these
            -- bounds are invalid. However, to distinguish the EBB from the
            -- regular block, as both have the same slot number, we need to
            -- look at the hashes. 'validateIteratorRange' doesn't have enough
            -- information to do that.
            let WithHash _startHash startChunkSlot = start
            when (startChunkSlot > endChunkSlot) $ do
              let startSlot = chunkSlotToSlot chunkInfo startChunkSlot
                  endSlot   = chunkSlotToSlot chunkInfo endChunkSlot
              throwUserError $ InvalidIteratorRangeError startSlot endSlot

            let ChunkSlot startChunk startRelSlot = startChunkSlot
                startIsEBB   = relativeSlotIsEBB startRelSlot
                curChunkInfo = CurrentChunkInfo currentChunk currentChunkOffset

            -- TODO avoid rereading the indices of the start thunk. We read
            -- from both the primary and secondary index in 'fillInStartBound'

            iteratorState <- iteratorStateForChunk hasFS currentIndex registry
              curChunkInfo endHash startChunk secondaryOffset startIsEBB

            varIteratorState <- newTVarM $ IteratorStateOpen iteratorState

            return $ mkIterator IteratorHandle
              { itHasFS   = hasFS
              , itIndex   = currentIndex
              , itState   = varIteratorState
              , itEnd     = endChunkSlot
              , itEndHash = endHash
              }
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

    -- | Fill in the end bound: if 'Nothing', use the current tip. Otherwise,
    -- check whether the bound exists in the database and return the
    -- corresponding 'ChunkSlot'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInEndBound
      :: HasCallStack
      => Index m hash h
      -> TipInfo hash BlockOrEBB   -- ^ Current tip
      -> Maybe (SlotNo, hash)      -- ^ End bound
      -> ExceptT (WrongBoundError hash) m (WithHash hash ChunkSlot)
      -- ^ We can't return 'TipInfo' here because the secondary index does
      -- not give us block numbers
    fillInEndBound index currentTip = \case
      -- End bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'ChunkSlot' accordingly.
      Just end -> do
        (chunkSlot, (entry, _blockSize), _secondaryOffset) <-
          getSlotInfo chunkInfo index end
        return (WithHash (Secondary.headerHash entry) chunkSlot)

      -- No end bound given, use the current tip, but convert the 'BlockOrEBB'
      -- to an 'ChunkSlot'.
      Nothing ->
        return $ chunkSlotForBlockOrEBB chunkInfo <$> fromTipInfo currentTip

    -- | Fill in the start bound: if 'Nothing', use the first block in the
    -- database. Otherwise, check whether the bound exists in the database and
    -- return the corresponding 'ChunkSlot' and 'SecondaryOffset'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInStartBound
      :: HasCallStack
      => Index m hash h
      -> Maybe (SlotNo, hash)  -- ^ Start bound
      -> ExceptT (WrongBoundError hash)
                  m
                  (SecondaryOffset, WithHash hash ChunkSlot)
    fillInStartBound index = \case
      -- Start bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'ChunkSlot' accordingly.
      Just start -> do
        (chunkSlot, (entry, _blockSize), secondaryOffset) <-
          getSlotInfo chunkInfo index start
        return (secondaryOffset, WithHash (Secondary.headerHash entry) chunkSlot)

      -- No start bound given, use the first block in the ImmutableDB as the
      -- start bound.
      Nothing -> lift $ findFirstFilledSlot firstChunkNo
        where
          findFirstFilledSlot chunk =
            Index.readFirstFilledSlot index chunk >>= \case
              -- We know the database is not empty, so this loop must end
              -- before we reach an chunk that doesn't yet exist (which would
              -- result in an error).
              Nothing      -> findFirstFilledSlot (nextChunkNo chunk)
              Just relSlot -> do
                  (Secondary.Entry { headerHash }, _) <-
                    Index.readEntry index chunk isEBB secondaryOffset
                  return (secondaryOffset, WithHash headerHash chunkSlot)
                where
                  -- The first entry in the secondary index file (i.e. the
                  -- first filled slot in the primary index) always starts at
                  -- 0.
                  secondaryOffset = 0
                  isEBB           = relativeSlotIsEBB relSlot
                  chunkSlot       = UnsafeChunkSlot chunk relSlot

    mkEmptyIterator :: Iterator hash m b
    mkEmptyIterator = Iterator
      { iteratorNext    = return IteratorExhausted
      , iteratorHasNext = return Nothing
      , iteratorClose   = return ()
      }

    mkIterator :: IteratorHandle hash m -> Iterator hash m b
    mkIterator ith = Iterator
      { iteratorNext    = iteratorNextImpl dbEnv ith registry blockComponent
      , iteratorHasNext = iteratorHasNextImpl    ith
      , iteratorClose   = iteratorCloseImpl      ith
      }

-- | Get information about the block or EBB at the given slot with the given
-- hash. If no such block exists, because the slot is empty or it contains a
-- block and/or EBB with a different hash, return a 'WrongBoundError'.
--
-- Return the 'ChunkSlot' corresponding to the block or EBB, the corresponding
-- entry (and 'BlockSize') from the secondary index file, and the
-- 'SecondaryOffset' of that entry.
--
-- The primary index is read to find out whether the slot is filled and what
-- the 'SecondaryOffset' is for the slot. The secondary index is read to check
-- the hash and to return the 'Secondary.Entry'.
--
-- PRECONDITION: the bound is in the past.
--
-- PRECONDITION: the database is not empty.
getSlotInfo
  :: (HasCallStack, IOLike m, Eq hash)
  => ChunkInfo
  -> Index m hash h
  -> (SlotNo, hash)
  -> ExceptT (WrongBoundError hash) m
             (ChunkSlot, (Secondary.Entry hash, BlockSize), SecondaryOffset)
getSlotInfo chunkInfo index (slot, hash) = do
    let (chunk, mIfBoundary, ifRegular) = chunkSlotForUnknownBlock chunkInfo slot

    -- Obtain the offsets in the secondary index file from the primary index
    -- file. The block /could/ still correspond to an EBB, a regular block or
    -- both. We will know which one it is when we can check the hashes from
    -- the secondary index file with the hash we have.
    toRead :: NonEmpty (IsEBB, SecondaryOffset) <- case mIfBoundary of
      Just ifBoundary -> do
        offsets <- lift $ Index.readOffsets index chunk
                            (chunkRelative <$> Two ifBoundary ifRegular)
        case offsets of
          Two Nothing Nothing                   ->
            throwError $ EmptySlotError slot
          Two (Just ebbOffset) (Just blkOffset) ->
            return ((IsEBB, ebbOffset) NE.:| [(IsNotEBB, blkOffset)])
          Two (Just ebbOffset) Nothing          ->
            return ((IsEBB, ebbOffset) NE.:| [])
          Two Nothing (Just blkOffset)          ->
            return ((IsNotEBB, blkOffset) NE.:| [])
      Nothing -> do
        offset <- lift $ Index.readOffset index chunk (chunkRelative ifRegular)
        case offset of
          Nothing        ->
            throwError $ EmptySlotError slot
          Just blkOffset ->
            return ((IsNotEBB, blkOffset) NE.:| [])

    entriesWithBlockSizes :: NonEmpty (Secondary.Entry hash, BlockSize) <- lift $
      Index.readEntries index chunk toRead

    -- Return the entry from the secondary index file that matches the
    -- expected hash.
    (secondaryOffset, (entry, blockSize))
      :: (SecondaryOffset, (Secondary.Entry hash, BlockSize)) <-
      case find ((== hash) . Secondary.headerHash . fst . snd)
                (NE.zip (fmap snd toRead) entriesWithBlockSizes) of
        Just found -> return found
        Nothing    -> throwError $ WrongHashError slot hash hashes
          where
            hashes = Secondary.headerHash . fst <$> entriesWithBlockSizes

    -- Use the secondary index entry to determine whether the slot + hash
    -- correspond to an EBB or a regular block.
    let chunkSlot = case (mIfBoundary, Secondary.blockOrEBB entry) of
                      (Just ifBoundary, EBB _) -> ifBoundary
                      _otherwise               -> ifRegular
    return (chunkSlot, (entry, blockSize), secondaryOffset)

iteratorNextImpl
  :: forall m hash b. (IOLike m, Eq hash)
  => ImmutableDBEnv m hash
  -> IteratorHandle hash m
  -> ResourceRegistry m
  -> BlockComponent (ImmutableDB hash m) b
  -> m (IteratorResult b)
iteratorNextImpl dbEnv it@IteratorHandle
                         { itHasFS = hasFS :: HasFS m h
                         , itIndex = index :: Index m hash h
                         , ..
                         } registry blockComponent = do
    -- The idea is that if the state is not 'IteratorStateExhausted, then the
    -- head of 'itChunkEntries' is always ready to be read. After reading it
    -- with 'readNextBlock' or 'readNextHeader', 'stepIterator' will advance
    -- the iterator to the next valid chunk slot if @step@ is True.
    atomically (readTVar itState) >>= \case
      -- Iterator already closed
      IteratorStateExhausted -> return IteratorExhausted
      IteratorStateOpen iteratorState@IteratorState{..} ->
        withOpenState dbEnv $ \_ st -> do
          let curChunkInfo = CurrentChunkInfo
                (currentChunk       st)
                (currentChunkOffset st)
              entry = NE.head itChunkEntries
          b <- getBlockComponent itChunkHandle itChunk entry blockComponent
          stepIterator curChunkInfo iteratorState
          return $ IteratorResult b
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

    getBlockComponent
      :: Handle h
      -> ChunkNo
      -> WithBlockSize (Secondary.Entry hash)
      -> BlockComponent (ImmutableDB hash m) b'
      -> m b'
    getBlockComponent itChunkHandle itChunk entryWithBlockSize = \case
        GetHash         -> return headerHash
        GetSlot         -> return $ slotNoOfBlockOrEBB chunkInfo blockOrEBB
        GetIsEBB        -> return $ case blockOrEBB of
          Block _ -> IsNotEBB
          EBB   _ -> IsEBB
        GetBlockSize    -> return blockSize
        GetHeaderSize   ->
          return $ fromIntegral $ Secondary.unHeaderSize headerSize
        GetRawBlock     ->
          readNextBlock  itChunkHandle entryWithBlockSize itChunk
        GetRawHeader    ->
          readNextHeader itChunkHandle entry
        GetBlock        ->
          return ()
        GetHeader       ->
          return ()
        GetPure a       ->
          return a
        GetApply f bc   ->
          getBlockComponent itChunkHandle itChunk entryWithBlockSize f <*>
          getBlockComponent itChunkHandle itChunk entryWithBlockSize bc
      where
        WithBlockSize blockSize entry = entryWithBlockSize
        Secondary.Entry { headerHash, headerSize, blockOrEBB } = entry

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAtCRC', i.e. @pread@ for reading from a given offset.
    readNextBlock
      :: Handle h
      -> WithBlockSize (Secondary.Entry hash)
      -> ChunkNo
      -> m ByteString
    readNextBlock eHnd (WithBlockSize size entry) chunk = do
        (bl, checksum') <- hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
        checkChecksum chunkFile blockOrEBB checksum checksum'
        return bl
      where
        Secondary.Entry { blockOffset, checksum, blockOrEBB } = entry
        offset    = AbsOffset $ Secondary.unBlockOffset blockOffset
        chunkFile = fsPathChunkFile chunk

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAt', i.e. @pread@ for reading from a given offset.
    readNextHeader
      :: Handle h
      -> Secondary.Entry hash
      -> m ByteString
    readNextHeader eHnd Secondary.Entry { blockOffset, headerOffset, headerSize } =
        -- We cannot check the checksum in this case, as we're not reading the
        -- whole block
        hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ Secondary.unHeaderSize headerSize
        offset = AbsOffset $
          (Secondary.unBlockOffset blockOffset) +
          fromIntegral (Secondary.unHeaderOffset headerOffset)

    -- | Move the iterator to the next position that can be read from,
    -- advancing chunks if necessary. If no next position can be found, the
    -- iterator is closed.
    stepIterator :: CurrentChunkInfo -> IteratorState hash m h -> m ()
    stepIterator curChunkInfo iteratorState@IteratorState {..} =
      case NE.nonEmpty (NE.tail itChunkEntries) of
        -- There are entries left in this chunk, so continue. See the
        -- invariant on 'itChunkEntries'
        Just itChunkEntries' -> atomically $ writeTVar itState $
          IteratorStateOpen iteratorState { itChunkEntries = itChunkEntries' }

        -- No more entries in this chunk, so open the next.
        Nothing -> do
          -- Release the resource, i.e., close the handle.
          void $ release itChunkKey
          -- If this was the final chunk, close the iterator
          if itChunk >= chunkIndex itEnd then
            iteratorCloseImpl it

          else
            openNextChunk curChunkInfo itEnd (nextChunkNo itChunk) >>= \iteratorState' ->
            atomically $ writeTVar itState $ IteratorStateOpen iteratorState'

    openNextChunk
      :: CurrentChunkInfo
      -> ChunkSlot  -- ^ The end bound
      -> ChunkNo    -- ^ The chunk to open
      -> m (IteratorState hash m h)
    openNextChunk curChunkInfo end chunk =
      Index.readFirstFilledSlot index chunk >>= \case
        -- This chunk is empty, look in the next one.
        --
        -- We still haven't encountered the end bound, so this loop must end
        -- when we reach the non-empty chunk containing the end bound. This
        -- cannot loop forever as an error would be thrown when opening the
        -- index file(s) of a non-existing chunk.
        Nothing      -> openNextChunk curChunkInfo end (nextChunkNo chunk)
        Just relSlot -> do
          -- Note that the only reason we actually open the primary index file
          -- is to see whether the first block in the chunk is an EBB or not.
          -- To see whether the chunk is empty, we could open the secondary
          -- index file directly and see whether it contains any blocks. The
          -- 'secondaryOffset' will be 0, as the first entry in the secondary
          -- index file always starts at offset 0. The same is true for
          -- 'findFirstFilledSlot'.
          let firstIsEBB      = relativeSlotIsEBB relSlot
              secondaryOffset = 0

          iteratorStateForChunk hasFS index registry curChunkInfo itEndHash
            chunk secondaryOffset firstIsEBB

iteratorHasNextImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m (Maybe (Either EpochNo SlotNo, hash))
iteratorHasNextImpl IteratorHandle { itState } =
    atomically $ readTVar itState <&> \case
      IteratorStateExhausted -> Nothing
      IteratorStateOpen IteratorState { itChunkEntries } ->
          Just (epochOrSlot, Secondary.headerHash nextEntry)
        where
          WithBlockSize _ nextEntry NE.:| _ = itChunkEntries
          epochOrSlot = case Secondary.blockOrEBB nextEntry of
            EBB epoch  -> Left epoch
            Block slot -> Right slot

iteratorCloseImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m ()
iteratorCloseImpl IteratorHandle { itState } = do
    atomically (readTVar itState) >>= \case
      -- Already closed
      IteratorStateExhausted -> return ()
      IteratorStateOpen IteratorState { itChunkKey } -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- 'release' might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar itState IteratorStateExhausted
        -- TODO: we must use 'unsafeRelease' instead of 'release' because we
        -- might close the iterator from an /untracked thread/, i.e., a thread
        -- that was not spawned by the resource registry (or the thread that
        -- opened the resource registry) in which the handle was allocated.
        --
        -- This happens in the consensus tests (but not in the actual node),
        -- where the protocol threads that open iterators (BlockFetchServer
        -- and ChainSyncServer) are spawned using a different resource
        -- registry (A) than the one the ImmutableDB (and ChainDB) use (B).
        -- When the ChainDB is closed (by the thread that opened B), we're
        -- closing all open iterators, i.e., the iterators opened by the
        -- protocol threads. So we're releasing handles allocated in resource
        -- registry A from a thread tracked by resource registry B. See #1390.
        void $ unsafeRelease itChunkKey

iteratorStateForChunk
  :: (HasCallStack, IOLike m, Eq hash)
  => HasFS m h
  -> Index m hash h
  -> ResourceRegistry m
  -> CurrentChunkInfo
  -> hash
     -- ^ Hash of the end bound
  -> ChunkNo
  -> SecondaryOffset
     -- ^ Where to start in the secondary index
  -> IsEBB
     -- ^ Whether the first expected block will be an EBB or not.
  -> m (IteratorState hash m h)
iteratorStateForChunk hasFS index registry
                      (CurrentChunkInfo curChunk curChunkOffset) endHash
                      chunk secondaryOffset firstIsEBB = do
    -- Open the chunk file. Allocate the handle in the registry so that it
    -- will be closed in case of an exception.
    (key, eHnd) <- allocate
      registry
      (\_key -> hOpen (fsPathChunkFile chunk) ReadMode)
      hClose

    -- If the last entry in @entries@ corresponds to the last block in the
    -- chunk, we cannot calculate the block size based on the next block.
    -- Instead, we calculate it based on the size of the chunk file.
    --
    -- IMPORTANT: for older chunks, this is fine, as the secondary index
    -- (entries) and the chunk file (size) are immutable. However, when doing
    -- this for the current chunk, there is a potential race condition between
    -- reading of the entries from the secondary index and obtaining the chunk
    -- file size: what if a new block was appended after reading the entries
    -- but before obtaining the chunk file size? Then the chunk file size will
    -- not correspond to the last entry we read, but to the block after it.
    -- Similarly if we switch the order of the two operations.
    --
    -- To avoid this race condition, we use the value of 'currentChunkOffset'
    -- from the state as the file size of the current chunk (stored in
    -- 'CurrentChunkInfo'). This value corresponds to the chunk file size at
    -- the time we /read the state/. We also know that the end bound of our
    -- iterator is always <= the tip from that same state, so all @entries@
    -- must be <= the tip from that state because we'll never stream beyond
    -- the tip. Remember that we only actually use the current chunk file size
    -- if the last entry we have read from the secondary index is the last
    -- entry in the file, in which case it would correspond to the tip from
    -- the state. In this case, the chunk file size (@curChunkOffset@) we are
    -- passed is consistent with the tip, as it was obtained from the same
    -- consistent state.
    chunkFileSize <- if chunk == curChunk
      then return (unBlockOffset curChunkOffset)
      else hGetSize eHnd

    entries <- Index.readAllEntries index secondaryOffset chunk
      ((== endHash) . Secondary.headerHash) chunkFileSize firstIsEBB

    case NE.nonEmpty entries of
      -- We still haven't encountered the end bound, so it cannot be
      -- that this non-empty chunk contains no entries <= the end bound.
      Nothing             -> error
        "impossible: there must be entries according to the primary index"

      Just itChunkEntries -> return IteratorState
        { itChunk        = chunk
        , itChunkHandle  = eHnd
        , itChunkKey     = key
          -- Force so we don't store any thunks in the state
        , itChunkEntries = forceElemsToWHNF itChunkEntries
        }
  where
    HasFS { hOpen, hClose, hGetSize } = hasFS
