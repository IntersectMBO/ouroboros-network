{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator (
    CurrentChunkInfo (..)
  , extractBlockComponent
  , getSlotInfo
  , streamImpl
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import           Data.Foldable (find)
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           GHC.Generics (Generic)

import           Cardano.Prelude (forceElemsToWHNF)

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block hiding (headerHash)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceKey,
                     ResourceRegistry, allocate, release, unsafeRelease)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.CRC
import           Ouroboros.Consensus.Storage.Serialisation

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
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}

-- | Internal handle to an iterator.
--
-- Note: in contrast to 'IteratorState', these fields remain static for the
-- lifetime of the iterator.
data IteratorHandle m blk h = IteratorHandle {
      ithHasFS    :: !(HasFS m h)
      -- ^ Bundled HasFS instance because of the existential @h@.
    , ithIndex    :: !(Index m blk h)
      -- ^ Bundled Index instance because of the existential @h@.
    , ithVarState :: !(StrictTVar m (IteratorStateOrExhausted m blk h))
      -- ^ The state of the iterator
    , ithEndChunk :: !ChunkNo
      -- ^ The chunk in which the last block to stream is located.
    , ithEndHash  :: !(HeaderHash blk)
      -- ^ The has of the last block the iterator should return.
    }

data IteratorStateOrExhausted m hash h =
    IteratorStateOpen !(IteratorState m hash h)
  | IteratorStateExhausted
  deriving (Generic, NoThunks)

data IteratorState m blk h = IteratorState {
      itsChunk        :: !ChunkNo
      -- ^ The current chunk the iterator is streaming from.
    , itsChunkHandle  :: !(Handle h)
      -- ^ A handle to the chunk file corresponding with 'itsChunk'.
    , itsChunkKey     :: !(ResourceKey m)
      -- ^ The 'ResourceKey' corresponding to the 'itsChunkHandle'. We use it to
      -- release the handle from the 'ResourceRegistry'.
      --
      -- NOTE: if we only close the handle but don't release the resource, the
      -- registry will still hold on to the (closed) handle/resource.
    , itsChunkEntries :: !(NonEmpty (WithBlockSize (Secondary.Entry blk)))
      -- ^ The entries from the secondary index corresponding to the current
      -- chunk. The first entry in the list is the next one to stream.
      --
      -- Invariant: all the entries in this list must be included in the stream.
      -- In other words, entries corresponding to blocks after the end bound are
      -- not included in this list.
    }
  deriving (Generic)

deriving instance (StandardHash blk, IOLike m) => NoThunks (IteratorState m blk h)

-- | Auxiliary data type that combines the 'currentChunk' and
-- 'currentChunkOffset' fields from 'OpenState'. This is used to avoid passing
-- the whole state around, and moreover, it avoids issues with existential @h@
-- type parameter.
data CurrentChunkInfo = CurrentChunkInfo !ChunkNo !BlockOffset

streamImpl ::
     forall m blk b.
     ( IOLike m
     , HasHeader blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , DecodeDiskDep (NestedCtxt Header) blk
     , ReconstructNestedCtxt Header blk
     , HasCallStack
     )
  => ImmutableDBEnv m blk
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> StreamFrom blk
  -> StreamTo   blk
  -> m (Either (MissingBlock blk) (Iterator m blk b))
streamImpl dbEnv registry blockComponent = \from to ->
    withOpenState dbEnv $ \hasFS OpenState{..} -> runExceptT $ do
      unless (validBounds from to) $
        lift $ throwApiMisuse $ InvalidIteratorRangeError from to

      endChunkSlot <- checkUpperBound currentIndex currentTip to

      --  When the lower bound is exclusive, we do the same as when it is
      --  inclusive. We set up the iterator to point at the lower bound. Only at
      --  the very end of this function do we advance it to the block after it,
      --  in the case of an exclusive lower bound.
      (secondaryOffset, startChunkSlot) <-
        checkLowerBound currentIndex currentTip from

      lift $ do
        -- 'validBounds' will catch nearly all invalid ranges, except for one:
        -- streaming from the regular block to the EBB in the same slot. The
        -- EBB comes before the regular block, so these bounds are invalid.
        -- However, to distinguish the EBB from the regular block, as both
        -- have the same slot number, we need to look at the hashes.
        -- 'validateBounds' doesn't have enough information to do that.
        when (startChunkSlot > endChunkSlot) $
          throwApiMisuse $ InvalidIteratorRangeError from to

        let ChunkSlot startChunk startRelSlot = startChunkSlot
            startIsEBB = relativeSlotIsEBB startRelSlot
            currentChunkInfo = CurrentChunkInfo currentChunk currentChunkOffset
            endHash = case to of
              StreamToInclusive (RealPoint _slot hash) -> hash

        iteratorState <-
          iteratorStateForChunk
            hasFS
            currentIndex
            registry
            currentChunkInfo
            endHash
            startChunk
            secondaryOffset
            startIsEBB

        varIteratorState <- newTVarIO $ IteratorStateOpen iteratorState

        let ith = IteratorHandle {
                ithHasFS    = hasFS
              , ithIndex    = currentIndex
              , ithVarState = varIteratorState
              , ithEndChunk = chunkIndex endChunkSlot
              , ithEndHash  = endHash
              }

        -- When streaming from an exclusive lower bound that is not genesis, we
        -- have opened the iterator at the bound itself, so we have to skip it
        -- first.
        case from of
          StreamFromExclusive (BlockPoint {}) ->
            stepIterator registry currentChunkInfo ith
          _otherwise -> return ()

        return $ mkIterator ith
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

    -- | Check the upper bound: check whether it exists in the database (return
    -- a 'MissingBlock' otherwise), and return the corresponding 'ChunkSlot'.
    checkUpperBound ::
         HasCallStack
      => Index m blk h
      -> WithOrigin (Tip blk)  -- ^ Current tip
      -> StreamTo blk
      -> ExceptT (MissingBlock blk) m ChunkSlot
      -- ^ We can't return 'TipInfo' here because the secondary index does
      -- not give us block numbers
    checkUpperBound index currentTip (StreamToInclusive endPt) = do
        (chunkSlot, _, _) <- getSlotInfo chunkInfo index currentTip endPt
        return chunkSlot

    -- | Check the lower bound: check whether it exists in the database (return
    -- a 'MissingBlock' otherwise), and return the corresponding 'ChunkSlot' and
    -- 'SecondaryOffset'.
    --
    -- PRECONDITION: the end bound has been checked already
    --
    -- PRECONDITION: the bounds passed the 'validBounds' check
    --
    -- Both preconditions combined guarantee us that the tip is not origin and
    -- that the lower bound is <= the tip.
    checkLowerBound ::
         HasCallStack
      => Index m blk h
      -> WithOrigin (Tip blk)  -- ^ Current tip
      -> StreamFrom blk
      -> ExceptT (MissingBlock blk) m (SecondaryOffset, ChunkSlot)
    checkLowerBound index currentTip = \case
        StreamFromInclusive startPt -> do
          (chunkSlot, _, secondaryOffset) <-
            getSlotInfo chunkInfo index currentTip startPt
          return (secondaryOffset, chunkSlot)
        StreamFromExclusive startPt -> case pointToWithOriginRealPoint startPt of
          Origin             -> lift $ findFirstBlock index
          NotOrigin startPt' -> do
            (chunkSlot, _, secondaryOffset) <-
              getSlotInfo chunkInfo index currentTip startPt'
            return (secondaryOffset, chunkSlot)

    mkIterator :: IteratorHandle m blk h -> Iterator m blk b
    mkIterator ith = Iterator {
          iteratorNext    = iteratorNextImpl    dbEnv ith registry blockComponent
        , iteratorHasNext = iteratorHasNextImpl dbEnv ith
        , iteratorClose   = iteratorCloseImpl         ith
        }

    -- | Find the 'SecondaryOffset' and 'ChunkSlot' corresponding to the first
    -- block in the ImmutableDB.
    --
    -- PRECONDITION: the ImmutableDB is not empty.
    findFirstBlock ::
         HasCallStack
      => Index m blk h
      -> m (SecondaryOffset, ChunkSlot)
    findFirstBlock index = go firstChunkNo
      where
        go :: ChunkNo -> m (SecondaryOffset, ChunkSlot)
        go chunk = Index.readFirstFilledSlot index chunk >>= \case
          Nothing      -> go (nextChunkNo chunk)
          Just relSlot -> return (0, chunkSlotForRelativeSlot chunk relSlot)

-- | Get information about the block or EBB at the given slot with the given
-- hash. If no such block exists, because the slot is empty, it contains a block
-- and/or EBB with a different hash, or it is newer than the current tip, return
-- a 'MissingBlock'.
--
-- Return the 'ChunkSlot' corresponding to the block or EBB, the corresponding
-- entry (and 'BlockSize') from the secondary index file, and the
-- 'SecondaryOffset' of that entry.
--
-- The primary index is read to find out whether the slot is filled and what
-- the 'SecondaryOffset' is for the slot. The secondary index is read to check
-- the hash and to return the 'Secondary.Entry'.
getSlotInfo ::
     forall m blk h. (HasCallStack, IOLike m, HasHeader blk)
  => ChunkInfo
  -> Index m blk h
  -> WithOrigin (Tip blk)  -- ^ Current tip
  -> RealPoint blk
  -> ExceptT (MissingBlock blk) m
             ( ChunkSlot
             , (Secondary.Entry blk, BlockSize)
             , SecondaryOffset
             )
getSlotInfo chunkInfo index currentTip pt@(RealPoint slot hash) = do
    let (chunk, mIfBoundary, ifRegular) =
          chunkSlotForUnknownBlock chunkInfo slot

    case currentTip of
      NotOrigin (Tip { tipSlotNo })
        | slot <= tipSlotNo
        -> return ()
      _otherwise
        -> throwError $ NewerThanTip pt (tipToPoint currentTip)

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
            throwError $ EmptySlot pt
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
            throwError $ EmptySlot pt
          Just blkOffset ->
            return ((IsNotEBB, blkOffset) NE.:| [])

    entriesWithBlockSizes :: NonEmpty (Secondary.Entry blk, BlockSize) <-
      lift $ Index.readEntries index chunk toRead

    -- Return the entry from the secondary index file that matches the
    -- expected hash.
    (secondaryOffset, (entry, blockSize)) <-
      case find ((== hash) . Secondary.headerHash . fst . snd)
                (NE.zip (fmap snd toRead) entriesWithBlockSizes) of
        Just found -> return found
        Nothing    -> throwError $ WrongHash pt hashes
          where
            hashes = Secondary.headerHash . fst <$> entriesWithBlockSizes

    -- Use the secondary index entry to determine whether the slot + hash
    -- correspond to an EBB or a regular block.
    let chunkSlot = case (mIfBoundary, Secondary.blockOrEBB entry) of
                      (Just ifBoundary, EBB _) -> ifBoundary
                      _otherwise               -> ifRegular
    return (chunkSlot, (entry, blockSize), secondaryOffset)


-- | Move the iterator to the next position that can be read from,
-- advancing chunks if necessary. If no next position can be found, the
-- iterator is closed.
--
-- PRECONDITION: the given 'IteratorState' matches the one stored in the
-- given 'StrictTVar'.
--
-- PRECONDITION: the iterator is not exhausted.
stepIterator ::
     forall m blk h. (HasCallStack, IOLike m, HasHeader blk)
  => ResourceRegistry m
  -> CurrentChunkInfo
  -> IteratorHandle m blk h
  -> m ()
stepIterator registry currentChunkInfo
             ith@IteratorHandle { ithHasFS, ithIndex, ithVarState, ithEndChunk, ithEndHash } =
    atomically (readTVar ithVarState) >>= \case
      IteratorStateExhausted ->
        error "precondition violated: iterator must not be exhausted"
      IteratorStateOpen its@IteratorState { itsChunkEntries, itsChunkKey, itsChunk } ->
        case NE.nonEmpty (NE.tail itsChunkEntries) of
          -- There are entries left in this chunk, so continue. See the
          -- invariant on 'itsChunkEntries'
          Just itsChunkEntries' ->
            atomically $ writeTVar ithVarState $
              IteratorStateOpen its { itsChunkEntries = itsChunkEntries' }

          -- No more entries in this chunk, so open the next.
          Nothing -> do
            -- Release the resource, i.e., close the handle.
            void $ release itsChunkKey
            -- If this was the final chunk, close the iterator
            if itsChunk >= ithEndChunk then
              iteratorCloseImpl ith
            else
              openNextChunk (nextChunkNo itsChunk) >>= \its' ->
                atomically $ writeTVar ithVarState $ IteratorStateOpen its'
  where
    openNextChunk ::
         ChunkNo    -- ^ The chunk to open
      -> m (IteratorState m blk h)
    openNextChunk chunk =
      Index.readFirstFilledSlot ithIndex chunk >>= \case
        -- This chunk is empty, look in the next one.
        --
        -- We still haven't encountered the end bound, so this loop must end
        -- when we reach the non-empty chunk containing the end bound. This
        -- cannot loop forever as an error would be thrown when opening the
        -- index file(s) of a non-existing chunk.
        Nothing      -> openNextChunk (nextChunkNo chunk)
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

          iteratorStateForChunk
            ithHasFS
            ithIndex
            registry
            currentChunkInfo
            ithEndHash
            chunk
            secondaryOffset
            firstIsEBB


iteratorNextImpl ::
     forall m blk b h.
     ( IOLike m
     , HasHeader blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , DecodeDiskDep (NestedCtxt Header) blk
     , ReconstructNestedCtxt Header blk
     )
  => ImmutableDBEnv m blk
  -> IteratorHandle m blk h
  -> ResourceRegistry m
  -> BlockComponent blk b
  -> m (IteratorResult b)
iteratorNextImpl dbEnv ith@IteratorHandle { ithHasFS, ithVarState } registry blockComponent = do
    -- The idea is that if the state is not 'IteratorStateExhausted', then the
    -- head of 'itsChunkEntries' is always ready to be read. After extracting
    -- the block component, 'stepIterator' will advance the iterator to the next
    -- block.
    atomically (readTVar ithVarState) >>= \case
      -- Iterator already closed
      IteratorStateExhausted -> return IteratorExhausted
      IteratorStateOpen IteratorState { itsChunkEntries, itsChunk, itsChunkHandle } ->
        withOpenState dbEnv $ \_ st -> do
          let entry = NE.head itsChunkEntries
              currentChunkInfo = CurrentChunkInfo
                                   (currentChunk st)
                                   (currentChunkOffset st)
          b <-
            extractBlockComponent
              ithHasFS
              chunkInfo
              itsChunk
              codecConfig
              checkIntegrity
              itsChunkHandle
              entry
              blockComponent
          stepIterator registry currentChunkInfo ith
          return $ IteratorResult b
  where
    ImmutableDBEnv { codecConfig, chunkInfo, checkIntegrity } = dbEnv

iteratorHasNextImpl ::
     IOLike m
  => ImmutableDBEnv m blk
  -> IteratorHandle m blk h
  -> STM m (Maybe (RealPoint blk))
iteratorHasNextImpl ImmutableDBEnv { chunkInfo } IteratorHandle { ithVarState } =
    readTVar ithVarState <&> \case
      IteratorStateExhausted -> Nothing
      IteratorStateOpen IteratorState { itsChunkEntries } ->
          Just (RealPoint slotNo (Secondary.headerHash nextEntry))
        where
          WithBlockSize _ nextEntry NE.:| _ = itsChunkEntries

          slotNo :: SlotNo
          slotNo = slotNoOfBlockOrEBB chunkInfo (Secondary.blockOrEBB nextEntry)

iteratorCloseImpl ::
     (HasCallStack, IOLike m)
  => IteratorHandle m blk h
  -> m ()
iteratorCloseImpl IteratorHandle { ithVarState } = do
    atomically (readTVar ithVarState) >>= \case
      -- Already closed
      IteratorStateExhausted -> return ()
      IteratorStateOpen IteratorState { itsChunkKey } -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- 'release' might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar ithVarState IteratorStateExhausted
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
        void $ unsafeRelease itsChunkKey

iteratorStateForChunk ::
     (HasCallStack, HasHeader blk, IOLike m)
  => HasFS m h
  -> Index m blk h
  -> ResourceRegistry m
  -> CurrentChunkInfo
  -> HeaderHash blk
     -- ^ Hash of the end bound
  -> ChunkNo
  -> SecondaryOffset
     -- ^ Where to start in the secondary index
  -> IsEBB
     -- ^ Whether the first expected block will be an EBB or not.
  -> m (IteratorState m blk h)
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

      Just itsChunkEntries -> return IteratorState {
          itsChunk        = chunk
        , itsChunkHandle  = eHnd
        , itsChunkKey     = key
          -- Force so we don't store any thunks in the state
        , itsChunkEntries = forceElemsToWHNF itsChunkEntries
        }
  where
    HasFS { hOpen, hClose, hGetSize } = hasFS

extractBlockComponent ::
     forall m blk b h.
     ( HasHeader blk
     , ReconstructNestedCtxt Header blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , DecodeDiskDep (NestedCtxt Header) blk
     , IOLike m
     )
  => HasFS m h
  -> ChunkInfo
  -> ChunkNo
  -> CodecConfig blk
  -> (blk -> Bool)
  -> Handle h
  -> WithBlockSize (Secondary.Entry blk)
  -> BlockComponent blk b
  -> m b
extractBlockComponent hasFS chunkInfo chunk ccfg checkIntegrity eHnd
                      (WithBlockSize blockSize entry) = go
  where
    go :: forall b'. BlockComponent blk b' -> m b'
    go = \case
        GetHash          -> return headerHash
        GetSlot          -> return slotNo
        GetIsEBB         -> return $ isBlockOrEBB blockOrEBB
        GetBlockSize     -> return blockSize
        GetHeaderSize    -> return $ fromIntegral $ Secondary.unHeaderSize headerSize
        GetRawBlock      -> readBlock
        GetRawHeader     -> readHeader
        GetNestedCtxt    -> readNestedCtxt
        GetBlock         -> do rawBlk <- go GetRawBlock
                               parseBlock rawBlk
        GetHeader        -> do rawHdr <- go GetRawHeader
                               ctxt   <- readNestedCtxt
                               parseHeader ctxt rawHdr
        GetVerifiedBlock -> do blk <- go GetBlock
                               unless (checkIntegrity blk) $
                                 throwUnexpectedFailure $ CorruptBlockError pt
                               return blk
        GetPure a        -> return a
        GetApply f bc    -> go f <*> go bc

    Secondary.Entry {
          blockOffset
        , checksum
        , headerHash
        , headerSize
        , headerOffset
        , blockOrEBB
        } = entry

    slotNo :: SlotNo
    slotNo = slotNoOfBlockOrEBB chunkInfo blockOrEBB

    pt :: RealPoint blk
    pt = RealPoint slotNo headerHash

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAtCRC', i.e. @pread@ for reading from a given offset.
    --
    -- In case the requested chunk is the current chunk, we will be reading
    -- from the chunk file while we're also writing to it. Are we guaranteed
    -- to read what have written? Duncan says: this is guaranteed at the OS
    -- level (POSIX), but not for Haskell handles, which might perform other
    -- buffering. However, the 'HasFS' implementation we're using uses POSIX
    -- file handles ("Ouroboros.Consensus.Storage.IO") so we're safe (other
    -- implementations of the 'HasFS' API guarantee this too).
    readBlock :: m Lazy.ByteString
    readBlock = do
        (bl, checksum') <- hGetExactlyAtCRC hasFS eHnd size offset
        checkChecksum chunkFile pt checksum checksum'
        return bl
      where
        size      = fromIntegral blockSize
        offset    = AbsOffset $ Secondary.unBlockOffset blockOffset
        chunkFile = fsPathChunkFile chunk

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAt', i.e. @pread@ for reading from a given offset.
    readHeader :: m Lazy.ByteString
    readHeader =
        -- We cannot check the checksum in this case, as we're not reading the
        -- whole block
        hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ Secondary.unHeaderSize headerSize
        offset = AbsOffset $
          (Secondary.unBlockOffset blockOffset) +
          fromIntegral (Secondary.unHeaderOffset headerOffset)

    readNestedCtxt :: m (SomeSecond (NestedCtxt Header) blk)
    readNestedCtxt = do
        bytes <- Short.toShort . Lazy.toStrict <$>
                   hGetExactlyAt hasFS eHnd size offset
        return $ reconstructNestedCtxt p bytes blockSize
      where
        p :: Proxy (Header blk)
        p = Proxy

        size   = fromIntegral (getPrefixLen (reconstructPrefixLen p))
        offset = AbsOffset $ Secondary.unBlockOffset blockOffset

    parseBlock :: Lazy.ByteString -> m blk
    parseBlock bytes = throwParseErrors bytes $
        CBOR.deserialiseFromBytes (decodeDisk ccfg) bytes

    parseHeader ::
         SomeSecond (NestedCtxt Header) blk
      -> Lazy.ByteString
      -> m (Header blk)
    parseHeader (SomeSecond ctxt) bytes = throwParseErrors bytes $
        CBOR.deserialiseFromBytes
          ((\f -> nest . DepPair ctxt . f) <$> decodeDiskDep ccfg ctxt)
          bytes

    throwParseErrors ::
         forall b'.
         Lazy.ByteString
      -> Either CBOR.DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> b')
      -> m b'
    throwParseErrors fullBytes = \case
        Right (trailing, f)
          | Lazy.null trailing
          -> return $ f fullBytes
          | otherwise
          -> throwUnexpectedFailure $
               TrailingDataError (fsPathChunkFile chunk) pt trailing
        Left err
          -> throwUnexpectedFailure $
               ParseError (fsPathChunkFile chunk) pt err
