{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Immutable on-disk database of binary blobs
--
-- = Internal format
--
-- The API of the ImmutableDB uses 'SlotNo' to indicate a location in the
-- chain\/immutable database. The contents of the database are not stored in
-- one big file that is appended to in eternity, but a separate file is
-- created for each 'ChunkNo'.
--
-- Within each 'ChunkNo', the entries are numbered by 'RelativeSlot's. Each
-- 'SlotNo' can be converted to a combination of an 'ChunkNo' and a 'RelativeSlot'
-- (= 'ChunkSlot') and vice versa. This conversion depends on the size of the
-- chunks: 'ChunkSize'. This size may not be the same for each chunk. When
-- opening the database, the user must give a 'ChunkInfo' that will be used to
-- find out the size of each chunk.
--
-- For example:
--
-- > Chunks:         <──────── 0 ────────> <────── 1 ──────>
-- > chunk size:               4                   3
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > 'RelativeSlot':   0   1   2   3   4     0   1   2   3
-- > 'SlotNo':        EBB  0   1   2   3    EBB  4   5   6
--
-- Not all chunks can contain EBBs; see 'ChunkInfo' for details.
--
-- = Errors
--
-- Whenever an 'Ouroboros.Consensus.Storage.ImmutableDB.Types.UnexpectedError'
-- is thrown during an operation, e.g., 'appendBinaryBlob', the database will be
-- automatically closed because we can not guarantee a consistent state in the
-- face of file system errors. See the 'reopen' operation and the paragraph
-- below about reopening the database for more information.
--
-- = (Re)opening the database
--
-- The database can be closed and reopened. In case the database was closed
-- because of an unexpected error, the same database can be reopened again
-- with 'reopen' using a 'ValidationPolicy', which will truncate invalid data
-- from the database until a valid prefix is recovered.
--
-- = Concurrency
--
-- The same database should not be opened multiple times concurrently.
--
-- TODO Should we ensure this with a lock file?
-- https://hackage.haskell.org/package/filelock-0.1.1.2/docs/System-FileLock.html
--
-- The database can have multiple readers, but should only have one writer.
--
--
-- = Layout on disk
--
-- The database is structured on disk as follows:
--
-- > /
-- >   00000.epoch
-- >   00000.primary
-- >   00000.secondary
-- >   ..
-- >   00008.epoch
-- >   00008.primary
-- >   00008.secondary
--
-- For each chunk, there are three files on disk:
--
--   * A \"chunk file\" that stores the actual binary blobs. But nothing
--     more, so nothing is stored for empty slots.
--
--   * A \"secondary index file\" that stores information about each block:
--     its hash, the slot number or epoch number in case of an EBB, a checksum
--     of the block, the offset of the block in the chunk file, and more. This
--     index is sparse to save space.
--
--   * A \"primary index file\" that maps slots to offsets in the secondary
--     index file.
module Ouroboros.Consensus.Storage.ImmutableDB.Impl
  ( withDB
  , ImmutableDbArgs (..)
    -- * Internals for testing purposes
  , openDBInternal
  , Internal (..)
  , deleteAfter
  ) where

import           Prelude hiding (truncate)

import           Control.Monad (replicateM_, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (get, lift, modify, put)
import           Control.Tracer (Tracer, traceWith)
import           Data.ByteString.Builder (Builder)
import           Data.Functor (($>))
import           GHC.Stack (HasCallStack)

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     runWithTempRegistry)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types hiding (allowExisting)
import           Ouroboros.Consensus.Storage.FS.CRC

import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unsafeEpochNoToChunkNo)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..), BlockSize, HeaderOffset (..),
                     HeaderSize (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Validation
import           Ouroboros.Consensus.Storage.ImmutableDB.Parser
                     (BlockSummary (..))

{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

-- | Open the database, creating it from scratch if necessary or reopening an
-- existing one using the given 'ValidationPolicy'.
--
-- See 'ValidationPolicy' for more details on the different validation
-- policies.
--
-- An 'ChunkFileParser' must be passed in order to reconstruct indices from
-- chunk files. The 'Word' that the 'ChunkFileParser' must return for each
-- 'SlotNo' is the size (in bytes) occupied by the (non-empty) block
-- corresponding to the 'SlotNo'. The only reason we need to know the size of
-- the blocks is to compute the offset of the end of the last block, so we can
-- know where to truncate the file to in case of invalid trailing data. For
-- all other blocks, we can derive this from the offset of the next block, but
-- there is of course no block after the last one.
--
-- __Note__: To be used in conjunction with 'withDB'.
withDB
  :: forall m h hash e a.
     (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash, Eq h)
  => ImmutableDbArgs m h hash e
  -> (ImmutableDB hash m -> m a)
  -> m a
withDB args = bracket open closeDB
  where
    open = fst <$> openDBInternal args

{------------------------------------------------------------------------------
  ImmutableDB arguments
------------------------------------------------------------------------------}

data ImmutableDbArgs m h hash e = ImmutableDbArgs
    { registry    :: ResourceRegistry m
    , hasFS       :: HasFS m h
    , chunkInfo   :: ChunkInfo
    , hashInfo    :: HashInfo hash
    , tracer      :: Tracer m (TraceEvent e hash)
    , cacheConfig :: Index.CacheConfig
    , valPol      :: ValidationPolicy
    , parser      :: ChunkFileParser e m (BlockSummary hash) hash
    }

{------------------------------------------------------------------------------
  Exposed internals and/or extra functionality for testing purposes
------------------------------------------------------------------------------}

data Internal hash m = Internal
  { -- | Delete everything in the database after the specified tip.
    --
    -- PRECONDITION: The tip must correspond to an existing block (unless it
    -- is 'TipGen').
    --
    -- The correctness of open iterators is not guaranteed, they should be
    -- closed before calling this operation.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    deleteAfter_
      :: HasCallStack => ImmTipWithInfo hash -> m ()
  }

-- | Wrapper around 'deleteAfter_' to ensure 'HasCallStack' constraint
--
-- See documentation of 'deleteAfter_'.
deleteAfter :: HasCallStack => Internal hash m -> ImmTipWithInfo hash -> m ()
deleteAfter = deleteAfter_

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

mkDBRecord :: (IOLike m, Eq hash, NoUnexpectedThunks hash)
           => ImmutableDBEnv m hash -> ImmutableDB hash m
mkDBRecord dbEnv = ImmutableDB
    { closeDB_                = closeDBImpl                dbEnv
    , getTip_                 = getTipImpl                 dbEnv
    , getBlockComponent_      = getBlockComponentImpl      dbEnv
    , getEBBComponent_        = getEBBComponentImpl        dbEnv
    , getBlockOrEBBComponent_ = getBlockOrEBBComponentImpl dbEnv
    , appendBlock_            = appendBlockImpl            dbEnv
    , appendEBB_              = appendEBBImpl              dbEnv
    , stream_                 = streamImpl                 dbEnv
    }

-- | For testing purposes:
--
-- * Exposes internal via 'Internal'
-- * Non-bracketed, as @quickcheck-state-machine@ doesn't support that.
openDBInternal
  :: forall m h hash e.
     (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash, Eq h)
  => ImmutableDbArgs m h hash e
  -> m (ImmutableDB hash m, Internal hash m)
openDBInternal ImmutableDbArgs {..} = runWithTempRegistry $ do
    let validateEnv = ValidateEnv
          { hasFS
          , chunkInfo
          , hashInfo
          , parser
          , tracer
          , cacheConfig
          }
    ost <- validateAndReopen validateEnv registry valPol

    stVar <- lift $ newMVar (DbOpen ost)

    let dbEnv = ImmutableDBEnv
          { hasFS            = hasFS
          , varInternalState = stVar
          , chunkFileParser  = parser
          , chunkInfo        = chunkInfo
          , hashInfo         = hashInfo
          , tracer           = tracer
          , registry         = registry
          , cacheConfig      = cacheConfig
          }
        db = mkDBRecord dbEnv
        internal = Internal
          { deleteAfter_ = deleteAfterImpl dbEnv
          }
    -- TODO move 'withTempResourceRegistry' outside of this function?
    --
    -- Note that we can still leak resources if the caller of
    -- 'openDBInternal' doesn't bracket his call with 'closeDB' or doesn't
    -- use a 'ResourceRegistry'.
    return ((db, internal), ost)

closeDBImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> m ()
closeDBImpl ImmutableDBEnv { hasFS, tracer, varInternalState } = do
    internalState <- takeMVar varInternalState
    case internalState of
      -- Already closed
      DbClosed -> do
        putMVar varInternalState internalState
        traceWith tracer $ DBAlreadyClosed
      DbOpen openState@OpenState {..} -> do
        -- Close the database before doing the file-system operations so that
        -- in case these fail, we don't leave the database open.
        putMVar varInternalState DbClosed
        cleanUp hasFS openState
        traceWith tracer DBClosed

deleteAfterImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> ImmTipWithInfo hash
  -> m ()
deleteAfterImpl dbEnv@ImmutableDBEnv { tracer } newTip =
  -- We're not using 'Index' in this function but truncating the index files
  -- directly.
  modifyOpenState dbEnv $ \hasFS -> do
    st@OpenState { currentIndex, currentTip } <- get
    let currentTipChunkSlot = (chunkSlotFor . forgetTipInfo) <$> currentTip
        newTipChunkSlot     = (chunkSlotFor . forgetTipInfo) <$> newTip

    when (newTipChunkSlot < currentTipChunkSlot) $ do
      ost <- lift $ do
        lift $ traceWith tracer $ DeletingAfter newTip
        -- Release the open handles, as we might have to remove files that are
        -- currently opened.
        lift $ cleanUp hasFS st
        newTipWithHash <- lift $ truncateTo hasFS st newTipChunkSlot
        let (newChunk, allowExisting) = case newTipChunkSlot of
              Origin                 -> (firstChunkNo, MustBeNew)
              At (ChunkSlot chunk _) -> (chunk, AllowExisting)
        -- Reset the index, as it can contain stale information. Also restarts
        -- the background thread expiring unused past chunks.
        lift $ Index.restart currentIndex newChunk
        mkOpenState hasFS currentIndex newChunk newTipWithHash
          allowExisting
      put ost
  where
    ImmutableDBEnv { chunkInfo, hashInfo } = dbEnv

    chunkSlotFor :: BlockOrEBB -> ChunkSlot
    chunkSlotFor = chunkSlotForBlockOrEBB chunkInfo

    truncateTo
      :: HasFS m h
      -> OpenState m hash h
      -> WithOrigin ChunkSlot
      -> m (ImmTipWithInfo hash)
    truncateTo hasFS OpenState {} = \case
      Origin                       ->
        removeFilesStartingFrom hasFS firstChunkNo $> Origin
      At (ChunkSlot chunk relSlot) -> do
        removeFilesStartingFrom hasFS (nextChunkNo chunk)

        -- Retrieve the needed info from the primary index file and then
        -- truncate it.
        primaryIndex <- Primary.load hasFS chunk
        Primary.truncateToSlotFS hasFS chunk relSlot
        let lastSecondaryOffset = Primary.offsetOfSlot primaryIndex relSlot
            isEBB               = relativeSlotIsEBB relSlot

        -- Retrieve the needed info from the secondary index file and then
        -- truncate it.
        (entry, blockSize) <- Secondary.readEntry hasFS hashInfo
          chunk isEBB lastSecondaryOffset
        Secondary.truncateToEntry hasFS hashInfo chunk lastSecondaryOffset

        -- Truncate the chunk file.
        case blockSize of
          -- The block is the last block in the chunk file, so no need to
          -- truncate
          Secondary.LastEntry      -> return ()
          Secondary.BlockSize size ->
              withFile hasFS chunkFile (AppendMode AllowExisting) $ \eHnd ->
                hTruncate hasFS eHnd offset
            where
              chunkFile = fsPathChunkFile chunk
              offset    = unBlockOffset (Secondary.blockOffset entry)
                        + fromIntegral size

        return newTip

getTipImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> m (ImmTipWithInfo hash)
getTipImpl dbEnv = do
    SomePair _hasFS OpenState { currentTip } <- getOpenState dbEnv
    return currentTip

getBlockComponentImpl
  :: forall m hash b. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> m (Maybe b)
getBlockComponentImpl dbEnv@ImmutableDBEnv { chunkInfo } blockComponent slot =
    withOpenState dbEnv $ \hasFS OpenState{..} -> do
      let inTheFuture = case forgetTipInfo <$> currentTip of
            Origin                -> True
            At (Block lastSlot')  -> slot > lastSlot'
            -- The slot (that's pointing to a regular block) corresponding to this
            -- EBB will be empty, as the EBB is the last thing in the database. So
            -- if @slot@ is equal to this slot, it is also referring to the future.
            At (EBB lastEBBEpoch) -> slot >= slotNoOfEBB chunkInfo lastEBBEpoch

      when inTheFuture $
        throwUserError $
          ReadFutureSlotError slot (forgetTipInfo <$> currentTip)

      let curChunkInfo = CurrentChunkInfo currentChunk currentChunkOffset
          chunkSlot    = chunkSlotForRegularBlock chunkInfo slot
      getChunkSlot hasFS chunkInfo currentIndex curChunkInfo
        blockComponent chunkSlot

getEBBComponentImpl
  :: forall m hash b. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> BlockComponent (ImmutableDB hash m) b
  -> EpochNo
  -> m (Maybe b)
getEBBComponentImpl dbEnv@ImmutableDBEnv { chunkInfo } blockComponent epoch =
    withOpenState dbEnv $ \hasFS OpenState{..} -> do
      let chunk       = unsafeEpochNoToChunkNo epoch
          inTheFuture = case forgetTipInfo <$> currentTip of
            Origin       -> True
            At (Block _) -> chunk > currentChunk
            At (EBB _)   -> chunk > currentChunk

      when inTheFuture $
        throwUserError $ ReadFutureEBBError epoch currentChunk

      let curChunkInfo = CurrentChunkInfo currentChunk currentChunkOffset
      getChunkSlot hasFS chunkInfo currentIndex curChunkInfo
        blockComponent (chunkSlotForBoundaryBlock chunkInfo epoch)

extractBlockComponent
  :: forall m h hash b. (HasCallStack, IOLike m)
  => HasFS m h
  -> ChunkInfo
  -> ChunkNo
  -- ^ Most recent chunk file (used to determine size of final block)
  -> CurrentChunkInfo
  -> (Secondary.Entry hash, BlockSize)
  -> BlockComponent (ImmutableDB hash m) b
  -> m b
extractBlockComponent hasFS chunkInfo chunk curChunkInfo (entry, blockSize) = \case
    GetHash  -> return headerHash
    GetSlot  -> return $ slotNoOfBlockOrEBB chunkInfo blockOrEBB
    GetIsEBB -> return $ case blockOrEBB of
      Block _ -> IsNotEBB
      EBB   _ -> IsEBB

    GetBlockSize -> case blockSize of
      Secondary.BlockSize size
        -> return size
      -- See the 'GetBlock' case for more info about 'Secondary.LastEntry'.
      Secondary.LastEntry
        | chunk == curChunk
        -> return $ fromIntegral $ curChunkOffset - blockOffset
        | otherwise
        -> do
          -- With cached indices, we'll never hit this case.
          offsetAfterLastBlock <- withFile hasFS chunkFile ReadMode $ \eHnd ->
            hGetSize hasFS eHnd
          return $ fromIntegral $ offsetAfterLastBlock - unBlockOffset blockOffset

    GetHeaderSize -> return $ fromIntegral $ unHeaderSize headerSize

    GetPure a -> return a

    GetApply f bc ->
      extractBlockComponent hasFS chunkInfo chunk curChunkInfo
        (entry, blockSize) f <*>
      extractBlockComponent hasFS chunkInfo chunk curChunkInfo
        (entry, blockSize) bc

    -- In case the requested chunk is the current chunk, we will be reading
    -- from the chunk file while we're also writing to it. Are we guaranteed
    -- to read what have written? Duncan says: this is guaranteed at the OS
    -- level (POSIX), but not for Haskell handles, which might perform other
    -- buffering. However, the 'HasFS' implementation we're using uses POSIX
    -- file handles ("Ouroboros.Consensus.Storage.IO") so we're safe (other
    -- implementations of the 'HasFS' API guarantee this too).

    GetRawBlock -> do
      -- Get the whole block
      let offset = AbsOffset $ unBlockOffset blockOffset
      (bl, checksum') <- withFile hasFS chunkFile ReadMode $ \eHnd ->
        case blockSize of
          -- It is the last entry in the file, so we don't know the size
          -- of the block.
          Secondary.LastEntry
            | chunk == curChunk
              -- Even though it was the last block in the secondary
              -- index file (and thus in the chunk file) when we read
              -- the secondary index file, it is possible that more
              -- blocks have been appended in the meantime. For this
              -- reason, we cannot simply read the until the end of the
              -- chunk file, because we would read the newly appended
              -- blocks too.
              --
              -- Instead, we derive the size of the block from
              -- @curChunkOffset@, which corresponds to the qoffset at
              -- the end of that block /at the time we read the state/.
              -- Note that we don't allow reading a block newer than the
              -- tip, which we obtained from the /same state/.
            -> let size = curChunkOffset - blockOffset in
               hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
            | otherwise
              -- If it is in an chunk in the past, it is immutable,
              -- so no blocks can have been appended since we retrieved
              -- the entry. We can simply read all remaining bytes, as
              -- it is the last block in the file.
            -> hGetAllAtCRC     hasFS eHnd                     offset
          Secondary.BlockSize size
            -> hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
      checkChecksum chunkFile blockOrEBB checksum checksum'
      return bl

    GetRawHeader ->
        -- Get just the header
        withFile hasFS chunkFile ReadMode $ \eHnd ->
          -- We cannot check the checksum in this case, as we're not reading
          -- the whole block
          hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ unHeaderSize headerSize
        offset = AbsOffset $
          unBlockOffset blockOffset +
          fromIntegral (unHeaderOffset headerOffset)
    GetBlock  -> return ()
    GetHeader -> return ()
  where
    Secondary.Entry
      { blockOffset, headerOffset, headerSize, headerHash, checksum
      , blockOrEBB
      } = entry
    CurrentChunkInfo curChunk curChunkOffset = curChunkInfo
    chunkFile = fsPathChunkFile chunk

getBlockOrEBBComponentImpl
  :: forall m hash b. (HasCallStack, IOLike m, Eq hash)
  => ImmutableDBEnv m hash
  -> BlockComponent (ImmutableDB hash m) b
  -> SlotNo
  -> hash
  -> m (Maybe b)
getBlockOrEBBComponentImpl dbEnv blockComponent slot hash =
    withOpenState dbEnv $ \hasFS OpenState{..} -> do

      let inTheFuture = case forgetTipInfo <$> currentTip of
            Origin -> True
            At b   -> slot > slotNoOfBlockOrEBB chunkInfo b

      when inTheFuture $
        throwUserError $ ReadFutureSlotError slot (forgetTipInfo <$> currentTip)

      let curChunkInfo = CurrentChunkInfo currentChunk currentChunkOffset

      errOrRes <- runExceptT $
        getSlotInfo chunkInfo currentIndex (slot, hash)
      case errOrRes of
        Left _ ->
          return Nothing
        Right (ChunkSlot chunk _, (entry, blockSize), _secondaryOffset) ->
          Just <$>
            extractBlockComponent hasFS chunkInfo chunk curChunkInfo
              (entry, blockSize) blockComponent
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

-- | Get the block component corresponding to the given 'ChunkSlot'.
--
-- Preconditions: the given 'ChunkSlot' is in the past.
getChunkSlot
  :: forall m h hash b. (HasCallStack, IOLike m)
  => HasFS m h
  -> ChunkInfo
  -> Index m hash h
  -> CurrentChunkInfo
  -> BlockComponent (ImmutableDB hash m) b
  -> ChunkSlot
  -> m (Maybe b)
getChunkSlot hasFS chunkInfo index curChunkInfo blockComponent chunkSlot =
    -- Check the primary index first
    Index.readOffset index chunk relativeSlot >>= \case
      -- Empty slot
      Nothing              -> return Nothing
      -- Filled slot; read the corresponding entry from the sparse secondary
      -- index
      Just secondaryOffset -> do
        -- TODO only read the hash in case of 'GetHash'?
        (entry, blockSize) <- Index.readEntry index chunk isEBB secondaryOffset
        Just <$>
          extractBlockComponent hasFS chunkInfo chunk curChunkInfo
            (entry, blockSize) blockComponent
  where
    ChunkSlot chunk relativeSlot = chunkSlot
    isEBB = relativeSlotIsEBB relativeSlot

appendBlockImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> SlotNo
  -> BlockNo
  -> hash
  -> BinaryInfo Builder
  -> m ()
appendBlockImpl dbEnv slot blockNumber headerHash binaryInfo =
    modifyOpenState dbEnv $ \hasFS -> do
      OpenState { currentTip, currentIndex } <- get

      let chunkSlot@(ChunkSlot chunk _) =
            chunkSlotForRegularBlock chunkInfo slot

      -- Check that we're not appending to the past
      let inThePast = case forgetTipInfo <$> currentTip of
            At (Block lastSlot)   -> slot  <= lastSlot
            At (EBB lastEBBEpoch) -> chunk <  unsafeEpochNoToChunkNo lastEBBEpoch
            Origin                -> False

      when inThePast $ lift $
        throwUserError $
          AppendToSlotInThePastError slot (forgetTipInfo <$> currentTip)

      appendChunkSlot hasFS chunkInfo currentIndex chunkSlot
        blockNumber (Block slot) headerHash binaryInfo
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

appendEBBImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> EpochNo
  -> BlockNo
  -> hash
  -> BinaryInfo Builder
  -> m ()
appendEBBImpl dbEnv epoch blockNumber headerHash binaryInfo =
    modifyOpenState dbEnv $ \hasFS -> do
      OpenState { currentChunk, currentTip, currentIndex } <- get

      -- Check that we're not appending to the past
      let chunk     = unsafeEpochNoToChunkNo epoch
          inThePast = case forgetTipInfo <$> currentTip of
            -- There is already a block in this chunk, so the EBB can no
            -- longer be appended in this chunk
            At (Block _) -> chunk <= currentChunk
            -- There is already an EBB in this chunk
            At (EBB _)   -> chunk <= currentChunk
            Origin       -> False

      when inThePast $ lift $ throwUserError $
        AppendToEBBInThePastError epoch currentChunk

      appendChunkSlot hasFS chunkInfo currentIndex
        (chunkSlotForBoundaryBlock chunkInfo epoch) blockNumber (EBB epoch)
        headerHash binaryInfo
  where
    ImmutableDBEnv { chunkInfo } = dbEnv

appendChunkSlot
  :: forall m h hash. (HasCallStack, IOLike m, Eq h)
  => HasFS m h
  -> ChunkInfo
  -> Index m hash h
  -> ChunkSlot  -- ^ The 'ChunkSlot' of the new block or EBB
  -> BlockNo    -- ^ The block number of the new block
  -> BlockOrEBB -- ^ Corresponds to the new block, will be installed as the
                -- new tip
  -> hash
  -> BinaryInfo Builder
  -> ModifyOpenState m hash h ()
appendChunkSlot hasFS chunkInfo index chunkSlot blockNumber blockOrEBB headerHash
                BinaryInfo { binaryBlob, headerOffset, headerSize } = do
    OpenState { currentChunk = initialChunk } <- get

    -- If the slot is in an chunk > the current one, we have to finalise the
    -- current one and start a new chunk file, possibly skipping some
    -- chunks.
    when (chunk > initialChunk) $ do
      let newChunksToStart :: Int
          newChunksToStart = fromIntegral $ countChunks chunk initialChunk
      replicateM_ newChunksToStart $ startNewChunk hasFS index chunkInfo

    -- We may have updated the state with 'startNewChunk', so get the
    -- (possibly) updated state, but first remember the current chunk
    OpenState {..} <- get

    -- Compute the next empty slot @m@, if we need to write to slot @n@, we
    -- will need to backfill @n - m@ slots.
    let nextFreeRelSlot :: RelativeSlot
        nextFreeRelSlot =
          if chunk > initialChunk
            -- If we had to start a new chunk, we start with slot 0. Note that
            -- in this case the currentTip will refer to something in an chunk
            -- before currentChunk.
            then firstBlockOrEBB chunkInfo chunk
            else case forgetTipInfo <$> currentTip of
              Origin -> firstBlockOrEBB chunkInfo firstChunkNo
              -- Invariant: the currently open chunk is never full
              At b   -> unsafeNextRelativeSlot . chunkRelative $
                          chunkSlotForBlockOrEBB chunkInfo b

    -- Append to the end of the chunk file.
    (blockSize, entrySize) <- lift $ lift $ do

        -- Write to the chunk file
        (blockSize, crc) <- hPutCRC hasFS currentChunkHandle binaryBlob

        -- Write to the secondary index file
        let entry = Secondary.Entry
              { blockOffset  = currentChunkOffset
              , headerOffset = HeaderOffset headerOffset
              , headerSize   = HeaderSize headerSize
              , checksum     = crc
              , headerHash   = headerHash
              , blockOrEBB   = blockOrEBB
              }
        entrySize <- fromIntegral <$> Index.appendEntry index chunk
          currentSecondaryHandle (WithBlockSize (fromIntegral blockSize) entry)

        -- Write to the primary index file
        let backfillOffsets = Primary.backfill
              relSlot nextFreeRelSlot currentSecondaryOffset
            offsets = backfillOffsets <> [currentSecondaryOffset + entrySize]
        Index.appendOffsets index currentPrimaryHandle offsets

        return (blockSize, entrySize)

    modify $ \st -> st
      { currentChunkOffset     = currentChunkOffset + fromIntegral blockSize
      , currentSecondaryOffset = currentSecondaryOffset + entrySize
      , currentTip             = At (TipInfo headerHash blockOrEBB blockNumber)
      }
  where
    ChunkSlot chunk relSlot = chunkSlot

startNewChunk
  :: forall m h hash. (HasCallStack, IOLike m, Eq h)
  => HasFS m h
  -> Index m hash h
  -> ChunkInfo
  -> ModifyOpenState m hash h ()
startNewChunk hasFS index chunkInfo = do
    st@OpenState {..} <- get

    -- We have to take care when starting multiple new chunks in a row. In the
    -- first call the tip will be in the current chunk, but in subsequent
    -- calls, the tip will still be in an chunk in the past, not the
    -- 'currentChunk'. In that case, we can't use the relative slot of the
    -- tip, since it will point to a relative slot in a past chunk. So when
    -- the current (empty) chunk is not the chunk containing the tip, we use
    -- relative slot 0 to calculate how much to pad.
    let nextFreeRelSlot :: NextRelativeSlot
        nextFreeRelSlot = case forgetTipInfo <$> currentTip of
          Origin ->
            NextRelativeSlot $ firstBlockOrEBB chunkInfo firstChunkNo
          At b ->
            if chunk == currentChunk
              then nextRelativeSlot relSlot
              else NextRelativeSlot $ firstBlockOrEBB chunkInfo currentChunk
            where
              ChunkSlot chunk relSlot = chunkSlotForBlockOrEBB chunkInfo b

    let backfillOffsets = Primary.backfillChunk
                            chunkInfo
                            currentChunk
                            nextFreeRelSlot
                            currentSecondaryOffset

    lift $ lift $
      Index.appendOffsets index currentPrimaryHandle backfillOffsets
      `finally` closeOpenHandles hasFS st

    st' <- lift $
      mkOpenState hasFS index (nextChunkNo currentChunk) currentTip MustBeNew

    put st'
