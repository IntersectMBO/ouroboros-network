{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- | Immutable on-disk database of binary blobs
--
-- = Internal format
--
-- The API of the ImmutableDB uses 'SlotNo' to indicate a location in the
-- chain\/immutable database. To distinguish EBBs from regular blocks, the hash
-- is used (together they form a 'RealPoint'). The contents of the database are
-- not stored in one big file that is appended to in eternity, but a separate
-- file is created for each 'ChunkNo'.
--
-- Within each 'ChunkNo', the entries are numbered by 'RelativeSlot's. Each
-- 'SlotNo' can be converted to a combination of an 'ChunkNo' and a
-- 'RelativeSlot' (= 'ChunkSlot') and vice versa. This conversion depends on the
-- size of the chunks: 'ChunkSize'. This size may not be the same for each
-- chunk. When opening the database, the user must give a 'ChunkInfo' that will
-- be used to find out the size of each chunk.
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
-- Whenever an 'UnexpectedFailure' is thrown during an operation, e.g.,
-- 'appendBlock', the database will be automatically closed because we can not
-- guarantee a consistent state in the face of file system errors.
--
-- = Opening the database
--
-- The database can be closed and opened again. In case the database was closed
-- because of an unexpected error. When the database is opened again, invalid
-- data will be truncated from the database until a valid prefix is recovered.
--
-- = Concurrency
--
-- The same database should not be opened multiple times concurrently.
-- This is ensured by the file lock of the ChainDB.
--
-- The database can have multiple readers, but should only have one writer.
--
--
-- = Layout on disk
--
-- The database is structured on disk as follows:
--
-- > /
-- >   00000.chunk
-- >   00000.primary
-- >   00000.secondary
-- >   ..
-- >   00008.chunk
-- >   00008.primary
-- >   00008.secondary
--
-- For each chunk, there are three files on disk:
--
--   * A \"chunk file\" that stores the actual blocks. But nothing more, so
--     nothing is stored for empty slots.
--
--   * A \"secondary index file\" that stores information about each block: its
--     hash, the slot number or epoch number in case of an EBB, a checksum of
--     the block, the offset of the block in the chunk file, and more. This
--     index is sparse to save space.
--
--   * A \"primary index file\" that maps slots to offsets in the secondary
--     index file.
module Ouroboros.Consensus.Storage.ImmutableDB.Impl (
    -- * Opening the databse
    ImmutableDbArgs (..)
  , ImmutableDbSerialiseConstraints
  , defaultArgs
  , openDB
    -- * Re-exported
  , ChunkFileError (..)
  , Index.CacheConfig (..)
  , TraceEvent (..)
  , ValidationPolicy (..)
    -- * Internals for testing purposes
  , Internal (..)
  , deleteAfter
  , openDBInternal
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (replicateM_, unless, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (get, lift, modify, put)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block hiding (headerHash)
import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     runWithTempRegistry)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types hiding (allowExisting)
import           Ouroboros.Consensus.Storage.FS.CRC
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..), HeaderOffset (..), HeaderSize (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Parser
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Validation

{------------------------------------------------------------------------------
  Opening the database
------------------------------------------------------------------------------}

data ImmutableDbArgs f m blk = ImmutableDbArgs {
      immCacheConfig      :: Index.CacheConfig
    , immCheckIntegrity   :: HKD f (blk -> Bool)
    , immChunkInfo        :: HKD f ChunkInfo
    , immCodecConfig      :: HKD f (CodecConfig blk)
    , immHasFS            :: SomeHasFS m
    , immRegistry         :: HKD f (ResourceRegistry m)
    , immTracer           :: Tracer m (TraceEvent blk)
    , immValidationPolicy :: ValidationPolicy
    }

-- | Default arguments
defaultArgs :: Applicative m => SomeHasFS m -> ImmutableDbArgs Defaults m blk
defaultArgs immHasFS = ImmutableDbArgs {
      immCacheConfig      = cacheConfig
    , immCheckIntegrity   = NoDefault
    , immChunkInfo        = NoDefault
    , immCodecConfig      = NoDefault
    , immHasFS
    , immRegistry         = NoDefault
    , immTracer           = nullTracer
    , immValidationPolicy = ValidateMostRecentChunk
    }
  where
    -- Cache 250 past chunks by default. This will take roughly 250 MB of RAM.
    -- At the time of writing (1/2020), there are 166 epochs, and we store one
    -- epoch per chunk, so even one year from now, we will be able to cache all
    -- chunks' indices in the chain.
    --
    -- If this number were too low, i.e., less than the number of chunks that
    -- that clients are requesting blocks from, we would constantly evict and
    -- reparse indices, causing a much higher CPU load.
    cacheConfig = Index.CacheConfig {
          pastChunksToCache = 250
        , expireUnusedAfter = 5 * 60 -- Expire after 1 minute
        }

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the ImmutableDB.
type ImmutableDbSerialiseConstraints blk =
  ( EncodeDisk blk blk
  , DecodeDisk blk (Lazy.ByteString -> blk)
  , DecodeDiskDep (NestedCtxt Header) blk
  , ReconstructNestedCtxt Header blk
  , HasBinaryBlockInfo blk
  )

{------------------------------------------------------------------------------
  Exposed internals and/or extra functionality for testing purposes
------------------------------------------------------------------------------}

data Internal m blk = Internal {
    -- | Delete everything in the database after the specified tip.
    --
    -- PRECONDITION: The tip must correspond to an existing block or genesis.
    --
    -- The correctness of open iterators is not guaranteed, they should be
    -- closed before calling this operation.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    deleteAfter_ :: HasCallStack => WithOrigin (Tip blk) -> m ()
  }

-- | Wrapper around 'deleteAfter_' to ensure 'HasCallStack' constraint
--
-- See documentation of 'deleteAfter_'.
deleteAfter :: HasCallStack => Internal m blk -> WithOrigin (Tip blk) -> m ()
deleteAfter = deleteAfter_

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

openDB ::
     forall m blk.
     ( IOLike m
     , GetPrevHash blk
     , ConvertRawHash blk
     , ImmutableDbSerialiseConstraints blk
     , HasCallStack
     )
  => ImmutableDbArgs Identity m blk
  -> m (ImmutableDB m blk)
openDB args = fst <$> openDBInternal args

-- | For testing purposes: exposes internals via 'Internal'
--
--
openDBInternal ::
     forall m blk.
     ( IOLike m
     , GetPrevHash blk
     , ConvertRawHash blk
     , ImmutableDbSerialiseConstraints blk
     , HasCallStack
     )
  => ImmutableDbArgs Identity m blk
  -> m (ImmutableDB m blk, Internal m blk)
openDBInternal ImmutableDbArgs { immHasFS = SomeHasFS hasFS, .. } = runWithTempRegistry $ do
    lift $ createDirectoryIfMissing hasFS True (mkFsPath [])
    let validateEnv = ValidateEnv {
            hasFS          = hasFS
          , chunkInfo      = immChunkInfo
          , tracer         = immTracer
          , cacheConfig    = immCacheConfig
          , codecConfig    = immCodecConfig
          , checkIntegrity = immCheckIntegrity
          }
    ost <- validateAndReopen validateEnv immRegistry immValidationPolicy

    stVar <- lift $ newMVar (DbOpen ost)

    let dbEnv = ImmutableDBEnv {
            hasFS            = hasFS
          , varInternalState = stVar
          , checkIntegrity   = immCheckIntegrity
          , chunkInfo        = immChunkInfo
          , tracer           = immTracer
          , registry         = immRegistry
          , cacheConfig      = immCacheConfig
          , codecConfig      = immCodecConfig
          }
        db = ImmutableDB {
            closeDB_           = closeDBImpl           dbEnv
          , getTip_            = getTipImpl            dbEnv
          , getBlockComponent_ = getBlockComponentImpl dbEnv
          , appendBlock_       = appendBlockImpl       dbEnv
          , stream_            = streamImpl            dbEnv
          }
        internal = Internal {
            deleteAfter_ = deleteAfterImpl dbEnv
          }
    -- TODO move 'withTempResourceRegistry' outside of this function?
    --
    -- Note that we can still leak resources if the caller of
    -- 'openDBInternal' doesn't bracket his call with 'closeDB' or doesn't
    -- use a 'ResourceRegistry'.
    return ((db, internal), ost)

closeDBImpl ::
     forall m blk. (HasCallStack, IOLike m)
  => ImmutableDBEnv m blk
  -> m ()
closeDBImpl ImmutableDBEnv { hasFS, tracer, varInternalState } = do
    internalState <- takeMVar varInternalState
    case internalState of
      -- Already closed
      DbClosed -> do
        putMVar varInternalState internalState
        traceWith tracer $ DBAlreadyClosed
      DbOpen openState -> do
        -- Close the database before doing the file-system operations so that
        -- in case these fail, we don't leave the database open.
        putMVar varInternalState DbClosed
        cleanUp hasFS openState
        traceWith tracer DBClosed

deleteAfterImpl ::
     forall m blk. (HasCallStack, ConvertRawHash blk, IOLike m, HasHeader blk)
  => ImmutableDBEnv m blk
  -> WithOrigin (Tip blk)
  -> m ()
deleteAfterImpl dbEnv@ImmutableDBEnv { tracer, chunkInfo } newTip =
  -- We're not using 'Index' in this function but truncating the index files
  -- directly.
  modifyOpenState dbEnv $ \hasFS -> do
    st@OpenState { currentIndex, currentTip } <- get

    when ((CompareTip <$> newTip) < (CompareTip <$> currentTip)) $ do
      lift $ lift $ do
        traceWith tracer $ DeletingAfter newTip
        -- Release the open handles, as we might have to remove files that are
        -- currently opened.
        cleanUp hasFS st
        truncateTo hasFS st newTipChunkSlot
        -- Reset the index, as it can contain stale information. Also restarts
        -- the background thread expiring unused past chunks.
        Index.restart currentIndex newChunk

      ost <- lift $ mkOpenState hasFS currentIndex newChunk newTip allowExisting
      put ost
  where
    newTipChunkSlot :: WithOrigin ChunkSlot
    newTipChunkSlot = chunkSlotForTip chunkInfo <$> newTip

    newChunk :: ChunkNo
    allowExisting :: AllowExisting
    (newChunk, allowExisting) = case newTipChunkSlot of
      Origin                        -> (firstChunkNo, MustBeNew)
      NotOrigin (ChunkSlot chunk _) -> (chunk, AllowExisting)

    truncateTo ::
         HasFS m h
      -> OpenState m blk h
      -> WithOrigin ChunkSlot
      -> m ()
    truncateTo hasFS OpenState {} = \case
      Origin                       ->
        removeFilesStartingFrom hasFS firstChunkNo
      NotOrigin (ChunkSlot chunk relSlot) -> do
        removeFilesStartingFrom hasFS (nextChunkNo chunk)

        -- Retrieve the needed info from the primary index file and then
        -- truncate it.
        primaryIndex <- Primary.load (Proxy @blk) hasFS chunk
        Primary.truncateToSlotFS hasFS chunk relSlot
        let lastSecondaryOffset = Primary.offsetOfSlot primaryIndex relSlot
            isEBB               = relativeSlotIsEBB relSlot

        -- Retrieve the needed info from the secondary index file and then
        -- truncate it.
        (entry :: Secondary.Entry blk, blockSize) <-
          Secondary.readEntry hasFS chunk isEBB lastSecondaryOffset
        Secondary.truncateToEntry (Proxy @blk) hasFS chunk lastSecondaryOffset

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

getTipImpl ::
     forall m blk. (HasCallStack, IOLike m, HasHeader blk)
  => ImmutableDBEnv m blk
  -> STM m (WithOrigin (Tip blk))
getTipImpl dbEnv = do
    SomePair _hasFS OpenState { currentTip } <- getOpenState dbEnv
    return currentTip

getBlockComponentImpl ::
     forall m blk b.
     ( HasHeader blk
     , ReconstructNestedCtxt Header blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , DecodeDiskDep (NestedCtxt Header) blk
     , IOLike m
     )
  => ImmutableDBEnv m blk
  -> BlockComponent blk b
  -> RealPoint blk
  -> m (Either (MissingBlock blk) b)
getBlockComponentImpl dbEnv blockComponent pt =
    withOpenState dbEnv $ \hasFS OpenState{..} -> runExceptT $ do
      slotInfo <- getSlotInfo chunkInfo currentIndex currentTip pt
      let (ChunkSlot chunk _, (entry, blockSize), _secondaryOffset) = slotInfo
          chunkFile = fsPathChunkFile chunk
          Secondary.Entry { blockOffset } = entry

      -- TODO don't open the 'chunkFile' unless we need to. In practice,
      -- we only use this to read (raw) blocks or (raw) headers, which
      -- does require opening the 'chunkFile'. Related: #2227.
      lift $ withFile hasFS chunkFile ReadMode $ \eHnd -> do

        actualBlockSize <- case blockSize of
          Secondary.BlockSize size
            -> return size
          -- See the 'GetBlock' case for more info about
          -- 'Secondary.LastEntry'.
          Secondary.LastEntry
            | chunk == currentChunk
            -> return $ fromIntegral $ currentChunkOffset - blockOffset
            | otherwise
            -> do
              -- With cached indices, we'll never hit this case.
              offsetAfterLastBlock <- hGetSize hasFS eHnd
              return $ fromIntegral $
                offsetAfterLastBlock - unBlockOffset blockOffset

        extractBlockComponent
          hasFS
          chunkInfo
          chunk
          codecConfig
          checkIntegrity
          eHnd
          (WithBlockSize actualBlockSize entry)
          blockComponent
  where
    ImmutableDBEnv { chunkInfo, codecConfig, checkIntegrity } = dbEnv

appendBlockImpl ::
     forall m blk.
     ( HasHeader blk
     , GetHeader blk
     , EncodeDisk blk blk
     , HasBinaryBlockInfo blk
     , IOLike m
     , HasCallStack
     )
  => ImmutableDBEnv m blk
  -> blk
  -> m ()
appendBlockImpl dbEnv blk =
    modifyOpenState dbEnv $ \hasFS -> do
      OpenState {
          currentTip   = initialTip
        , currentIndex = index
        , currentChunk = initialChunk
        } <- get

      -- Check that we're not appending to the past
      let blockAfterTip =
            NotOrigin (CompareTip blockTip) > (CompareTip <$> initialTip)

      unless blockAfterTip $ lift $
        throwApiMisuse $
          AppendBlockNotNewerThanTipError
            (blockRealPoint blk)
            (tipToPoint initialTip)

      -- If the slot is in a chunk > the current one, we have to finalise the
      -- current one and start a new chunk file, possibly skipping some chunks.
      when (chunk > initialChunk) $ do
        let newChunksToStart :: Int
            newChunksToStart = fromIntegral $ countChunks chunk initialChunk
        replicateM_ newChunksToStart $
          startNewChunk hasFS index chunkInfo initialChunk

      -- We may have updated the state with 'startNewChunk', so get the
      -- (possibly) updated state.
      OpenState {
          currentTip
        , currentChunkHandle
        , currentChunkOffset
        , currentSecondaryHandle
        , currentSecondaryOffset
        , currentPrimaryHandle
        } <- get

      -- Compute the next empty slot @m@, if we need to write to slot @n@, we
      -- will need to backfill @n - m@ slots.
      let nextFreeRelSlot :: RelativeSlot
          nextFreeRelSlot =
            if chunk > initialChunk
              -- If we had to start a new chunk, we start with slot 0. Note that
              -- in this case the 'currentTip' will refer to something in a
              -- chunk before 'currentChunk'.
              then firstBlockOrEBB chunkInfo chunk
              else case currentTip of
                Origin        -> firstBlockOrEBB chunkInfo firstChunkNo
                -- Invariant: the currently open chunk is never full
                NotOrigin tip -> unsafeNextRelativeSlot . chunkRelative $
                                   chunkSlotForTip chunkInfo tip

      -- Append to the end of the chunk file.
      (blockSize, entrySize) <- lift $ lift $ do

          -- Write to the chunk file
          let bytes = CBOR.toLazyByteString $ encodeDisk codecConfig blk
          (blockSize, crc) <- hPutAllCRC hasFS currentChunkHandle bytes

          -- Write to the secondary index file
          let entry = Secondary.Entry {
                  blockOffset  = currentChunkOffset
                , headerOffset = HeaderOffset headerOffset
                , headerSize   = HeaderSize headerSize
                , checksum     = crc
                , headerHash   = tipHash blockTip
                , blockOrEBB   = blockOrEBB
                }
          entrySize <-
            fromIntegral <$>
              Index.appendEntry
                index
                chunk
                currentSecondaryHandle
                (WithBlockSize (fromIntegral blockSize) entry)

          -- Write to the primary index file
          let backfillOffsets =
                Primary.backfill
                  relSlot
                  nextFreeRelSlot
                  currentSecondaryOffset
              offsets = backfillOffsets <> [currentSecondaryOffset + entrySize]
          Index.appendOffsets index currentPrimaryHandle offsets

          return (blockSize, entrySize)

      modify $ \st -> st
        { currentChunkOffset     = currentChunkOffset + fromIntegral blockSize
        , currentSecondaryOffset = currentSecondaryOffset + entrySize
        , currentTip             = NotOrigin blockTip
        }
  where
    ImmutableDBEnv { chunkInfo, codecConfig } = dbEnv

    newBlockIsEBB :: Maybe EpochNo
    newBlockIsEBB = blockIsEBB blk

    blockOrEBB :: BlockOrEBB
    blockOrEBB = case newBlockIsEBB of
        Just epochNo -> EBB epochNo
        Nothing      -> Block (blockSlot blk)

    ChunkSlot chunk relSlot = chunkSlotForBlockOrEBB chunkInfo blockOrEBB

    blockTip :: Tip blk
    blockTip = blockToTip blk

    BinaryBlockInfo {..} = getBinaryBlockInfo blk

startNewChunk ::
     forall m h blk. (HasCallStack, IOLike m, Eq h)
  => HasFS m h
  -> Index m blk h
  -> ChunkInfo
  -> ChunkNo  -- ^ Chunk containing the tip
  -> ModifyOpenState m blk h ()
startNewChunk hasFS index chunkInfo tipChunk = do
    st@OpenState {..} <- get

    -- We have to take care when starting multiple new chunks in a row. In the
    -- first call the tip will be in the current chunk, but in subsequent
    -- calls, the tip will still be in an chunk in the past, not the
    -- 'currentChunk'. In that case, we can't use the relative slot of the
    -- tip, since it will point to a relative slot in a past chunk. So when
    -- the current (empty) chunk is not the chunk containing the tip, we use
    -- relative slot 0 to calculate how much to pad.
    let nextFreeRelSlot :: NextRelativeSlot
        nextFreeRelSlot = case currentTip of
          Origin ->
            NextRelativeSlot $ firstBlockOrEBB chunkInfo firstChunkNo
          NotOrigin tip ->
            if tipChunk == currentChunk then
              let ChunkSlot _ relSlot = chunkSlotForTip chunkInfo tip
              in nextRelativeSlot relSlot
            else
              NextRelativeSlot $ firstBlockOrEBB chunkInfo currentChunk

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
