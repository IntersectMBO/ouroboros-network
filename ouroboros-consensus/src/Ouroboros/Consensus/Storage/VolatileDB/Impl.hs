{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
-- | Volatile on-disk database of binary blobs
--
-- = Logic
--
-- The db is a key-value store of binary blocks and is parameterised by the
-- block type @blk@.
--
-- The database uses in-memory indexes, which are created on each (re)opening.
-- Reopening includes parsing all blocks in the @dbFolder@, so it can be an
-- expensive operation if the database gets big. That's why the intention of
-- this db is to be used for only the tip of the blockchain, when there is
-- still volatility on which blocks are included. The db is agnostic to the
-- format of the blocks, so a decoder must be provided. In addition to
-- 'getBlock' and 'putBlock', the db provides also the ability to
-- garbage-collect old blocks. The actual garbage-collection happens in terms
-- of files and not blocks: a file is deleted/garbage-collected if all blocks
-- in it have a slot number less than the slot number for which garbage
-- collection was triggered. This type of garbage collection makes the
-- deletion of blocks depend on the number of blocks we insert in each file,
-- as well as the order of insertion, so it's not deterministic on blocks
-- themselves.
--
-- = Errors
--
-- When an exception occurs while modifying the db, we close the database as a
-- safety measure, e.g., in case a file could not be written to disk, as we
-- can no longer make sure the in-memory indices match what's stored on the
-- file system. When reopening, we validate the blocks stored in the file
-- system and reconstruct the in-memory indices.
--
-- NOTE: this means that when a thread modifying the db is killed, the db will
-- close. This is an intentional choice to simplify things.
--
-- The in-memory indices can always be reconstructed from the file system.
-- This is important, as we must be resilient against unexpected shutdowns,
-- power losses, etc.
--
-- We achieve this by only performing basic operations on the db:
-- * 'putBlock' only appends a new block on a file. Losing an update means we
--   only lose a block, which can be recovered.
-- * 'garbageCollect' only deletes whole files.
-- * there is no operation that modifies blocks. Thanks to that we need not
--   keep any rollback journals to make sure we are safe in case of unexpected
--   shutdowns.
--
-- We only throw 'VolatileDBError'. File-system errors, are caught, wrapped in
-- a 'VolatileDBError', and rethrown. We must make sure that all calls to
-- 'HasFS' functions are properly wrapped. This wrapping is automatically done
-- when inside the scope of 'modifyOpenState' and 'withOpenState'. Otherwise,
-- use 'wrapFsError'.
--
-- = Concurrency
--
-- The same db should only be opened once. Multiple threads can share the same
-- db as concurency is fully supported.
--
-- = FS Layout:
--
-- The on-disk representation is as follows:
--
-- > dbFolder/
-- >   blocks-0.dat
-- >   blocks-1.dat
-- >   ...
--
-- Files not fitting the naming scheme are ignored. The numbering of these
-- files does not correlate to the blocks stored in them.
--
-- Each file stores a fixed number of blocks, specified by 'maxBlocksPerFile'.
-- If the db finds files with less blocks than this limit, it will start
-- appending to the newest of them if there are no newer full files. Otherwise
-- it will create a new file.
--
-- There is an implicit ordering of block files, which is NOT alpharithmetic
-- For example blocks-20.dat < blocks-100.dat
--
-- = Recovery
--
-- The VolatileDB will always try to recover to a consistent state even if this
-- means deleting all of its contents. In order to achieve this, it truncates
-- the files containing blocks if some blocks fail to parse, are invalid, or are
-- duplicated.
module Ouroboros.Consensus.Storage.VolatileDB.Impl (
      -- * Opening the database
      openDB
    , VolatileDbArgs (..)
    , VolatileDbSerialiseConstraints
    , defaultArgs
      -- * Re-exported
    , BlocksPerFile
    , mkBlocksPerFile
    , BlockValidationPolicy (..)
    , ParseError (..)
    , TraceEvent (..)
    , extractBlockInfo
    ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Tracer (Tracer, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack
import           System.FilePath ((</>))

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAWLock
import           Ouroboros.Consensus.Util.ResourceRegistry (allocateTemp,
                     runWithTempRegistry)

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
                     (HasBinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import           Ouroboros.Consensus.Storage.VolatileDB.API
import           Ouroboros.Consensus.Storage.VolatileDB.Error
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (FileInfo)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo as FileInfo
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.State
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

{------------------------------------------------------------------------------
  Initialisation
------------------------------------------------------------------------------}

data VolatileDbArgs m blk = forall h. Eq h => VolatileDbArgs {
      checkIntegrity   :: blk -> Bool
    , codecConfig      :: CodecConfig blk
    , hasFS            :: HasFS m h
    , maxBlocksPerFile :: BlocksPerFile
    , tracer           :: Tracer m (TraceEvent blk)
    , validationPolicy :: BlockValidationPolicy
    }

-- | Default arguments when using the 'IO' monad
--
-- The following fields must still be defined:
--
-- * 'checkIntegrity'
-- * 'blocksPerFile'
-- * 'codecConfig'
-- * 'validation'
defaultArgs :: FilePath -> VolatileDbArgs IO blk
defaultArgs fp = VolatileDbArgs {
      hasFS            = ioHasFS $ MountPoint (fp </> "volatile")
    , tracer           = nullTracer
      -- Fields without a default
    , checkIntegrity   = error "no default for checkIntegrity"
    , codecConfig      = error "no default for codecConfig"
    , maxBlocksPerFile = error "no default for maxBlocksPerFile"
    , validationPolicy = error "no default for validationPolicy"
    }

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the VolatileDB.
class ( EncodeDisk blk blk
      , DecodeDisk blk (Lazy.ByteString -> blk)
      , DecodeDiskDep (NestedCtxt Header) blk
      , HasNestedContent Header blk
      , HasBinaryBlockInfo blk
      ) => VolatileDbSerialiseConstraints blk

openDB ::
     forall m blk.
     ( HasCallStack
     , IOLike m
     , HasHeader blk
     , GetPrevHash blk
     , VolatileDbSerialiseConstraints blk
     )
  => VolatileDbArgs m blk
  -> m (VolatileDB m blk)
openDB VolatileDbArgs {..} = runWithTempRegistry $ do
    lift $ createDirectoryIfMissing hasFS True (mkFsPath [])
    ost   <- mkOpenState hasFS parser tracer maxBlocksPerFile
    stVar <- lift $ RAWLock.new (DbOpen ost)
    let env = VolatileDBEnv {
            hasFS            = hasFS
          , varInternalState = stVar
          , maxBlocksPerFile = maxBlocksPerFile
          , tracer           = tracer
          , codecConfig      = codecConfig
          }
        volDB = VolatileDB {
            closeDB             = closeDBImpl             env
          , getBlockComponent   = getBlockComponentImpl   env
          , putBlock            = putBlockImpl            env
          , garbageCollect      = garbageCollectImpl      env
          , filterByPredecessor = filterByPredecessorImpl env
          , getBlockInfo        = getBlockInfoImpl        env
          , getMaxSlotNo        = getMaxSlotNoImpl        env
          }
    return (volDB, ost)
  where
    parser :: Parser m blk
    parser = blockFileParser codecConfig hasFS checkIntegrity validationPolicy

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

closeDBImpl :: IOLike m => VolatileDBEnv m blk -> m ()
closeDBImpl VolatileDBEnv { varInternalState, tracer, hasFS } = do
    mbInternalState <-
      RAWLock.withWriteAccess varInternalState $ \st -> return (DbClosed, st)
    case mbInternalState of
      DbClosed -> traceWith tracer DBAlreadyClosed
      DbOpen ost ->
        wrapFsError $ closeOpenHandles hasFS ost

getBlockComponentImpl ::
     forall m blk b.
     ( IOLike m
     , HasHeader blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , HasNestedContent Header blk
     , DecodeDiskDep (NestedCtxt Header) blk
     , HasCallStack
     )
  => VolatileDBEnv m blk
  -> BlockComponent (VolatileDB m blk) b
  -> HeaderHash blk
  -> m (Maybe b)
getBlockComponentImpl env@VolatileDBEnv { codecConfig } blockComponent hash =
    withOpenState env $ \hasFS OpenState { currentRevMap } ->
      case Map.lookup hash currentRevMap of
        Nothing                -> return Nothing
        Just internalBlockInfo -> Just <$>
          getBlockComponent hasFS internalBlockInfo blockComponent
  where
    getBlockComponent ::
         forall b' h. HasFS m h
      -> InternalBlockInfo blk
      -> BlockComponent (VolatileDB m blk) b'
      -> m b'
    getBlockComponent hasFS ibi = \case
        GetHash       -> return hash
        GetSlot       -> return biSlotNo
        GetIsEBB      -> return biIsEBB
        GetBlockSize  -> return $ fromIntegral $ unBlockSize ibiBlockSize
        GetHeaderSize -> return biHeaderSize
        GetPure a     -> return a
        GetApply f bc ->
          getBlockComponent hasFS ibi f <*> getBlockComponent hasFS ibi bc
        GetBlock      ->
          getBlockComponent hasFS ibi GetRawBlock >>= parseBlock
        GetRawBlock   -> withFile hasFS ibiFile ReadMode $ \hndl -> do
          let size   = fromIntegral $ unBlockSize ibiBlockSize
              offset = unBlockOffset ibiBlockOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetHeader     ->
          getBlockComponent hasFS ibi GetRawHeader >>= parseHeader
        GetRawHeader  -> withFile hasFS ibiFile ReadMode $ \hndl -> do
          let size   = fromIntegral biHeaderSize
              offset = unBlockOffset ibiBlockOffset + fromIntegral biHeaderOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetNestedCtxt -> return ibiNestedCtxt
      where
        InternalBlockInfo { ibiBlockInfo = BlockInfo {..}, .. } = ibi

        parseBlock :: Lazy.ByteString -> m blk
        parseBlock bytes = throwParseErrors (BlockPoint biSlotNo hash) bytes $
            CBOR.deserialiseFromBytes (decodeDisk codecConfig) bytes

        parseHeader :: Lazy.ByteString -> m (Header blk)
        parseHeader bytes = throwParseErrors (BlockPoint biSlotNo hash) bytes $
            case ibiNestedCtxt of
              SomeBlock ctxt ->
                CBOR.deserialiseFromBytes
                  ((\f -> nest . DepPair ctxt . f) <$>
                      decodeDiskDep codecConfig ctxt)
                  bytes

    throwParseErrors ::
         forall b'.
         Point blk
      -> Lazy.ByteString
      -> Either CBOR.DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> b')
      -> m b'
    throwParseErrors pt fullBytes = \case
        Right (trailing, f)
          | Lazy.null trailing -> return $ f fullBytes
          | otherwise          -> throwM $ UnexpectedError $ TrailingData pt trailing
        Left err               -> throwM $ UnexpectedError $ ParseFailure pt err

-- | This function follows the approach:
-- (1) hPut bytes to the file
-- (2) if full hClose the write file
-- (3)         hOpen a new write file
-- (4) update the Internal State.
--
-- If there is an error after (1) or after (2) we should make sure that when
-- we reopen a db from scratch, it can successfully recover, even if it does
-- not find an empty file to write and all other files are full.
--
-- We should also make sure that the db can recover if we get an
-- exception/error at any moment and that we are left with an empty Internal
-- State.
--
-- We should be careful about not leaking open fds when we open a new file,
-- since this can affect garbage collection of files.
putBlockImpl ::
     forall m blk.
     ( HasHeader blk
     , GetPrevHash blk
     , EncodeDisk blk blk
     , HasBinaryBlockInfo blk
     , HasNestedContent Header blk
     , IOLike m
     )
  => VolatileDBEnv m blk
  -> blk
  -> m ()
putBlockImpl env@VolatileDBEnv{ maxBlocksPerFile, tracer, codecConfig }
             blk =
    appendOpenState env $ \hasFS -> do
      OpenState { currentRevMap, currentWriteHandle } <- get
      if Map.member biHash currentRevMap then
        lift $ lift $ traceWith tracer $ BlockAlreadyHere biHash
      else do
        let bytes = CBOR.toLazyByteString $ encodeDisk codecConfig blk
        bytesWritten <- lift $ lift $ hPutAll hasFS currentWriteHandle bytes
        fileIsFull <- state $ updateStateAfterWrite bytesWritten
        when fileIsFull $ nextFile hasFS
  where
    blockInfo@BlockInfo { biHash, biSlotNo, biPrevHash } =
        extractBlockInfo codecConfig blk

    updateStateAfterWrite
      :: forall h.
         Word64
      -> OpenState blk h
      -> (Bool, OpenState blk h)  -- ^ True: current file is full
    updateStateAfterWrite bytesWritten st@OpenState{..} =
        (FileInfo.isFull maxBlocksPerFile fileInfo', st')
      where
        fileInfo = fromMaybe
            (error $ "VolatileDB invariant violation:"
                    ++ "Current write file not found in Index.")
            (Index.lookup currentWriteId currentMap)
        fileInfo' = FileInfo.addBlock biSlotNo biHash fileInfo
        currentMap' = Index.insert currentWriteId fileInfo' currentMap
        internalBlockInfo' = InternalBlockInfo {
            ibiFile         = currentWritePath
          , ibiBlockOffset  = BlockOffset currentWriteOffset
          , ibiBlockSize    = BlockSize $ fromIntegral bytesWritten
          , ibiBlockInfo    = blockInfo
          , ibiNestedCtxt   = case unnest (getHeader blk) of
                                DepPair nestedCtxt _ -> SomeBlock nestedCtxt
          }
        currentRevMap' = Map.insert biHash internalBlockInfo' currentRevMap
        st' = st {
            currentWriteOffset = currentWriteOffset + bytesWritten
          , currentMap         = currentMap'
          , currentRevMap      = currentRevMap'
          , currentSuccMap     = insertMapSet biPrevHash biHash currentSuccMap
          , currentMaxSlotNo   = currentMaxSlotNo `max` MaxSlotNo biSlotNo
          }

-- | Garbage collect all files of which the highest slot is less than the
-- given slot.
--
-- We first check whether we actually can garbage collect any file. If we can,
-- we obtain the more expensive write lock and remove the files that can be
-- garbage collected. We update the 'InternalState' for each garbage collected
-- file.
--
-- If an exception is thrown while garbage collecting, we close the database.
-- This means we don't have to worry the file system getting out of sync with
-- the in-memory indices, as the indices are rebuilt when reopening.
--
-- NOTE: the current file is never garbage collected.
garbageCollectImpl ::
     forall m blk. (IOLike m, HasHeader blk)
  => VolatileDBEnv m blk
  -> SlotNo
  -> m ()
garbageCollectImpl env slot = do
    -- Check if we can actually GC something using a cheaper read (allowing
    -- for more concurrency) before obtaining the more expensive exclusive
    -- write lock.
    usefulGC <- atomically $ getterSTM gcPossible env

    when usefulGC $
      writeOpenState env $ \hasFS -> do
        -- This event will be picked up by ghc-events-analyze
        lift $ lift $ traceEventM "START garbage collection"
        -- Note that this is /monotonic/: if 'usefulGC' is @True@, then
        -- 'filesToGC' has to be non-empty.
        --
        -- Only a single thread performs garbage collection, so no files could
        -- have been GC'ed in the meantime. The only thing that could have
        -- happened is that blocks have been appended. If they have been
        -- appended to the current file, nothing changes, as we never GC the
        -- current file anyway. If a new file was opened, either we can now GC
        -- the previous file (increase in the number of files to GC) or not
        -- (same number of files to GC).
        filesToGC <- gets getFilesToGC
        mapM_ (garbageCollectFile hasFS) filesToGC
        -- Recompute the 'MaxSlotNo' based on the files left in the
        -- VolatileDB. This value can never go down, except to 'NoMaxSlotNo'
        -- (when we GC everything), because a GC can only delete blocks < a
        -- slot.
        modify $ \st -> st {
            currentMaxSlotNo = FileInfo.maxSlotNoInFiles
                                 (Index.elems (currentMap st))
          }
        lift $ lift $ traceEventM "STOP garbage collection"
  where
    -- | Return 'True' if a garbage collection would actually garbage collect
    -- at least one file.
    gcPossible :: OpenState blk h -> Bool
    gcPossible = not . null . getFilesToGC

    -- | Return the list of files that can be garbage collected.
    getFilesToGC :: OpenState blk h -> [(FileId, FileInfo (HeaderHash blk))]
    getFilesToGC st = filter canGC . Index.toAscList . currentMap $ st
      where
        -- We don't GC the current file. This is unlikely to happen in
        -- practice anyway, and it makes things simpler.
        canGC (fileId, fileInfo) =
          FileInfo.canGC fileInfo slot && fileId /= currentWriteId st

-- | Garbage collect the given file /unconditionally/, updating the
-- 'OpenState'.
--
-- Important to note here is that, every call should leave the file system in
-- a consistent state, without depending on other calls. We achieve this by
-- only needed a single system call: 'removeFile'.
--
-- NOTE: the updated 'OpenState' is inconsistent in the follow respect:
-- the cached 'currentMaxSlotNo' hasn't been updated yet.
--
-- This may throw an FsError.
garbageCollectFile ::
     forall m h blk. (MonadThrow m, HasHeader blk)
  => HasFS m h
  -> (FileId, FileInfo (HeaderHash blk))
  -> ModifyOpenState m blk h ()
garbageCollectFile hasFS (fileId, fileInfo) = do

    lift $ lift $ removeFile hasFS $ filePath fileId

    st@OpenState { currentMap, currentRevMap, currentSuccMap } <- get

    let hashes          = FileInfo.hashes fileInfo
        currentRevMap'  = Map.withoutKeys currentRevMap hashes
        deletedPairs    =
          mapMaybe
            (\h -> (, h) . biPrevHash . ibiBlockInfo <$> Map.lookup h currentRevMap)
            (Set.toList hashes)
        currentSuccMap' =
          foldl' (flip (uncurry deleteMapSet)) currentSuccMap deletedPairs

    put st {
        currentMap     = Index.delete fileId currentMap
      , currentRevMap  = currentRevMap'
      , currentSuccMap = currentSuccMap'
      }

filterByPredecessorImpl ::
     forall m blk. (IOLike m, HasHeader blk)
  => VolatileDBEnv m blk
  -> STM m (ChainHash blk -> Set (HeaderHash blk))
filterByPredecessorImpl = getterSTM $ \st hash ->
    fromMaybe Set.empty (Map.lookup hash (currentSuccMap st))

getBlockInfoImpl ::
     forall m blk. (IOLike m, HasHeader blk)
  => VolatileDBEnv m blk
  -> STM m (HeaderHash blk -> Maybe (BlockInfo blk))
getBlockInfoImpl = getterSTM $ \st hash ->
    ibiBlockInfo <$> Map.lookup hash (currentRevMap st)

getMaxSlotNoImpl ::
     forall m blockId. IOLike m
  => VolatileDBEnv m blockId
  -> STM m MaxSlotNo
getMaxSlotNoImpl = getterSTM currentMaxSlotNo

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- | Creates a new file and updates the 'OpenState' accordingly.
-- This may throw an FsError.
nextFile ::
     forall h m blk. (IOLike m, Eq h)
  => HasFS m h -> ModifyOpenState m blk h ()
nextFile hasFS = do
    st@OpenState { currentWriteHandle = curHndl, currentWriteId, currentMap } <- get

    let currentWriteId' = currentWriteId + 1
        file = filePath currentWriteId'

    lift $ lift $ hClose hasFS curHndl

    hndl <- lift $ allocateTemp
      (hOpen   hasFS file (AppendMode MustBeNew))
      (hClose' hasFS)
      ((==) . currentWriteHandle)
    put st {
        currentWriteHandle = hndl
      , currentWritePath   = file
      , currentWriteId     = currentWriteId'
      , currentWriteOffset = 0
      , currentMap         = Index.insert currentWriteId' FileInfo.empty
                                currentMap
      }

-- | Gets part of the 'OpenState' in 'STM'.
getterSTM ::
     forall m blk a. IOLike m
  => (forall h. OpenState blk h -> a)
  -> VolatileDBEnv m blk
  -> STM m a
getterSTM fromSt VolatileDBEnv { varInternalState } = do
    mSt <- RAWLock.read varInternalState
    case mSt of
      DbClosed  -> throwM $ UserError $ ClosedDBError Nothing
      DbOpen st -> return $ fromSt st
