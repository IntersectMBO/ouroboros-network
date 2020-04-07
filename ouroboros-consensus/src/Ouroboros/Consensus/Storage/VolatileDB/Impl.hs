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
-- The db is a key-value store of binary blocks and is parametric on the key
-- of blocks, named @blockId@.
--
-- The database uses in-memory indexes, which are created on each (re)opening.
-- Reopening includes parsing all blocks in the @dbFolder@, so it can be an
-- expensive operation if the database gets big. That's why the intention of
-- this db is to be used for only the tip of the blockchain, when there is
-- still volatility on which blocks are included. The db is agnostic to the
-- format of the blocks, so a parser must be provided. In addition to
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
module Ouroboros.Consensus.Storage.VolatileDB.Impl
    ( -- * Opening a database
      openDB
    , VolatileDbArgs (..)
    ) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Builder as BS
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAWLock
import           Ouroboros.Consensus.Util.ResourceRegistry (allocateTemp,
                     runWithTempRegistry)

import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB.API
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (FileInfo)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo as FileInfo
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.State
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

data VolatileDbArgs m h blockId e = VolatileDbArgs
    { hasFS            :: HasFS m h
    , maxBlocksPerFile :: BlocksPerFile
    , tracer           :: Tracer m (TraceEvent e blockId)
    , parser           :: Parser e m blockId
    }

openDB :: ( HasCallStack
          , IOLike m
          , Ord                blockId
          , NoUnexpectedThunks blockId
          , Eq h
          )
       => VolatileDbArgs m h blockId e
       -> m (VolatileDB blockId m)
openDB VolatileDbArgs {..} = runWithTempRegistry $ do
    ost   <- mkOpenState hasFS parser tracer maxBlocksPerFile
    stVar <- lift $ RAWLock.new (DbOpen ost)
    let env = VolatileDBEnv {
            hasFS          = hasFS
          , varInternalState  = stVar
          , maxBlocksPerFile = maxBlocksPerFile
          , parser           = parser
          , tracer           = tracer
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

closeDBImpl :: IOLike m
            => VolatileDBEnv m blockId
            -> m ()
closeDBImpl VolatileDBEnv { varInternalState, tracer, hasFS } = do
    mbInternalState <-
      RAWLock.withWriteAccess varInternalState $ \st -> return (DbClosed, st)
    case mbInternalState of
      DbClosed -> traceWith tracer DBAlreadyClosed
      DbOpen ost ->
        wrapFsError $ closeOpenHandles hasFS ost

getBlockComponentImpl
  :: forall m blockId b. (IOLike m, Ord blockId, HasCallStack)
  => VolatileDBEnv m blockId
  -> BlockComponent (VolatileDB blockId m) b
  -> blockId
  -> m (Maybe b)
getBlockComponentImpl env blockComponent blockId =
    withOpenState env $ \hasFS OpenState { currentRevMap } ->
      case Map.lookup blockId currentRevMap of
        Nothing                -> return Nothing
        Just internalBlockInfo -> Just <$>
          getBlockComponent hasFS internalBlockInfo blockComponent
  where
    getBlockComponent
      :: forall b' h.
         HasFS m h
      -> InternalBlockInfo blockId
      -> BlockComponent (VolatileDB blockId m) b'
      -> m b'
    getBlockComponent hasFS ib = \case
        GetHash         -> return blockId
        GetSlot         -> return bslot
        GetIsEBB        -> return bisEBB
        GetBlockSize    -> return $ fromIntegral $ unBlockSize ibBlockSize
        GetHeaderSize   -> return bheaderSize
        GetPure a       -> return a
        GetApply f bc   ->
          getBlockComponent hasFS ib f <*> getBlockComponent hasFS ib bc
        GetBlock        -> return ()
        GetRawBlock     -> withFile hasFS ibFile ReadMode $ \hndl -> do
          let size   = unBlockSize ibBlockSize
              offset = ibBlockOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetHeader       -> return ()
        GetRawHeader    -> withFile hasFS ibFile ReadMode $ \hndl -> do
          let size   = fromIntegral bheaderSize
              offset = ibBlockOffset + fromIntegral bheaderOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
      where
        InternalBlockInfo { ibBlockInfo = BlockInfo {..}, .. } = ib

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
putBlockImpl :: forall m blockId. (IOLike m, Ord blockId)
             => VolatileDBEnv m blockId
             -> BlockInfo blockId
             -> BS.Builder
             -> m ()
putBlockImpl env@VolatileDBEnv{ maxBlocksPerFile, tracer }
             blockInfo@BlockInfo { bbid, bslot, bpreBid }
             builder =
    appendOpenState env $ \hasFS -> do
      OpenState { currentRevMap, currentWriteHandle } <- get
      if Map.member bbid currentRevMap then
        lift $ lift $ traceWith tracer $ BlockAlreadyHere bbid
      else do
        bytesWritten <- lift $ lift $ hPut hasFS currentWriteHandle builder
        fileIsFull <- state $ updateStateAfterWrite bytesWritten
        when fileIsFull $ nextFile hasFS
  where
    updateStateAfterWrite
      :: forall h.
         Word64
      -> OpenState blockId h
      -> (Bool, OpenState blockId h)  -- ^ True: current file is full
    updateStateAfterWrite bytesWritten st@OpenState{..} =
        (FileInfo.isFull maxBlocksPerFile fileInfo', st')
      where
        fileInfo = fromMaybe
            (error $ "VolatileDB invariant violation:"
                    ++ "Current write file not found in Index.")
            (Index.lookup currentWriteId currentMap)
        fileBlockInfo = FileInfo.mkFileBlockInfo (BlockSize bytesWritten) bbid
        fileInfo' = FileInfo.addBlock bslot currentWriteOffset fileBlockInfo fileInfo
        currentMap' = Index.insert currentWriteId fileInfo' currentMap
        internalBlockInfo' = InternalBlockInfo {
            ibFile         = currentWritePath
          , ibBlockOffset  = currentWriteOffset
          , ibBlockSize    = BlockSize bytesWritten
          , ibBlockInfo    = blockInfo
          }
        currentRevMap' = Map.insert bbid internalBlockInfo' currentRevMap
        st' = st {
            currentWriteOffset = currentWriteOffset + bytesWritten
          , currentMap         = currentMap'
          , currentRevMap      = currentRevMap'
          , currentSuccMap     = insertMapSet currentSuccMap (bbid, bpreBid)
          , currentMaxSlotNo   = currentMaxSlotNo `max` MaxSlotNo bslot
          }

-- NOTE: the current file is never garbage collected.

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
garbageCollectImpl :: forall m blockId. (IOLike m, Ord blockId)
                   => VolatileDBEnv m blockId
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
            currentMaxSlotNo = FileInfo.maxSlotInFiles
              (Index.elems (currentMap st))
          }
        lift $ lift $ traceEventM "STOP garbage collection"
  where
    -- | Return 'True' if a garbage collection would actually garbage collect
    -- at least one file.
    gcPossible :: OpenState blockId h -> Bool
    gcPossible = not . null . getFilesToGC

    -- | Return the list of files that can be garbage collected.
    getFilesToGC :: OpenState blockId h -> [(FileId, FileInfo blockId)]
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
garbageCollectFile
  :: forall m h blockId. (MonadThrow m, Ord blockId)
  => HasFS m h
  -> (FileId, FileInfo blockId)
  -> ModifyOpenState m blockId h ()
garbageCollectFile hasFS (fileId, fileInfo) = do

    lift $ lift $ removeFile hasFS $ filePath fileId

    st@OpenState { currentMap, currentRevMap, currentSuccMap } <- get

    let bids            = FileInfo.blockIds fileInfo
        currentRevMap'  = Map.withoutKeys currentRevMap (Set.fromList bids)
        deletedPairs    = mapMaybe
          (\b -> (b,) . bpreBid . ibBlockInfo <$> Map.lookup b currentRevMap)
          bids
        currentSuccMap' = foldl' deleteMapSet currentSuccMap deletedPairs

    put st {
        currentMap     = Index.delete fileId currentMap
      , currentRevMap  = currentRevMap'
      , currentSuccMap = currentSuccMap'
      }

filterByPredecessorImpl :: forall m blockId. (IOLike m, Ord blockId)
                        => VolatileDBEnv m blockId
                        -> STM m (WithOrigin blockId -> Set blockId)
filterByPredecessorImpl = getterSTM $ \st blockId ->
    fromMaybe Set.empty (Map.lookup blockId (currentSuccMap st))

getBlockInfoImpl :: forall m blockId. (IOLike m, Ord blockId)
                 => VolatileDBEnv m blockId
                 -> STM m (blockId -> Maybe (BlockInfo blockId))
getBlockInfoImpl = getterSTM $ \st blockId ->
    ibBlockInfo <$> Map.lookup blockId (currentRevMap st)

getMaxSlotNoImpl :: forall m blockId. IOLike m
                 => VolatileDBEnv m blockId
                 -> STM m MaxSlotNo
getMaxSlotNoImpl = getterSTM currentMaxSlotNo

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- | Creates a new file and updates the 'OpenState' accordingly.
-- This may throw an FsError.
nextFile :: forall h m blockId. (IOLike m, Eq h)
         => HasFS m h -> ModifyOpenState m blockId h ()
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
getterSTM :: forall m blockId a. IOLike m
          => (forall h. OpenState blockId h -> a)
          -> VolatileDBEnv m blockId
          -> STM m a
getterSTM fromSt VolatileDBEnv { varInternalState } = do
    mSt <- RAWLock.read varInternalState
    case mSt of
      DbClosed  -> throwM $ UserError $ ClosedDBError Nothing
      DbOpen st -> return $ fromSt st
