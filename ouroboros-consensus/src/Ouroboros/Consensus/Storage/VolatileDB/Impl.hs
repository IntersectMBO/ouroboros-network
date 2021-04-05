{-# LANGUAGE ConstraintKinds           #-}
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
{-# LANGUAGE TypeApplications          #-}
-- | Volatile on-disk database of blocks
--
-- = Logic
--
-- The VolatileDB is a key-value store of blocks indexed by their hashes. It is
-- parameterised by the block type @blk@.
--
-- The \"volatile\" in the name refers to the fact that the blocks stored in it
-- make up the /volatile/ part of the chain, i.e., the last @k@ blocks of the
-- chain, which can still be rolled back. Not only the last @k@ blocks of the
-- current chain are stored in this database, but also blocks of forks which we
-- have switched from or will switch to.
--
-- The VolatileDB appends new blocks sequentially to a file. When
-- 'volMaxBlocksPerFile' are stored in the current file, a new file is started.
--
-- The VolatileDB provides four main operations:
--
-- 1. Adding blocks with 'putBlock'
-- 2. Get blocks or information about them with 'getBlockComponent'
-- 3. Accessing the in-memory indices using 'getBlockInfo' and
--   'filterByPredecessor'
-- 4. Garbage collecting blocks older than a given slot using 'garbageCollect'
--
-- Garbage collection will only delete a file from the VolatileDB when all
-- blocks in it have a slot older than the one passed to 'garbageCollect'.
--
-- = Errors
--
-- When an exception occurs while modifying the VolatileDB, we close the
-- database as a safety measure, e.g., in case a file could not be written to
-- disk, as we can no longer make sure the in-memory indices match what's stored
-- on the file system. When reopening, we validate the blocks stored in the file
-- system and reconstruct the in-memory indices.
--
-- NOTE: this means that when a thread modifying the VolatileDB is killed, the
-- database will be closed. This is an intentional choice to simplify things.
--
-- The in-memory indices can always be reconstructed from the file system.
-- This is important, as we must be resilient against unexpected shutdowns,
-- power losses, etc.
--
-- We achieve this by only performing basic operations on the VolatileDB:
-- * 'putBlock' only appends a new block to a file. Losing an update means we
--   only lose a block, which is not a problem, it can be redownloaded.
-- * 'garbageCollect' only deletes entire files.
-- * There is no operation that modifies a file in-place. This means we do not
--   have to keep any rollback journals to make sure we are safe in case of
--   unexpected shutdowns.
--
-- We only throw 'VolatileDBError'. File-system errors are caught, wrapped in a
-- 'VolatileDBError', and rethrown. We make sure that all calls to 'HasFS'
-- functions are properly wrapped. This wrapping is automatically done when
-- inside the scope of 'modifyOpenState' and 'withOpenState'. Otherwise, we use
-- 'wrapFsError'.
--
-- = Concurrency
--
-- A single folder should only be used by a single VolatileDB. Naturally, a
-- VolatileDB can be accessed concurrently by multiple threads.
--
-- = File-system layout:
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
-- Each file stores a fixed number of blocks, specified by
-- 'volMaxBlocksPerFile'. When opening the VolatileDB, it will start appending
-- to the file with the highest number that is not yet full. If all are full or
-- none exist, a new file will be created.
--
-- There is an implicit ordering of block files, which is NOT alpharithmetic.
-- For example, @blocks-20.dat@ < @blocks-100.dat@.
--
-- = Recovery
--
-- The VolatileDB will always try to recover to a consistent state even if this
-- means deleting all of its contents. In order to achieve this, it truncates
-- the files containing blocks if some blocks fail to parse, are invalid, or are
-- duplicated.
module Ouroboros.Consensus.Storage.VolatileDB.Impl (
    -- * Opening the database
    VolatileDbArgs (..)
  , VolatileDbSerialiseConstraints
  , defaultArgs
  , openDB
    -- * Re-exported
  , BlockValidationPolicy (..)
  , BlocksPerFile
  , ParseError (..)
  , TraceEvent (..)
  , extractBlockInfo
  , mkBlocksPerFile
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad (unless, when)
import           Control.Monad.State.Strict (get, gets, lift, modify, put,
                     state)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAWLock
import           Ouroboros.Consensus.Util.ResourceRegistry (allocateTemp,
                     runWithTempRegistry)

import           Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Storage.VolatileDB.API
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (FileInfo)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo as FileInfo
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.State
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

{------------------------------------------------------------------------------
  Opening the database
------------------------------------------------------------------------------}

data VolatileDbArgs f m blk = VolatileDbArgs {
      volCheckIntegrity   :: HKD f (blk -> Bool)
    , volCodecConfig      :: HKD f (CodecConfig blk)
    , volHasFS            :: SomeHasFS m
    , volMaxBlocksPerFile :: BlocksPerFile
    , volTracer           :: Tracer m (TraceEvent blk)
    , volValidationPolicy :: BlockValidationPolicy
    }

-- | Default arguments
defaultArgs :: Applicative m => SomeHasFS m -> VolatileDbArgs Defaults m blk
defaultArgs volHasFS = VolatileDbArgs {
      volCheckIntegrity   = NoDefault
    , volCodecConfig      = NoDefault
    , volHasFS
    , volMaxBlocksPerFile = mkBlocksPerFile 1000
    , volTracer           = nullTracer
    , volValidationPolicy = NoValidation
    }

-- | 'EncodeDisk' and 'DecodeDisk' constraints needed for the VolatileDB.
type VolatileDbSerialiseConstraints blk =
  ( EncodeDisk blk blk
  , DecodeDisk blk (Lazy.ByteString -> blk)
  , DecodeDiskDep (NestedCtxt Header) blk
  , HasNestedContent Header blk
  , HasBinaryBlockInfo blk
  )

openDB ::
     forall m blk.
     ( HasCallStack
     , IOLike m
     , HasHeader blk
     , GetPrevHash blk
     , VolatileDbSerialiseConstraints blk
     )
  => VolatileDbArgs Identity m blk
  -> m (VolatileDB m blk)
openDB VolatileDbArgs { volHasFS = SomeHasFS hasFS, .. } = runWithTempRegistry $ do
    lift $ createDirectoryIfMissing hasFS True (mkFsPath [])
    ost <-
      mkOpenState
        volCodecConfig
        hasFS
        volCheckIntegrity
        volValidationPolicy
        volTracer
        volMaxBlocksPerFile
    stVar <- lift $ RAWLock.new (DbOpen ost)
    let env = VolatileDBEnv {
            hasFS            = hasFS
          , varInternalState = stVar
          , maxBlocksPerFile = volMaxBlocksPerFile
          , tracer           = volTracer
          , codecConfig      = volCodecConfig
          , checkIntegrity   = volCheckIntegrity
          }
        volatileDB = VolatileDB {
            closeDB             = closeDBImpl             env
          , getBlockComponent   = getBlockComponentImpl   env
          , putBlock            = putBlockImpl            env
          , garbageCollect      = garbageCollectImpl      env
          , filterByPredecessor = filterByPredecessorImpl env
          , getBlockInfo        = getBlockInfoImpl        env
          , getMaxSlotNo        = getMaxSlotNoImpl        env
          }
    return (volatileDB, ost)

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

closeDBImpl ::
     forall m blk. (IOLike m, HasHeader blk)
  => VolatileDBEnv m blk
  -> m ()
closeDBImpl VolatileDBEnv { varInternalState, tracer, hasFS } = do
    mbInternalState <-
      RAWLock.withWriteAccess varInternalState $ \st -> return (DbClosed, st)
    case mbInternalState of
      DbClosed -> traceWith tracer DBAlreadyClosed
      DbOpen ost ->
        wrapFsError (Proxy @blk) $ closeOpenHandles hasFS ost

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
  -> BlockComponent blk b
  -> HeaderHash blk
  -> m (Maybe b)
getBlockComponentImpl env@VolatileDBEnv { codecConfig, checkIntegrity } blockComponent hash =
    withOpenState env $ \hasFS OpenState { currentRevMap } ->
      case Map.lookup hash currentRevMap of
        Nothing                -> return Nothing
        Just internalBlockInfo -> Just <$>
          getBlockComponent hasFS internalBlockInfo blockComponent
  where
    getBlockComponent ::
         forall b' h. HasFS m h
      -> InternalBlockInfo blk
      -> BlockComponent blk b'
      -> m b'
    getBlockComponent hasFS ibi = \case
        GetHash          -> return hash
        GetSlot          -> return biSlotNo
        GetIsEBB         -> return biIsEBB
        GetBlockSize     -> return $ fromIntegral $ unBlockSize ibiBlockSize
        GetHeaderSize    -> return biHeaderSize
        GetPure a        -> return a
        GetApply f bc    ->
          getBlockComponent hasFS ibi f <*> getBlockComponent hasFS ibi bc
        GetBlock         ->
          getBlockComponent hasFS ibi GetRawBlock >>= parseBlock
        GetRawBlock      -> withFile hasFS ibiFile ReadMode $ \hndl -> do
          let size   = fromIntegral $ unBlockSize ibiBlockSize
              offset = unBlockOffset ibiBlockOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetHeader        ->
          getBlockComponent hasFS ibi GetRawHeader >>= parseHeader
        GetRawHeader     -> withFile hasFS ibiFile ReadMode $ \hndl -> do
          let size   = fromIntegral biHeaderSize
              offset = unBlockOffset ibiBlockOffset + fromIntegral biHeaderOffset
          hGetExactlyAt hasFS hndl size (AbsOffset offset)
        GetNestedCtxt    -> return ibiNestedCtxt
        GetVerifiedBlock ->
          getBlockComponent hasFS ibi GetBlock >>= \blk -> do
            unless (checkIntegrity blk) $
              throwIO $ UnexpectedFailure $ CorruptBlockError @blk hash
            return blk
      where
        InternalBlockInfo { ibiBlockInfo = BlockInfo {..}, .. } = ibi

        parseBlock :: Lazy.ByteString -> m blk
        parseBlock bytes = throwParseErrors bytes $
            CBOR.deserialiseFromBytes (decodeDisk codecConfig) bytes

        parseHeader :: Lazy.ByteString -> m (Header blk)
        parseHeader bytes = throwParseErrors bytes $
            case ibiNestedCtxt of
              SomeSecond ctxt ->
                CBOR.deserialiseFromBytes
                  ((\f -> nest . DepPair ctxt . f) <$>
                      decodeDiskDep codecConfig ctxt)
                  bytes

        pt :: RealPoint blk
        pt = RealPoint biSlotNo hash

        throwParseErrors ::
             forall b''.
             Lazy.ByteString
          -> Either CBOR.DeserialiseFailure (Lazy.ByteString, Lazy.ByteString -> b'')
          -> m b''
        throwParseErrors fullBytes = \case
            Right (trailing, f)
              | Lazy.null trailing
              -> return $ f fullBytes
              | otherwise
              -> throwIO $ UnexpectedFailure $ TrailingDataError ibiFile pt trailing
            Left err
              -> throwIO $ UnexpectedFailure $ ParseError ibiFile pt err

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
    blockInfo@BlockInfo { biHash, biSlotNo, biPrevHash } = extractBlockInfo blk

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
                                DepPair nestedCtxt _ -> SomeSecond nestedCtxt
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
        lift $ lift $ traceEventIO "START garbage collection"
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
        lift $ lift $ traceEventIO "STOP garbage collection"
  where
    -- | Return 'True' if a garbage collection would actually garbage collect
    -- at least one file.
    gcPossible :: OpenState blk h -> Bool
    gcPossible = not . null . getFilesToGC

    -- | Return the list of files that can be garbage collected.
    getFilesToGC :: OpenState blk h -> [(FileId, FileInfo blk)]
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
  -> (FileId, FileInfo blk)
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
     forall m blk. (IOLike m, HasHeader blk)
  => VolatileDBEnv m blk
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
     forall m blk a. (IOLike m, HasHeader blk)
  => (forall h. OpenState blk h -> a)
  -> VolatileDBEnv m blk
  -> STM m a
getterSTM fromSt VolatileDBEnv { varInternalState } = do
    mSt <- RAWLock.read varInternalState
    case mSt of
      DbClosed  -> throwIO $ ApiMisuse @blk $ ClosedDBError Nothing
      DbOpen st -> return $ fromSt st
