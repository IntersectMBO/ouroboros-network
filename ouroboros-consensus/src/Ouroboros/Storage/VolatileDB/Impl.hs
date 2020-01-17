{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
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
-- The db is a key-value store of binary blocks and is parametric
-- on the key of blocks, named blockId. The only constraints are that one must
-- provide a function (toSlot :: blockId -> SlotNo), as well as an Ord instance
-- of blockId. The db expects NO properties for this Ord instance, not even one
-- that makes toBlock monotonic.

-- The database uses in memory indexes, which are created on each reopening.
-- reopening includes parsing all blocks of the dbFolder, so it can be an
-- expensive operation if the database gets big. That's why the intention of
-- this db is to be used for only the tip of the blockchain, when there is still
-- volatility on which blocks are included. The db is agnostic to the format of
-- the blocks, so a parser must be provided. In addition to getBlock and
-- putBlock, the db provides also the ability to garbage-collect old blocks.
-- The actual garbage-collection happens in terms of files and not blocks: a
-- file is deleted/garbage-collected only if its latest block is old enough. A
-- block is old enough if its toSlot value is old enough and not based on its
-- Ord instance. This type of garbage collection makes the deletion of blocks
-- depend on the number of blocks we insert on each file, as well as the order
-- of insertion, so it's not deterministic on blocks themselves.
--
-- = Errors
--
-- On any exception or error the db closes and its Internal State is lost,
-- inluding in memory indexes. We try to make sure that even on errors the
-- fs represantation of the db remains consistent and the Internal State
-- can be recovered on reopening. In general we try to make sure that at
-- any point, losing the in-memory Internal State is not fatal to the db
-- as it can recovered. This is important since we must always expect unexpected
-- shutdowns, power loss, sleep mode etc.
-- This is achived by leting only basic operations on the db:
-- + putBlock only appends a new block on a file. Losing an update means we only
--   lose a block, which can be recovered.
-- + garbage collect deletes only whole files.
-- + there is no modify block operation. Thanks to that we need not keep any
--   rollback journals to make sure we are safe in case of unexpected shutdowns.
--
-- We only throw VolatileDBError. All internal errors, like io errors, are
-- cought, wrapped and rethrown. For all new calls of HasFs, we must make sure
-- that they are used properly wrapped. All top-level function of this module
-- are safe. You can safely use HasFs calls in modifyState or wrapFsError
-- actions.
--
-- = Concurrency
--
-- The same db should only be opened once
-- Multiple threads can share the same db as concurency if fully supported.
--
-- = FS Layout:
--
-- On disk represantation is as follows:
--
--  dbFolder\
--    blocks-0.dat
--    blocks-1.dat
--    ...
--
--  If on opening any other filename which does not follow blocks-i.dat is found
--  an error is raised. The Ordering of blocks is not guarranteed to be
--  followed, files can be garbage-collected.
--
--  Each file stores a fixed number of slots, specified by _maxBlocksPerFile.
--  If the db finds files with less blocks than this max, it will start
--  appending to the newest of them, if it's the newest of all files. If it's
--  not the newest of all files it will create a new file to append blocks..
--
--  There is an implicit ordering of block files, which is NOT alpharithmetic
--  For example blocks-20.dat < blocks-100.dat
--
module Ouroboros.Storage.VolatileDB.Impl
    ( -- * Opening a database
      openDB
      -- * tests only
    , VolatileDBEnv(..)
    , InternalState(..)
    , OpenOrClosed(..)
    , filePath
    , openDBFull
    ) where

import           Control.Monad
import qualified Data.ByteString.Builder as BS
import           Data.List (find, sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (safeMaximumOn)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common (BlockComponent (..))
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..),
                     ThrowCantCatch (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.FileInfo (FileInfo)
import qualified Ouroboros.Storage.VolatileDB.FileInfo as FileInfo
import           Ouroboros.Storage.VolatileDB.Index (Index)
import qualified Ouroboros.Storage.VolatileDB.Index as Index
import           Ouroboros.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
  Main Types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h e. VolatileDBEnv {
      _dbHasFS          :: !(HasFS m h)
    , _dbErr            :: !(ErrorHandling VolatileDBError m)
    , _dbErrSTM         :: !(ThrowCantCatch VolatileDBError (STM m))
    , _dbInternalState  :: !(StrictMVar m (OpenOrClosed blockId h))
    , _maxBlocksPerFile :: !Int
    , _parser           :: !(Parser e m blockId)
    }

data OpenOrClosed blockId h =
    VolatileDbOpen !(InternalState blockId h)
  | VolatileDbClosed
  deriving (Generic, NoUnexpectedThunks)

volatileDbIsOpen :: OpenOrClosed blockId h -> Bool
volatileDbIsOpen (VolatileDbOpen _) = True
volatileDbIsOpen VolatileDbClosed   = False

data InternalState blockId h = InternalState {
      _currentWriteHandle :: !(Handle h)
      -- ^ The only open file we append blocks to.
    , _currentWritePath   :: !FsPath
      -- ^ The path of the file above.
    , _currentWriteId     :: !FileId
      -- ^ The 'FileId' of the same file.
    , _currentWriteOffset :: !Word64
      -- ^ The offset of the same file.
    , _nextNewFileId      :: !Int
      -- ^ The next file name Id.
    , _currentMap         :: !(Index blockId)
      -- ^ The contents of each file.
    , _currentRevMap      :: !(ReverseIndex blockId)
      -- ^ Where to find each block based on its slot number.
    , _currentSuccMap     :: !(SuccessorsIndex blockId)
      -- ^ The successors for each block.
    , _currentMaxSlotNo   :: !MaxSlotNo
      -- ^ Highest ever stored SlotNo.
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

openDB :: ( HasCallStack
          , IOLike m
          , Ord                blockId
          , NoUnexpectedThunks blockId
          , Typeable           blockId
          , Show               blockId
          )
       => HasFS m h
       -> ErrorHandling VolatileDBError m
       -> ThrowCantCatch VolatileDBError (STM m)
       -> Parser e m blockId
       -> Int
       -> m (VolatileDB blockId m)
openDB h e e' p m = fst <$> openDBFull h e e' p m

openDBFull :: ( HasCallStack
              , IOLike m
              , Ord                blockId
              , NoUnexpectedThunks blockId
              , Typeable           blockId
              , Show               blockId
              )
           => HasFS m h
           -> ErrorHandling VolatileDBError m
           -> ThrowCantCatch VolatileDBError (STM m)
           -> Parser e m blockId
           -> Int
           -> m (VolatileDB blockId m, VolatileDBEnv m blockId)
openDBFull hasFS err errSTM parser maxBlocksPerFile = do
    env <- openDBImpl hasFS err errSTM parser maxBlocksPerFile
    return $ (, env) VolatileDB {
        closeDB           = closeDBImpl  env
      , isOpenDB          = isOpenDBImpl env
      , reOpenDB          = reOpenDBImpl env
      , getBlockComponent = getBlockComponentImpl env
      , putBlock          = putBlockImpl env
      , garbageCollect    = garbageCollectImpl env
      , getIsMember       = getIsMemberImpl env
      , getBlockIds       = getBlockIdsImpl env
      , getSuccessors     = getSuccessorsImpl env
      , getPredecessor    = getPredecessorImpl env
      , getMaxSlotNo      = getMaxSlotNoImpl env
      }

openDBImpl :: ( HasCallStack
              , IOLike m
              , Ord                blockId
              , NoUnexpectedThunks blockId
              , Typeable           blockId
              , Show               blockId
              )
           => HasFS m h
           -> ErrorHandling VolatileDBError m
           -> ThrowCantCatch VolatileDBError (STM m)
           -> Parser e m blockId
           -> Int -- ^ @maxBlocksPerFile@
           -> m (VolatileDBEnv m blockId)
openDBImpl hasFS@HasFS{..} err errSTM parser maxBlocksPerFile =
    if maxBlocksPerFile <= 0
    then EH.throwError err $ UserError . InvalidArgumentsError $
      "maxBlocksPerFile should be positive"
    else do
      st <- mkInternalStateDB hasFS err parser maxBlocksPerFile
      stVar <- newMVar $ VolatileDbOpen st
      return $ VolatileDBEnv hasFS err errSTM stVar maxBlocksPerFile parser

closeDBImpl :: IOLike m
            => VolatileDBEnv m blockId
            -> m ()
closeDBImpl VolatileDBEnv{..} = do
    mbInternalState <- swapMVar _dbInternalState VolatileDbClosed
    case mbInternalState of
      VolatileDbClosed -> return ()
      VolatileDbOpen InternalState{..} ->
        wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle
  where
    HasFS{..} = _dbHasFS

isOpenDBImpl :: IOLike m
             => VolatileDBEnv m blockId
             -> m Bool
isOpenDBImpl VolatileDBEnv{..} = do
    mSt <- readMVar _dbInternalState
    return $ volatileDbIsOpen mSt

-- | Property: @'closeDB' >> 'reOpenDB'@  should be a no-op. This is true
-- because 'reOpenDB' will always append to the last created file.
reOpenDBImpl :: ( HasCallStack
                , IOLike m
                , Ord      blockId
                , Typeable blockId
                , Show     blockId
                )
             => VolatileDBEnv m blockId
             -> m ()
reOpenDBImpl VolatileDBEnv{..} =
    modifyMVar _dbInternalState $ \case
      VolatileDbOpen st -> return (VolatileDbOpen st, ())
      VolatileDbClosed -> do
        st <- mkInternalStateDB _dbHasFS _dbErr _parser _maxBlocksPerFile
        return (VolatileDbOpen st, ())

getBlockComponentImpl
  :: forall m blockId b. (IOLike m, Ord blockId, HasCallStack)
  => VolatileDBEnv m blockId
  -> BlockComponent (VolatileDB blockId m) b
  -> blockId
  -> m (Maybe b)
getBlockComponentImpl env@VolatileDBEnv{..} blockComponent blockId =
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
      case Map.lookup blockId _currentRevMap of
        Nothing                -> return (st, Nothing)
        Just internalBlockInfo -> ((st, ) . Just) <$>
          getBlockComponent hasFS internalBlockInfo blockComponent
  where
    getBlockComponent
      :: forall b' h.
         HasFS m h
      -> InternalBlockInfo blockId
      -> BlockComponent (VolatileDB blockId m) b'
      -> m b'
    getBlockComponent hasFS ib@InternalBlockInfo {..} = \case
      GetHash         -> return blockId
      GetSlot         -> return ibSlot
      GetIsEBB        -> return ibIsEBB
      GetBlockSize    -> return $ fromIntegral $ unBlockSize ibBlockSize
      GetHeaderSize   -> return ibHeaderSize
      GetPure a       -> return a
      GetApply f bc   ->
        getBlockComponent hasFS ib f <*> getBlockComponent hasFS ib bc
      GetBlock        -> return ()
      GetRawBlock     -> withFile hasFS ibFile ReadMode $ \hndl -> do
        let size   = unBlockSize ibBlockSize
            offset = ibSlotOffset
        hGetExactlyAt hasFS hndl size (AbsOffset offset)
      GetHeader       -> return ()
      GetRawHeader    -> withFile hasFS ibFile ReadMode $ \hndl -> do
        let size   = fromIntegral ibHeaderSize
            offset = ibSlotOffset + fromIntegral ibHeaderOffset
        hGetExactlyAt hasFS hndl size (AbsOffset offset)

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
putBlockImpl env@VolatileDBEnv{..} BlockInfo{..} builder =
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} ->
      if Map.member bbid _currentRevMap
      then return (st, ()) -- putting an existing block is a no-op.
      else do
        bytesWritten <- hPut hasFS _currentWriteHandle builder
        updateStateAfterWrite hasFS st bytesWritten
  where
    updateStateAfterWrite :: forall h.
                             HasFS m h
                          -> InternalState blockId h
                          -> Word64
                          -> m (InternalState blockId h, ())
    updateStateAfterWrite hasFS@HasFS{..} st@InternalState{..} bytesWritten =
        if FileInfo.isFull _maxBlocksPerFile fileInfo'
        then (,()) <$> nextFile hasFS _dbErr env st'
        else return (st', ())
      where
        fileInfo = fromMaybe
            (error $ "VolatileDB invariant violation:"
                    ++ "Current write file not found in Index.")
            (Index.lookup _currentWriteId _currentMap)
        fileInfo' = FileInfo.addSlot bslot _currentWriteOffset
            (FileInfo.mkFileSlotInfo (BlockSize bytesWritten) bbid) fileInfo
        currentMap' = Index.insert _currentWriteId fileInfo' _currentMap
        internalBlockInfo' = InternalBlockInfo {
            ibFile         = _currentWritePath
          , ibSlotOffset   = _currentWriteOffset
          , ibBlockSize    = BlockSize bytesWritten
          , ibSlot         = bslot
          , ibPreBid       = bpreBid
          , ibIsEBB        = bisEBB
          , ibHeaderOffset = bheaderOffset
          , ibHeaderSize   = bheaderSize
          }
        currentRevMap' = Map.insert bbid internalBlockInfo' _currentRevMap
        st' = st {
            _currentWriteOffset = _currentWriteOffset + bytesWritten
          , _currentMap         = currentMap'
          , _currentRevMap      = currentRevMap'
          , _currentSuccMap     = insertMapSet _currentSuccMap (bbid, bpreBid)
          , _currentMaxSlotNo   = _currentMaxSlotNo `max` MaxSlotNo bslot
          }

-- | The approach we follow here is to try to garbage collect each file.
-- For each file we update the fs and then we update the Internal State.
-- If some fs update fails, we are left with an empty Internal State and a
-- subset of the deleted files in fs. Any unexpected failure (power loss,
-- other exceptions) has the same results, since the Internal State will
-- be empty on re-opening. This is ok only if any fs updates leave the fs
-- in a consistent state every moment.
--
-- This approach works since we always close the Database in case of errors,
-- but we should rethink it if this changes in the future.
garbageCollectImpl :: forall m blockId. (IOLike m, Ord blockId)
                   => VolatileDBEnv m blockId
                   -> SlotNo
                   -> m ()
garbageCollectImpl env@VolatileDBEnv{..} slot =
    modifyState env $ \hasFS st -> do
      st' <- foldM (tryCollectFile hasFS env slot) st
              (sortOn fst $ Index.toList (_currentMap st))
      return (st', ())

-- | For the given file, we check if it should be garbage collected and
-- return the updated InternalState.
--
-- Important note here is that, every call should leave the fs in a
-- consistent state, without depending on other calls. This is achieved
-- so far, since fs calls are reduced to removeFile and truncate 0.
--
-- This may throw an FsError.
tryCollectFile :: forall m h blockId
               .  (MonadThrow m, Ord blockId)
               => HasFS m h
               -> VolatileDBEnv m blockId
               -> SlotNo
               -> InternalState blockId h
               -> (FileId, FileInfo blockId)
               -> m (InternalState blockId h)
tryCollectFile hasFS@HasFS{..} env slot st@InternalState{..} (fileId, fileInfo) =
    if  | not canGC     -> return st
        | not isCurrent -> do
            removeFile $ filePath fileId
            return st {
                _currentMap     = Index.delete fileId _currentMap
              , _currentRevMap  = currentRevMap'
              , _currentSuccMap = succMap'
              }
        | isCurrentNew  -> return st
        | otherwise     -> do
            -- We reach this case if we have to garbage collect the current file
            -- we are appending blocks to. For this to happen, a garbage
            -- collection would have to be triggered for a slot which is bigger
            -- than any recently inserted blocks.
            --
            -- 'reOpenFile' technically truncates the file to 0 offset, so any
            -- concurrent readers may fail. This may become an issue after:
            -- <https://github.com/input-output-hk/ouroboros-network/issues/767>
            st' <- reOpenFile hasFS (_dbErr env) env st
            return st' {
                _currentRevMap  = currentRevMap'
              , _currentSuccMap = succMap'
              }
  where
    canGC          = FileInfo.canGC fileInfo slot
    isCurrent      = fileId == _currentWriteId
    isCurrentNew   = _currentWriteOffset == 0
    bids           = FileInfo.blockIds fileInfo
    currentRevMap' = Map.withoutKeys _currentRevMap (Set.fromList bids)
    deletedPairs   =
        mapMaybe (\b -> (b,) . ibPreBid <$> Map.lookup b _currentRevMap) bids
    succMap'       = foldl deleteMapSet _currentSuccMap deletedPairs

getIsMemberImpl :: forall m blockId. (IOLike m, Ord blockId)
                => VolatileDBEnv m blockId
                -> STM m (blockId -> Bool)
getIsMemberImpl = getterSTM $ \st bid -> Map.member bid (_currentRevMap st)

getBlockIdsImpl :: forall m blockId. (IOLike m)
                => VolatileDBEnv m blockId
                -> m [blockId]
getBlockIdsImpl = getter $ Map.keys . _currentRevMap

getSuccessorsImpl :: forall m blockId. (IOLike m, Ord blockId)
                  => VolatileDBEnv m blockId
                  -> STM m (WithOrigin blockId -> Set blockId)
getSuccessorsImpl = getterSTM $ \st blockId ->
    fromMaybe Set.empty (Map.lookup blockId (_currentSuccMap st))

getPredecessorImpl :: forall m blockId. (IOLike m, Ord blockId, HasCallStack)
                   => VolatileDBEnv m blockId
                   -> STM m (blockId -> WithOrigin blockId)
getPredecessorImpl = getterSTM $ \st blockId ->
    maybe (error msg) ibPreBid (Map.lookup blockId (_currentRevMap st))
  where
    msg = "precondition violated: block not member of the VolatileDB"

getMaxSlotNoImpl :: forall m blockId. IOLike m
                 => VolatileDBEnv m blockId
                 -> STM m MaxSlotNo
getMaxSlotNoImpl = getterSTM _currentMaxSlotNo

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- | Creates a new file and updates the 'InternalState' accordingly.
-- This may throw an FsError.
nextFile :: forall h m blockId. IOLike m
         => HasFS m h
         -> ErrorHandling VolatileDBError m
         -> VolatileDBEnv m blockId
         -> InternalState blockId h
         -> m (InternalState blockId h)
nextFile HasFS{..} _err VolatileDBEnv{..} st@InternalState{..} = do
    hClose _currentWriteHandle
    hndl <- hOpen file (AppendMode MustBeNew)
    return st {
        _currentWriteHandle = hndl
      , _currentWritePath   = file
      , _currentWriteId     = _nextNewFileId
      , _currentWriteOffset = 0
      , _currentMap         = Index.insert _nextNewFileId FileInfo.empty
                                _currentMap
      , _nextNewFileId      = _nextNewFileId + 1
      }
  where
    file = filePath _nextNewFileId

-- | Truncates a file to 0 and update its state accordingly.
-- This may throw an FsError.
reOpenFile :: forall m h blockId
           .  (MonadThrow m)
           => HasFS m h
           -> ErrorHandling VolatileDBError m
           -> VolatileDBEnv m blockId
           -> InternalState blockId h
           -> m (InternalState blockId h)
reOpenFile HasFS{..} _err VolatileDBEnv{..} st@InternalState{..} = do
    -- The manual for truncate states that it does not affect offset.
    -- However the file is open on Append Only, so it should automatically go
    -- to the end before each write.
   hTruncate _currentWriteHandle 0
   return st {
        _currentMap = Index.insert _currentWriteId FileInfo.empty _currentMap
      , _currentWriteOffset = 0
      }

mkInternalStateDB :: ( HasCallStack
                     , MonadThrow m
                     , MonadCatch m
                     , Ord      blockId
                     , Typeable blockId
                     , Show     blockId
                     )
                  => HasFS m h
                  -> ErrorHandling VolatileDBError m
                  -> Parser e m blockId
                  -> Int
                  -> m (InternalState blockId h)
mkInternalStateDB hasFS@HasFS{..} err parser maxBlocksPerFile =
    wrapFsError hasFsErr err $ do
      createDirectoryIfMissing True dbDir
      allFiles <- map toFsPath . Set.toList <$> listDirectory dbDir
      filesWithIds <- fromEither err $ parseAllFds allFiles
      mkInternalState hasFS err parser maxBlocksPerFile filesWithIds
  where
    dbDir = mkFsPath []

    toFsPath :: String -> FsPath
    toFsPath file = mkFsPath [file]

-- | Makes the 'InternalState' by parsing all files.
--
-- It may create a new file to append new blocks to or use an existing one.
mkInternalState
  :: forall blockId m h e. (
       HasCallStack
     , MonadCatch m
     , Ord      blockId
     , Typeable blockId
     , Show     blockId
     )
  => HasFS m h
  -> ErrorHandling VolatileDBError m
  -> Parser e m blockId
  -> Int
  -> [(FileId, FsPath)]
  -> m (InternalState blockId h)
mkInternalState hasFS err parser n files =
    wrapFsError (hasFsErr hasFS) err $
      go Index.empty Map.empty Map.empty Nothing [] files
  where
    -- | This is the file with the maximum 'FileId' in the db.
    lastFile = safeMaximumOn fst files
    newFileInfo curMap newIndex =
      ( filePath newIndex
      , newIndex
      , Index.insert newIndex FileInfo.empty curMap
      , FileSize 0 )

    truncateOnError Nothing _ _ = return ()
    truncateOnError (Just _) file offset =
      -- The handle of the parser is closed at this point. We need
      -- to reopen the file in 'AppendMode' now (parser opens with
      -- 'ReadMode').
      --
      -- Note that no file is open at this point, so we can safely
      -- open with 'AppendMode' any file, without the fear of opening
      -- multiple concurrent writers, which is not allowed, or concurrent
      -- read with truncate.
      --
      withFile hasFS file (AppendMode AllowExisting) $ \hndl ->
          hTruncate hasFS hndl (fromIntegral offset)

    -- | For each file in the db, this function parses, updates the
    -- internal state and calls itself for the rest of the files.
    go :: Index blockId
       -> ReverseIndex blockId
       -> SuccessorsIndex blockId
       -> Maybe (blockId, SlotNo)
       -> [(FileId, FsPath, FileSize)] -- ^ Info of files with < n blocks.
       -> [(FileId, FsPath)]
       -> m (InternalState blockId h)
    go currentMap currentRevMap succMap _maxSlot lessThanN [] = do
        hndl <- hOpen hasFS fileToWrite (AppendMode AllowExisting)
        return InternalState {
            _currentWriteHandle = hndl
          , _currentWritePath   = fileToWrite
          , _currentWriteId     = fdToWrite
          , _currentWriteOffset = unFileSize offset'
          , _nextNewFileId      = nextNewFileId'
          , _currentMap         = currentMap'
          , _currentRevMap      = currentRevMap
          , _currentSuccMap     = succMap
          , _currentMaxSlotNo   = FileInfo.maxSlotInFiles
                                    (Index.elems currentMap')
          }
      where
        (fileToWrite, fdToWrite, currentMap', offset') =
          case lastFile of
            Nothing ->
              -- The db is empty. Create a new file with 'FileId' 0.
              newFileInfo currentMap 0
            Just (lastFd, _) ->
              case find (\(fileId, _, _) -> fileId == lastFd) lessThanN of
                Nothing ->
                  -- If the last file is full, we need to create a new one.
                  newFileInfo currentMap $ lastFd + 1
                Just (wrFd, wrFile, size) ->
                  -- If the last file is not full, then this is the file
                  -- we're looking for.
                  (wrFile, wrFd, currentMap, size)
        nextNewFileId' = fdToWrite + 1

    go currentMap currentRevMap succMap maxSlot lessThanN ((fd, file):rest) = do
        (blocks, mErr) <- getParsedInfo parser file
        updateAndGo blocks mErr
      where
        -- | Updates the state and call 'go' for the rest of the files.
        updateAndGo :: [(SlotOffset, (BlockSize, BlockInfo blockId))]
                    -> Maybe e
                    -> m (InternalState blockId h)
        updateAndGo blocks mErr = do
            truncateOnError mErr file offset
            newRevMap <- fromEither err $ reverseMap file currentRevMap fileMap
            go newMap newRevMap newSuccMap newMaxSlot newHaveLessThanN rest
          where
            offset = case reverse blocks of
              [] -> 0
              (slotOffset, (blockSize,_)) : _ ->
                -- The file offset is given by the offset of the last
                -- block plus its size.
                slotOffset + unBlockSize blockSize
            fileMap = Map.fromList blocks
            (fileInfo, maxSlotOfFile) = FileInfo.fromParsedInfo blocks
            newMap = Index.insert fd fileInfo currentMap
            newMaxSlot = maxSlotList $ catMaybes [maxSlot, maxSlotOfFile]
            -- For each block we need to update the succesor Map of its
            -- predecesor.
            newSuccMap = foldr
              (\(_,(_, blockInfo)) succMap' ->
                insertMapSet succMap' (bbid blockInfo, bpreBid blockInfo))
              succMap
              blocks
            newHaveLessThanN = if FileInfo.isFull n fileInfo
              then lessThanN
              else (fd, file, FileSize offset) : lessThanN

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyState :: forall blockId m r. (HasCallStack, IOLike m)
            => VolatileDBEnv m blockId
            -> (forall h
               .  HasFS m h
               -> InternalState blockId h
               -> m (InternalState blockId h, r)
               )
            -> m r
modifyState VolatileDBEnv{_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (tryVolDB hasFsErr _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (_, r) -> return r
  where
    ErrorHandling{..} = _dbErr
    HasFS{..}         = hasFS

    open :: m (OpenOrClosed blockId h)
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    open = uninterruptibleMask_ $ takeMVar _dbInternalState

    close
      :: OpenOrClosed blockId h
      -> ExitCase (Either VolatileDBError (InternalState blockId h, r))
      -> m ()
    close mst ec = do
        -- It is crucial to replace the TMVar.
        putMVar _dbInternalState mst'
        followUp
      where
        (mst', followUp) = case ec of
          -- If we were interrupted, restore the original state.
          ExitCaseAbort                         -> (mst, return ())
          ExitCaseException _ex                 -> (mst, return ())
          -- In case of success, update to the newest state.
          ExitCaseSuccess (Right (newState, _)) ->
            (VolatileDbOpen newState, return ())
          -- In case of an error (not an exception), close the DB for safety.
          ExitCaseSuccess (Left _)              ->
            (VolatileDbClosed, closeOpenHandle mst)

    mutation :: OpenOrClosed blockId h
             -> m (InternalState blockId h, r)
    mutation VolatileDbClosed          = throwError $ UserError ClosedDBError
    mutation (VolatileDbOpen oldState) = action hasFS oldState

    -- TODO what if this fails?
    closeOpenHandle :: OpenOrClosed blockId h -> m ()
    closeOpenHandle VolatileDbClosed                    = return ()
    closeOpenHandle (VolatileDbOpen InternalState {..}) =
      wrapFsError hasFsErr _dbErr $ hClose _currentWriteHandle

-- | Gets part of the 'InternalState', without modifying it.
getter :: IOLike m
       => (forall h. InternalState blockId h -> a)
       -> VolatileDBEnv m blockId
       -> m a
getter fromSt VolatileDBEnv{..} = do
    mSt <- readMVar _dbInternalState
    case mSt of
      VolatileDbClosed  -> EH.throwError _dbErr $ UserError ClosedDBError
      VolatileDbOpen st -> return $ fromSt st

-- | Gets part of the 'InternalState' in 'STM'.
getterSTM :: forall m blockId a. IOLike m
          => (forall h. InternalState blockId h -> a)
          -> VolatileDBEnv m blockId
          -> STM m a
getterSTM fromSt VolatileDBEnv{..} = do
    mSt <- readMVarSTM _dbInternalState
    case mSt of
      VolatileDbClosed  -> EH.throwError' _dbErrSTM $ UserError ClosedDBError
      VolatileDbOpen st -> return $ fromSt st

-- | For each block found in a parsed file, we insert its 'InternalBlockInfo'.
-- If the block is already found in the 'ReverseIndex' or is duplicated in the
-- same file, we abort and return an error.
reverseMap :: forall blockId. (
                Ord      blockId
              , Typeable blockId
              , Show     blockId
              )
           => FsPath
           -> ReverseIndex blockId
           -> Map SlotOffset (BlockSize, BlockInfo blockId)
           -> Either VolatileDBError (ReverseIndex blockId)
reverseMap file revMap mp = foldM go revMap (Map.toList mp)
  where
    go :: ReverseIndex blockId
       -> (SlotOffset, (BlockSize, BlockInfo blockId))
       -> Either VolatileDBError (ReverseIndex blockId)
    go rv (offset, (size, BlockInfo {..})) = case Map.lookup bbid rv of
        Nothing -> Right $ Map.insert bbid internalBlockInfo rv
        Just blockInfo -> Left $ UnexpectedError . ParserError
          $ DuplicatedSlot bbid file (ibFile blockInfo)
      where
        internalBlockInfo = InternalBlockInfo {
            ibFile         = file
          , ibSlotOffset   = offset
          , ibBlockSize    = size
          , ibSlot         = bslot
          , ibPreBid       = bpreBid
          , ibIsEBB        = bisEBB
          , ibHeaderOffset = bheaderOffset
          , ibHeaderSize   = bheaderSize
          }
