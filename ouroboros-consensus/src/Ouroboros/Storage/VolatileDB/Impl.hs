{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
-- Volatile on-disk database of binary blobs
--
-- Logic
--
-- The db is a key-value store of binary blocks and is parametric
-- on the key of blocks, named blockId. The only constraints are that one must provide
-- a function (toSlot :: blockId -> SlotNo), as well as an Ord instance of blockId.
-- The db expects NO properties for this Ord instance, not even one that makes
-- toBlock monotonic.

-- The database uses in memory indexes, which are created on each reopening. reopening
-- includes parsing all blocks of the dbFolder, so it can be an expensive operation
-- if the database gets big. That's why the intention of this db is to be used for only
-- the tip of the blockchain, when there is still volatility on which blocks are included.
-- The db is agnostic to the format of the blocks, so a parser must be provided.
-- In addition to getBlock and putBlock, the db provides also the ability to garbage-collect
-- old blocks. The actual garbage-collection happens in terms of files and not blocks: a file
-- is deleted/garbage-collected only if its latest block is old enough. A block is old enough
-- if its toSlot value is old enough and not based on its Ord instance. This type of garbage
-- collection makes the deletion of blocks depend on the number of blocks we insert on
-- each file, as well as the order of insertion, so it's not deterministic on blocks themselves.
--
-- Errors
--
-- On any exception or error the db closes and its Internal State is lost, inluding in memory
-- indexes. We try to make sure that even on errors the fs represantation of the db remains
-- consistent and the Internal State can be recovered on reopening. In general we try to make
-- sure that at any point, losing the in-memory Internal State is not fatal to the db as it can
-- recovered. This is important since we must always expect unexpected shutdowns, power loss,
-- sleep mode etc. This is achived by leting only basic operations on the db:
-- + putBlock only appends a new block on a file. Losing an update means we only lose a block,
--   which can be recovered.
-- + garbage collect deletes only whole files.
-- + there is no modify block operation. Thanks to that we need not keep any rollback journals
--   to make sure we are safe in case of unexpected shutdowns.
--
-- Concurrency
--
-- The same db should only be opened once
-- Multiple threads can share the same db as concurency if fully supported.
--
-- FS Layout:
--
-- On disk represantation is as follows:
--
--  dbFolder\
--    blocks-0.dat
--    blocks-1.dat
--    ...
--
--  If on opening any other filename which does not follow blocks-i.dat is found
--  an error is raised. The Ordering of blocks is not guarranteed to be followed,
--  files can be garbage-collected.
--
--  Each file stores a fixed number of slots, specified by _maxBlocksPerFile.
--  If the db finds files with less blocks than this max, it will start appending
--  to the newest of them, if it's the newest of all files. If it's not the newest
--  of all files it will create a new file to append blocks..
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
    , filePath
    , getInternalState
    , openDBFull
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import           Data.List (sortOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack
import qualified System.IO as IO

import           Ouroboros.Consensus.Util (SomePair (..))

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
  Main Types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h. VolatileDBEnv {
      _dbHasFS          :: !(HasFS m h)
    , _dbErr            :: !(ErrorHandling (VolatileDBError blockId) m)
    , _dbInternalState  :: !(TMVar m (Maybe (InternalState blockId h)))
    , _maxBlocksPerFile :: !Int
    , _parser           :: !(Parser (ParserError blockId) m blockId)
    , _toSlot           :: (blockId -> SlotNo)
    }

data InternalState blockId h = InternalState {
      _currentWriteHandle :: !h -- The unique open file we append blocks.
    , _currentWritePath   :: !String -- The path of the file above.
    , _currentWriteOffset :: !Word64 -- The 'WriteHandle' for the same file.
    , _nextWriteFiles     :: ![(String, Word)] -- The path and the size of the next files to write
    , _nextNewFileId      :: !Int -- The next file name Id.
    , _currentBlockId     :: !(Maybe blockId) -- The newest block in the db.
    , _currentMap         :: !(Index blockId) -- The content of each file.
    , _currentRevMap      :: !(ReverseIndex blockId) -- Where to find each block from slot.
    , _currentSuccMap     :: !(SuccessorsIndex blockId) -- successors for each block.
    }

{------------------------------------------------------------------------------
  VolatileDB API
------------------------------------------------------------------------------}

openDB :: (HasCallStack, MonadCatch m, MonadSTM m, Ord blockId)
       => HasFS m h
       -> ErrorHandling (VolatileDBError blockId) m
       -> Parser (ParserError blockId) m blockId
       -> Int
       -> (blockId -> SlotNo)
       -> m (VolatileDB blockId m)
openDB h e p m t = fst <$> openDBFull h e p m t

openDBFull :: (HasCallStack, MonadCatch m, MonadSTM m, Ord blockId)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> Parser (ParserError blockId) m blockId
           -> Int
           -> (blockId -> SlotNo)
           -> m (VolatileDB blockId m, VolatileDBEnv m blockId)
openDBFull hasFS err parser maxBlocksPerFile toSlot = do
    env <- openDBImpl hasFS err parser maxBlocksPerFile toSlot
    let db = VolatileDB {
          closeDB        = closeDBImpl  env
        , isOpenDB       = isOpenDBImpl env
        , reOpenDB       = reOpenDBImpl env
        , getBlock       = getBlockImpl env
        , putBlock       = putBlockImpl toSlot env
        , garbageCollect = garbageCollectImpl env
        , getIsMember    = getIsMemberImpl env
        , getBlockIds    = getBlockIdsImpl env
        , getSuccessors  = getSuccessorsImpl env
        }
    return (db, env)

-- After opening the db once, the same @maxBlocksPerFile@ must be provided all
-- next opens.
openDBImpl :: (HasCallStack, MonadThrow m, MonadSTM m, Ord blockId)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> Parser (ParserError blockId) m blockId
           -> Int
           -> (blockId -> SlotNo)
           -> m (VolatileDBEnv m blockId)
openDBImpl hasFS@HasFS{..} err parser maxBlocksPerFile toSlot =
    if maxBlocksPerFile <= 0
    then EH.throwError err $ InvalidArgumentsError "maxBlocksPerFile can't be 0"
    else do
        st <- mkInternalStateDB hasFS err parser maxBlocksPerFile toSlot
        stVar <- atomically $ newTMVar $ Just st
        return $ VolatileDBEnv hasFS err stVar maxBlocksPerFile parser toSlot

closeDBImpl :: (MonadSTM m)
            => VolatileDBEnv m blockId
            -> m ()
closeDBImpl VolatileDBEnv{..} = do
        mbInternalState <- atomically (swapTMVar _dbInternalState Nothing)
        case mbInternalState of
            Nothing -> return ()
            Just InternalState{..} ->
                hClose _currentWriteHandle
  where
    HasFS{..} = _dbHasFS

isOpenDBImpl :: (MonadSTM m)
             => VolatileDBEnv m blockId
             -> m Bool
isOpenDBImpl VolatileDBEnv{..} = do
    mSt <- atomically (readTMVar _dbInternalState)
    return $ isJust mSt

-- closeDB . reOpenDB is a no-op. This is achieved because when we reOpen
-- we try to append on the latest created file.
reOpenDBImpl :: (HasCallStack, MonadCatch m, MonadSTM m, Ord blockId)
             => VolatileDBEnv m blockId
             -> m ()
reOpenDBImpl VolatileDBEnv{..} = do
    modifyTMVar _dbInternalState $ \mbSt -> case mbSt of
        Just (st@InternalState{..}) -> return (Just st, ())
        Nothing -> do
            st <- mkInternalStateDB _dbHasFS _dbErr _parser _maxBlocksPerFile _toSlot
            return (Just st, ())

getBlockImpl :: (MonadSTM m, MonadCatch m, Ord blockId)
             => VolatileDBEnv m blockId
             -> blockId
             -> m (Maybe ByteString)
getBlockImpl env@VolatileDBEnv{..} slot = do
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} -> do
        case Map.lookup slot _currentRevMap of
            Nothing -> return (st, Nothing)
            Just (file, w, n, _) ->  do
                bs <- withFile hasFS [file] IO.ReadMode $ \hndl -> do
                        _ <- hSeek hndl IO.AbsoluteSeek (fromIntegral w)
                        hGet hndl (fromIntegral n)
                return (st, Just bs)

-- This function follows the approach:
-- (1) hPut bytes to the file
-- (2) if full hClose the write file
-- (3)         hOpen a new write file
-- (4) update the Internal State.
-- If there is an error after (1) or after (2) we should make sure that when we reopen a db from scratch,
-- it can succesfully recover if it does not find an empty file to write and all other files are full.
-- We should also make sure that the fs can be recovered if we get an exception/error at any moment
-- and that we are left with an empty Internal State.
-- We should be careful about not leaking open fds when we open a new file, since this can affect garbage
-- collection of files.
putBlockImpl :: forall m. (MonadCatch m, MonadSTM m)
             => forall blockId. (Ord blockId)
             => (blockId -> SlotNo)
             -> VolatileDBEnv m blockId
             -> blockId
             -> blockId
             -> BS.Builder
             -> m ()
putBlockImpl toSlot env@VolatileDBEnv{..} blockId preBlockId builder = do
    modifyState env $ \hasFS@HasFS{..} st@InternalState{..} -> do
        case Map.lookup blockId _currentRevMap of
            Just _ -> return (st, ()) -- trying to put an existing blockId is a no-op.
            Nothing -> do
                let (mmaxSlot, nBlocks, fileMp) = fromMaybe
                        (error "Volatile db invariant violation: Current write file not found in Index.")
                        (Map.lookup _currentWritePath _currentMap)
                bytesWritten <- hPut _currentWriteHandle builder
                let fileMp' = Map.insert _currentWriteOffset (fromIntegral bytesWritten, blockId) fileMp
                    nBlocks' = nBlocks + 1
                    mp = Map.insert _currentWritePath (updateSlotNoBlockId mmaxSlot [toSlot blockId], nBlocks', fileMp') _currentMap
                    revMp = Map.insert blockId (_currentWritePath, _currentWriteOffset, fromIntegral bytesWritten, preBlockId) _currentRevMap
                    blockId' = fst <$> updateSlot toSlot _currentBlockId [blockId]
                    st' = st {
                          _currentWriteOffset = _currentWriteOffset + fromIntegral bytesWritten
                        , _currentBlockId     = blockId'
                        , _currentMap         = mp
                        , _currentRevMap      = revMp
                        , _currentSuccMap     = insertMapSet _currentSuccMap (blockId, preBlockId)
                    }
                if nBlocks' < fromIntegral _maxBlocksPerFile
                then return (st', ())
                else (\s -> (s,())) <$> nextFile hasFS _dbErr env st'

-- The approach we follow here is to try to garbage collect each file.
-- For each file we update the fs and then we update the Internal State.
-- If some fs update fails, we are left with an empty Internal State and a subset
-- of the deleted files in fs. Any unexpected failure (power loss, other exceptions)
-- has the same results, since the Internal State will be empty on re-opening.
-- This is ok only if any fs updates leave the fs in a consistent state every moment.
-- This approach works since we always close the Database in case of errors,
-- but we should rethink it if this changes in the future.
garbageCollectImpl :: forall m blockId. (MonadCatch m, MonadSTM m, Ord blockId)
                   => VolatileDBEnv m blockId
                   -> SlotNo
                   -> m ()
garbageCollectImpl env@VolatileDBEnv{..} slot = do
    modifyState env $ \hasFS st -> do
        st' <- foldM (tryCollectFile hasFS env slot) st (sortOn (unsafeParseFd . fst) $ Map.toList (_currentMap st))
        let currentSlot' = if Map.size (_currentMap st') == 0 then Nothing else (_currentBlockId st')
        let st'' = st'{_currentBlockId = currentSlot'}
        return (st'', ())

-- For the given file, we check if it should be garbage collected.
-- At the same time we return the updated InternalState.
-- Important note here is that, every call should leave the fs in a
-- consistent state, without depending on other calls.
-- This is achieved so far, since fs calls are reduced to
-- removeFile and truncate 0.
tryCollectFile :: forall m h blockId. (MonadThrow m, Ord blockId)
               => HasFS m h
               -> VolatileDBEnv m blockId
               -> SlotNo
               -> InternalState blockId h
               -> (String, (Maybe SlotNo, Int, Map Word64 (Word64, blockId)))
               -> m (InternalState blockId h)
tryCollectFile hasFS@HasFS{..} env@VolatileDBEnv{..} slot st@InternalState{..} (file, (mmaxSlot, _, fileMp)) =
    let isLess       = not $ cmpMaybe mmaxSlot slot
        isCurrent    = file == _currentWritePath
        isCurrentNew = _currentWriteOffset == 0
        bids = snd <$> Map.elems fileMp
        rv' = Map.withoutKeys _currentRevMap (Set.fromList bids)
        deletedPairs = mapMaybe (\b -> (b,) . forth <$> Map.lookup b _currentRevMap) bids
        succMap' = foldl deleteMapSet _currentSuccMap deletedPairs
    in if   | not isLess    -> return st
            | not isCurrent -> do
                removeFile [file]
                return st { _currentMap = Map.delete file _currentMap
                          , _currentRevMap = rv'
                          , _currentSuccMap = succMap'
                          }
            | isCurrentNew  -> return st
            | True          -> do
                st' <- reOpenFile hasFS _dbErr env st
                return st' { _currentRevMap = rv'
                           , _currentSuccMap = succMap'
                           }

getInternalState :: forall m blockId. MonadSTM m
                 => VolatileDBEnv m blockId
                 -> m (SomePair (HasFS m) (InternalState blockId))
getInternalState VolatileDBEnv{..} = do
    mSt <- atomically (readTMVar _dbInternalState)
    case mSt of
        Nothing -> EH.throwError _dbErr ClosedDBError
        Just st -> return (SomePair _dbHasFS st)

getIsMemberImpl :: forall m blockId. (MonadSTM m, Ord blockId)
                => VolatileDBEnv m blockId
                -> m (blockId -> Bool)
getIsMemberImpl VolatileDBEnv{..} = do
    mSt <- atomically (readTMVar _dbInternalState)
    case mSt of
        Nothing -> EH.throwError _dbErr ClosedDBError
        Just st -> return (\bid -> Map.member bid (_currentRevMap st))

getBlockIdsImpl :: forall m blockId. (MonadSTM m, Ord blockId)
                => VolatileDBEnv m blockId
                -> m [blockId]
getBlockIdsImpl VolatileDBEnv{..} = do
    mSt <- atomically (readTMVar _dbInternalState)
    case mSt of
        Nothing -> EH.throwError _dbErr ClosedDBError
        Just st -> return $ Map.keys $ _currentRevMap st

getSuccessorsImpl :: forall m blockId. (MonadSTM m, Ord blockId)
                  => VolatileDBEnv m blockId
                  -> blockId
                  -> m (Set blockId)
getSuccessorsImpl VolatileDBEnv{..} blockId = do
    mSt <- atomically (readTMVar _dbInternalState)
    case mSt of
        Nothing -> EH.throwError _dbErr ClosedDBError
        Just st -> return $
            fromMaybe Set.empty (Map.lookup blockId (_currentSuccMap st))

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- db first tries to fill files from _nextWriteFiles list.
-- If none find it creates new ones.
nextFile :: forall h m. MonadSTM m
         => forall blockId. Ord blockId
         => HasFS m h
         -> ErrorHandling (VolatileDBError blockId) m
         -> VolatileDBEnv m blockId
         -> InternalState blockId h
         -> m (InternalState blockId h)
nextFile HasFS{..} _err VolatileDBEnv{..} st = do
    hClose $ _currentWriteHandle st
    case _nextWriteFiles st of
        [] -> do
            let file = filePath $ _nextNewFileId st
            -- TODO(kde) check if file exists already. Issue #292
            hndl <- hOpen [file] IO.AppendMode
            return $ st {
                  _currentWriteHandle = hndl
                , _currentWritePath   = file
                , _currentWriteOffset = 0
                , _nextNewFileId      = (_nextNewFileId st) + 1
                , _currentMap         = Map.insert file (Nothing, 0, Map.empty) (_currentMap st)
            }
        (file, size) : rest -> do
            -- This file should already exist, so it does not fall under issue 292
            hndl <- hOpen [file] IO.AppendMode
            return $ st {
                  _currentWriteHandle = hndl
                , _currentWritePath   = file
                , _currentWriteOffset = fromIntegral size
                , _nextWriteFiles     = rest
            }


reOpenFile :: forall m h blockId. (MonadThrow m)
           => HasFS m h
           -> ErrorHandling (VolatileDBError blockId) m
           -> VolatileDBEnv m blockId
           -> InternalState blockId h
           -> m (InternalState blockId h)
reOpenFile HasFS{..} _err VolatileDBEnv{..} st@InternalState{..} = do
    -- The manual for truncate states that it does not affect offset.
    -- However the file is open on Append Only, so it should automatically go to the end
    -- before each write.
   hTruncate _currentWriteHandle 0
   return $ st {
         _currentMap = Map.insert _currentWritePath (Nothing, 0, Map.empty) _currentMap
       , _currentWriteOffset = 0
    }

mkInternalStateDB :: (HasCallStack, MonadThrow m, Ord blockId)
                  => HasFS m h
                  -> ErrorHandling (VolatileDBError blockId) m
                  -> Parser (ParserError blockId) m blockId
                  -> Int
                  -> (blockId -> SlotNo)
                  -> m (InternalState blockId h)
mkInternalStateDB hasFS@HasFS{..} err parser maxBlocksPerFile toSlot = do
    allFiles <- do
        createDirectoryIfMissing True []
        listDirectory []
    mkInternalState hasFS err parser maxBlocksPerFile allFiles toSlot

mkInternalState :: forall blockId m h. (MonadThrow m, HasCallStack, Ord blockId)
                => HasFS m h
                -> ErrorHandling (VolatileDBError blockId) m
                -> Parser (ParserError blockId) m blockId
                -> Int
                -> Set String
                -> (blockId -> SlotNo)
                -> m (InternalState blockId h)
mkInternalState hasFS@HasFS{..} err parser n files toSlot = do
    lastFd <- findNextFd err files
    let
        go :: Index blockId
           -> ReverseIndex blockId
           -> SuccessorsIndex blockId
           -> Maybe blockId
           -> [(FileId, String, FileSize)] -- The relative path and size of the files with less than n blocks, if any found already.
           -> [String]
           -> m (InternalState blockId h)
        go mp revMp succMp mBlockId haveLessThanN leftFiles = case leftFiles of
            [] -> do
                (fileToWrite, nextWriteFiles', nextNewFileId', mp', offset') <- case (sortOn (\(a,_,_) -> a) haveLessThanN, lastFd) of
                        ([], Nothing) -> return (filePath 0, [], 1, Map.insert (filePath 0) (Nothing, 0, Map.empty) mp, 0)
                        (_, Nothing) ->
                            error $ "Volatile db invariant violation: A file was found with less than " <> show n <>
                                    " blocks, but there are no files parsed."
                        ([], Just lst) -> let fd' = lst + 1 in
                            -- If all files are full, we just open a new file.
                            return (filePath fd', [], lst + 2, Map.insert (filePath fd') (Nothing, 0, Map.empty) mp, 0)
                        (ls, Just lst) ->
                            -- last can't fail because the list is not empty.
                            let (fd,wrfile,size) = last ls
                            in if fd == lst then
                                return (wrfile, [], lst + 1, mp, size)
                               else let fd' = lst + 1 in
                                -- If it's not the last file, we just ignore it and open a
                                -- new one.
                                return (filePath fd', [], lst + 2, Map.insert (filePath fd') (Nothing, 0, Map.empty) mp, 0)
                -- TODO(kde) note that this file may or may not exist. Issue #292
                hndl <- hOpen [fileToWrite] IO.AppendMode
                return $ InternalState {
                      _currentWriteHandle = hndl
                    , _currentWritePath   = fileToWrite
                    , _currentWriteOffset = offset'
                    , _nextWriteFiles     = nextWriteFiles'
                    , _nextNewFileId      = nextNewFileId'
                    , _currentBlockId     = mBlockId
                    , _currentMap         = mp'
                    , _currentRevMap      = revMp
                    , _currentSuccMap     = succMp
                }
            file : restFiles -> do
                let path = [file]
                (ls, mErr) <- parse parser path
                let offset = case ls of
                        [] -> 0
                        _  -> let (so,(bs, _, _)) = last ls in so + bs
                let fileMp = Map.fromList ls
                case mErr of
                    Nothing -> return ()
                    Just (DecodeFailed _str _n) -> do
                        -- the handle of the parser is closed at this point. We need to
                        -- reOpen the file in AppendMode now (parser opens with ReadMode).
                        -- Note that no file is open at this point, so we can safely open
                        -- with AppendMode any file, without the fear of opening multiple
                        -- concurrent writers, which is not allowed.

                        -- TODO(kde) we should add a Warning log here.
                        withFile hasFS path IO.AppendMode $ \hndl ->
                            hTruncate hndl (fromIntegral offset)
                        return ()
                    Just e -> EH.throwError err $ VParserError e
                let fileMpNoPred = firstTwo <$> fileMp
                let mMaxBlockId = fst <$> maxSlotMap fileMpNoPred toSlot
                let nBlocks = Map.size fileMp
                newRevMp <- fromEither err $ reverseMap file revMp fileMp
                let newMp = Map.insert file (toSlot <$> mMaxBlockId, nBlocks, fileMpNoPred) mp
                let newSlot = updateSlot toSlot mBlockId (snd <$> (concatMap Map.elems $ (\(_, _, x) -> x) <$> Map.elems newMp))
                let newSuccMp = foldr (\(_,(_, b, bpred)) pm -> insertMapSet pm (b, bpred)) succMp ls
                -- error here is reasonable because we have already checked that all filenames parse.
                let fd = fromMaybe (error $ "file name " <> file <> " failed to parse") (parseFd file)
                let newHaveLessThanN = if nBlocks >= n
                        then haveLessThanN
                        else (fd, file, offset) : haveLessThanN
                go newMp newRevMp newSuccMp (fst <$> newSlot) newHaveLessThanN restFiles
    go Map.empty Map.empty Map.empty Nothing [] (Set.toList files)


modifyState :: forall blockId m r. (HasCallStack, MonadSTM m, MonadCatch m)
            => VolatileDBEnv m blockId
            -> (forall h. HasFS m h -> (InternalState blockId h) -> m (InternalState blockId h, r))
            -> m r
modifyState VolatileDBEnv{_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (EH.try _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (_, r) -> return r
  where
    ErrorHandling{..} = _dbErr
    HasFS{..}         = hasFS

    open :: m (Maybe (InternalState blockId h))
    open = atomically $ takeTMVar _dbInternalState

    close :: Maybe (InternalState blockId h)
          -> ExitCase (Either (VolatileDBError blockId) (InternalState blockId h, r))
          -> m ()
    close mst ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> atomically $ putTMVar _dbInternalState mst
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        atomically $ putTMVar _dbInternalState Nothing
        closeOpenHandle mst
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (newState, _)) ->
        atomically $ putTMVar _dbInternalState (Just newState)
      -- In case of an error (not an exception), close the DB for safety
      ExitCaseSuccess (Left _) -> do
        atomically $ putTMVar _dbInternalState Nothing
        closeOpenHandle mst

    mutation :: Maybe (InternalState blockId h)
             -> m (InternalState blockId h, r)
    mutation Nothing         = throwError ClosedDBError
    mutation (Just oldState) = action hasFS oldState

    -- TODO what if this fails?
    closeOpenHandle :: Maybe (InternalState blockId h) -> m ()
    closeOpenHandle Nothing                   = return ()
    closeOpenHandle (Just InternalState {..}) = hClose _currentWriteHandle


reverseMap :: forall blockId. Ord blockId
           => String
           -> ReverseIndex blockId
           -> Map SlotOffset (BlockSize, blockId, blockId)
           -> Either (VolatileDBError blockId) (ReverseIndex blockId)
reverseMap file revMp mp = foldM f revMp (Map.toList mp)
    where
        f :: ReverseIndex blockId
          -> (SlotOffset, (BlockSize, blockId, blockId))
          -> Either (VolatileDBError blockId) (Map blockId (String, SlotOffset, Word64, blockId))
        f rv (w, (n, slot, preBid)) = case Map.lookup slot revMp of
            Nothing -> Right $ Map.insert slot (file, w, n, preBid) rv
            Just (file', _w', _n', _) -> Left $ VParserError
                $ DuplicatedSlot $ Map.fromList [(slot, ([file], [file']))]

-- Throws an error if one of the given file names does not parse.
findNextFd :: forall m blockId. Monad m
           => ErrorHandling (VolatileDBError blockId) m
           -> Set String
           -> m (Maybe FileId)
findNextFd err files = foldM go Nothing files
    where
        maxMaybe :: Ord a => Maybe a -> a -> a
        maxMaybe ma a = case ma of
            Nothing -> a
            Just a' -> max a' a
        go :: Maybe FileId -> String -> m (Maybe FileId)
        go fd file = case parseFd file of
            Nothing -> EH.throwError err $ VParserError $ InvalidFilename file
            Just fd' -> return $ Just $ maxMaybe fd fd'
