{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Immutable on-disk database of binary blobs
--
-- = Logical format
--
-- The database consists of a series of consecutive epochs (identified by
-- 'Epoch'), starting from 0. Each epoch has a size ('EpochSize'), which
-- indicates how many relative slots ('RelativeSlot') it has available. Epochs
-- do not necessarily have the same size. Do not confuse a 'RelativeSlot' with
-- a 'Ouroboros.Network.Block.Slot', the former is used to identify slots
-- within an epoch whereas the latter identifies a slot on the whole chain,
-- across epochs. The numbering of relative slots restarts with each epoch. A
-- 'Ouroboros.Network.Block.Slot' can be converted to a combination of an
-- epoch and a relative slot, given that the size of each (preceding) epoch is
-- known. For convenience, we do not use 'Ouroboros.Network.Block.Slot'
-- directly in this module to identify slots, but 'EpochSlot', which is the
-- product of an 'Epoch' and a 'RelativeSlot'.
--
-- For example:
--
-- > Epochs:         <──────── 0 ────────> <────── 1 ──────>
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > Relative slots:   0   1   2   3   4     0   1   2   3
-- > Slots:            0   1   2   3   4     5   6   7   8
--
--
-- = Functionality
--
-- After opening the database, preferably with 'withDB', you can append new
-- binary blobs to relative slots in the opened epoch with 'appendBinaryBlob'.
-- The database is append-only, so you cannot append a blob to a relative slot
-- in the past. You can, however, skip relative slots, e.g., append to slot 0
-- and then to slot 5, but afterwards, you can no longer append to slots 1-4.
-- You can only store at most one blob in each relative slot, you cannot
-- append multiple blobs to one relative slot. It is not allowed to store an
-- empty blob (implementation note: we would need a way to distinguish a slot
-- filled with an empty blob from an unfilled slot).
--
-- To find out the next available relative slot (and epoch), you can use
-- 'getNextEpochSlot'. This is the next slot that can be appended to.
--
-- For example, starting with an empty epoch 0 with size 5:
--
-- > <──────── 0 ────────>
-- > ┌───┬───┬───┬───┬───┐
-- > │   │   │   │   │   │
-- > └───┴───┴───┴───┴───┘
-- >   0   1   2   3   4
--
-- 'getNextEpochSlot' will return @'EpochSlot' 0 0@. If we now use
-- 'appendBinaryBlob' to append the blob \"a\" to @'EpochSlot' 0 0@ and \"b\"
-- @'EpochSlot' 0 3@, we get:
--
-- > <──────── 0 ────────>
-- > ┌───┬───┬───┬───┬───┐
-- > │ a │   │   │ b │   │
-- > └───┴───┴───┴───┴───┘
-- >   0   1   2   3   4
--
-- Slots 1 and 2 are empty and can never be appended to again (neither can 0
-- and 3 be appended to again). 'getNextEpochSlot' will now return
-- @'EpochSlot' 0 4@.
--
-- The function 'startNewEpoch' finalises the current epoch (full or not) and
-- starts a new empty epoch with the given size.
--
-- For example, continuing with the last example, starting a new epoch with
-- size 4 gives us:
--
-- > <──────── 0 ────────> <────── 1 ──────>
-- > ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- > │ a │   │   │ b │   │ │   │   │   │   │
-- > └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- >   0   1   2   3   4     0   1   2   3
--
-- Now epoch 0 is finalised and can never again be appended to.
-- 'getNextEpochSlot' will now return @'EpochSlot' 1 0@.
--
-- Besides appending blobs to the database, you can also retrieve blobs from
-- the database using 'getBinaryBlob'. Given an 'EpochSlot', it will retrieve
-- the binary blob at that location in the database. In case the 'EpochSlot'
-- pointed to an unfilled slot, 'Nothing' is returned. Naturally, only blobs
-- in the past (< 'getNextEpochSlot') can be retrieved.
--
-- To efficiently iterate over the binary blobs stored in the database within
-- a certain range, indicated with two 'EpochSlot's, you can use
-- 'streamBinaryBlobs' that gives you an 'Iterator' with its own API.
--
-- You can use 'isOpen' to check whether a database is open or closed.
--
-- You can use 'getDBFolder' to find out in which folder the database was
-- opened.
--
-- The database can be validated using the 'validateDB' function.
--
-- = (Re)opening the database
--
-- The database can be closed and reopened. However, you cannot reopen the
-- database on a finalised epoch. It is possible to reopen the same epoch the
-- database was appending to the last time it was open. It is not allowed to
-- skip epochs when reopening the database. When opening a database, an error
-- will be thrown if any of the files corresponding to past epochs are
-- missing. The contents of these files are not checked, you can use
-- 'validateDB' for this.
--
--
-- = Errors
--
-- Whenever a 'FileSystemError' is thrown during a write operation
-- ('appendBinaryBlob', 'startNewEpoch'), the database will be automatically
-- closed because we can not guarantee a consistent state in the face of file
-- system errors. The open database files are truncated to their last valid
-- state, i.e. the state before the failed write operation. The userq is
-- advised to use 'validateDB' and before reopening the database.
--
--
-- = Concurrency
--
-- The same database should not be opened multiple times concurrently.
--
-- TODO Should we ensure this with a lock file?
-- https://hackage.haskell.org/package/filelock-0.1.1.2/docs/System-FileLock.html
--
-- However, an opened database is thread-safe.
--
--
-- = Layout on disk
--
-- The database is structured on disk as follows:
--
-- > dbfolder/
-- >   epoch-000.dat
-- >   index-000.dat
-- >   ..
-- >   epoch-008.dat
-- >   index-008.dat
--
-- For each epoch, there are two files on disk:
--
--   * An \"epoch file\" that stores the actual binary blobs.
--
--   * An \"index file\" that stores the offsets of the binary blobs. These
--     are used to efficiently seek within the epoch file.
--
-- == Index file layout
--
-- The index file has the following layout:
--
-- > ┌────────┬────────┬────────┬┄┄┄┄┄
-- > │offset 0│offset 1│offset 2│ ...
-- > └────────┴────────┴────────┴┄┄┄┄┄
--
-- Where each @offset i@ is the offset in the epoch file where relative slot
-- @i@ starts. Each @offset i@ is a 'Word64' (8 bytes).
--
-- For example, say we have written \"a\" to relative slot 0, \"bravo\" to
-- relative slot 1, and \"haskell\" to slot 4. We get the following index file
-- (only the row with @offset@ is stored):
--
-- > slot:     0   1   2   3   4    5
-- >         ┌───┬───┬───┬───┬───┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┘
--
-- Note that the last slot we appended to was the 5th slot, the slot with
-- index 4, but there are 6 offsets in the index. In other words, the index
-- contains @slots + 1@ entries (or @lastSlot + 2@). The last offset is the
-- offset for the next slot that can be appended too and can also be used to
-- find out the size of the last binary blob.
--
-- When skipping slots (and thus creating unfilled slots) while appending, the
-- index is /backfilled/ to indicate that some slots are unfilled. The last
-- offset will be repeated (note the repeated offset 6 in the example above)
-- to indicate that these slots have size 0.
--
-- The first offset will always be 0 as the first binary blob will be written
-- to the start of the epoch file.
--
-- When starting a new epoch file ('startNewEpoch'), the index of the current
-- epoch will be padded to @epochSize + 1@ offsets to record that the slots
-- after the last slot that was appended to are empty.
--
-- For example, continuing with the index above, assuming the @epochSize@ is
-- 7, calling 'startNewEpoch' pads the index file to the following:
--
-- > slot:     0   1   2   3   4    5    6    7
-- >         ┌───┬───┬───┬───┬───┬────┬────┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │ 13 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┴────┴────┘
--
-- The last offset was repeated twice to indicate that slots 5 and 6 were
-- unfilled.
--
-- When closing the database, the index is not padded, because the database
-- may be reopened on the same epoch to continue appending to it.
--
module Ouroboros.Storage.ImmutableDB.Impl
  ( -- * Opening a database
    openDB
  ) where

import           Control.Monad (void, when, unless, forM_, zipWithM_)
import           Control.Monad.Catch (MonadMask, ExitCase(..), generalBracket,
                                      finally)
import           Control.Monad.Class.MonadSTM (MonadSTM(..))
import           Control.Monad.Except (ExceptT(ExceptT), runExceptT,
                                       throwError, catchError)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (isSuffixOf)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Set as Set

import           GHC.Stack (HasCallStack, callStack)

import           System.IO (IOMode(..), SeekMode(..))

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.Class.Types
import           Ouroboros.Storage.Util

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Util


{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}


-- | An opaque handle to an immutable database of binary blobs.
data ImmutableDBHandle m = ImmutableDBHandle
    { _dbInternalState :: !(TMVar m (Maybe (InternalState m)))
    , _dbFolder        :: !FsPath
    }

-- TODO needed for StateMachine, but otherwise it doesn't make much sense
instance Eq (ImmutableDBHandle m) where
  (==) = (==) `on` _dbFolder

data InternalState m = InternalState
    { _currentEpoch             :: !Epoch
    -- ^ The current 'Epoch' the immutable store is writing into.
    , _currentEpochWriteHandle  :: !(FsHandleE m)
    -- ^ The write handle for the current epoch file.
    , _currentIndexWriteHandle  :: !(FsHandleE m)
    -- ^ The write handle for the current index file.
    , _currentEpochOffsets      :: NonEmpty SlotOffset
    -- ^ The offsets to which blobs have been written in the current epoch
    -- file, stored from last to first.
    , _nextExpectedRelativeSlot :: !RelativeSlot
    -- ^ The next relative slot we expect to see to append data to the epoch
    -- file.
    --
    -- Invariant: we can't append new data passing as input a slot less than
    -- the expected one.
    , _epochSizes               :: !(Map Epoch EpochSize)
    -- ^ The size of all past epochs and the current epoch.
    --
    -- Invariant: for each @epoch@, if @epoch <= '_currentEpoch'@, then
    -- @'Map.elem' epoch '_epochSizes'@.
    , _nextIteratorID           :: !IteratorID
    -- ^ The ID of the next iterator that will be created.
    }

{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

-- | Open the database, creating it from scratch if the 'FsPath' points to a
-- non-existing directory.
--
-- A 'Map' with the size for each epoch (the past epochs and the most recent
-- one) must be passed. When a size is missing for an epoch, a
-- 'MissingEpochSizeError' error will be thrown.
--
-- The given 'Epoch' is opened for appending. If the given epoch was
-- previously finalised, an 'OpenFinalisedEpochError' error will be thrown. If
-- the epoch is not the first epoch (0), all previous epoch files must be
-- present on disk and have complete indices (excluding the opened epoch). If
-- an epoch or index file is missing, a 'MissingFileError' is thrown. The
-- actual contents of the epoch and index files are not checked.
--
-- __Note__: To be used in conjunction with 'withDB'.
openDB :: (HasCallStack, MonadSTM m, HasFSE m, MonadMask m)
       => FsPath -> Epoch -> Map Epoch EpochSize
       -> m (Either ImmutableDBError (ImmutableDB m))
openDB dbFolder epoch epochSizes = runExceptT $ do
    dbh <- ExceptT $ openDBImpl dbFolder epoch epochSizes
    return ImmutableDB
      { closeDB           = closeDBImpl           dbh
      , isOpen            = isOpenImpl            dbh
      , getNextEpochSlot  = getNextEpochSlotImpl  dbh
      , getDBFolder       = dbFolder
      , getBinaryBlob     = getBinaryBlobImpl     dbh
      , appendBinaryBlob  = appendBinaryBlobImpl  dbh
      , startNewEpoch     = startNewEpochImpl     dbh
      , streamBinaryBlobs = streamBinaryBlobsImpl dbh
      }


{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

openDBImpl :: forall m. (HasCallStack, MonadSTM m, HasFSE m, MonadMask m)
           => FsPath
           -> Epoch
           -> Map Epoch EpochSize
           -> m (Either ImmutableDBError (ImmutableDBHandle m))
openDBImpl dbFolder currentEpoch epochSizes = runExceptT $ do
    checkEpochSizes
    filesInDBFolder <- liftFsError $ do
      createDirectoryIfMissing True dbFolder
      Set.toList <$> listDirectory dbFolder
    let datFiles = filter (".dat" `isSuffixOf`) filesInDBFolder
    checkEpochConsistency datFiles
    checkPreviousEpochs datFiles
    case currentEpoch of
      -- If it is the first epoch, just open it
      0 -> do
        st <- mkInternalState dbFolder 0 epochSizes initialIteratorId
        stVar <- atomically $ newTMVar (Just st)
        return $ ImmutableDBHandle stVar dbFolder
      -- If it is not the first epoch, first open the previous epoch and
      -- finalise it using 'startNewEpochImpl'.
      _ -> do
        st <- mkInternalState dbFolder (currentEpoch - 1) epochSizes
          initialIteratorId
        stVar <- atomically $ newTMVar (Just st)
        let db = ImmutableDBHandle stVar dbFolder
        epochSize <- lookupEpochSize currentEpoch epochSizes
        _newEpoch <- ExceptT $ startNewEpochImpl db epochSize
        return db
  where
    -- | Check that each @epoch, epoch <= 'currentEpoch'@ is present in
    -- 'epochSizes'. Report the first missing one starting from epoch 0.
    checkEpochSizes :: HasCallStack => ExceptT ImmutableDBError m ()
    checkEpochSizes = zipWithM_
      (\expected mbActual ->
        case mbActual of
          Just actual | actual == expected -> return ()
          _ -> throwUserError $ MissingEpochSizeError expected)
      [0..currentEpoch]
      -- Pad with 'Nothing's to stop early termination of 'zipWithM_'
      (map (Just . fst) (Map.toAscList epochSizes) ++ repeat Nothing)

    -- | Check that there is no newer epoch than the one we're opening
    checkEpochConsistency :: HasCallStack
                          => [String] -> ExceptT ImmutableDBError m ()
    checkEpochConsistency = mapM_ $ \name -> case parseEpochNumber name of
      Just n | n > currentEpoch ->
        throwUserError $ OpenFinalisedEpochError currentEpoch n
      _ -> return ()

    -- | Check if all previous epochs have index and epoch files.
    checkPreviousEpochs :: HasCallStack
                        => [String] -> ExceptT ImmutableDBError m ()
    checkPreviousEpochs datFiles = do
      let fileSet = Set.fromList datFiles
      forM_ (Map.toAscList epochSizes) $ \(epoch, _epochSize) ->
        -- checkEpochConsistency has already checked that there are no files
        -- for a later epoch. This check just excludes the current epoch file,
        -- which can be new and may not exist on disk.
        when (epoch < currentEpoch) $ do
          let epochFile = renderFile "epoch" epoch
              indexFile = renderFile "index" epoch
          when (head epochFile `Set.notMember` fileSet) $
            throwUnexpectedError $ MissingFileError epochFile callStack
          when (head indexFile `Set.notMember` fileSet) $
            throwUnexpectedError $ MissingFileError indexFile callStack

closeDBImpl :: (HasCallStack, MonadSTM m, MonadMask m, HasFSE m)
            => ImmutableDBHandle m -> m (Either ImmutableDBError ())
closeDBImpl ImmutableDBHandle {..} = runExceptT $ do
    mbInternalState <- atomically $ swapTMVar _dbInternalState Nothing
    case mbInternalState of
      -- Already closed
      Nothing -> return ()
      Just InternalState {..} -> liftFsError $
        hClose _currentEpochWriteHandle `finally`
          hClose _currentIndexWriteHandle

isOpenImpl :: MonadSTM m => ImmutableDBHandle m -> m Bool
isOpenImpl ImmutableDBHandle {..} = isJust <$> atomically (readTMVar _dbInternalState)

getNextEpochSlotImpl :: (HasCallStack, MonadSTM m)
                     => ImmutableDBHandle m
                     -> m (Either ImmutableDBError EpochSlot)
getNextEpochSlotImpl db@ImmutableDBHandle {..} = runExceptT $ do
    InternalState {..} <- getInternalState db
    return $ EpochSlot _currentEpoch _nextExpectedRelativeSlot

getBinaryBlobImpl
  :: forall m. (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
  => ImmutableDBHandle m
  -> EpochSlot
  -> m (Either ImmutableDBError (Maybe ByteString))
getBinaryBlobImpl db@ImmutableDBHandle {..} readEpochSlot = runExceptT $ do
    let EpochSlot epoch relativeSlot = readEpochSlot
    InternalState {..} <- getInternalState db

    -- Check if the requested slot is not in the future.
    let nextExpectedEpochSlot =
          EpochSlot _currentEpoch _nextExpectedRelativeSlot
    when (readEpochSlot >= nextExpectedEpochSlot) $
      throwUserError $ ReadFutureSlotError readEpochSlot nextExpectedEpochSlot

    -- Check if the requested slot within the epoch, i.e. it may not be
    -- greater than or equal to the epoch size.
    epochSize <- lookupEpochSize epoch _epochSizes
    when (getRelativeSlot relativeSlot >= epochSize) $
      throwUserError $ SlotGreaterThanEpochSizeError relativeSlot epoch
                       epochSize

    -- We don't have to check if the epoch and index file exists thanks to the
    -- checks in 'openDB'. If they have disappeared after the check in
    -- 'openDB', an error at the 'HasFS' level will be thrown.
    let epochFile = _dbFolder <> renderFile "epoch" epoch
        indexFile = _dbFolder <> renderFile "index" epoch

    -- TODO We're reading from a file we're also writing to. Are we guaranteed
    -- to read what have written? The HasFS tests say so, but we prefer a
    -- (POSIX) reference.
    liftFsError $
      withFile epochFile ReadMode $ \eHnd ->
      withFile indexFile ReadMode $ \iHnd -> do
        -- Grab the offset in bytes of the requested slot.
        let indexSeekPosition = fromEnum relativeSlot * indexEntrySizeBytes
        _ <- hSeek iHnd AbsoluteSeek (toEnum indexSeekPosition)

        -- Compute the offset on disk and the blob size.
        (blobOffset, !blobSize) <- do
          let nbBytesToGet = indexEntrySizeBytes * 2
          -- Note the use of hGetRightSize: we must get enough bytes from the
          -- index file, otherwise 'decodeIndexEntry' (and its variant) would
          -- fail.
          bytes <- hGetRightSize iHnd nbBytesToGet indexFile
          let !start = decodeIndexEntry   bytes
          let !end   = decodeIndexEntryAt indexEntrySizeBytes bytes
          return (fromIntegral start, fromIntegral (end - start))

        case blobSize of
          0 -> return Nothing
          _ -> do
            -- Seek in the epoch file
            _ <- hSeek eHnd AbsoluteSeek blobOffset
            Just <$> hGetRightSize eHnd blobSize epochFile

appendBinaryBlobImpl :: (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
                     => ImmutableDBHandle m
                     -> RelativeSlot
                     -> BS.Builder
                     -> m (Either ImmutableDBError ())
appendBinaryBlobImpl db relativeSlot builder = runExceptT $
    modifyInternalState db $ \st@InternalState {..} -> do
      let eHnd = _currentEpochWriteHandle
          iHnd = _currentIndexWriteHandle

      -- Check that the slot is >= the expected next slot and thus not in the
      -- past.
      when (relativeSlot < _nextExpectedRelativeSlot) $
        throwUserError $ AppendToSlotInThePastError relativeSlot
                         _nextExpectedRelativeSlot

      -- Check if the requested slot within the epoch, i.e. it may not be
      -- greater than or equal to the epoch size.
      epochSize <- lookupEpochSize _currentEpoch _epochSizes
      when (getRelativeSlot relativeSlot >= epochSize) $
        throwUserError $ SlotGreaterThanEpochSizeError relativeSlot _currentEpoch
                         epochSize

      -- If necessary, backfill the index file for any slot we missed.
      let lastEpochOffset = NE.head _currentEpochOffsets
          (backfillOffsets, backfill) =
            indexBackfill relativeSlot _nextExpectedRelativeSlot
                          lastEpochOffset

      newOffset <- truncateToLastStateOnError st $ do
            -- Append to the end of the epoch file.
            bytesWritten <- hPut eHnd builder

            -- Update the index file.
            let newOffset = lastEpochOffset + bytesWritten
            void $ hPut iHnd (backfill <> encodeIndexEntry newOffset)

            return newOffset

      return (st { _currentEpochOffsets =
                     (newOffset NE.:| backfillOffsets) <> _currentEpochOffsets
                 , _nextExpectedRelativeSlot = relativeSlot + 1
                 }, ())

startNewEpochImpl :: (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
                  => ImmutableDBHandle m
                  -> EpochSize
                  -> m (Either ImmutableDBError Epoch)
startNewEpochImpl db newEpochSize = runExceptT $ modifyInternalState db $ \st -> do
    let InternalState {..} = st
    -- First pad the index file of the current epoch to full size so that it
    -- is clear when opening it again later on that it was finalised.
    epochSize <- lookupEpochSize _currentEpoch _epochSizes

    -- Calculate what to pad the file with
    let lastEpochOffset = NE.head _currentEpochOffsets
        -- An index file of n slots has n + 1 offsets, so pretend we need to
        -- backfill to slot n
        lastSlot = RelativeSlot epochSize
        (_backfillOffsets, backfill) =
          indexBackfill lastSlot _nextExpectedRelativeSlot
            lastEpochOffset

    truncateToLastStateOnError st $
      void $ hPut _currentIndexWriteHandle backfill
      -- Don't close the handle here, because 'truncateToLastStateOnError'
      -- will try to truncate the files in case of an FsError.

    liftFsError $
      hClose _currentIndexWriteHandle `finally`
        hClose _currentEpochWriteHandle

    let newEpoch      = succ _currentEpoch
        newEpochSizes = Map.insert newEpoch newEpochSize _epochSizes
    newState <- mkInternalState (_dbFolder db) newEpoch newEpochSizes
                  _nextIteratorID
                `catchError` \e -> do
                  -- In case 'mkInternalState' fails, truncate to the last
                  -- state to match the state 'modifyInternalState' will
                  -- restore to.
                  liftFsError $ truncateToLastState st
                  throwError e
    return (newState, newEpoch)


{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}

-- | Internal handle to an iterator
data IteratorHandle m = IteratorHandle
  { _it_state :: !(TVar m (Maybe (IteratorState m)))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and closed.
  , _it_end   :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  }

data IteratorState m = IteratorState
  { _it_next         :: !EpochSlot
    -- ^ The location of the next binary blob to read.
    --
    -- TODO check invariants with code/assertions + check them in the tests
    --
    -- Let @next@ be the 'EpochSlot' in '_it_next' and @index@ the 'Index' in
    -- '_it_epoch_index':
    --
    -- __Invariant 1__: @next <= '_it_end'@
    --
    -- __Invariant 2__: @'_epoch' next@ matches the epoch of '_it_epoch_handle'
    -- and '_it_epoch_index'.
    --
    -- __Invariant 3__: @'_relativeSlot' next@ points to a filled slot in the
    -- index.
    --
    -- __Invariant 4__: '_it_epoch_handle' points to where @next@ can be read
    -- from.
  , _it_epoch_handle :: !(FsHandleE m)
    -- ^ A handle to the epoch file corresponding with '_it_next'.
  , _it_epoch_index  :: Index
    -- ^ We load the index file for the epoch we are currently iterating over
    -- in-memory, as it's going to be small anyway (usually ~150kb).
  }

streamBinaryBlobsImpl :: forall m
                       . (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
                      => ImmutableDBHandle m
                      -> EpochSlot  -- ^ When to start streaming (inclusive).
                      -> EpochSlot  -- ^ When to stop streaming (inclusive).
                      -> m (Either ImmutableDBError (Iterator m))
streamBinaryBlobsImpl db@ImmutableDBHandle {..} start end = runExceptT $ do
    InternalState {..} <- getInternalState db
    let EpochSlot startEpoch startSlot = start
        nextExpectedEpochSlot =
          EpochSlot _currentEpoch _nextExpectedRelativeSlot

    validateIteratorRange nextExpectedEpochSlot _epochSizes start end

    -- Helper function to open the index file of an epoch.
    let openIndex epoch
          | epoch == _currentEpoch
          = return $ indexFromSlotOffsets _currentEpochOffsets
          | otherwise
          = liftFsError $ loadIndex _dbFolder epoch

    startIndex <- openIndex startEpoch
    -- TODO validate the index? check monotonically increasing + final offset
    -- = length epoch file

    -- Check if the index slot is filled, otherwise find the next filled
    -- one. If there is none in this epoch, open the next epoch until you
    -- find one. If we didn't find a filled slot before reaching 'end',
    -- return Nothing.
    mbIndexAndNext <- case containsSlot startIndex startSlot &&
                           isFilledSlot startIndex startSlot of
      -- The above 'containsSlot' condition is needed because we do not know
      -- whether the index has the right size.
      True  -> return $ Just (startIndex, start)
      False -> case nextFilledSlot startIndex startSlot of
        Just slot
          -- There is a filled slot, but we've gone too far
          | EpochSlot startEpoch slot > end -> return Nothing
          -- There is a filled slot after startSlot in this epoch
          | otherwise -> return $ Just (startIndex, EpochSlot startEpoch slot)
        -- No filled slot in the start epoch, open the next
        Nothing -> lookInLaterEpochs (startEpoch + 1)
          where
            lookInLaterEpochs epoch
              -- Because we have checked that end is valid, this check is
              -- enough to guarantee that we will never open an epoch in the
              -- future
              | epoch > _epoch end = return Nothing
              | otherwise = do
                index <- openIndex epoch
                case firstFilledSlot index of
                  Just slot
                    -- We've gone too far
                    | EpochSlot epoch slot > end -> return Nothing
                    | otherwise -> return $ Just (index, EpochSlot epoch slot)
                  Nothing -> lookInLaterEpochs (epoch + 1)

    mbIteratorState <- case mbIndexAndNext of
      -- No filled slot found, so just create a closed iterator
      Nothing -> return Nothing
      Just (index, next@(EpochSlot nextEpoch nextSlot)) -> liftFsError $ do
        -- Invariant 1 = OK by the search above for a filled slot

        eHnd <- hOpen (_dbFolder <> renderFile "epoch" nextEpoch) ReadMode
        -- Invariant 2 = OK

        -- Invariant 3 = OK by the search above for a filled slot

        -- Position the epoch handle at the right place. Invariant 4 = OK
        _ <- hSeek eHnd AbsoluteSeek (fromIntegral
                                        (offsetOfSlot index nextSlot))

        return $ Just IteratorState
          { _it_next = next
          , _it_epoch_handle = eHnd
          , _it_epoch_index = index
          }

    itState <- atomically $ newTVar mbIteratorState

    let ith = IteratorHandle
          { _it_state = itState
          , _it_end   = end
          }
    -- Safely increment '_nextIteratorID' in the 'InternalState'.
    modifyInternalState db $ \st@InternalState {..} ->
      let it = Iterator
            { iteratorNext  = iteratorNextImpl db ith
            , iteratorClose = iteratorCloseImpl   ith
            , iteratorID    = _nextIteratorID
            }
          st' = st { _nextIteratorID = succ _nextIteratorID }
      in return (st', it)


iteratorNextImpl :: forall m. (HasCallStack, MonadSTM m, MonadMask m, HasFSE m)
                 => ImmutableDBHandle m
                 -> IteratorHandle m
                 -> m (Either ImmutableDBError IteratorResult)
iteratorNextImpl db it@IteratorHandle {..} = runExceptT $ do
    -- The idea is that if the state is not Nothing, then '_it_next' is always
    -- ready to be read. After reading it with 'readNext', 'stepIterator' will
    -- advance the iterator to the next valid epoch slot.
    mbIteratorState <- atomically $ readTVar _it_state
    case mbIteratorState of
      -- Iterator already closed
      Nothing -> return IteratorExhausted
      -- Valid @next@ thanks to Invariant 1, so go ahead and read it
      Just iteratorState -> do
        blob <- readNext iteratorState
        -- Advance the iterator before return the read blob, so it has a valid
        -- @next@ to read the next time.
        stepIterator iteratorState
        return $ IteratorResult (_it_next iteratorState) blob
  where
    readNext :: IteratorState m -> ExceptT ImmutableDBError m ByteString
    readNext IteratorState { _it_epoch_handle = eHnd
                           , _it_next = EpochSlot epoch slot
                           , _it_epoch_index = index } = do
      -- Grab the blob size from the cached index
      let blobSize = sizeOfSlot index slot

      -- Read from the epoch file. No need for seeking: as we are streaming,
      -- we are already positioned at the correct place (Invariant 4).
      let epochFile = _dbFolder db <> renderFile "epoch" epoch
      liftFsError $ hGetRightSize eHnd (fromIntegral blobSize) epochFile

    -- Move the iterator to the next position that can be read from, advancing
    -- epochs if necessary. If no next position can be found, the iterator is
    -- closed.
    stepIterator :: IteratorState m -> ExceptT ImmutableDBError m ()
    stepIterator its@IteratorState { _it_epoch_handle = eHnd
                                   , _it_next = EpochSlot epoch currentSlot
                                   , _it_epoch_index = index } =
      case nextFilledSlot index currentSlot of
        -- We're still in the same epoch
        Just nextSlot
          | next <= _it_end
          -> atomically $ writeTVar _it_state $ Just its { _it_next = next }
             -- Invariant 1 is OK (see condition), Invariant 2 is unchanged,
             -- Invariant 3 is OK (thanks to nextFilledSlot), Invariant 4 is
             -- OK (readNext moved the handle + nextFilledSlot).
          | otherwise
          -> ExceptT $ iteratorCloseImpl it
          where
            next = EpochSlot epoch nextSlot

        -- Epoch exhausted, open the next epoch
        Nothing -> do
          liftFsError $ hClose eHnd
          st <- getInternalState db
          openNextNonEmptyEpoch (epoch + 1) st

    -- Start opening epochs (starting from the given epoch number) until we
    -- encounter a non-empty one, then update the iterator state accordingly.
    -- If no non-empty epoch can be found, the iterator is closed.
    openNextNonEmptyEpoch :: Epoch -> InternalState m
                          -> ExceptT ImmutableDBError m ()
    openNextNonEmptyEpoch epoch st@InternalState {..}
      | epoch > _epoch _it_end
      = ExceptT $ iteratorCloseImpl it
      | otherwise = do
        -- Thanks to the guard we know that epoch <= _epoch _it_end. We also
        -- know that _epoch _it_end is <= _currentEpoch, so we know that epoch
        -- <= _currentEpoch.
        index <- case epoch == _currentEpoch of
          True  -> return $ indexFromSlotOffsets _currentEpochOffsets
          False -> liftFsError $ loadIndex (_dbFolder db) epoch

        case firstFilledSlot index of
          -- Empty epoch -> try the next one
          Nothing -> openNextNonEmptyEpoch (epoch + 1) st
          Just slot
            -- Slot is after the end -> stop
            | EpochSlot epoch slot > _it_end -> ExceptT $ iteratorCloseImpl it
            | otherwise -> do
              let epochFile = _dbFolder db <> renderFile "epoch" epoch
              eHnd <- liftFsError $ hOpen epochFile ReadMode
              -- No seek needed, as we will start streaming from the first
              -- offset.
              --
              -- Invariant 1 is OK (see the guard above), Invariant 2 is OK,
              -- Invariant 3 is OK (thanks to firstFilledSlot), Invariant 4 is
              -- OK.
              atomically $ writeTVar _it_state $ Just IteratorState
                { _it_next = EpochSlot epoch slot
                , _it_epoch_handle = eHnd
                , _it_epoch_index = index
                }

iteratorCloseImpl :: (HasCallStack, MonadSTM m, HasFSE m)
                  => IteratorHandle m
                  -> m (Either ImmutableDBError ())
iteratorCloseImpl IteratorHandle {..} = runExceptT $ do
    mbIteratorState <- atomically $ readTVar _it_state
    case mbIteratorState of
      -- Already closed
      Nothing -> return ()
      Just IteratorState {..} -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- hClose might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar _it_state Nothing
        liftFsError $ hClose _it_epoch_handle


{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}


-- | Create the internal state based on a potentially empty epoch.
--
-- Open the epoch and index files for appending. If they already existed, the
-- state is reconstructed from the index file on disk.
--
-- If the existing index file is invalid ('isValidIndex'), an
-- 'InvalidFileError' is thrown.
--
-- If no index file exists for the given epoch, it is created and initialised
-- by writing a 0 to it.
mkInternalState :: (HasCallStack, MonadMask m, HasFSE m)
                => FsPath
                -> Epoch
                -> Map Epoch EpochSize
                -> IteratorID
                -> ExceptT ImmutableDBError m (InternalState m)
mkInternalState dbFolder epoch epochSizes nextIteratorID = do
    let epochFile = dbFolder <> renderFile "epoch" epoch
        indexFile = dbFolder <> renderFile "index" epoch
    -- TODO in case of an FsError, we leave new files behind, preventing the
    -- DB from being reopened (OpenFinalisedEpochError)
    (indexExists, eHnd, iHnd) <- liftFsError $ (,,) <$>
      doesFileExist indexFile <*>
      hOpen epochFile AppendMode <*> hOpen indexFile AppendMode
    -- Previously, we used @((==) 0) <$> hSeek iHnd RelativeSeek 0@ to detect
    -- whether the index file is a new file. This assumed that opening a file
    -- in @AppendMode@ has the effect of immediately seeking to the end of the
    -- file. In reality, the initial seek position is simply 0, but each write
    -- is automatically preceded with a seek to the end. So instead, simply
    -- use @doesFileExist@. TODO see Edsko's comment -> hGetFileSize
    epochOffsets <- case indexExists of
      -- TODO this function is also called by 'startNewEpoch', in which case
      -- it should never happen that the index file of the (future) epoch
      -- already exists. This is checked when opening the database, but
      -- somebody might have created the index file afterwards, behind our
      -- backs. Should we throw an error for this?
      True -> do
        -- If the index file already existed, read it in its entirety and
        -- reconstruct the list of offsets from it.
        existingIndex <- liftFsError $ withFile indexFile ReadMode $ \hnd ->
          indexFromByteString . BL.toStrict . BS.toLazyByteString <$>
          readAll hnd
        -- An already existing index may be invalid.
        unless (isValidIndex existingIndex) $
          throwUnexpectedError $ InvalidFileError indexFile callStack
        return $ indexToSlotOffsets existingIndex

      False -> do
        -- Initialise the new index file
        _ <- liftFsError $ hPut iHnd (encodeIndexEntry 0)
        return $ 0 NE.:| []

    return InternalState
      { _currentEpoch             = epoch
      , _currentEpochWriteHandle  = eHnd
      , _currentIndexWriteHandle  = iHnd
      , _currentEpochOffsets      = epochOffsets
      , _nextExpectedRelativeSlot = fromIntegral (length epochOffsets - 1)
                                    -- TODO what if it is > epochSize? Check
                                    -- how startNewEpoch deals with this.
                                    -- -> Error
      , _epochSizes               = epochSizes
      , _nextIteratorID           = nextIteratorID
      }

-- | Get the 'InternalState' of the given database, throw a 'ClosedDBError' in
-- case it is closed.
getInternalState :: (HasCallStack, MonadSTM m)
                 => ImmutableDBHandle m
                 -> ExceptT ImmutableDBError m (InternalState m)
getInternalState ImmutableDBHandle {..} = do
    mbInternalState <- atomically (readTMVar _dbInternalState)
    case mbInternalState of
       Nothing            -> throwUserError ClosedDBError
       Just internalState -> return internalState

-- | Modify the internal state of the database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case a 'FileSystemError' is thrown, the database is closed to prevent
-- further appending to a database in a potentially inconsistent state.
--
-- __Note__: This /takes/ the 'TMVar', /then/ runs the action (which might be
-- in 'IO'), and then puts the 'TMVar' back, just like 'modifyMVar' does.
-- Consequently, it has the same gotchas that 'modifyMVar' does; the effects
-- are observable and it is susceptible to deadlock.
--
-- TODO(adn): we should really just use 'MVar' rather than 'TMVar', but we
-- currently don't have a simulator for code using 'MVar'.
modifyInternalState :: forall m r. (HasCallStack, MonadSTM m, MonadMask m)
                    => ImmutableDBHandle m
                    -> (   InternalState m
                        -> ExceptT ImmutableDBError m (InternalState m, r))
                    -> ExceptT ImmutableDBError m r
modifyInternalState ImmutableDBHandle {..} action = ExceptT $
    fmap snd . fst <$> generalBracket open close (runExceptT . mutation)
  where
    -- We use @m (Either e a)@ instead of @EitherT e m a@ for 'generalBracket'
    -- so that 'close' knows which error is thrown (@Either e (s, r)@ vs. @(s,
    -- r)@).
    open :: m (Maybe (InternalState m))
    open = atomically $ takeTMVar _dbInternalState

    close :: Maybe (InternalState m)
          -> ExitCase (Either ImmutableDBError (InternalState m, r))
          -> m ()
    close mbSt ec = atomically $ case ec of
      -- Restore the original state in case of an abort or an exception
      ExitCaseAbort         -> putTMVar _dbInternalState mbSt
      ExitCaseException _ex -> putTMVar _dbInternalState mbSt
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (st', _)) -> putTMVar _dbInternalState (Just st')
      -- In case of an error (not an exception)
      ExitCaseSuccess (Left (UnexpectedError {})) ->
        -- When unexpected, close the DB for safety
        putTMVar _dbInternalState Nothing
      ExitCaseSuccess (Left (UserError {})) ->
        -- When a user error, just restore the previous state
        putTMVar _dbInternalState mbSt

    mutation :: HasCallStack
             => Maybe (InternalState m)
             -> ExceptT ImmutableDBError m (InternalState m, r)
    mutation Nothing   = throwUserError ClosedDBError
    mutation (Just st) = action st

-- | Perform the given action, restoring the last valid state in case of an
-- 'FsError' (see 'truncateToLastState'). The 'FsError' is rethrown.
truncateToLastStateOnError :: (MonadMask m, HasFSE m)
                           => InternalState m
                           -> ExceptT FsError m a
                           -> ExceptT ImmutableDBError m a
truncateToLastStateOnError st m = liftFsError $
    m `catchError` \e -> do
      truncateToLastState st
      throwError e

-- | Restore the last valid state by truncating the epoch and index files to
-- their last valid offsets, derived from the given valid 'InternalState'.
truncateToLastState :: (MonadMask m, HasFSE m)
                    => InternalState m
                    -> ExceptT FsError m ()
truncateToLastState InternalState {..} = do
    let lastValidEpochOffset = NE.head _currentEpochOffsets
        lastValidIndexOffset = fromIntegral $
          (fromEnum _nextExpectedRelativeSlot + 1) *
          indexEntrySizeBytes
    -- TODO what if one of the truncates fails?
    hTruncate _currentEpochWriteHandle lastValidEpochOffset `finally`
      hTruncate _currentIndexWriteHandle lastValidIndexOffset
    return ()


-- | Return what to backfill the index file with.
--
-- Such situation may arise in case we \"skip\" some relative slots, and we
-- write into the DB, say, for example, every other relative slot. In this
-- case, we do need to backfill the index file with offsets for the skipped
-- relative slots. Similarly, before we start a new epoch, we must backfill
-- the index file of the current epoch file to indicate that it is finalised.
--
-- For example, say we have written \"a\" to relative slot 0 and \"bravo\" to
-- relative slot 1. We have the following index file:
--
-- > slot:     0   1   2
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │
-- >         └───┴───┴───┘
--
-- Now we want to store \"haskell\" in relative slot 4, skipping 2 and 3. We
-- first have to backfill the index by repeating the last offset for the two
-- missing slots:
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │
-- >         └───┴───┴───┴───┴───┘
--
-- After backfilling (writing the offset 6 twice), we can write the next
-- offset:
--
-- > slot:     0   1   2   3   4   5
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13│
-- >         └───┴───┴───┴───┴───┴───┘
--
indexBackfill :: RelativeSlot  -- ^ The slot to write to (>= next expected slot)
              -> RelativeSlot  -- ^ The next expected slot to write to
              -> SlotOffset    -- ^ The last 'SlotOffset' written to
              -> ([SlotOffset], BS.Builder)
indexBackfill (RelativeSlot slot) (RelativeSlot nextExpected) lastOffset =
    (replicate gap lastOffset,
     mconcat $ replicate gap (encodeIndexEntry lastOffset))
  where
    gap = fromIntegral $ slot - nextExpected
