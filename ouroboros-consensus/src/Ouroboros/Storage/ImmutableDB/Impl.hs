{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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
-- starts a new empty epoch with the given size. The size of an epoch may not
-- be 0. TODO check this
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
-- = Errors
--
-- Whenever an 'UnexpectedError' is thrown during a write operation
-- ('appendBinaryBlob', 'startNewEpoch'), the database will be automatically
-- closed because we can not guarantee a consistent state in the face of file
-- system errors. See the 'reopen' operation and the paragraph below about
-- reopening the database for more information.
--
-- = (Re)opening the database
--
-- The database can be closed and reopened. In case the database was closed
-- because of an unexpected error, the same database can be reopened again
-- with 'reopen' using a 'RecoveryPolicy'.
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
  ( openDB, lastEpochOnDisk, openLastEpoch
  ) where

import           Control.Exception (assert)
import           Control.Monad (when, unless, mplus, foldM, forM_, zipWithM_)
import           Control.Monad.Catch (MonadMask, ExitCase(..), generalBracket)
import           Control.Monad.Class.MonadSTM (MonadSTM(..))

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import           Data.Either (isRight)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           GHC.Stack (HasCallStack, callStack)

import           System.IO (IOMode (..), SeekMode (..))

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}


-- | An opaque handle to an immutable database of binary blobs.
data ImmutableDBHandle m = ImmutableDBHandle
    { _dbInternalState :: !(TMVar m (Either ClosedState (OpenState m)))
    , _dbFolder        :: !FsPath
    }

-- | Internal state when the database is open.
data OpenState m = OpenState
    { _currentEpoch             :: !Epoch
    -- ^ The current 'Epoch' the immutable store is writing into.
    , _currentEpochWriteHandle  :: !(FsHandle m)
    -- ^ The write handle for the current epoch file.
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


-- | Internal state when the database is closed. This contains data that
-- should be restored when the database is reopened. Data not present here
-- will be recovered when reopening.
data ClosedState = ClosedState
    { _closedEpochSizes     :: !(Map Epoch EpochSize)
    -- ^ See '_epochSizes'.
    , _closedNextIteratorID :: !IteratorID
    -- ^ See '_nextIteratorID'.
    , _closedLastEpoch      :: !Epoch
    -- ^ See '_currentEpoch'.
    }

{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

-- | Open the database, creating it from scratch if the 'FsPath' points to a
-- non-existing directory or reopening an existing one using the given
-- 'RecoveryPolicy'.
--
-- In addition to the database, the 'EpochSlot' of the last blob stored in it
-- is returned, or 'Nothing' in case the database is empty.
--
-- A 'Map' with the size for each epoch (the past epochs and the most recent
-- one) must be passed. When a size is missing for an epoch present on disk, a
-- 'MissingEpochSizeError' error will be thrown.
--
-- The given 'Epoch' is opened for appending. If the given epoch was
-- previously finalised, i.e. there are files corresponding to later epochs,
-- an 'OpenFinalisedEpochError' error will be thrown, unless the
-- 'RestoreLastValidEpoch' recovery policy is used.
--
-- See 'RecoveryPolicy' for more details on the different policies.
--
-- __Note__: To be used in conjunction with 'withDB'.
openDB :: (HasCallStack, MonadSTM m, MonadMask m)
       => HasFS m
       -> ErrorHandling ImmutableDBError m
       -> FsPath -> Epoch -> Map Epoch EpochSize -> RecoveryPolicy e m
       -> m (ImmutableDB m, Maybe EpochSlot)
openDB hasFS err dbFolder epoch epochSizes recPol = do
    (dbh, lastBlobLocation) <-
      openDBImpl hasFS err dbFolder epoch epochSizes recPol
    return $ (, lastBlobLocation) $ ImmutableDB
      { closeDB           = closeDBImpl           hasFS     dbh
      , isOpen            = isOpenImpl                      dbh
      , reopen            = reopenImpl            hasFS err dbh
      , getNextEpochSlot  = getNextEpochSlotImpl        err dbh
      , getBinaryBlob     = getBinaryBlobImpl     hasFS err dbh
      , appendBinaryBlob  = appendBinaryBlobImpl  hasFS err dbh
      , startNewEpoch     = startNewEpochImpl     hasFS err dbh
      , streamBinaryBlobs = streamBinaryBlobsImpl hasFS err dbh
      , immutableDBErr    = err
      }

-- | Return the last epoch for which a file is stored on disk.
--
-- The file(s) in question are not checked, they might be empty or invalid.
lastEpochOnDisk :: (HasCallStack, Monad m)
                => HasFS m
                -> FsPath
                -> m (Maybe Epoch)
lastEpochOnDisk HasFS{..} dbFolder = do
    filesInDBFolder <- listDirectory dbFolder
    return $ case mapMaybe extractEpochNumber $ Set.toList filesInDBFolder of
      [] -> Nothing
      epochs  -> Just $ maximum epochs
  where
    extractEpochNumber :: String -> Maybe Epoch
    extractEpochNumber s = do
      ("epoch", epoch) <- parseDBFile s
      return epoch

-- | Open the database at the last epoch on disk.
--
-- Combination of 'openDB' and 'lastEpochOnDisk'.
openLastEpoch :: (HasCallStack, MonadSTM m, MonadMask m)
              => HasFS m
              -> ErrorHandling ImmutableDBError m
              -> FsPath -> Map Epoch EpochSize -> RecoveryPolicy e m
              -> m (ImmutableDB m, Maybe EpochSlot)
openLastEpoch hasFS err dbFolder epochSizes recPol = do
    epochToOpen <- fromMaybe 0 <$> lastEpochOnDisk hasFS dbFolder
    openDB hasFS err dbFolder epochToOpen epochSizes recPol

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

openDBImpl :: forall m e. (HasCallStack, MonadSTM m, MonadMask m)
           => HasFS m
           -> ErrorHandling ImmutableDBError m
           -> FsPath
           -> Epoch
           -> Map Epoch EpochSize
           -> RecoveryPolicy e m
           -> m (ImmutableDBHandle m, LastBlobLocation)
openDBImpl hasFS@HasFS{..} err dbFolder epoch epochSizes recPol = do
    createDirectoryIfMissing True dbFolder
    checkEpochSizes epoch
    (epochToOpen, mbLastBlobLocation) <-
      recovery hasFS err dbFolder epoch epochSizes recPol

    st <- mkOpenState hasFS err dbFolder epochToOpen epochSizes initialIteratorId

    stVar <- atomically $ newTMVar (Right st)
    let db = ImmutableDBHandle stVar dbFolder
    case mbLastBlobLocation of
      Just lastBlobLocation -> return (db, lastBlobLocation)
      Nothing               -> (db,) <$> lastBlobInDB hasFS err st dbFolder
  where
    -- | Check that each @epoch, epoch <= epochToOpen@ is present in
    -- 'epochSizes'. Report the first missing one starting from epoch 0.
    checkEpochSizes :: HasCallStack => Epoch -> m ()
    checkEpochSizes epochToOpen = zipWithM_
      (\expected mbActual ->
        case mbActual of
          Just actual | actual == expected -> return ()
          _ -> throwUserError err $ MissingEpochSizeError expected)
      [0..epochToOpen]
      -- Pad with 'Nothing's to stop early termination of 'zipWithM_'
      (map (Just . fst) (Map.toAscList epochSizes) ++ repeat Nothing)

closeDBImpl :: (HasCallStack, MonadSTM m, MonadMask m)
            => HasFS m
            -> ImmutableDBHandle m
            -> m ()
closeDBImpl hasFS@HasFS{..} ImmutableDBHandle {..} = do
    internalState <- atomically $ takeTMVar _dbInternalState
    case internalState of
      -- Already closed
      Left  _ -> atomically $ putTMVar _dbInternalState internalState
      Right OpenState {..} -> do
        let closedState = closedStateFromInternalState internalState
        -- Close the database before doing the file-system operations so that
        -- in case these fail, we don't leave the database open.
        atomically $ putTMVar _dbInternalState (Left closedState)
        hClose _currentEpochWriteHandle
        -- Also write an index file of the current (possible incomplete) epoch
        writeSlotOffsets hasFS _dbFolder _currentEpoch _currentEpochOffsets

isOpenImpl :: MonadSTM m => ImmutableDBHandle m -> m Bool
isOpenImpl ImmutableDBHandle {..} =
    isRight <$> atomically (readTMVar _dbInternalState)


-- Note that recovery will stop when an 'FsError' is thrown, as usual.
reopenImpl :: forall m e. (HasCallStack, MonadSTM m, MonadMask m)
           => HasFS m
           -> ErrorHandling ImmutableDBError m
           -> ImmutableDBHandle m
           -> RecoveryPolicy e m
           -> m LastBlobLocation
reopenImpl hasFS@HasFS{..} err@ErrorHandling{..} ImmutableDBHandle{..} recPol = do
    internalState <- atomically $ takeTMVar _dbInternalState
    (openState, mbLastBlobLocation) <- case internalState of
      -- When still open,
      Right ost -> do
        -- Put the state back and do nothing
        atomically $ putTMVar _dbInternalState internalState
        return (ost, Nothing)
      Left ClosedState {..} -> do
        (epochToReopen, mbLastBlobLocation) <-
          -- Important: put back the state when the recovery policy threw an
          -- error, otherwise we have an empty TMVar.
          onException
            (recovery hasFS err _dbFolder _closedLastEpoch _closedEpochSizes recPol)
            (atomically $ putTMVar _dbInternalState internalState)

        -- TODO the index file is opened again by 'mkOpenState' after we have
        -- already opened it in 'recovery'. Should we avoid try to avoid this?
        newOpenState <- mkOpenState hasFS err _dbFolder epochToReopen
                          _closedEpochSizes _closedNextIteratorID
        atomically $ putTMVar _dbInternalState (Right newOpenState)
        return (newOpenState, mbLastBlobLocation)
    case mbLastBlobLocation of
      Just lastBlobLocation -> return lastBlobLocation
      Nothing               -> lastBlobInDB hasFS err openState _dbFolder
  where
    -- On both ImmutableDBError and on FsError
    onException m onErr =
      EH.onException hasFsErr (EH.onException err m onErr) onErr


getNextEpochSlotImpl :: (HasCallStack, MonadSTM m)
                     => ErrorHandling ImmutableDBError m
                     -> ImmutableDBHandle m
                     -> m EpochSlot
getNextEpochSlotImpl err db@ImmutableDBHandle {..} = do
    OpenState {..} <- getOpenState err db
    return $ EpochSlot _currentEpoch _nextExpectedRelativeSlot

getBinaryBlobImpl
  :: forall m. (HasCallStack, MonadSTM m, MonadMask m)
  => HasFS m
  -> ErrorHandling ImmutableDBError m
  -> ImmutableDBHandle m
  -> EpochSlot
  -> m (Maybe ByteString)
getBinaryBlobImpl hasFS@HasFS{..} err db@ImmutableDBHandle {..} readEpochSlot = do
    let EpochSlot epoch relativeSlot = readEpochSlot
    OpenState {..} <- getOpenState err db

    -- Check if the requested slot is not in the future.
    let nextExpectedEpochSlot =
          EpochSlot _currentEpoch _nextExpectedRelativeSlot
    when (readEpochSlot >= nextExpectedEpochSlot) $
      throwUserError err $ ReadFutureSlotError
                             readEpochSlot
                             nextExpectedEpochSlot

    -- Check if the requested slot within the epoch, i.e. it may not be
    -- greater than or equal to the epoch size.
    epochSize <- lookupEpochSize err epoch _epochSizes
    when (getRelativeSlot relativeSlot >= epochSize) $
      throwUserError err $ SlotGreaterThanEpochSizeError
                             relativeSlot
                             epoch
                             epochSize

    let epochFile = _dbFolder <> renderFile "epoch" epoch
        indexFile = _dbFolder <> renderFile "index" epoch

    (blobOffset, blobSize) <- case epoch == _currentEpoch of
      -- If the requested epoch is the current epoch, the offsets are still in
      -- memory
      True ->
        case NE.drop toDrop _currentEpochOffsets of
            (offsetAfter:offset:_) -> return (offset, offsetAfter - offset)
            _ -> error "impossible: _currentEpochOffsets out of sync"
          where
            toDrop = fromEnum (lastSlot - relativeSlot)
            lastSlot = _nextExpectedRelativeSlot - 1

      -- Otherwise, the offsets will have to be read from an index file
      False -> withFile hasFS indexFile ReadMode $ \iHnd -> do
        -- Grab the offset in bytes of the requested slot.
        let indexSeekPosition = fromIntegral (getRelativeSlot relativeSlot)
                              * fromIntegral indexEntrySizeBytes
        hSeek iHnd AbsoluteSeek indexSeekPosition
        -- Compute the offset on disk and the blob size.
        let nbBytesToGet = indexEntrySizeBytes * 2
        -- Note the use of hGetRightSize: we must get enough bytes from the
        -- index file, otherwise 'decodeIndexEntry' (and its variant) would
        -- fail.
        bytes <- hGetRightSize hasFS iHnd nbBytesToGet indexFile
        let !start = decodeIndexEntry   bytes
            !end   = decodeIndexEntryAt indexEntrySizeBytes bytes
        return (start, end - start)

    -- In case the requested is still the current epoch, we will be reading
    -- from the epoch file while we're also writing to it. Are we guaranteed
    -- to read what have written? Duncan says: this is guaranteed at the OS
    -- level (POSIX), but not for Haskell handles, which might perform other
    -- buffering. However, the 'HasFS' implementation we're using uses POSIX
    -- file handles ("Ouroboros.Storage.IO") so we're safe (other
    -- implementations of the 'HasFS' API guarantee this too).
    case blobSize of
      0 -> return Nothing
      _ -> withFile hasFS epochFile ReadMode $ \eHnd -> do
        -- Seek in the epoch file
        hSeek eHnd AbsoluteSeek (fromIntegral blobOffset)
        Just <$> hGetRightSize hasFS eHnd (fromIntegral blobSize) epochFile

appendBinaryBlobImpl :: (HasCallStack, MonadSTM m, MonadMask m)
                     => HasFS m
                     -> ErrorHandling ImmutableDBError m
                     -> ImmutableDBHandle m
                     -> RelativeSlot
                     -> BS.Builder
                     -> m ()
appendBinaryBlobImpl hasFS@HasFS{..} err db relativeSlot builder =
    modifyOpenState hasFS err db $ \st@OpenState {..} -> do
      let eHnd = _currentEpochWriteHandle

      -- Check that the slot is >= the expected next slot and thus not in the
      -- past.
      when (relativeSlot < _nextExpectedRelativeSlot) $
        throwUserError err $ AppendToSlotInThePastError
                               relativeSlot
                               _nextExpectedRelativeSlot

      -- Check if the requested slot within the epoch, i.e. it may not be
      -- greater than or equal to the epoch size.
      epochSize <- lookupEpochSize err _currentEpoch _epochSizes
      when (getRelativeSlot relativeSlot >= epochSize) $
        throwUserError err $ SlotGreaterThanEpochSizeError
                               relativeSlot
                               _currentEpoch
                               epochSize

      -- If necessary, backfill the index file for any slot we missed.
      let lastEpochOffset = NE.head _currentEpochOffsets
          backfillOffsets = indexBackfill relativeSlot _nextExpectedRelativeSlot
                                          lastEpochOffset


      -- Append to the end of the epoch file.
      bytesWritten <- hPut eHnd builder
      let newOffset = lastEpochOffset + bytesWritten

      -- When the '_nextExpectedRelativeSlot' is the last one of the epoch
      -- (according to the epoch size), we still increment it, but an
      -- 'appendBinaryBlob' call for that slot will fail with a
      -- 'SlotGreaterThanEpochSizeError'. The user is required to first
      -- manually call 'startNewEpoch'.
      return (st { _currentEpochOffsets =
                     (newOffset NE.:| backfillOffsets) <> _currentEpochOffsets
                 , _nextExpectedRelativeSlot = relativeSlot + 1
                 }, ())

startNewEpochImpl :: (HasCallStack, MonadSTM m, MonadMask m)
                  => HasFS m
                  -> ErrorHandling ImmutableDBError m
                  -> ImmutableDBHandle m
                  -> EpochSize
                  -> m Epoch
startNewEpochImpl hasFS@HasFS{..} err db newEpochSize = modifyOpenState hasFS err db $ \st -> do
    let OpenState {..} = st

    -- We can close the epoch file
    hClose _currentEpochWriteHandle

    -- Find out the size of the epoch, so we can pad the _currentEpochOffsets
    -- to match the size before writing them to the index file. When looking
    -- at the index file, it will then be clear that the epoch is finalised.
    epochSize <- lookupEpochSize err _currentEpoch _epochSizes

    -- Calculate what to pad the file with
    let lastEpochOffset = NE.head _currentEpochOffsets
        -- An index file of n slots has n + 1 offsets, so pretend we need to
        -- backfill to slot n
        lastSlot = RelativeSlot epochSize
        backfillOffsets = indexBackfill lastSlot _nextExpectedRelativeSlot
                                        lastEpochOffset
        -- Prepend the backfillOffsets to the current offsets to get a
        -- non-empty list of all the offsets. Note that this list is stored in
        -- reverse order.
        allOffsets = foldr NE.cons _currentEpochOffsets backfillOffsets

    -- Now write the offsets to the index file to disk
    writeSlotOffsets hasFS (_dbFolder db) _currentEpoch allOffsets

    let newEpoch      = succ _currentEpoch
        newEpochSizes = Map.insert newEpoch newEpochSize _epochSizes
    newState <- mkOpenState
                  hasFS
                  err
                  (_dbFolder db)
                  newEpoch
                  newEpochSizes
                  _nextIteratorID
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
  , _it_epoch_handle :: !(FsHandle m)
    -- ^ A handle to the epoch file corresponding with '_it_next'.
  , _it_epoch_index  :: Index
    -- ^ We load the index file for the epoch we are currently iterating over
    -- in-memory, as it's going to be small anyway (usually ~150kb).
  }

streamBinaryBlobsImpl :: forall m
                       . (HasCallStack, MonadSTM m, MonadMask m)
                      => HasFS m
                      -> ErrorHandling ImmutableDBError m
                      -> ImmutableDBHandle m
                      -> Maybe EpochSlot  -- ^ When to start streaming (inclusive).
                      -> Maybe EpochSlot  -- ^ When to stop streaming (inclusive).
                      -> m (Iterator m)
streamBinaryBlobsImpl hasFS@HasFS{..} err db@ImmutableDBHandle {..} mbStart mbEnd = do
    OpenState {..} <- getOpenState err db
    let nextExpectedEpochSlot =
          EpochSlot _currentEpoch _nextExpectedRelativeSlot

    validateIteratorRange err nextExpectedEpochSlot
      (\epoch -> lookupEpochSize err epoch _epochSizes) mbStart mbEnd

    -- If the database is empty, just return an empty iterator (directly
    -- exhausted)
    if nextExpectedEpochSlot == EpochSlot 0 0
    then mkEmptyIterator
    else do

      -- Fill in missing bounds
      lastAppendedSlot <- case nextExpectedEpochSlot of
        -- EpochSlot 0 0 is already handled before, so we can ignore it.
        EpochSlot epoch 0 -> do
          epochSize <- lookupEpochSize err (epoch - 1) _epochSizes
          return $ EpochSlot (epoch - 1) (fromIntegral epochSize - 1)
        EpochSlot epoch relSlot ->
          return $ EpochSlot epoch (pred relSlot)
      let start = fromMaybe (EpochSlot 0 0) mbStart
          EpochSlot startEpoch startSlot = start
          end = fromMaybe lastAppendedSlot mbEnd

      -- Helper function to open the index file of an epoch.
      let openIndex epoch
            | epoch == _currentEpoch
            = return $ indexFromSlotOffsets _currentEpochOffsets
            | otherwise
            = do
              index <- loadIndex' hasFS err _dbFolder epoch
              unless (isValidIndex index) $
                throwUnexpectedError err $
                  InvalidFileError
                    (_dbFolder <> renderFile "index" epoch)
                    callStack
              return index

      startIndex <- openIndex startEpoch

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
        Just (index, next@(EpochSlot nextEpoch nextSlot)) -> do
          -- Invariant 1 = OK by the search above for a filled slot

          eHnd <- hOpen (_dbFolder <> renderFile "epoch" nextEpoch) ReadMode
          -- Invariant 2 = OK

          -- Invariant 3 = OK by the search above for a filled slot

          -- Position the epoch handle at the right place. Invariant 4 = OK
          hSeek eHnd AbsoluteSeek (fromIntegral (offsetOfSlot index nextSlot))

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
      -- Safely increment '_nextIteratorID' in the 'OpenState'.
      modifyOpenState hasFS err db $ \st@OpenState {..} ->
        let it = Iterator
              { iteratorNext  = iteratorNextImpl  hasFS err db ith
              , iteratorClose = iteratorCloseImpl hasFS        ith
              , iteratorID    = _nextIteratorID
              }
            st' = st { _nextIteratorID = succ _nextIteratorID }
        in return (st', it)
  where
    mkEmptyIterator :: m (Iterator m)
    mkEmptyIterator =
      modifyOpenState hasFS err db $ \st@OpenState {..} ->
        let it = Iterator
              { iteratorNext  = return IteratorExhausted
              , iteratorClose = return ()
              , iteratorID    = _nextIteratorID
              }
            st' = st { _nextIteratorID = succ _nextIteratorID }
        in return (st', it)

iteratorNextImpl :: forall m. (HasCallStack, MonadSTM m, MonadMask m)
                 => HasFS m
                 -> ErrorHandling ImmutableDBError m
                 -> ImmutableDBHandle m
                 -> IteratorHandle m
                 -> m IteratorResult
iteratorNextImpl hasFS@HasFS{..} err db it@IteratorHandle {..} = do
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
    readNext :: IteratorState m -> m ByteString
    readNext IteratorState { _it_epoch_handle = eHnd
                           , _it_next = EpochSlot epoch slot
                           , _it_epoch_index = index } = do
      -- Grab the blob size from the cached index
      let blobSize = sizeOfSlot index slot

      -- Read from the epoch file. No need for seeking: as we are streaming,
      -- we are already positioned at the correct place (Invariant 4).
      let epochFile = _dbFolder db <> renderFile "epoch" epoch
      hGetRightSize hasFS eHnd (fromIntegral blobSize) epochFile

    -- Move the iterator to the next position that can be read from, advancing
    -- epochs if necessary. If no next position can be found, the iterator is
    -- closed.
    stepIterator :: IteratorState m -> m ()
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
          -> iteratorCloseImpl hasFS it
          where
            next = EpochSlot epoch nextSlot

        -- Epoch exhausted, open the next epoch
        Nothing -> do
          hClose eHnd
          st <- getOpenState err db
          openNextNonEmptyEpoch (epoch + 1) st

    -- Start opening epochs (starting from the given epoch number) until we
    -- encounter a non-empty one, then update the iterator state accordingly.
    -- If no non-empty epoch can be found, the iterator is closed.
    openNextNonEmptyEpoch :: Epoch -> OpenState m -> m ()
    openNextNonEmptyEpoch epoch st@OpenState {..}
      | epoch > _epoch _it_end
      = iteratorCloseImpl hasFS it
      | otherwise = do
        -- Thanks to the guard we know that epoch <= _epoch _it_end. We also
        -- know that _epoch _it_end is <= _currentEpoch, so we know that epoch
        -- <= _currentEpoch.
        index <- case epoch == _currentEpoch of
          True  -> return $ indexFromSlotOffsets _currentEpochOffsets
          False -> loadIndex' hasFS err (_dbFolder db) epoch

        case firstFilledSlot index of
          -- Empty epoch -> try the next one
          Nothing -> openNextNonEmptyEpoch (epoch + 1) st
          Just slot
            -- Slot is after the end -> stop
            | EpochSlot epoch slot > _it_end -> iteratorCloseImpl hasFS it
            | otherwise -> do
              let epochFile = _dbFolder db <> renderFile "epoch" epoch
              eHnd <- hOpen epochFile ReadMode
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

iteratorCloseImpl :: (HasCallStack, MonadSTM m)
                  => HasFS m
                  -> IteratorHandle m
                  -> m ()
iteratorCloseImpl HasFS{..} IteratorHandle {..} = do
    mbIteratorState <- atomically $ readTVar _it_state
    case mbIteratorState of
      -- Already closed
      Nothing -> return ()
      Just IteratorState {..} -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- hClose might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar _it_state Nothing
        hClose _it_epoch_handle

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}


-- | Create the internal open state based on a potentially empty epoch.
--
-- Open the epoch file for appending. If it already existed, the state is
-- reconstructed from the index file on disk.
--
-- If the existing index file is invalid ('isValidIndex'), an
-- 'InvalidFileError' is thrown. If the size of the epoch file does not match
-- the last offset in the index file, an 'InvalidFileError' is thrown.
mkOpenState :: (HasCallStack, MonadMask m)
            => HasFS m
            -> ErrorHandling ImmutableDBError m
            -> FsPath
            -> Epoch
            -> Map Epoch EpochSize
            -> IteratorID
            -> m (OpenState m)
mkOpenState hasFS@HasFS{..} err dbFolder epoch epochSizes nextIteratorID = do
    let epochFile = dbFolder <> renderFile "epoch" epoch
        indexFile = dbFolder <> renderFile "index" epoch
    indexExists  <- doesFileExist indexFile
    epochOffsets <- case indexExists of
      -- TODO this function is also called by 'startNewEpoch', in which case
      -- it should never happen that the index file of the (future) epoch
      -- already exists. This is checked when opening the database, but
      -- somebody might have created the index file afterwards, behind our
      -- backs.
      True -> do
        -- If the index file already exists, read it in its entirety and
        -- reconstruct the list of offsets from it.
        existingIndex <- loadIndex' hasFS err dbFolder epoch
        -- An already existing index may be invalid.
        unless (isValidIndex existingIndex) $
          throwUnexpectedError err $ InvalidFileError indexFile callStack
        return $ indexToSlotOffsets existingIndex

      False -> return $ 0 NE.:| []

    -- Recalculate nextExpectedRelativeSlot based on the offsets from the
    -- index file (if there was one)
    let nextExpectedRelativeSlot :: RelativeSlot
        nextExpectedRelativeSlot = fromIntegral (length epochOffsets - 1)
    epochSize <- lookupEpochSize err epoch epochSizes
    -- Note that when the file is finalised, 'nextExpectedRelativeSlot' will
    -- be equal to 'epochSize'. This is valid, the user should call
    -- 'startNewEpoch' with an epoch size.
    when (nextExpectedRelativeSlot > fromIntegral epochSize) $
      throwUserError err $ SlotGreaterThanEpochSizeError
                             nextExpectedRelativeSlot
                             epoch
                             epochSize

    eHnd          <- hOpen epochFile AppendMode
    -- If 'hGetSize' fails, 'hClose' the handle, otherwise we leave an open
    -- handle lying around. This is particularly important for the state
    -- machine tests.
    epochFileSize <- EH.onException hasFsErr (hGetSize eHnd) (hClose eHnd)

    -- The last offset of the index file must match the epoch file size
    when (epochFileSize /= NE.head epochOffsets) $ do
      hClose eHnd
      throwUnexpectedError err $ InvalidFileError epochFile callStack

    return OpenState
      { _currentEpoch             = epoch
      , _currentEpochWriteHandle  = eHnd
      , _currentEpochOffsets      = epochOffsets
      , _nextExpectedRelativeSlot = nextExpectedRelativeSlot
      , _epochSizes               = epochSizes
      , _nextIteratorID           = nextIteratorID
      }

-- | Get the 'OpenState' of the given database, throw a 'ClosedDBError' in
-- case it is closed.
getOpenState :: (HasCallStack, MonadSTM m)
             => ErrorHandling ImmutableDBError m
             -> ImmutableDBHandle m
             -> m (OpenState m)
getOpenState err ImmutableDBHandle {..} = do
    internalState <- atomically (readTMVar _dbInternalState)
    case internalState of
       Left  _         -> throwUserError err ClosedDBError
       Right openState -> return openState

-- | Modify the internal state of an open database.
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
modifyOpenState :: forall m r. (HasCallStack, MonadSTM m, MonadMask m)
                => HasFS m
                -> ErrorHandling ImmutableDBError m
                -> ImmutableDBHandle m
                -> (OpenState m -> m (OpenState m, r))
                -> m r
modifyOpenState HasFS{..} err@ErrorHandling{..} ImmutableDBHandle {..} action = do
    (mr, ()) <- generalBracket open close (EH.try err . mutation)
    case mr of
      Left  e      -> throwError e
      Right (_, r) -> return r
  where
    -- We use @m (Either e a)@ instead of @EitherT e m a@ for 'generalBracket'
    -- so that 'close' knows which error is thrown (@Either e (s, r)@ vs. @(s,
    -- r)@).

    open :: m (Either ClosedState (OpenState m))
    open = atomically $ takeTMVar _dbInternalState

    close :: Either ClosedState (OpenState m)
          -> ExitCase (Either ImmutableDBError (OpenState m, r))
          -> m ()
    close st ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> atomically $ putTMVar _dbInternalState st
      -- In case of an exception, most likely at the HasFS layer, close the DB
      -- for safety.
      ExitCaseException _ex -> do
        atomically $ putTMVar _dbInternalState $
          Left $ closedStateFromInternalState st
        closeOpenHandles st
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (ost, _)) ->
        atomically $ putTMVar _dbInternalState (Right ost)
      -- In case of an error (not an exception)
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- When unexpected, close the DB for safety
        atomically $ putTMVar _dbInternalState $
          Left $ closedStateFromInternalState st
        closeOpenHandles st
      ExitCaseSuccess (Left (UserError {})) ->
        -- When a user error, just restore the previous state
        atomically $ putTMVar _dbInternalState st

    mutation :: HasCallStack
             => Either ClosedState (OpenState m)
             -> m (OpenState m, r)
    mutation (Left _)    = throwUserError err ClosedDBError
    mutation (Right ost) = action ost

    -- TODO what if this fails?
    closeOpenHandles :: Either ClosedState (OpenState m) -> m ()
    closeOpenHandles (Left _) = return ()
    closeOpenHandles (Right OpenState {..}) = hClose _currentEpochWriteHandle

-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: Either ClosedState (OpenState m)
                             -> ClosedState
closedStateFromInternalState (Left cst) = cst
closedStateFromInternalState (Right OpenState {..}) = ClosedState
  { _closedEpochSizes     = _epochSizes
  , _closedNextIteratorID = _nextIteratorID
  , _closedLastEpoch      = _currentEpoch
  }


-- | The location of the last blob stored in the database. 'Nothing' in case
-- the database stores no blobs, i.e. is empty.
--
-- This type synonyms is meant for internal used, mainly to avoid @'Maybe'
-- ('Maybe' 'EpochSlot')@.
type LastBlobLocation = Maybe EpochSlot

-- | Return the 'EpochSlot' corresponding to the last blob stored in the
-- database. When the database is empty, 'Nothing' is returned.
--
-- When the current epoch is still empty, opens the index of the previous
-- epoch until a filled slot is found.
lastBlobInDB :: forall m. (HasCallStack, MonadMask m)
             => HasFS m
             -> ErrorHandling ImmutableDBError m
             -> OpenState m
             -> FsPath
             -> m LastBlobLocation
lastBlobInDB hasFS@HasFS{..} err OpenState{..} dbFolder
    | Just relSlot <- lastFilledSlot (indexFromSlotOffsets _currentEpochOffsets)
      -- TODO this can be done more efficiently without converting to an Index.
    = return $ Just $ EpochSlot _currentEpoch relSlot
    | otherwise
    = go _currentEpoch
  where
    -- | Open previous index files looking for the last filled slot.
    -- Look at the epoch file /before/ the given epoch.
    go :: Epoch -> m LastBlobLocation
    go 0          = return Nothing
    go epochAfter = do
      let epoch = epochAfter - 1
      index <- loadIndex' hasFS err dbFolder epoch
      case lastFilledSlot index of
        Just relSlot -> return $ Just $ EpochSlot epoch relSlot
        Nothing      -> go epoch


-- | Go through all files, making two sets: the set of epoch-xxx.dat
-- files, and the set of index-xxx.dat files, discarding all others.
dbFilesOnDisk :: Set String -> (Set Epoch, Set Epoch)
dbFilesOnDisk = foldr categorise mempty
  where
    categorise file fs@(epochFiles, indexFiles) = case parseDBFile file of
      Just ("epoch", n) -> (Set.insert n epochFiles, indexFiles)
      Just ("index", n) -> (epochFiles, Set.insert n indexFiles)
      _                 -> fs

data IndexCheckResult
  = Match Index
    -- ^ The reconstructed index and the index from the index file match.
  | UseReconstructed Index
    -- ^ Use the reconstructed index because the index from the index file is
    -- invalid and should be overwritten.
  | UseFromFile Index Bool
    -- ^ Use the index from the index file as it is valid and contains
    -- trailing empty slots not present in the reconstructed index.
    --
    -- The 'Bool' argument will be 'True' when the index file contained some
    -- more trailing invalid data that must be removed.
  deriving (Eq, Show)


-- | Given a valid index reconstructed from an epoch file and a possibly
-- invalid index read from the index file, check whether the latter is valid
-- and/or contains more useful data.
--
-- A reconstructed index knows nothing about trailing empty slots whereas an
-- index from an index file may be aware of trailing empty slots in the epoch.
--
-- Only trailing empty slots that pad the index to the epoch size are
-- accepted. We don't want an index that ends with empty slots unless it is a
-- finalised epoch, as such an index cannot be the result of regular
-- operations.
checkIndices :: Index  -- ^ The valid reconstructed index
             -> Index  -- ^ The possibly invalid index from the index file.
             -> EpochSize
             -> IndexCheckResult
checkIndices reconstructedIndex indexFromFile epochSize
    | reconstructedIndex == indexFromFile
    = Match reconstructedIndex
    | reconstructedIndex `isPrefixOf` indexFromFile
    , indexSlots extendedIndex == epochSize
    = UseFromFile extendedIndex moreSlots
    | otherwise
    = UseReconstructed reconstructedIndex
  where
    (extendedIndex, moreSlots) =
      extendWithTrailingUnfilledSlotsFrom reconstructedIndex indexFromFile

-- | Execute the 'RecoveryPolicy'.
--
-- Precondition: the size for each epoch <= the epoch to reopen must be
-- present in the map of epoch sizes.
recovery :: forall m e. (HasCallStack, MonadMask m)
         => HasFS m
         -> ErrorHandling ImmutableDBError m
         -> FsPath -> Epoch -> Map Epoch EpochSize
         -> RecoveryPolicy e m
         -> m (Epoch, Maybe LastBlobLocation)
            -- ^ The epoch to (re)open and the last block location if we know
            -- it.
recovery hasFS@HasFS{..} err dbFolder epochToReopen epochSizes recPol = do
    filesInDBFolder <- listDirectory dbFolder
    let (epochFiles, indexFiles) = dbFilesOnDisk filesInDBFolder
    case recPol of
      NoValidation -> do
        errorOnFilesStartingFrom (succ epochToReopen) epochFiles indexFiles
        return (epochToReopen, Nothing)

      ValidateMostRecentEpoch epochFileParser
        | epochToReopen `Set.member` epochFiles -> do
          -- Epoch file is on disk, so validate it
          lastBlobLocation <- validateEpochs [epochToReopen] epochFileParser
          errorOnFilesStartingFrom (succ epochToReopen) epochFiles indexFiles
          -- When we found the lastBlobLocation, return it. If we didn't find
          -- it, don't say the 'LastBlobLocation' is Nothing (= empty
          -- database), because we didn't look at previous epochs. Just say we
          -- don't know the 'LastBlobLocation'
          return (epochToReopen, maybe Nothing (Just . Just) lastBlobLocation)

        | otherwise -> do
          -- No epoch file on disk to validate -> a fresh epoch.

          -- When the index file exists, but the epoch file not, report that
          -- the latter is missing.
          let indexFile = dbFolder <> renderFile "index" epochToReopen
              epochFile = dbFolder <> renderFile "epoch" epochToReopen
          indexFileExists <- doesFileExist indexFile
          when indexFileExists $
            throwUnexpectedError err $ MissingFileError epochFile callStack

          errorOnFilesStartingFrom (succ epochToReopen) epochFiles indexFiles
          return (epochToReopen, Nothing)

      ValidateAllEpochs epochFileParser -> do
        lastBlobLocation <- validateEpochs [0..epochToReopen] epochFileParser
        errorOnFilesStartingFrom (succ epochToReopen) epochFiles indexFiles
        return (epochToReopen, Just lastBlobLocation)

      RestoreToLastValidEpoch epochFileParser -> do
        (freshEpochOrEpochToReopen, lastBlobLocation) <-
           restoreToLastValidEpoch [0..epochToReopen] epochFileParser
        -- When it's a fresh epoch, remove all files starting from that epoch
        -- (inclusive), as a left-over index file may be lying around. When
        -- reopening an epoch, remove all files starting from the next epoch.
        removeFilesStartingFrom (either id succ freshEpochOrEpochToReopen)
          epochFiles indexFiles
        return (either id id freshEpochOrEpochToReopen, Just lastBlobLocation)

  where
    -- | Returns True when the epoch is finalised. If so, the epoch should not
    -- be reopened, but the next \"fresh\" epoch (@epoch + 1@) should be
    -- opened. Also returns the last slot stored in the epoch.
    --
    -- Precondition: the epoch file corresponding to the given epoch must
    -- exist on disk.
    --
    -- The index file doesn't have to exist.
    --
    -- See inline comments for the exact validation/reparation that is
    -- performed.
    validateEpoch :: HasCallStack
                  => Bool  -- ^ True = repair invalid/missing files, False =
                           -- throw an error for invalid/missing files
                  -> Epoch -> EpochFileParser e m (Int, RelativeSlot)
                  -> m (Bool, Maybe RelativeSlot)
    validateEpoch repair epoch epochFileParser = do
      -- According to the precondition, this should never throw a
      -- 'MissingEpochSizeError'.
      epochSize <- lookupEpochSize err epoch epochSizes

      let epochFile = dbFolder <> renderFile "epoch" epoch
      (slotOffsets, mbErr) <- first reconstructSlotOffsets <$>
        runEpochFileParser epochFileParser epochFile
      let reconstructedIndex = indexFromSlotOffsets slotOffsets

      -- When an error was reported, it means that the epoch file contained
      -- some more data that could not be parsed.
      when (isJust mbErr) $
        if repair
          -- Truncate the epoch to get rid of this extra data
          then withFile hasFS epochFile AppendMode $ \eHnd ->
                 hTruncate eHnd (NE.head slotOffsets)
          else throwUnexpectedError err $ InvalidFileError epochFile callStack

      -- Examine the index file on disk
      let indexFile = dbFolder <> renderFile "index" epoch
      mbIndexAndTrailingJunk <- do
        indexFileExists <- doesFileExist indexFile
        if indexFileExists
          then Just <$> loadIndex hasFS dbFolder epoch
          else return Nothing

      (index, overwrite) <- case mbIndexAndTrailingJunk of
        Nothing
          | repair
          -> return (reconstructedIndex, True)
          | otherwise
          -> throwUnexpectedError err $ MissingFileError indexFile callStack

        Just (indexFromFile, mbJunk)
          | not repair, isJust mbJunk
          -> throwUnexpectedError err $ InvalidFileError indexFile callStack
          | otherwise
          -> case checkIndices reconstructedIndex indexFromFile epochSize of
               Match            index -> return (index, isJust mbJunk)
               UseReconstructed index
                 | not repair
                 -> throwUnexpectedError err $
                    InvalidFileError indexFile callStack
                 | otherwise
                 -> return (index, True)
               UseFromFile _ True
                 | not repair
                 -> throwUnexpectedError err $
                    InvalidFileError indexFile callStack
               -- We only use the index from the file when it exactly matches
               -- the index reconstructed from the epoch file, or when it pads
               -- the reconstructed index to the epoch size. So we will never
               -- use an index file to restore to an empty slot that is not
               -- the last of the epoch.
               UseFromFile index moreJunk
                 -> return (index, isJust mbJunk || moreJunk)

      when overwrite $ writeIndex hasFS dbFolder epoch index

      return (epochSize == indexSlots index, lastFilledSlot index)


    -- | Validate all the given epochs using 'validateEpoch'. Return the
    -- location of the last blob stored in the epochs.
    validateEpochs :: HasCallStack
                   => [Epoch]
                   -> EpochFileParser e m (Int, RelativeSlot)
                   -> m LastBlobLocation
    validateEpochs epochs epochFileParser = foldM go Nothing epochs
      where
        go :: LastBlobLocation -> Epoch -> m LastBlobLocation
        go lastBlobLocation epoch = do
          let epochFile = dbFolder <> renderFile "epoch" epoch
          epochFileExists <- doesFileExist epochFile
          if epochFileExists
            then do
              (_, mbLastFilledSlot) <- validateEpoch False epoch epochFileParser
              return $ mplus (EpochSlot epoch <$> mbLastFilledSlot)
                             lastBlobLocation
            else throwUnexpectedError err $ MissingFileError epochFile callStack

    -- | Validate and try to repair all files in the list of epochs. When an
    -- epoch is encountered that is not yet finalised, reopen it.
    --
    --'Left': fresh epoch that has no corresponding files on disk, or
    -- rather, it __may__ not have any.
    --
    -- 'Right': reopen an epoch that has an epoch file and a (possibly
    -- reconstructed) index file on disk.
    restoreToLastValidEpoch :: HasCallStack
                            => [Epoch]
                            -> EpochFileParser e m (Int, RelativeSlot)
                            -> m (Either Epoch Epoch, LastBlobLocation)
    restoreToLastValidEpoch epochs epochFileParser = go 0 Nothing epochs
      where
        go :: Epoch -> LastBlobLocation -> [Epoch]
           -> m (Either Epoch Epoch, LastBlobLocation)
        go prevEpoch lastBlobLocation [] =
          return (Right prevEpoch, lastBlobLocation)
        go _ lastBlobLocation (epoch:epochs') = do
          let epochFile = dbFolder <> renderFile "epoch" epoch
          epochFileExists <- doesFileExist epochFile
          if not epochFileExists
            -- If an epoch file is missing (and all the ones before it were
            -- finalised), open this epoch file as a fresh one.
            then return (Left epoch, lastBlobLocation)
            else do
              (isFinalised, mbLastFilledSlot) <-
                validateEpoch True epoch epochFileParser
              let lastBlobLocation' = mplus
                    (EpochSlot epoch <$> mbLastFilledSlot) lastBlobLocation
              if isFinalised
                then go epoch lastBlobLocation' epochs'
                else return (Right epoch, lastBlobLocation')

    -- | Throw an 'InvalidFileError' for any epoch or index files on disk that
    -- are from an epoch later than the given one. The two sets indicates
    -- which epoch/index files are present on disk.
    errorOnFilesStartingFrom :: HasCallStack
                             => Epoch -> Set Epoch -> Set Epoch -> m ()
    errorOnFilesStartingFrom epoch epochFiles indexFiles
      | Just lastEpoch <- Set.lookupMax epochFiles
      , lastEpoch > epoch
      = throwUserError err $ OpenFinalisedEpochError epoch lastEpoch
      | Just lastIndex <- Set.lookupMax indexFiles
      , lastIndex > epoch
      = throwUserError err $ OpenFinalisedEpochError epoch lastIndex
      | otherwise
      = return ()

    -- | Remove all epoch and index starting from the given epoch (included).
    -- The two sets indicates which epoch/index files are present on disk.
    removeFilesStartingFrom :: Epoch -> Set Epoch -> Set Epoch -> m ()
    removeFilesStartingFrom epoch epochFiles indexFiles = do
        forM_ (takeWhile (>= epoch) (Set.toDescList epochFiles)) $ \e ->
          removeFile (dbFolder <> renderFile "epoch" e)
        forM_ (takeWhile (>= epoch) (Set.toDescList indexFiles)) $ \i ->
          removeFile (dbFolder <> renderFile "index" i)

-- | Given a list of increasing 'SlotOffset's together with the 'Int' (blob
-- size) and 'RelativeSlot' corresponding to the offset, reconstruct a
-- non-empty list of (decreasing) slot offsets.
--
-- The input list (typically returned by 'EpochFileParser') is assumed to be
-- valid: __strictly__ monotonically increasing offsets as well as
-- __strictly__ monotonically increasing relative slots.
--
-- The 'RelativeSlot's are used to detect empty/unfilled slots that will
-- result in repeated offsets in the output, indicating that the size of the
-- slot is 0.
--
-- The output list will always have 0 as last element.
reconstructSlotOffsets :: [(SlotOffset, (Int, RelativeSlot))]
                       -> NonEmpty SlotOffset
reconstructSlotOffsets = go 0 [] 0
  where
    go :: SlotOffset
       -> [SlotOffset]
       -> RelativeSlot
       -> [(SlotOffset, (Int, RelativeSlot))]
       -> NonEmpty SlotOffset
    go offsetAfterLast offsets expectedRelSlot ((offset, (len, relSlot)):olrs') =
      assert (offsetAfterLast == offset) $
      assert (relSlot >= expectedRelSlot) $
      let backfill = indexBackfill relSlot expectedRelSlot offset
      in go (offset + fromIntegral len) (offset : backfill <> offsets)
            (succ relSlot) olrs'
    go offsetAfterLast offsets _lastRelSlot [] = offsetAfterLast NE.:| offsets

-- | Variant of 'loadIndex' that throws an 'InvalidFileError' when 'loadIndex'
-- signalled that there was some invalid trailing data.
loadIndex' :: (HasCallStack, MonadMask m)
           => HasFS m
           -> ErrorHandling ImmutableDBError m
           -> FsPath
           -> Epoch
           -> m Index
loadIndex' hasFS err dbFolder epoch = do
    (index, mbJunk) <- loadIndex hasFS dbFolder epoch
    case mbJunk of
      Just _  -> throwUnexpectedError err $
        InvalidFileError (dbFolder <> renderFile "index" epoch) callStack
      Nothing -> return index

-- | Return the slots to backfill the index file with.
--
-- A situation may arise in which we \"skip\" some relative slots, and we
-- write into the DB, for example, every other relative slot. In this case, we
-- need to backfill the index file with offsets for the skipped relative
-- slots. Similarly, before we start a new epoch, we must backfill the index
-- file of the current epoch file to indicate that it is finalised.
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
-- For the example above, the output of this funciton would thus be: @[6, 6]@.
--
indexBackfill :: RelativeSlot  -- ^ The slot to write to (>= next expected slot)
              -> RelativeSlot  -- ^ The next expected slot to write to
              -> SlotOffset    -- ^ The last 'SlotOffset' written to
              -> [SlotOffset]
indexBackfill (RelativeSlot slot) (RelativeSlot nextExpected) lastOffset =
    replicate gap lastOffset
  where
    gap = fromIntegral $ slot - nextExpected
