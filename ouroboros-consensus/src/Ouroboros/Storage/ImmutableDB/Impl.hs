{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Immutable on-disk database of binary blobs
--
-- = Internal format
--
-- The API of the ImmutableDB uses 'Slot' to indicate a location in the
-- chain\/immutable database. The contents of the database are not stored in
-- one big file that is appended to in eternity, but a separate file is
-- created for each 'Epoch'.
--
-- Within each 'Epoch', the entries are numbered by 'RelativeSlot's. Each
-- 'Slot' can be converted to a combination of an 'Epoch' and a 'RelativeSlot'
-- (= 'EpochSlot') and vice versa. This conversion depends on the size of the
-- epochs: 'EpochSize'. This size will not be the same for each epoch. When
-- opening the database, the user must give a function of type 'Epoch -> m
-- EpochSize' that will be used to find out (and cache using
-- 'CumulEpochSizes') the size of each epoch.
--
-- For example:
--
-- > Epochs:         <──────── 0 ────────> <────── 1 ──────>
-- > Epoch size:               5                   4
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > 'RelativeSlot':   0   1   2   3   4     0   1   2   3
-- > 'Slot':           0   1   2   3   4     5   6   7   8
--
-- = Errors
--
-- Whenever an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError' is thrown
-- during a write operation, e.g., 'appendBinaryBlob', the database will be
-- automatically closed because we can not guarantee a consistent state in the
-- face of file system errors. See the 'reopen' operation and the paragraph
-- below about reopening the database for more information.
--
-- = (Re)opening the database
--
-- The database can be closed and reopened. In case the database was closed
-- because of an unexpected error, the same database can be reopened again
-- with 'reopen' using a 'ValidationPolicy'.
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
-- >   epoch-000.dat
-- >   index-000.dat
-- >   ..
-- >   epoch-008.dat
-- >   index-008.dat
--
-- For each epoch, there are two files on disk:
--
--   * An \"epoch file\" that stores the actual binary blobs. But nothing
--     more, so nothing is stored for empty slots.
--
--   * An \"index file\" that stores the offsets of the binary blobs. These
--     are used to efficiently seek within the epoch file. Index files are
--     only written for \"finalised\" epochs, i.e. an epoch that can no longer
--     be appended to because a new epoch was started.
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
-- @i@ starts. Each @offset i@ is a 'Data.World.Word64' (8 bytes).
--
-- For example, say we have written \"a\" to relative slot 0, \"bravo\" to
-- relative slot 1, and \"haskell\" to slot 4. We get the following index file
-- (the row @offset@ is what is stored in the index file):
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┘
--
-- Note that the last slot we appended to was the 5th slot, the slot with
-- index 4, but there are 6 offsets in the index. In other words, the index
-- contains @slots + 1@ entries (or @lastSlot + 2@). The last offset is the
-- offset for the next slot that can be appended to and can also be used to
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
-- When a new epoch file is started, the index of the current epoch will be
-- padded to @epochSize + 1@ offsets to record that the slots after the last
-- slot that was appended to are empty.
--
-- For example, continuing with the index above, assuming the @epochSize@ is
-- 7, after starting a new epoch, the index file will be padded to the
-- following:
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
module Ouroboros.Storage.ImmutableDB.Impl
  ( openDB
  ) where

import           Prelude hiding (truncate)

import           Control.Exception (assert)
import           Control.Monad (forM, forM_, replicateM_, unless, void, when)
import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import           Control.Monad.Class.MonadThrow (ExitCase (..),
                     MonadCatch (generalBracket), MonadThrow)
import           Control.Monad.State (StateT (..), execStateT, get, lift,
                     modify, put, runStateT, state)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import           Data.Coerce (coerce)
import           Data.Either (isRight)
import           Data.Foldable (minimumBy)
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, isJust, maybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           GHC.Stack (HasCallStack, callStack)

import           System.IO (IOMode (..), SeekMode (..))

import           Ouroboros.Consensus.Util (SomePair (..), whenJust)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util

import           Ouroboros.Storage.ImmutableDB.API hiding (getEpochSize)
import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes (CumulEpochSizes,
                     EpochSlot (..), RelativeSlot (..))
import qualified Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CES
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}


-- | The environment used by the immutable database.
data ImmutableDBEnv m = forall h e. ImmutableDBEnv
    { _dbHasFS           :: !(HasFS m h)
    , _dbErr             :: !(ErrorHandling ImmutableDBError m)
    , _dbInternalState   :: !(TMVar m (Either (ClosedState m) (OpenState m h)))
    , _dbEpochFileParser :: !(EpochFileParser e m (Int, Slot))
    }

-- | Internal state when the database is open.
data OpenState m h = OpenState
    { _currentEpoch            :: !Epoch
    -- ^ The current 'Epoch' the immutable store is writing to.
    , _currentEpochWriteHandle :: !h
    -- ^ The write handle for the current epoch file.
    , _currentEpochOffsets     :: !(NonEmpty SlotOffset)
    -- ^ The offsets to which blobs have been written in the current epoch
    -- file, stored from last to first.
    , _nextExpectedSlot        :: !Slot
    -- ^ The next slot that we can append data to in the epoch file.
    , _getEpochSize            :: !(Epoch -> m EpochSize)
    -- ^ Function to get the size of an epoch.
    , _cumulEpochSizes         :: !CumulEpochSizes
    -- ^ Cache of epoch sizes + conversion between 'Slot' and 'EpochSlot' and
    -- vice versa.
    , _nextIteratorID          :: !IteratorID
    -- ^ The ID of the next iterator that will be created.
    }


-- | Internal state when the database is closed. This contains data that
-- should be restored when the database is reopened. Data not present here
-- will be recovered when reopening.
data ClosedState m = ClosedState
    { _closedGetEpochSize    :: !(Epoch -> m EpochSize)
    -- ^ See '_getEpochSize'.
    , _closedCumulEpochSizes :: !CumulEpochSizes
    -- ^ See '_cumulEpochSizes'
    , _closedNextIteratorID  :: !IteratorID
    -- ^ See '_nextIteratorID'.
    }

{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

-- | Open the database, creating it from scratch if necessary or reopening an
-- existing one using the given 'ValidationPolicy'.
--
-- In addition to the database, the 'Slot' of the last blob stored in it is
-- returned, or 'Nothing' in case the database is empty.
--
-- A function that can be used to look up the size of an epoch must be passed.
-- This function must:
--
-- * For each epoch, return a strictly positive epoch size,
-- * Always return the same epoch size for the same given epoch.
--
-- See 'ValidationPolicy' for more details on the different validation
-- policies.
--
-- An 'EpochFileParser' must be passed in order to reconstruct indices from
-- epoch files.
--
-- __Note__: To be used in conjunction with 'withDB'.
openDB :: (HasCallStack, MonadSTM m, MonadCatch m)
       => HasFS m h
       -> ErrorHandling ImmutableDBError m
       -> (Epoch -> m EpochSize)
       -> ValidationPolicy
       -> EpochFileParser e m (Int, Slot)
       -> m (ImmutableDB m, Maybe Slot)
openDB = openDBImpl

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

mkDBRecord :: (MonadSTM m, MonadCatch m)
           => ImmutableDBEnv m
           -> ImmutableDB m
mkDBRecord dbEnv = ImmutableDB
    { closeDB           = closeDBImpl           dbEnv
    , isOpen            = isOpenImpl            dbEnv
    , reopen            = reopenImpl            dbEnv
    , getNextSlot       = getNextSlotImpl       dbEnv
    , getBinaryBlob     = getBinaryBlobImpl     dbEnv
    , appendBinaryBlob  = appendBinaryBlobImpl  dbEnv
    , streamBinaryBlobs = streamBinaryBlobsImpl dbEnv
    , immutableDBErr    = _dbErr dbEnv
    }

openDBImpl :: forall m h e. (HasCallStack, MonadSTM m, MonadCatch m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> (Epoch -> m EpochSize)
           -> ValidationPolicy
           -> EpochFileParser e m (Int, Slot)
           -> m (ImmutableDB m, Maybe Slot)
openDBImpl hasFS@HasFS{..} err getEpochSize valPol epochFileParser = do
    firstEpochSize <- getEpochSize 0
    let ces0 = CES.singleton firstEpochSize

    (st, lastBlobLocation) <- validateAndReopen hasFS err getEpochSize valPol
      epochFileParser ces0 initialIteratorId

    stVar <- atomically $ newTMVar (Right st)

    let dbEnv = ImmutableDBEnv hasFS err stVar epochFileParser
        db    = mkDBRecord dbEnv
        ces1  = _cumulEpochSizes st
    return (db, epochSlotInThePastToSlot ces1 <$> lastBlobLocation)

closeDBImpl :: (HasCallStack, MonadSTM m)
            => ImmutableDBEnv m
            -> m ()
closeDBImpl ImmutableDBEnv {..} = do
    internalState <- atomically $ takeTMVar _dbInternalState
    case internalState of
      -- Already closed
      Left  _ -> atomically $ putTMVar _dbInternalState internalState
      Right OpenState {..} -> do
        let !closedState = closedStateFromInternalState internalState
        -- Close the database before doing the file-system operations so that
        -- in case these fail, we don't leave the database open.
        atomically $ putTMVar _dbInternalState (Left closedState)
        hClose _currentEpochWriteHandle
  where
    HasFS{..} = _dbHasFS

isOpenImpl :: MonadSTM m => ImmutableDBEnv m -> m Bool
isOpenImpl ImmutableDBEnv {..} =
    isRight <$> atomically (readTMVar _dbInternalState)

-- Note that 'validate' will stop when an 'FsError' is thrown, as usual.
reopenImpl :: (HasCallStack, MonadSTM m, MonadThrow m)
           => ImmutableDBEnv m
           -> ValidationPolicy
           -> Maybe TruncateFrom
           -> m (Maybe Slot)
reopenImpl dbEnv@ImmutableDBEnv {..} valPol mbTruncateFrom = do
    internalState <- atomically $ takeTMVar _dbInternalState
    case internalState of
      -- When still open,
      Right ost
          -- but we have to truncate, so first close the database, then reopen
          -- it again so we end up in the last case
        | Just _ <- mbTruncateFrom
        -> do
          let !closedState = closedStateFromInternalState internalState
          -- Close the database before doing the file-system operations so
          -- that in case these fail, we don't leave the database open.
          atomically $ putTMVar _dbInternalState (Left closedState)
          hClose (_currentEpochWriteHandle ost)
          reopenImpl dbEnv valPol mbTruncateFrom

          -- no need to truncate, so basically a no-op
        | otherwise
        -> do
          -- Put the state back and do nothing
          atomically $ putTMVar _dbInternalState internalState
          lastBlobLocation <- lastBlobInDB _dbHasFS _dbErr ost
          let ces = _cumulEpochSizes ost
          return $ epochSlotInThePastToSlot ces <$> lastBlobLocation

      -- Closed, do all the work
      Left ClosedState {..} ->
        -- Important: put back the state when an error is thrown, otherwise we
        -- have an empty TMVar.
        onException hasFsErr _dbErr
          (atomically $ putTMVar _dbInternalState internalState) $ do

            -- Truncate if requested. First convert the @'TruncateFrom'
            -- 'Slot'@ to an 'EpochSlot'.
            ces' <- case mbTruncateFrom of
              Nothing                  -> return _closedCumulEpochSizes
              Just (TruncateFrom slot) ->
                flip execStateT _closedCumulEpochSizes $ do
                  epochSlot <- CES.getNewEpochSizesUntilM
                    (`CES.slotToEpochSlot` slot) _closedGetEpochSize
                  truncate _dbHasFS _dbEpochFileParser _closedGetEpochSize
                    epochSlot

            (!newOpenState, lastBlobLocation) <- validateAndReopen _dbHasFS
              _dbErr _closedGetEpochSize valPol _dbEpochFileParser
              ces' _closedNextIteratorID

            atomically $ putTMVar _dbInternalState (Right newOpenState)

            return $ epochSlotInThePastToSlot ces' <$> lastBlobLocation
  where
    HasFS{..}         = _dbHasFS
    ErrorHandling{..} = _dbErr


-- | Execute some error handler when an 'ImmutableDBError' or an 'FsError' is
-- thrown while executing an action.
onException :: Monad m
            => ErrorHandling FsError m
            -> ErrorHandling ImmutableDBError m
            -> m b  -- ^ What to do when an error is thrown
            -> m a  -- ^ The action to execute
            -> m a
onException fsErr err onErr m =
    EH.onException fsErr (EH.onException err m onErr) onErr

getNextSlotImpl :: (HasCallStack, MonadSTM m)
                => ImmutableDBEnv m
                -> m Slot
getNextSlotImpl dbEnv = do
    SomePair _hasFS OpenState {..} <- getOpenState dbEnv
    return _nextExpectedSlot

getBinaryBlobImpl
  :: forall m. (HasCallStack, MonadSTM m, MonadThrow m)
  => ImmutableDBEnv m
  -> Slot
  -> m (Maybe ByteString)
getBinaryBlobImpl dbEnv@ImmutableDBEnv {..} slot = do
    SomePair _hasFS OpenState {..} <- getOpenState dbEnv
    -- TODO what if a failed update closes the db while we're in the middle of
    -- this?

    when (slot >= _nextExpectedSlot) $
      throwUserError _dbErr $ ReadFutureSlotError slot _nextExpectedSlot

    let EpochSlot epoch relativeSlot =
          slotInThePastToEpochSlot _cumulEpochSizes slot
        epochFile = renderFile "epoch" epoch
        indexFile = renderFile "index" epoch

    (blobOffset, blobSize) <- case epoch == _currentEpoch of
      -- If the requested epoch is the current epoch, the offsets are still in
      -- memory
      True ->
        case NE.drop toDrop _currentEpochOffsets of
            (offsetAfter:offset:_) -> return (offset, offsetAfter - offset)
            _ -> error "impossible: _currentEpochOffsets out of sync"
          where
            EpochSlot _ lastRelSlot =
              slotInThePastToEpochSlot _cumulEpochSizes (_nextExpectedSlot - 1)
            -- The subtraction above cannot underflow thanks to the @slot >=
            -- _nextExpectedSlot@ check in the beginning.
            toDrop = fromEnum (lastRelSlot - relativeSlot)
            -- Similary for this subtraction

      -- Otherwise, the offsets will have to be read from an index file
      False -> withFile _dbHasFS indexFile ReadMode $ \iHnd -> do
        -- Grab the offset in bytes of the requested slot.
        let indexSeekPosition = fromIntegral (getRelativeSlot relativeSlot)
                              * fromIntegral indexEntrySizeBytes
        hSeek iHnd AbsoluteSeek indexSeekPosition
        -- Compute the offset on disk and the blob size.
        let nbBytesToGet = indexEntrySizeBytes * 2
        -- Note the use of hGetRightSize: we must get enough bytes from the
        -- index file, otherwise 'decodeIndexEntry' (and its variant) would
        -- fail.
        bytes <- hGetRightSize _dbHasFS iHnd nbBytesToGet indexFile
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
      _ -> withFile _dbHasFS epochFile ReadMode $ \eHnd -> do
        -- Seek in the epoch file
        hSeek eHnd AbsoluteSeek (fromIntegral blobOffset)
        Just <$> hGetRightSize _dbHasFS eHnd (fromIntegral blobSize) epochFile
  where
    HasFS{..} = _dbHasFS

appendBinaryBlobImpl :: forall m. (HasCallStack, MonadSTM m, MonadCatch m)
                     => ImmutableDBEnv m
                     -> Slot
                     -> Builder
                     -> m ()
appendBinaryBlobImpl dbEnv@ImmutableDBEnv{..} slot builder =
    modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do
      OpenState { _currentEpoch, _nextExpectedSlot } <- get

      -- Check that the slot is >= the expected next slot and thus not in the
      -- past.
      when (slot < _nextExpectedSlot) $ lift $ throwUserError _dbErr $
        AppendToSlotInThePastError slot _nextExpectedSlot

      -- If the slot is in an epoch > the current one, we have to finalise the
      -- current one and start a new epoch file, possibly skipping some
      -- epochs.
      EpochSlot epoch relSlot <- zoomCumul $ CES.slotToEpochSlotM slot
      -- The operation above can update the _cumulEpochSizes
      when (epoch > _currentEpoch) $ do
        let newEpochsToStart :: Int
            newEpochsToStart = fromIntegral $ epoch - _currentEpoch
        -- Start as many new epochs as needed. Pass the updated state
        -- around.
        replicateM_ newEpochsToStart (startNewEpoch hasFS)

      -- We may have updated the state with 'startNewEpoch', so get the
      -- (possibly) updated state.
      OpenState {..} <- get
      -- If necessary, backfill for any slots we skipped in the current epoch
      nextExpectedRelSlot <- zoomCumul $ CES.slotToRelativeSlotM _nextExpectedSlot
      -- The operation above can update the _cumulEpochSizes
      let lastEpochOffset = NE.head _currentEpochOffsets
          backfillOffsets = indexBackfill relSlot nextExpectedRelSlot
                                          lastEpochOffset

      -- Append to the end of the epoch file.
      bytesWritten <- lift $ onException hasFsErr _dbErr
        (hClose _currentEpochWriteHandle)
        (hPut _currentEpochWriteHandle builder)
        -- In 'modifyOpenState': when an exception occurs, we close the handle
        -- if there is one in the initial open state. However, we might have
        -- opened a new one when we called 'startNewEpoch', and this handle
        -- will be different from the one in the initial state, so
        -- 'modifyOpenState' cannot close it in case of an exception. So take
        -- care of it here.

      let newOffset = lastEpochOffset + bytesWritten

      modify $ \st -> st
        { _currentEpochOffsets =
            (newOffset NE.:| backfillOffsets) <> _currentEpochOffsets
        , _nextExpectedSlot = slot + 1
        }

startNewEpoch :: (HasCallStack, MonadSTM m, MonadCatch m)
              => HasFS m h
              -> StateT (OpenState m h) m ()
startNewEpoch hasFS@HasFS{..} = do
    OpenState {..} <- get
    -- We can close the epoch file
    lift $ hClose _currentEpochWriteHandle

    -- Find out the size of the current epoch, so we can pad the
    -- _currentEpochOffsets to match the size before writing them to the index
    -- file. When looking at the index file, it will then be clear that the
    -- epoch is finalised.
    epochSize <- zoomCumul $ CES.getEpochSizeM _currentEpoch

    -- Calculate what to pad the file with
    EpochSlot epoch nextExpectedRelSlot <- zoomCumul $
      CES.slotToEpochSlotM _nextExpectedSlot

    -- The above calls may have modified the _cumulEpochSizes, so get it
    -- again.
    OpenState {..} <- get
    let lastEpochOffset = NE.head _currentEpochOffsets
        -- An index file of n slots has n + 1 offsets, so pretend we need to
        -- backfill to slot n
        lastRelSlot = succ $ CES.lastRelativeSlot epochSize
        backfillOffsets
          | 0 <- nextExpectedRelSlot, epoch /= _currentEpoch
            -- Edge case: last slot of the epoch is filled, so the next
            -- expected relative slot actually refers to slot 0 of the next
            -- epoch. No backfill needed
          = []
          | otherwise
          = indexBackfill lastRelSlot nextExpectedRelSlot lastEpochOffset

        -- Prepend the backfillOffsets to the current offsets to get a
        -- non-empty list of all the offsets. Note that this list is stored in
        -- reverse order.
        allOffsets = foldr NE.cons _currentEpochOffsets backfillOffsets

    -- Now write the offsets to the index file to disk
    lift $ writeSlotOffsets hasFS _currentEpoch allOffsets

    st <- lift $ mkOpenStateNewEpoch hasFS (succ _currentEpoch) _getEpochSize
      _cumulEpochSizes _nextIteratorID

    put st

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}


-- | Internal handle to an iterator
data IteratorHandle m = forall h. IteratorHandle
  { _it_hasFS :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , _it_state :: !(TVar m (Maybe (IteratorState h)))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , _it_end   :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  }

data IteratorState h = IteratorState
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
  , _it_epoch_handle :: !h
    -- ^ A handle to the epoch file corresponding with '_it_next'.
  , _it_epoch_index  :: Index
    -- ^ We load the index file for the epoch we are currently iterating over
    -- in-memory, as it's going to be small anyway (usually ~150kb).
  }

streamBinaryBlobsImpl :: forall m
                       . (HasCallStack, MonadSTM m, MonadCatch m)
                      => ImmutableDBEnv m
                      -> Maybe Slot  -- ^ When to start streaming (inclusive).
                      -> Maybe Slot  -- ^ When to stop streaming (inclusive).
                      -> m (Iterator m)
streamBinaryBlobsImpl dbEnv@ImmutableDBEnv {..} mbStart mbEnd = do
    SomePair hasFS st@OpenState {..} <- getOpenState dbEnv
    let ces = _cumulEpochSizes

    validateIteratorRange _dbErr _nextExpectedSlot mbStart mbEnd

    mbLastBlob <- lastBlobInDB hasFS _dbErr st
    case mbLastBlob of
      -- The database is empty, just return an empty iterator (directly
      -- exhausted)
      Nothing       -> mkEmptyIterator
      Just lastBlob -> do
        -- Fill in missing bounds
        let start@(EpochSlot startEpoch startSlot) =
              maybe (EpochSlot 0 0) (slotInThePastToEpochSlot ces) mbStart
            end = maybe lastBlob (slotInThePastToEpochSlot ces) mbEnd

        -- Helper function to open the index file of an epoch.
        let openIndex epoch
              | epoch == _currentEpoch
              = return $ indexFromSlotOffsets _currentEpochOffsets
              | otherwise
              = loadIndex' hasFS _dbErr epoch

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
                  -- enough to guarantee that we will never open an epoch in
                  -- the future
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

            eHnd <- hOpen (renderFile "epoch" nextEpoch) ReadMode
            -- Invariant 2 = OK

            -- Invariant 3 = OK by the search above for a filled slot

            -- Position the epoch handle at the right place. Invariant 4 = OK
            let offset = fromIntegral (offsetOfSlot index nextSlot)
            -- Close the handle if the seek fails
            onException hasFsErr _dbErr (hClose eHnd) $
              hSeek eHnd AbsoluteSeek offset

            return $ Just IteratorState
              { _it_next = next
              , _it_epoch_handle = eHnd
              , _it_epoch_index = index
              }

        itState <- atomically $ newTVar mbIteratorState

        let ith = IteratorHandle
              { _it_hasFS = _dbHasFS
              , _it_state = itState
              , _it_end   = end
              }
        -- Safely increment '_nextIteratorID' in the 'OpenState'.
        modifyOpenState dbEnv $ \_hasFS -> state $ \st'@OpenState {..} ->
          let it = Iterator
                { iteratorNext  = iteratorNextImpl  dbEnv ith
                , iteratorClose = iteratorCloseImpl       ith
                , iteratorID    = _nextIteratorID
                }
          in (it, st' { _nextIteratorID = succ _nextIteratorID })
  where
    HasFS{..} = _dbHasFS

    mkEmptyIterator :: m (Iterator m)
    mkEmptyIterator =
      modifyOpenState dbEnv $ \_hasFS -> state $ \st@OpenState {..} ->
        let it = Iterator
              { iteratorNext  = return IteratorExhausted
              , iteratorClose = return ()
              , iteratorID    = _nextIteratorID
              }
        in (it, st { _nextIteratorID = succ _nextIteratorID })


iteratorNextImpl :: forall m. (HasCallStack, MonadSTM m, MonadThrow m)
                 => ImmutableDBEnv m
                 -> IteratorHandle m
                 -> m IteratorResult
iteratorNextImpl dbEnv it@IteratorHandle {_it_hasFS = hasFS :: HasFS m h, ..} = do
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
        SomePair _hasFS st <- getOpenState dbEnv
        let ces = _cumulEpochSizes st
            slot = epochSlotInThePastToSlot ces (_it_next iteratorState)
        return $ IteratorResult slot blob
  where
    HasFS{..} = hasFS

    readNext :: IteratorState h -> m ByteString
    readNext IteratorState { _it_epoch_handle = eHnd
                           , _it_next = EpochSlot epoch slot
                           , _it_epoch_index = index } = do
      -- Grab the blob size from the cached index
      let blobSize = sizeOfSlot index slot

      -- Read from the epoch file. No need for seeking: as we are streaming,
      -- we are already positioned at the correct place (Invariant 4).
      let epochFile = renderFile "epoch" epoch
      hGetRightSize hasFS eHnd (fromIntegral blobSize) epochFile

    -- Move the iterator to the next position that can be read from, advancing
    -- epochs if necessary. If no next position can be found, the iterator is
    -- closed.
    stepIterator :: IteratorState h -> m ()
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
          -> iteratorCloseImpl it
          where
            next = EpochSlot epoch nextSlot

        -- Epoch exhausted, open the next epoch
        Nothing -> do
          hClose eHnd
          SomePair _hasFS st <- getOpenState dbEnv
          openNextNonEmptyEpoch (epoch + 1) st

    -- Start opening epochs (starting from the given epoch number) until we
    -- encounter a non-empty one, then update the iterator state accordingly.
    -- If no non-empty epoch can be found, the iterator is closed.
    openNextNonEmptyEpoch :: Epoch -> OpenState m h' -> m ()
    openNextNonEmptyEpoch epoch st@OpenState {..}
      | epoch > _epoch _it_end
      = iteratorCloseImpl it
      | otherwise = do
        -- Thanks to the guard we know that epoch <= _epoch _it_end. We also
        -- know that _epoch _it_end is <= _currentEpoch, so we know that epoch
        -- <= _currentEpoch.
        index <- case epoch == _currentEpoch of
          True  -> return $ indexFromSlotOffsets _currentEpochOffsets
          False -> loadIndex' hasFS (_dbErr dbEnv) epoch

        case firstFilledSlot index of
          -- Empty epoch -> try the next one
          Nothing -> openNextNonEmptyEpoch (epoch + 1) st
          Just slot
            -- Slot is after the end -> stop
            | EpochSlot epoch slot > _it_end -> iteratorCloseImpl it
            | otherwise -> do
              let epochFile = renderFile "epoch" epoch
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
                  => IteratorHandle m
                  -> m ()
iteratorCloseImpl IteratorHandle {..} = do
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
  where
    HasFS{..} = _it_hasFS

{------------------------------------------------------------------------------
  Internal functions
------------------------------------------------------------------------------}

-- | Perform validation as per the 'ValidationPolicy' using 'validate',
-- truncate to the last filled slot in the database if necessary, create an
-- 'OpenState' corresponding to the epoch to reopen, and also return the
-- location of the last filled slot in the database, or 'Nothing' when empty.
validateAndReopen :: (HasCallStack, MonadThrow m)
                  => HasFS m h
                  -> ErrorHandling ImmutableDBError m
                  -> (Epoch -> m EpochSize)
                  -> ValidationPolicy
                  -> EpochFileParser e m (Int, Slot)
                  -> CumulEpochSizes
                  -> IteratorID
                  -> m (OpenState m h, Maybe EpochSlot)
validateAndReopen hasFS err getEpochSize valPol epochFileParser ces nextIteratorID = do
    ((mbLastEpochAndIndex, mbLastBlobLocation), ces') <-
      flip runStateT ces $
      validate hasFS err getEpochSize valPol epochFileParser

    -- If we get to this point, validation didn't throw an error and all files
    -- on disk must be valid. The only things we have to deal with are:
    -- + The last epoch(s) on disk might not contain slots or might end with
    --   some empty slots. We must truncate to the last filled slot in the
    --   database.
    -- + If only the most recent epoch was validated, 'validate' will
    --   not return an index file
    --
    -- Note: if the last epoch on disk is not finalised, it will not have an
    -- index file. However, 'validate' has returned its reconstructed index.

    case mbLastEpochAndIndex of
      -- The database is empty, open a fresh state
      Nothing -> do
        ost <- mkOpenStateNewEpoch hasFS 0 getEpochSize ces' nextIteratorID
        return (ost, Nothing)

      Just (lastEpochOnDisk, index) -> do

        -- Figure out the lastBlobLocation if we don't already know it
        lastBlobLocation <- case mbLastBlobLocation of
          Just lastBlobLocation
            -> return lastBlobLocation
          Nothing
            | 0 <- lastEpochOnDisk
              -- If we didn't find a blob in epoch 0, there are none
            -> return Nothing
            | otherwise
            -> lastBlobOnDisk hasFS err (lastEpochOnDisk - 1)

        -- Truncate to the last filled slot
        ost <- case lastBlobLocation of
          -- We already handled the case of an empty database, so in this
          -- case, the database is not empty, but doesn't contain a blob. So
          -- it only contains empty slots. Truncate it completely.
          Nothing
            -> do
              truncateFromEpoch hasFS 0
              mkOpenStateNewEpoch hasFS 0 getEpochSize ces' nextIteratorID

          Just (EpochSlot epoch relSlot)
            | epoch   == lastEpochOnDisk
            , relSlot == lastSlot index
              -- No truncation needed
            -> mkOpenState hasFS epoch getEpochSize ces' nextIteratorID index

            | otherwise
              -- Truncation needed
            -> do
              let indexOrEpochFileParser
                    | epoch == lastEpochOnDisk = Left index
                    | otherwise                = Right epochFileParser
              (index', ces'') <- flip runStateT ces' $
                truncateFromEpochSlot hasFS indexOrEpochFileParser getEpochSize
                  (EpochSlot epoch (succ relSlot))
              mkOpenState hasFS epoch getEpochSize ces'' nextIteratorID index'

        return (ost, lastBlobLocation)

-- | Create the internal open state based on an epoch with the given 'Index'.
--
-- Open the epoch file for appending.
mkOpenState :: (HasCallStack, MonadThrow m)
            => HasFS m h
            -> Epoch
            -> (Epoch -> m EpochSize)
            -> CumulEpochSizes
            -> IteratorID
            -> Index
            -> m (OpenState m h)
mkOpenState HasFS{..} epoch getEpochSize ces nextIteratorID index = do
    let epochFile    = renderFile "epoch" epoch
        epochOffsets = indexToSlotOffsets index

    -- Add missing epoch sizes to ces'
    (epochStartSlot, ces') <- flip runStateT ces $ CES.getNewEpochSizesUntilM
      (`CES.epochSlotToSlot` EpochSlot epoch 0) getEpochSize

    -- Use the offsets from the index file (if there was one) to determine the
    -- next relative slot to write to.
    let nextExpectedRelSlot = fromIntegral (length epochOffsets - 1)
        nextExpectedSlot    = epochStartSlot + nextExpectedRelSlot

    eHnd <- hOpen epochFile AppendMode

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _nextExpectedSlot        = nextExpectedSlot
      , _getEpochSize            = getEpochSize
      , _cumulEpochSizes         = ces'
      , _nextIteratorID          = nextIteratorID
      }

-- | Create the internal open state for a new empty epoch.
--
-- Open the epoch file for appending.
mkOpenStateNewEpoch :: (HasCallStack, MonadThrow m)
                    => HasFS m h
                    -> Epoch
                    -> (Epoch -> m EpochSize)
                    -> CumulEpochSizes
                    -> IteratorID
                    -> m (OpenState m h)
mkOpenStateNewEpoch HasFS{..} epoch getEpochSize ces nextIteratorID = do
    let epochFile    = renderFile "epoch" epoch
        epochOffsets = 0 NE.:| []

    -- Add missing epoch sizes to ces'
    (epochStartSlot, ces') <- flip runStateT ces $ CES.getNewEpochSizesUntilM
      (`CES.epochSlotToSlot` EpochSlot epoch 0) getEpochSize

    eHnd <- hOpen epochFile AppendMode
    -- TODO Use new O_EXCL create when we expect it to be empty, see #292

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _nextExpectedSlot        = epochStartSlot
      , _getEpochSize            = getEpochSize
      , _cumulEpochSizes         = ces'
      , _nextIteratorID          = nextIteratorID
      }


-- | Get the 'OpenState' of the given database, throw a 'ClosedDBError' in
-- case it is closed.
--
-- NOTE: Since the 'OpenState' is parameterized over a type parameter @h@ of
-- handles, which is not visible from the type of the @ImmutableDBEnv@,
-- we return a @SomePair@ here that returns the open state along with a 'HasFS'
-- instance for the /same/ type parameter @h@. Note that it would be impossible
-- to use an existing 'HasFS' instance already in scope otherwise, since the
-- @h@ parameters would not be known to match.
getOpenState :: (HasCallStack, MonadSTM m)
             => ImmutableDBEnv m
             -> m (SomePair (HasFS m) (OpenState m))
getOpenState ImmutableDBEnv {..} = do
    internalState <- atomically (readTMVar _dbInternalState)
    case internalState of
       Left  _         -> throwUserError _dbErr ClosedDBError
       Right openState -> return (SomePair _dbHasFS openState)

-- | Modify the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case a 'FileSystemError' is thrown, the database is closed to prevent
-- further appending to a database in a potentially inconsistent state.
--
-- __Note__: This /takes/ the 'TMVar', /then/ runs the action (which might be
-- in 'IO'), and then puts the 'TMVar' back, just like
-- 'Control.Concurrent.MVar.modifyMVar' does. Consequently, it has the same
-- gotchas that @modifyMVar@ does; the effects are observable and it is
-- susceptible to deadlock.
--
-- TODO(adn): we should really just use 'Control.Concurrent.MVar.MVar' rather
-- than 'TMVar', but we currently don't have a simulator for code using
-- @MVar@.
modifyOpenState :: forall m r. (HasCallStack, MonadSTM m, MonadCatch m)
                => ImmutableDBEnv m
                -> (forall h. HasFS m h -> StateT (OpenState m h) m r)
                -> m r
modifyOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (EH.try _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (r, _) -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    -- We use @m (Either e a)@ instead of @EitherT e m a@ for 'generalBracket'
    -- so that 'close' knows which error is thrown (@Either e (s, r)@ vs. @(s,
    -- r)@).

    open :: m (Either (ClosedState m) (OpenState m h))
    open = atomically $ takeTMVar _dbInternalState

    close :: Either (ClosedState m) (OpenState m h)
          -> ExitCase (Either ImmutableDBError (r, OpenState m h))
          -> m ()
    close !st ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> atomically $ putTMVar _dbInternalState st
      -- In case of an exception, most likely at the HasFS layer, close the DB
      -- for safety.
      ExitCaseException _ex -> do
        let !cst = closedStateFromInternalState st
        atomically $ putTMVar _dbInternalState (Left cst)
        closeOpenHandles st
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (_, ost)) ->
        atomically $ putTMVar _dbInternalState (Right ost)
      -- In case of an error (not an exception)
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- When unexpected, close the DB for safety
        let !cst = closedStateFromInternalState st
        atomically $ putTMVar _dbInternalState (Left cst)
        closeOpenHandles st
      ExitCaseSuccess (Left (UserError {})) ->
        -- When a user error, just restore the previous state
        atomically $ putTMVar _dbInternalState st

    mutation :: HasCallStack
             => Either (ClosedState m) (OpenState m h)
             -> m (r, OpenState m h)
    mutation (Left _)    = throwUserError _dbErr ClosedDBError
    mutation (Right ost) = runStateT (action hasFS) ost

    -- TODO what if this fails?
    closeOpenHandles :: Either (ClosedState m) (OpenState m h) -> m ()
    closeOpenHandles (Left _)               = return ()
    closeOpenHandles (Right OpenState {..}) = hClose _currentEpochWriteHandle


-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: Either (ClosedState m) (OpenState m h)
                             -> ClosedState m
closedStateFromInternalState (Left cst) = cst
closedStateFromInternalState (Right OpenState {..}) = ClosedState
  { _closedGetEpochSize    = _getEpochSize
  , _closedCumulEpochSizes = _cumulEpochSizes
  , _closedNextIteratorID  = _nextIteratorID
  }

-- | Run the given function using '_getEpochSize' from the open state,
-- updating the '_cumulEpochSizes' from the open state, storing its updated
-- value afterwards.
zoomCumul :: Monad m
          => (    (Epoch -> m EpochSize)
               -> StateT CumulEpochSizes m a
             )
          -> StateT (OpenState m h) m a
zoomCumul m = do
    OpenState { _cumulEpochSizes = ces, _getEpochSize } <- get
    (a, ces') <- lift $ runStateT (m _getEpochSize) ces
    modify $ \st -> st { _cumulEpochSizes = ces' }
    return a

-- | Convert an 'EpochSlot' in the past (<= the current epoch) using an up to
-- date 'CumulEpochSizes' to a 'Slot'.
--
-- This conversion may not fail, as the 'EpochSlot' must be in the past, and
-- all past epoch sizes are known.
epochSlotInThePastToSlot :: HasCallStack
                         => CumulEpochSizes -> EpochSlot -> Slot
epochSlotInThePastToSlot ces epochSlot = fromMaybe
  (error "Could not convert EpochSlot to Slot") $
  CES.epochSlotToSlot ces epochSlot

-- | Convert a 'Slot' in the past (<= the next slot to write to) using an up
-- to date 'CumulEpochSizes' to an 'EpochSlot'.
--
--This conversion may not fail, as the 'Slot' must be in the past, and all
-- past epoch sizes are known.
slotInThePastToEpochSlot :: HasCallStack
                         => CumulEpochSizes -> Slot -> EpochSlot
slotInThePastToEpochSlot ces slot = fromMaybe
  (error "Could not convert Slot to EpochSlot") $
  CES.slotToEpochSlot ces slot

-- | Return the 'EpochSlot' corresponding to the last blob stored in the
-- database. When the database is empty, 'Nothing' is returned.
--
-- When the current epoch is still empty, the indices of previous epochs are
-- opened until a filled slot is found.
lastBlobInDB :: forall m h. (HasCallStack, MonadThrow m)
             => HasFS m h
             -> ErrorHandling ImmutableDBError m
             -> OpenState m h
             -> m (Maybe EpochSlot)
lastBlobInDB hasFS err OpenState {..}
    | Just relSlot <- lastFilledSlot (indexFromSlotOffsets _currentEpochOffsets)
      -- TODO low priority (seldomly called): this can be done more
      -- efficiently without converting to an Index.
    = return $ Just $ EpochSlot _currentEpoch relSlot
    | 0 <- _currentEpoch
      -- If in epoch 0 and the in-memory offsets told us there is no blob,
      -- don't look at the disk, also because there are no indices on disk.
    = return Nothing
    | otherwise
    = lastBlobOnDisk hasFS err (_currentEpoch - 1)


-- | Open index files looking for the last filled slot, starting from the
-- given epoch and going to previous epochs.
--
-- Assumes index files are present and valid.
lastBlobOnDisk :: (HasCallStack, MonadThrow m)
               => HasFS m h
               -> ErrorHandling ImmutableDBError m
               -> Epoch
               -> m (Maybe EpochSlot)
lastBlobOnDisk hasFS err = go
  where
    go epoch = do
      index <- loadIndex' hasFS err epoch
      case lastFilledSlot index of
        Just relSlot -> return $ Just $ EpochSlot epoch relSlot
        Nothing
          | 0 <- epoch -> return Nothing
          | otherwise  -> go (epoch - 1)

-- | Go through all files, making two sets: the set of epoch-xxx.dat
-- files, and the set of index-xxx.dat files, discarding all others.
dbFilesOnDisk :: Set String -> (Set Epoch, Set Epoch)
dbFilesOnDisk = foldr categorise mempty
  where
    categorise file fs@(epochFiles, indexFiles) = case parseDBFile file of
      Just ("epoch", n) -> (Set.insert n epochFiles, indexFiles)
      Just ("index", n) -> (epochFiles, Set.insert n indexFiles)
      _                 -> fs

-- | The location of the last blob stored in the database. 'Nothing' in case
-- the database stores no blobs, i.e. is empty.
--
-- This type synonyms is meant for internal used, mainly to avoid @'Maybe'
-- ('Maybe' 'Slot')@.
type LastBlobLocation = Maybe EpochSlot

-- | Truncate to the given 'EpochSlot'.
--
-- May leave an unfilled slot as the last slot, it is up to the caller to
-- choose the right 'TruncateFrom' so that the last slot in the database is
-- filled.
--
-- Assumes everything stored on disk before the truncation point is valid. If
-- the index is missing of the epoch to truncate in, the 'EpochFileParser'
-- will be used. If it is present, it must at least be valid up to the slot to
-- truncate to. The remainder of the index file may be invalid. The contents
-- of an epoch after the truncation point are allowed to be invalid.
truncate :: (HasCallStack, MonadThrow m)
         => HasFS m h
         -> EpochFileParser e m (Int, Slot)
         -> (Epoch -> m EpochSize)
         -> EpochSlot
         -> StateT CumulEpochSizes m ()
truncate hasFS epochFileParser getEpochSize epochSlot = case epochSlot of
    EpochSlot epoch 0 -> lift $ truncateFromEpoch hasFS epoch
    _                 -> void $
      truncateFromEpochSlot hasFS (Right epochFileParser) getEpochSize
        epochSlot

-- | Truncate everything starting from the given epoch (inclusive).
--
-- Doesn't read any index files.
truncateFromEpoch :: (HasCallStack, MonadThrow m)
                  => HasFS m h
                  -> Epoch
                  -> m ()
truncateFromEpoch = removeFilesStartingFrom

-- | Truncate from the given 'EpochSlot' (inclusive).
--
-- In order to truncate to some relative slot, we need to know the right
-- offset of it. Either we get it from an in-memory 'Index' that corresponds
-- to the epoch, or we try to load the index from disk. However, if the epoch
-- is unfinalised, it will not have an index file, so in that case,
-- reconstruct it using the 'EpochFileParser'.
--
-- Return the truncated index of the epoch.
--
-- The index file is deleted from disk, as we only want complete index files
-- on disk.
truncateFromEpochSlot :: (HasCallStack, MonadThrow m)
                      => HasFS m h
                      -> Either Index (EpochFileParser e m (Int, Slot))
                      -> (Epoch -> m EpochSize)
                      -> EpochSlot
                      -> StateT CumulEpochSizes m Index
truncateFromEpochSlot hasFS@HasFS{..} indexOrEpochFileParser getEpochSize epochSlot = do
    let EpochSlot epoch relSlot = epochSlot
    lift $ removeFilesStartingFrom hasFS (succ epoch)
    let indexFile = renderFile "index" epoch
        epochFile = renderFile "epoch" epoch
    indexFileExists <- lift $ doesFileExist indexFile
    -- Remove an index file that might be laying around
    when indexFileExists $ lift $ removeFile indexFile

    index <- case indexOrEpochFileParser of
      Left  index           -> return index
      Right epochFileParser ->
        -- TODO we could try reading the index file if it exists and accept it
        -- when it is complete
        fst <$> reconstructIndex epochFile epochFileParser getEpochSize
    let truncatedIndexSize :: EpochSize
        truncatedIndexSize = coerce relSlot
        truncatedIndex     = truncateToSlots truncatedIndexSize index
    -- Truncate the epoch file
    lift $ withFile hasFS epochFile AppendMode $ \eHnd ->
      hTruncate eHnd (lastSlotOffset truncatedIndex)
    return truncatedIndex


-- | Remove all epoch and index starting from the given epoch (included).
removeFilesStartingFrom :: (HasCallStack, Monad m)
                        => HasFS m h
                        -> Epoch
                        -> m ()
removeFilesStartingFrom HasFS{..} epoch = do
    filesInDBFolder <- listDirectory []
    let (epochFiles, indexFiles) = dbFilesOnDisk filesInDBFolder
    forM_ (takeWhile (>= epoch) (Set.toDescList epochFiles)) $ \e ->
      removeFile (renderFile "epoch" e)
    forM_ (takeWhile (>= epoch) (Set.toDescList indexFiles)) $ \i ->
      removeFile (renderFile "index" i)

-- | Execute the 'ValidationPolicy'.
--
-- * Epoch files are the main source of truth. Index files can be
--   reconstructed from the epoch files using the 'EpochFileParser'.
--
-- * Only complete index files (with the same number of slots as the epoch
--   size) are valid. An error will be thrown when an incomplete or invalid
--   index file is encountered.
--
-- * The last, unfinalised epoch will not have an index file. We do our best
--   to only reconstruct its index once.
--
-- * Index files are checked against the indices reconstructed from the epoch
--   files. Reconstructed indices are unaware of empty trailing slots. Special
--   case: when the last slot of an epoch is filled, the reconstructed index
--   gives us all the information we need, because there can't be any trailing
--   empty slots that only the index file could now about. In this case, we
--   overwrite the index file if it is missing or invalid without throwing an
--   error. This means that if all index files are missing, but the last slot
--   of each epoch is filled, we can reconstruct all index files from the
--   epochs without needing any truncation or throwing an error.
--
-- * An error will be thrown for the \"earliest\" invalid slot in the
--   database. A 'TruncateFrom' can be extracted from the error using
--   'extractTruncateFrom', and can be used to truncate the database (using
--   'reopen') so that all invalid\/missing files are removed or truncated,
--   and that the remaining contents of the database are valid. So the error
--   is thrown that requires the most truncation to fix, so that a single
--   truncation (and validation) is enough to bring the database back in a
--   valid state.
validate :: forall m h e. (HasCallStack, MonadThrow m)
         => HasFS m h
         -> ErrorHandling ImmutableDBError m
         -> (Epoch -> m EpochSize)
         -> ValidationPolicy
         -> EpochFileParser e m (Int, Slot)
         -> StateT CumulEpochSizes m ( Maybe (Epoch, Index)
                                     , Maybe LastBlobLocation
                                     )
            -- ^ The last epoch on disk and its index (or 'Nothing' when there
            -- is no epoch on disk), and the last block location if we know it
            -- ('Nothing' if we don't know it).
validate hasFS@HasFS{..} err getEpochSize valPol epochFileParser = do
    filesInDBFolder <- lift $ listDirectory []
    let epochFiles        = fst $ dbFilesOnDisk filesInDBFolder
        mbLastEpochOnDisk = Set.lookupMax epochFiles
    case mbLastEpochOnDisk of
      Nothing -> do
        -- Throw an error for left-over index files
        errorOnIndexFileFrom 0
        return (Nothing, Just Nothing)

      Just lastEpochOnDisk -> case valPol of

        ValidateMostRecentEpoch -> do
          (index, lastBlobLocation) <- validateEpochs lastEpochOnDisk []
          -- If there are index files for epochs after the last epoch file on
          -- disk, then some epoch file(s) must be missing.
          errorOnIndexFileFrom (succ lastEpochOnDisk)
          return ( Just (lastEpochOnDisk, index)
               -- When we found the lastBlobLocation, return it. If we didn't
               -- find it, don't say the 'LastBlobLocation' is Nothing (=
               -- empty database), because we didn't look at previous epochs.
               -- Just say we don't know the 'LastBlobLocation'
                 , maybe Nothing (Just . Just) lastBlobLocation
                 )

        ValidateAllEpochs -> do
          (index, lastBlobLocation) <- validateEpochs 0 [1..lastEpochOnDisk]
          errorOnIndexFileFrom (succ lastEpochOnDisk)
          return ( Just (lastEpochOnDisk, index)
                   -- We looked at all epochs, so we know the last blob
                   -- location.
                 , Just lastBlobLocation
                 )
  where
    -- | Validate all the given epochs using 'validateEpoch'. Return the index
    -- of the last epoch in the list (or the given epoch if the list is
    -- empty). Also returns the location of the last known blob, which is not
    -- necessarily in the last epoch.
    --
    -- When this functions returns without throwing an error, all the given
    -- epochs are valid.
    validateEpochs :: HasCallStack
                   => Epoch
                   -> [Epoch]
                   -> StateT CumulEpochSizes m (Index, LastBlobLocation)
    validateEpochs epoch epochs = do
        index <- validateEpoch epoch (null epochs)
        let lastBlobLocation = EpochSlot epoch <$> lastFilledSlot index
        go index lastBlobLocation epochs
      where
        go index lastBlobLocation [] = return (index, lastBlobLocation)
        go _     lastBlobLocation (epoch':epochs') = do
          index' <- validateEpoch epoch' (null epochs')
          let lastBlobLocation'
                | Just relSlot <- lastFilledSlot index'
                = Just $ EpochSlot epoch' relSlot
                | otherwise
                = lastBlobLocation
          go index' lastBlobLocation' epochs'

    -- | Validates the epoch and index file of the given epoch.
    --
    -- Reconstructs the index by parsing the epoch file. If there remains
    -- unparsed data, the epoch file is invalid.
    --
    -- Reads the index from the index file. If there remains unparsed data,
    -- the index file is invalid.
    --
    -- If the reconstructed index and the index from the index file don't
    -- match, we blame it on the index file and report an error.
    --
    -- Note that an index file can tell us more than the reconstructed index,
    -- i.e. the presence of trailing empty slots, which we will accept as the
    -- truth.
    --
    -- Special case: if the last slot of the epoch is filled, the index file
    -- won't tell us any more than the reconstructed index, so overwrite it
    -- when missing or invalid and don't report an error.
    --
    -- Important: throw the error that requires the \"earliest\" or the most
    -- truncation.
    validateEpoch :: HasCallStack
                  => Epoch
                  -> Bool  -- ^ This epoch is the last epoch on disk
                  -> StateT CumulEpochSizes m Index
    validateEpoch epoch isLastEpoch = do
      epochSize <- CES.getEpochSizeM epoch getEpochSize

      ces <- get
      -- Local helper function to create a 'TruncateFrom'
      let mkTruncateFrom :: EpochSlot -> TruncateFrom
          mkTruncateFrom epochSlot =
            TruncateFrom (epochSlotInThePastToSlot ces epochSlot)
          truncateFromEpochStart   = mkTruncateFrom (EpochSlot epoch 0)

      let epochFile = renderFile "epoch" epoch
      epochFileExists <- lift $ doesFileExist epochFile
      unless epochFileExists $ lift $ throwUnexpectedError err $
        MissingFileError epochFile truncateFromEpochStart callStack

      -- Read the epoch file and reconstruct an index from it.
      (reconstructedIndex, mbErr) <- reconstructIndex epochFile epochFileParser
        getEpochSize

      -- In the following block we collect all errors (in the state) so we can
      -- decide afterwards which error we should throw, i.e. the error which
      -- requires the most truncation.
      (index, errors) <- lift $ flip runStateT [] $ do
            -- Local helper to add an error
        let addError trunc e = modify ((trunc, e):)

        -- Truncation point based on the last valid blob in the epoch file
        let truncLastValidBlobInEpoch
              | Just relSlot <- lastFilledSlot reconstructedIndex
              = mkTruncateFrom (EpochSlot epoch (succ relSlot))
              | otherwise
              = truncateFromEpochStart

        whenJust mbErr $ const $ addError truncLastValidBlobInEpoch $
          InvalidFileError epochFile truncLastValidBlobInEpoch callStack

        -- If the last slot of the epoch is filled, we don't need an index
        -- file. We can reconstruct it and don't have to throw an error.
        let lastSlotFilled = indexSlots reconstructedIndex == epochSize

        let indexFile = renderFile "index" epoch
        indexFileExists <- lift $ doesFileExist indexFile
        if
            -- If the last slot of the epoch is filled, we know all we need to
            -- know from the reconstructed index, as there can't be any
            -- trailing empty slots that the reconstructed index will be
            -- unaware of. Write the reconstructed index to disk if needed.
          | lastSlotFilled -> do
            overwrite <- if indexFileExists
              then do
                (indexFromFile, mbJunk) <- lift $ loadIndex hasFS epoch
                return $ indexFromFile /= reconstructedIndex || isJust mbJunk
              else return True
            when overwrite $
              -- TODO log
              lift $ writeIndex hasFS epoch reconstructedIndex
            return reconstructedIndex

          | indexFileExists -> do
            (indexFromFile, mbJunk) <- lift $ loadIndex hasFS epoch
            if | reconstructedIndex `isPrefixOf` indexFromFile -> do
                 -- A reconstructed index knows nothing about trailing empty
                 -- slots whereas an index from an index file may be aware of
                 -- trailing empty slots in the epoch.
                 --
                 -- If the index from the index file pads the end of the
                 -- reconstructed index with empty slots so that the epoch is
                 -- full, we accept it, otherwise it is incomplete and thus
                 -- invalid.
                 --
                 -- We don't want an index that ends with empty slots unless
                 -- it is a finalised epoch, as such an index cannot be the
                 -- result of regular operations.
                 let extendedIndex = extendWithTrailingUnfilledSlotsFrom
                       reconstructedIndex indexFromFile
                 if indexSlots extendedIndex /= epochSize ||
                    indexSlots indexFromFile > indexSlots extendedIndex
                   then do
                     addError truncLastValidBlobInEpoch $
                       InvalidFileError indexFile truncLastValidBlobInEpoch
                         callStack
                     lift $ removeFile indexFile
                     return reconstructedIndex
                   else do
                     -- The index file was correct. Unless it contained some
                     -- junk at the end.
                     whenJust mbJunk $ const $ lift $
                       -- TODO log
                       writeIndex hasFS epoch extendedIndex
                     return extendedIndex

               | otherwise -> do
                 -- No prefix: the index file is invalid
                 addError truncLastValidBlobInEpoch $
                   InvalidFileError indexFile truncLastValidBlobInEpoch
                     callStack
                 -- The index file must be removed, because truncating to the
                 -- last blob in the epoch might not truncate enough of the
                 -- invalid index file.
                 lift $ removeFile indexFile
                 return reconstructedIndex

            -- When we're validating the last epoch on disk, there will be no
            -- index on disk, just use the reconstructed index. There is also
            -- no need to write an index file, as only complete epochs should
            -- have an index file.
          | isLastEpoch -> return reconstructedIndex

            -- No index file while there should be one
          | otherwise -> do
            addError truncLastValidBlobInEpoch $
              MissingFileError indexFile truncLastValidBlobInEpoch
                callStack
            return reconstructedIndex

      -- We're now out of the block that collects all the errors in the state
      case errors of
        [] -> return index
        _  -> lift $ throwBestError errors

    -- | Throw the error from the list with the minimal 'TruncateFrom' or none
    -- if the list is empty.
    --
    -- Precondition: the list must be non-empty
    throwBestError :: [(TruncateFrom, UnexpectedError)] -> m a
    throwBestError es = throwUnexpectedError err bestError
      where
        (_, bestError) = minimumBy (compare `on` fst) es

    -- | Throw a 'MissingFileError' if there is an index file on disk that
    -- corresponds to an epoch equal or greater than the given one. The index
    -- files on disk are given in the form of a 'Set' of epochs, as returned
    -- by 'dbFilesOnDisk'.
    --
    -- Important: the error will refer to the smallest epoch equal or greater
    -- than the given epoch, so that truncating from this epoch will solve the
    -- problem.
    errorOnIndexFileFrom :: HasCallStack
                         => Epoch -> StateT CumulEpochSizes m ()
    errorOnIndexFileFrom epoch = do
      filesInDBFolder <- lift $ listDirectory []
      let indexFiles = snd $ dbFilesOnDisk filesInDBFolder
      whenJust (Set.lookupGE epoch indexFiles) $ \firstIndexFrom -> do
        epochStartSlot <- CES.getNewEpochSizesUntilM
          (`CES.epochSlotToSlot` EpochSlot epoch 0) getEpochSize
        let trunc = TruncateFrom epochStartSlot

        lift $ throwUnexpectedError err $ MissingFileError
          (renderFile "epoch" firstIndexFrom) trunc callStack


-- | Reconstruct an 'Index' from the given epoch file using the
-- 'EpochFileParser'.
--
-- Also returns the error returned by the 'EpochFileParser'.
reconstructIndex :: (HasCallStack, MonadThrow m)
                 => FsPath
                 -> EpochFileParser e m (Int, Slot)
                 -> (Epoch -> m EpochSize)
                 -> StateT CumulEpochSizes m (Index, Maybe e)
reconstructIndex epochFile epochFileParser getEpochSize = do
    (offsetsAndSizesAndSlots, mbErr) <- lift $
      runEpochFileParser epochFileParser epochFile
    offsetsAndSizesAndRelSlots <-
      forM offsetsAndSizesAndSlots $ \(offset, (size, slot)) -> do
        relSlot <- CES.slotToRelativeSlotM slot getEpochSize
        return (offset, (size, relSlot))
    let slotOffsets = reconstructSlotOffsets offsetsAndSizesAndRelSlots
        index       = indexFromSlotOffsets slotOffsets
    return (index, mbErr)

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

-- | Variant of 'loadIndex' that ignores any junk returned by 'loadIndex' (the
-- second return value).
loadIndex' :: (HasCallStack, MonadThrow m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> Epoch
           -> m Index
loadIndex' hasFS _err epoch = fst <$> loadIndex hasFS epoch
    -- TODO throw an error when there is junk or the index is invalid?

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
