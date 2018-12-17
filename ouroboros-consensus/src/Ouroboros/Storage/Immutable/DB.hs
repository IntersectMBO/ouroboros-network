{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

{-- |

A very simple and hopefully efficient implementation of the immutable
on-disk storage, also suitable as reference implementation for more
sophisticated ones.

--}

module Ouroboros.Storage.Immutable.DB
  ( -- * Types
    ImmutableDB -- opaque
  , Epoch
  , EpochSize
  , RelativeSlot(..)
  , EpochSlot(..)
    -- * 'ImmutableDBError'
  , ImmutableDBError(..)
  , sameDBError
  , prettyImmutableDBError
    -- * API
  , withDB
  , openDB
  , closeDB
  , getNextEpochSlot
  , getEpochSize
  , getBinaryBlob
  , appendBinaryBlob
  , startNewEpoch
  -- * Utility functions
  , liftFsError
  ) where

import           Control.Monad (void, when, unless, zipWithM_)
import           Control.Monad.Catch (MonadCatch, MonadMask, ExitCase(..),
                                      generalBracket, bracket, finally)
import           Control.Monad.Except (MonadError, ExceptT(ExceptT),
                                       withExceptT, runExceptT, throwError,
                                       catchError)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Word (Word, Word64)

import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           GHC.Stack (CallStack, HasCallStack, callStack,
                            prettyCallStack)

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.Util as I

import           Ouroboros.Network.MonadClass


{-- | Our database is structured on disk this way:

db /
  epoch-001.dat
  index-001.dat
  ..
  epoch-008.dat
  index-008.dat
  ..

The key idea is to have, for each epoch, two files on disk:
  * An \"epoch file\" is where the data is actually stored;
  * An \"index file\" used to efficiently seek within the epoch file for the
    relevant data.

TODO: to prevent corruption the same database may only be opened once. Should
we ensure this with a lock file?

--}

-- This is just a placeholder as we don't have (yet) a proper 'Epoch' type in
-- this codebase.
type Epoch = Word

type EpochSize = Word

-- | A /relative/ slot within an Epoch.
newtype RelativeSlot = RelativeSlot { getRelativeSlot :: Word }
                       deriving (Eq, Ord, Enum, Num, Show)

-- | The combination of an 'Epoch' and a 'RelativeSlot' within the epoch.
data EpochSlot = EpochSlot
  { _epoch        :: !Epoch
  , _relativeSlot :: !RelativeSlot
  } deriving (Eq, Ord, Show)


{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

data InternalState m = InternalState {
      _currentEpoch                    :: !Epoch
    -- ^ The current 'Epoch' the immutable store is writing into.
    , _currentEpochWriteHandle         :: !(FsHandleE m)
    -- ^ The write handle for the current epoch file.
    , _currentIndexWriteHandle         :: !(FsHandleE m)
    -- ^ The write handle for the current index file.
    , _currentLastIndexOffset          :: !Word64
    -- ^ It keeps around the last offset stored within the index file,
    -- so that we don't have to seek for it when we append.
    , _currentNextExpectedRelativeSlot :: !RelativeSlot
    -- ^ The next relative slot we expect to see to append data to the epoch
    -- file.
    --
    -- Invariant: we can't append new data passing as input a slot less than
    -- the expected one.
    , _currentEpochSizes               :: !(Map Epoch EpochSize)
    -- ^ The size of all past epochs and the current epoch.
    --
    -- Invariant: for each @epoch@, if @epoch <= '_currentEpoch'@, then
    -- @'Map.elem' epoch '_currentEpochSizes'@.
    }

mkInternalState :: (HasCallStack, MonadCatch m, HasFSE m)
                => FsPath
                -> Epoch
                -> Map Epoch EpochSize
                -> ExceptT ImmutableDBError m (InternalState m)
mkInternalState dbFolder epoch epochSizes = do
    let epochFile = dbFolder <> renderFile "epoch" epoch
        indexFile = dbFolder <> renderFile "index" epoch
    eHnd <- liftFsError (hOpen epochFile AppendMode)
    iHnd <- liftFsError (do
                h <- hOpen indexFile AppendMode
                isNewFile <- ((==) 0) <$> hSeek h RelativeSeek 0
                -- Initialise the new index file properly, if needed.
                when isNewFile $ void (hPut h (I.encodeIndexEntry 0))
                return h
            )
    return $ InternalState epoch eHnd iHnd 0 (RelativeSlot 0) epochSizes


-- | An opaque handle to an immutable database of binary blobs.
data ImmutableDB m = ImmutableDB {
      _dbInternalState :: !(TMVar m (InternalState m))
    , _dbFolder        :: !FsPath
    }


{------------------------------------------------------------------------------
  API
------------------------------------------------------------------------------}

-- | Errors which might arise when working with this database.
data ImmutableDBError
  = FileSystemError FsError
  | OpenFinalisedEpochError Epoch Epoch CallStack
  -- ^ When opening the DB, the user requested access to an epoch which was
  -- already finalised, i.e. not writable anymore.
  --
  -- The first parameter is the requested epoch and the second parameter is
  -- the last epoch that can be opened.
  | AppendToSlotInThePastError RelativeSlot RelativeSlot CallStack
  -- ^ When trying to append a new binary blob at the end of an epoch file,
  -- the input slot was not monotonically increasing w.r.t. the last slot that
  -- was appended to in the epoch.
  --
  -- The first parameter is the input slot and the second parameter is the
  -- next slot available for appending.
  | ReadFutureSlotError EpochSlot EpochSlot CallStack
  -- ^ When trying to read the slot from the epoch, the slot was not yet
  -- occupied, either because it's too far in the future or because it is in
  -- the process of being written.
  --
  -- The first parameter is the requested epoch + slot and the second
  -- parameter is the last slot that can be read.
  | SlotGreaterThanEpochSizeError RelativeSlot EpochSize CallStack
  -- ^ When reading or appending to a slot in an epoch, the input slot was
  -- greater than or equal to the size of epoch, and thus beyond the last slot
  -- in the epoch.
  | MissingFileError FsPath CallStack
  -- ^ A missing epoch or index file.
  | MissingEpochSizeError Epoch CallStack
  -- ^ When opening a DB, the size for each past epoch and the opened epoch
  -- must be passed. The 'Epoch' parameter is the epoch for which the size was
  -- missing.
  deriving (Show)

-- | Check if 'ImmutableDBError's are equal while ignoring their 'CallStack's.
sameDBError :: ImmutableDBError -> ImmutableDBError -> Bool
sameDBError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (FileSystemError {}, _) -> False
    (OpenFinalisedEpochError is1 ns1 _, OpenFinalisedEpochError is2 ns2 _) ->
        is1 == is2 && ns1 == ns2
    (OpenFinalisedEpochError {}, _) -> False
    (AppendToSlotInThePastError is1 ns1 _, AppendToSlotInThePastError is2 ns2 _) ->
        is1 == is2 && ns1 == ns2
    (AppendToSlotInThePastError {}, _) -> False
    (ReadFutureSlotError exp1 act1 _, ReadFutureSlotError exp2 act2 _) ->
        exp1 == exp2 && act1 == act2
    (ReadFutureSlotError {}, _) -> False
    (SlotGreaterThanEpochSizeError s1 sz1 _, SlotGreaterThanEpochSizeError s2 sz2 _) ->
        s1 == s2 && sz1 == sz2
    (SlotGreaterThanEpochSizeError {}, _) -> False
    (MissingFileError p1 _, MissingFileError p2 _) ->
        p1 == p2
    (MissingFileError {}, _) -> False
    (MissingEpochSizeError mis1 _, MissingEpochSizeError mis2 _) ->
        mis1 == mis2
    (MissingEpochSizeError {}, _) -> False

-- | Pretty-print an 'ImmutableDBError', including its callstack.
prettyImmutableDBError :: ImmutableDBError -> String
prettyImmutableDBError = \case
    FileSystemError fs -> prettyFSError fs
    OpenFinalisedEpochError e1 e2 cs ->
           "OpenFinalisedEpochError (input epoch was "
        <> show e1 <> ", most recent epoch found: "
        <> show e2 <> "): "
        <> prettyCallStack cs
    AppendToSlotInThePastError (RelativeSlot is) (RelativeSlot es) cs ->
           "AppendToSlotInThePastError (input slot was "
        <> show is <> ", expected was "
        <> show es <> "): "
        <> prettyCallStack cs
    ReadFutureSlotError requested lastSlot cs ->
           "ReadFutureSlotError (requested was "
        <> show requested <> ", last was "
        <> show lastSlot <> "): "
        <> prettyCallStack cs
    SlotGreaterThanEpochSizeError (RelativeSlot is) sz cs ->
           "SlotGreaterThanEpochSizeError (input slot was "
        <> show is <> ", size was "
        <> show sz <> "): "
        <> prettyCallStack cs
    MissingFileError path cs ->
           "MissingFileError ("
        <> show path <> "): "
        <> prettyCallStack cs
    MissingEpochSizeError missing cs ->
           "MissingEpochSizeError (missing size for epoch "
        <> show missing <> "): "
        <> prettyCallStack cs

withDB :: (HasCallStack, MonadSTM m, MonadMask m, HasFSE m)
       => FsPath
       -> Epoch
       -> Map Epoch EpochSize
       -> (ImmutableDB m -> m (Either ImmutableDBError a))
       -> m (Either ImmutableDBError a)
withDB fsPath epoch epochSizes action = runExceptT $
    bracket (openDB fsPath epoch epochSizes) closeDB (ExceptT . action)

-- | Opens the database, creating it from scratch if the 'FilePath' points to
-- a non-existing directory. Internally it preloads the most recent 'Epoch'.
--
-- A map with the size for each epoch (the past epochs and the most recent
-- one) must be passed. When a size is missing for an epoch, a
-- 'MissingEpochSizeError' error will be thrown.
--
-- __Note__: Use 'withDB' instead of this function.
openDB :: (HasCallStack, MonadCatch m, MonadSTM m, HasFSE m)
       => FsPath
       -> Epoch
       -> Map Epoch EpochSize
       -> ExceptT ImmutableDBError m (ImmutableDB m)
openDB fp currentEpoch epochSizes = do
    checkEpochSizes
    allFiles <- liftFsError $ do
        createDirectoryIfMissing True fp
        listDirectory fp
    checkEpochConsistency allFiles
    st    <- mkInternalState fp currentEpoch epochSizes
    stVar <- atomically $ newTMVar st
    return $ ImmutableDB stVar fp
  where
    -- | Check that each @epoch, epoch <= 'currentEpoch'@ is present in
    -- 'epochSizes'. Report the first missing one starting from epoch 0.
    checkEpochSizes :: (HasCallStack, MonadError ImmutableDBError m)
                    => m ()
    checkEpochSizes = zipWithM_
      (\expected mbActual ->
        case mbActual of
          Just actual | actual == expected -> return ()
          _ -> throwError (MissingEpochSizeError expected callStack))
      [0..currentEpoch]
      -- Pad with 'Nothing's to stop early termination of 'zipWithM_'
      (map (Just . fst) (Map.toAscList epochSizes) ++ repeat Nothing)


    checkEpochConsistency :: (HasCallStack, MonadError ImmutableDBError m)
                          => [String]
                          -> m ()
    checkEpochConsistency = mapM_ $ \name -> case parseEpochNumber name of
             Just n | n > currentEpoch
               -> throwError (OpenFinalisedEpochError currentEpoch n callStack)
             _ -> return ()

    parseEpochNumber :: String -> Maybe Epoch
    parseEpochNumber = readMaybe
                     . T.unpack
                     . snd
                     . T.breakOnEnd "-"
                     . fst
                     . T.breakOn "."
                     . T.pack


-- | Closes the database, also closing any open handle.
--
-- __Note__: Use 'withDB' instead of this function.
closeDB :: (HasCallStack, MonadSTM m, MonadMask m, HasFSE m)
        => ImmutableDB m -> ExceptT ImmutableDBError m ()
closeDB ImmutableDB{..} = do
    InternalState{..} <- atomically (takeTMVar _dbInternalState)
    liftFsError $ do
      hClose _currentEpochWriteHandle `finally` hClose _currentIndexWriteHandle

-- | Return the next free 'RelativeSlot' in the current epoch as an
-- 'EpochSlot'.
getNextEpochSlot :: (HasFSE m, MonadSTM m)
                 => ImmutableDB m
                 -> m (Either ImmutableDBError EpochSlot)
getNextEpochSlot ImmutableDB {..} = runExceptT $ do
    InternalState {..} <- atomically (readTMVar _dbInternalState)
    return $ EpochSlot _currentEpoch _currentNextExpectedRelativeSlot

-- | Look up the size of the given 'Epoch'.
getEpochSize :: (MonadSTM m)
             => ImmutableDB m
             -> Epoch
             -> m (Maybe EpochSize)
getEpochSize ImmutableDB {..} epoch = do
  InternalState {..} <- atomically (readTMVar _dbInternalState)
  return $ Map.lookup epoch _currentEpochSizes


-- | Get the binary blob stored at the given 'EpochSlot'.
--
-- Returns 'Nothing' if no blob was stored at the given (valid) slot.
--
-- Throws a 'ReadFutureSlotError' if the requested slot is in the future, i.e
-- >= 'getNextEpochSlot'.
--
-- Throws a 'SlotGreaterThanEpochSizeError' if the requested (relative) slot
-- was exceeded the size of the correspondin epoch.
--
-- Throws a 'MissingFileError' if the epoch or index file corresponding with
-- the requested epoch was missing from disk.
getBinaryBlob :: forall m. (HasCallStack, HasFSE m, MonadSTM m)
              => ImmutableDB m
              -> EpochSlot
              -> m (Either ImmutableDBError (Maybe ByteString))
getBinaryBlob ImmutableDB {..} readEpochSlot@(EpochSlot epoch relativeSlot) = runExceptT $ do
    InternalState {..} <- atomically (readTMVar _dbInternalState)

    -- Step 0: Check if the requested slot is not in the future.
    let nextExpectedEpochSlot =
          EpochSlot _currentEpoch _currentNextExpectedRelativeSlot
    when (readEpochSlot >= nextExpectedEpochSlot) $
      throwError $ ReadFutureSlotError readEpochSlot nextExpectedEpochSlot callStack

    -- Step 1: Check if the requested slot within the epoch, i.e. it may not
    -- be greater than or equal to the epoch size.
    epochSize <- lookupEpochSize epoch _currentEpochSizes
    when (getRelativeSlot relativeSlot >= epochSize) $
      throwError $ SlotGreaterThanEpochSizeError relativeSlot epochSize callStack

    -- Step 2: Check if the epoch and index file exists. This is important
    -- because we might read from a past epoch other than the currently opened
    -- epoch and we don't know yet whether its file(s) actually exist(s).
    let epochFile = _dbFolder <> renderFile "epoch" epoch
        indexFile = _dbFolder <> renderFile "index" epoch
    (indexFileExists, epochFileExists) <- liftFsError $
      (,) <$> doesFileExist indexFile <*> doesFileExist epochFile
    unless indexFileExists $ throwError $ MissingFileError indexFile callStack
    unless epochFileExists $ throwError $ MissingFileError epochFile callStack

    liftFsError $
      withFile epochFile ReadMode $ \eHnd ->
        withFile indexFile ReadMode $ \iHnd -> (do
          -- Step 3: grab the offset in bytes of the requested slot.
          let indexSeekPosition = fromEnum relativeSlot * indexEntrySizeBytes
          _ <- hSeek iHnd AbsoluteSeek (toEnum indexSeekPosition)

          -- Step 4: computes the offset on disk and the blob size.
          (blobOffset, !blobSize) <- do
            bytes <- hGet iHnd (indexEntrySizeBytes * 2)
            -- TODO best place to do this validation?
            when (indexEntrySizeBytes + 7 > BS.length bytes - 1) $
              throwError (FsError FsReachedEOF indexFile callStack)
            let !start = decodeIndexEntry   bytes
            let !end   = decodeIndexEntryAt indexEntrySizeBytes bytes
            return (start, end - start)

          if blobSize == 0
            then return Nothing
            else do
              -- Step 5: Seek in the epoch file
              _ <- hSeek eHnd AbsoluteSeek blobOffset
              Just <$> hGet eHnd (fromEnum blobSize)) `catchError`
          \case
            FsError FsReachedEOF _ _ -> return Nothing
            e                        -> throwError e

-- | Appends a binary blob at the given relative slot in the current epoch
-- file.
--
-- Throws an 'AppendToSlotInThePastError' if the given relative slot is before
-- the one returned by 'getNextEpochSlot'
--
-- Throws a 'SlotGreaterThanEpochSizeError' if the relative slot exceeds the
-- size of the current epoch.
--
-- TODO the binary blob may not be empty
appendBinaryBlob :: (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
                 => ImmutableDB m
                 -> RelativeSlot
                 -> BS.Builder
                 -> m (Either ImmutableDBError ())
appendBinaryBlob db relativeSlot builder = runExceptT $ do
    modifyTMVar (_dbInternalState db) $ \st@InternalState {..} -> do
      let eHnd = _currentEpochWriteHandle
          iHnd = _currentIndexWriteHandle

      -- Step 0: Check that the slot is >= the expected next slot and thus
      -- not in the past.
      when (relativeSlot < _currentNextExpectedRelativeSlot) $
        throwError $ AppendToSlotInThePastError relativeSlot
                     _currentNextExpectedRelativeSlot callStack

      -- Step 1: Check if the requested slot within the epoch, i.e. it may not
      -- be greater than or equal to the epoch size.
      epochSize <- lookupEpochSize _currentEpoch _currentEpochSizes
      when (getRelativeSlot relativeSlot >= epochSize) $
        throwError $ SlotGreaterThanEpochSizeError relativeSlot epochSize callStack

      -- Step 2: If necessary, backfill the index file for any slot we missed.
      let (gap, backfill) = indexBackfill relativeSlot
                                          _currentNextExpectedRelativeSlot
                                          _currentLastIndexOffset
      liftFsError $ do
          when (gap /= 0) $ void $ hPut iHnd backfill

          -- Step 3: Append to the end of the epoch file
          bytesWritten <- hPut eHnd builder

          -- Step 4: Update the index file
          let newOffset = _currentLastIndexOffset + bytesWritten
          void $ hPut iHnd (I.encodeIndexEntry newOffset)

          return $ (st { _currentLastIndexOffset          = newOffset
                       , _currentNextExpectedRelativeSlot = relativeSlot + 1
                       }, ())

-- | Close the current epoch and start a new epoch (the epoch number is
-- incremented by 1).
--
-- The size of the new epoch must be started.
startNewEpoch :: (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
              => ImmutableDB m
              -> EpochSize
              -> m (Either ImmutableDBError ())
startNewEpoch db newEpochSize = runExceptT $ do
    modifyTMVar (_dbInternalState db) $ \InternalState {..} -> do
      let newEpoch      = succ _currentEpoch
          newEpochSizes = Map.insert newEpoch newEpochSize _currentEpochSizes
      newState <- mkInternalState (_dbFolder db) newEpoch newEpochSizes
      liftFsError $ hClose _currentIndexWriteHandle `finally`
                    hClose _currentEpochWriteHandle
      return $ (newState, ())

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

renderFile :: String -> Epoch -> FsPath
renderFile fileType epoch = [printf "%s-%03d.dat" fileType epoch]


liftFsError :: Functor m => ExceptT FsError m a -> ExceptT ImmutableDBError m a
liftFsError = withExceptT FileSystemError


-- | The analogue of `modifyMVar`
--
-- NOTE: This /takes/ the 'TMVar', _then_ runs the action (which might be in
-- `IO`), and then puts the 'TMVar' back, just like 'modifyMVar' does.
-- Consequently, it has the same gotchas that 'modifyMVar' does; the effects
-- are observable and it is susceptible to deadlock.
--
-- TODO: By rights we should really just use 'MVar' rather than TMVar',
-- but we currently don't have a simulator for code using 'MVar'.
-- NOTE: Use of `generalBracket` (rather than the `mask ... restore` pattern
-- of old) is essential to make sure that this still behaves correctly in monad
-- stacks containing `ExceptT`.
modifyTMVar :: (MonadSTM m, MonadMask m)
            => TMVar m a
            -> (a -> m (a,b))
            -> m b
modifyTMVar var action =
  snd . fst <$> generalBracket (atomically $ takeTMVar var)
                   (\oldState ec -> atomically $ case ec of
                        ExitCaseSuccess (newState,_) -> putTMVar var newState
                        ExitCaseException _ex        -> putTMVar var oldState
                        ExitCaseAbort                -> putTMVar var oldState
                   ) action


-- | Look up the size of the given 'Epoch'. Variant of 'getEpochSize' for
-- internal use.
--
-- This should should not fail if the epoch <= the currently opened epoch and
-- the given mapping is retrieved from the DB, as 'openDB' and 'startNewEpoch'
-- make sure this mapping is complete.
--
-- Throws an 'MissingEpochSizeError' is the epoch is not in the map.
lookupEpochSize :: (HasCallStack, MonadError ImmutableDBError m)
                => Epoch -> Map Epoch EpochSize
                -> m EpochSize
lookupEpochSize epoch epochSizes
  | Just epochSize <- Map.lookup epoch epochSizes
  = return epochSize
  | otherwise
  = throwError $ MissingEpochSizeError epoch callStack


-- | The size of each entry in the index file, namely 8 bytes for the offset
-- (represented as a Word64).
indexEntrySizeBytes :: Int
indexEntrySizeBytes = 8
{-# INLINE indexEntrySizeBytes #-}


-- | Backfills the index file.
--
-- Such situation may arise in case we \"skip\" some slots, and we write into
-- the DB, say, for example, every other slot. In this case, we do need to
-- backfill the index file to take into account all the missed slots.
--
-- The question is: backfill with what? The easiest choice is to backfill with
-- the last seen offset, so that when getting data out of the DB, the
-- difference between these two offsets is 0, signalling there is no data to
-- be fetched for that slot.
--
-- There are edge cases, however. First of all, we need to store inside the
-- 'InternalState' the \"last absolute slot seen\", because we need to check
-- it against the input one to determine whether or not we missed a slot.
--
-- However, the \"last absolute slot seen\" must be a 'Maybe', because any
-- other default value (like 0 for example) would be incorrect. But this poses
-- another problem: suppose we never wrote into this epoch file yet, and the
-- user decides to write in relative slot 3. Now we have a problem: we do not
-- have any previously stored slot, yet we do need to backfill, because we
-- missed 0 1 and 2. This complicates a bit the logic as we have to handle
-- this case separately. Returns the size of the \"gap\" alongside the
-- (potentially empty) 'BS.Builder' to write.
indexBackfill :: RelativeSlot
              -> RelativeSlot
              -> Word64
              -> (Int, BS.Builder)
indexBackfill (RelativeSlot relativeSlot) (RelativeSlot nextExpected) lastOffset
    -- If we missed slot 0 and we are starting some slot ahead, we do need
    -- to backfill
    | nextExpected == 0 && relativeSlot /= 0  =
        let gap = fromEnum relativeSlot
        in (gap, mconcat (replicate gap (I.encodeIndexEntry lastOffset)))
    | otherwise =
        let gap     = fromEnum $ relativeSlot - nextExpected
            builder = mconcat  $ replicate gap (I.encodeIndexEntry lastOffset)
        in (gap, builder)
