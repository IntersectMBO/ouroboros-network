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

module Ouroboros.Storage.Immutable.DB (
    ImmutableDB -- opaque
  , RelativeSlot(..)
  , ImmutableDBError(..)
  , sameDBError
  , withDB
  , getBinaryBlob
  , appendBinaryBlob
  -- * Utility functions
  , liftFsError
  -- * Pretty-printing things
  , prettyImmutableDBError
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T
import           Data.Word
import           Text.Printf
import           Text.Read (readMaybe)

import           GHC.Stack

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
--}

-- This is just a placeholder as we don't have (yet) a proper 'Epoch' type in
-- this codebase.
type Epoch = Word

-- | A /relative/ 'Slot' within an Epoch.
newtype RelativeSlot = RelativeSlot { getRelativeSlot :: Word }
                       deriving (Eq, Num, Show)

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

data InternalState m = InternalState {
      _currentEpoch                    :: !Epoch
    -- ^ The current 'Epoch' the immutable store is writing into.
    , _currentEpochWriteHandle         :: !(FsHandleE m)
    -- ^ The 'WriteHandle' for the current epoch file.
    , _currentIndexWriteHandle         :: !(FsHandleE m)
    -- ^ The 'WriteHandle' for the current index file.
    , _currentLastIndexOffset          :: !Word64
    -- ^ It keeps around the last offset stored within the index file,
    -- so that we don't have to seek for it when we append.
    , _currentNextExpectedRelativeSlot :: !RelativeSlot
    -- ^ The next relative slot we expect to see to append data to the epoch file.
    -- Invariant: we can't append new data passing as input a slot less than
    -- the expected one.
    }


liftFsError :: Functor m => ExceptT FsError m a -> ExceptT ImmutableDBError m a
liftFsError = withExceptT FileSystemError

mkInternalState :: forall m. (HasCallStack, MonadCatch m, HasFSE m)
                => FsPath
                -> Epoch
                -> ExceptT ImmutableDBError m (InternalState m)
mkInternalState dbFolder epoch = do
    let epochFile = dbFolder <> renderFile "epoch" epoch
    let indexFile = dbFolder <> renderFile "index" epoch
    eHnd <- liftFsError (hOpen epochFile AppendMode)
    iHnd <- liftFsError (do
                h <- hOpen indexFile AppendMode
                isNewFile <- ((==) 0) <$> hSeek h RelativeSeek 0
                -- Initialise the new index file properly, if needed.
                when isNewFile $ void (hPut h (I.encodeIndexEntry 0))
                return h
            )
    return $ InternalState epoch eHnd iHnd 0 (RelativeSlot 0)


-- | An opaque handle to an immutable database of binary blobs.
data ImmutableDB m = ImmutableDB {
      _dbInternalState :: !(TMVar m (InternalState m))
    , _dbFolder        :: !FsPath
    }


{------------------------------------------------------------------------------
  API
------------------------------------------------------------------------------}

-- | Errors which might arise when working with this database.
data ImmutableDBError =
    FileSystemError FsError
  | EpochIsReadOnlyError Epoch Epoch CallStack
  -- ^ When opening the DB, the user requested access to an epoch which was
  -- already finalised, i.e. not writable anymore.
  | InconsistentSlotError RelativeSlot RelativeSlot CallStack
  -- ^ When trying to append a new binary blob at the end of an epoch file,
  -- the input slot was not monotonically increasing with reference to the last
  -- performed on that epoch file.
  | SlotDoesNotExistError (Epoch, RelativeSlot) (Epoch, RelativeSlot) CallStack
  -- ^ When trying to read from the given 'Epoch' the input 'RelativeSlot'
  -- was not there, either because it's too far in the future or because is
  -- in the process of being written.
  deriving Show

-- | Check two 'ImmutableDBError for shallow equality, i.e. if they have the same
-- type constructor, ignoring the 'CallStack'.
sameDBError :: ImmutableDBError -> ImmutableDBError -> Bool
sameDBError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2
    (FileSystemError _, _) -> False
    (EpochIsReadOnlyError ep1 ep2 _, EpochIsReadOnlyError ep3 ep4 _) ->
        ep1 == ep3 && ep2 == ep4
    (EpochIsReadOnlyError _ _ _, _) -> False
    (InconsistentSlotError s1 s2 _, InconsistentSlotError s3 s4 _) ->
        s1 == s3 && s2 == s4
    (InconsistentSlotError _ _ _, _) -> False
    (SlotDoesNotExistError expected actual _, SlotDoesNotExistError exp2 act2 _) ->
        expected == exp2 && actual == act2
    (SlotDoesNotExistError _ _ _, _) -> False

prettyImmutableDBError :: ImmutableDBError -> String
prettyImmutableDBError = \case
    FileSystemError fs   -> prettyFSError fs
    EpochIsReadOnlyError e1 e2 cs ->
           "EpochIsReadOnlyError (input epoch was "
        <> show e1
        <> ", most recent epoch found: " <> show e2 <>"): "
        <> prettyCallStack cs
    InconsistentSlotError (RelativeSlot is) (RelativeSlot es) cs   ->
           "InconsistentSlotError (input epoch was "
        <> show is
        <> ", expected was "
        <> show es
        <> "): "
        <> prettyCallStack cs
    SlotDoesNotExistError current requested cs   ->
           "SlotDoesNotExistError (current is "
        <> show current
        <> ", requested was "
        <> show requested
        <> "): "
        <> prettyCallStack cs

renderFile :: String -> Epoch -> FsPath
renderFile fileType epoch = [printf "%s-%03d.dat" fileType epoch]

withDB :: (HasCallStack, MonadSTM m, MonadMask m, HasFSE m)
       => FsPath
       -> Epoch
       -> (ImmutableDB m -> m (Either ImmutableDBError a))
       -> m (Either ImmutableDBError a)
withDB fsPath epoch action = runExceptT $
    bracket (openDB fsPath epoch) closeDB (ExceptT . action)

-- | Opens the database, creating it from scratch if the 'FilePath' points to
-- a non-existing directory. Internally it preloads the most recent 'Epoch'.
openDB :: (HasCallStack, MonadCatch m, MonadSTM m, HasFSE m)
       => FsPath
       -> Epoch
       -> ExceptT ImmutableDBError m (ImmutableDB m)
openDB fp currentEpoch = do
      allFiles <- liftFsError $ do
          createDirectoryIfMissing True fp
          listDirectory fp
      checkEpochConsistency allFiles
      st    <- mkInternalState fp currentEpoch
      stVar <- atomically $ newTMVar st
      return $ ImmutableDB stVar fp
  where
      checkEpochConsistency :: (HasCallStack, MonadError ImmutableDBError m)
                            => [String]
                            -> m ()
      checkEpochConsistency [] = return ()
      checkEpochConsistency (x:xs) =
          case parseEpochNumber x of
               Nothing -> checkEpochConsistency xs
               Just n  -> if n > currentEpoch
                             then throwError (EpochIsReadOnlyError currentEpoch n callStack)
                             else checkEpochConsistency xs

      parseEpochNumber :: String -> Maybe Epoch
      parseEpochNumber = readMaybe
                       . T.unpack
                       . snd
                       . T.breakOnEnd "-"
                       . fst
                       . T.breakOn "."
                       . T.pack


-- | Closes the database, also closing any handle which have been opened in
-- the process.
closeDB :: (MonadSTM m, MonadMask m, HasFSE m)
        => ImmutableDB m -> ExceptT ImmutableDBError m ()
closeDB ImmutableDB{..} = do
    InternalState{..} <- atomically (takeTMVar _dbInternalState)
    liftFsError $ do
      hClose _currentEpochWriteHandle `finally` hClose _currentIndexWriteHandle

{------------------------------------------------------------------------------
  Low-level API
------------------------------------------------------------------------------}


-- | The analogue of `modifyMVar`
--
-- NOTE: This /takes/ the 'TMVar', _then_ runs the action (which might be in `IO`),
-- and then puts the 'TMVar' back, just like 'modifyMVar' does. Consequently,
-- it has the same gotchas that 'modifyMVar' does; the effects are observable
-- and it is susceptible to deadlock.
--
-- TODO: By rights we should really just use 'MVar' rather than TMVar',
-- but we currently don't have a simular for code using 'MVar'.
-- NOTE: Use of `generalBracket` (rather than the `mask ... restore` pattern
-- of old) is essential to make sure that this still behaves correctly in monad
-- stacks containing `ExceptT`.
modifyTMVar :: (MonadSTM m, MonadMask m)
            => TMVar m a
            -> (a -> m (a,b))
            -> m b
modifyTMVar m action =
  snd . fst <$> generalBracket (atomically $ takeTMVar m)
                   (\oldState ec -> atomically $ case ec of
                        ExitCaseSuccess (newState,_) -> putTMVar m newState
                        ExitCaseException _ex        -> putTMVar m oldState
                        ExitCaseAbort                -> putTMVar m oldState
                   ) action

modifyInternalState :: (MonadMask m, HasFSE m, MonadSTM m)
                    => ImmutableDB m
                    -> Epoch
                    -> (InternalState m -> ExceptT ImmutableDBError m (InternalState m, a))
                    -> ExceptT ImmutableDBError m a
modifyInternalState db newEpoch action = do
    modifyTMVar (_dbInternalState db) $ \oldState -> do
        case _currentEpoch oldState < newEpoch of
            True -> do
                ns <- mkInternalState (_dbFolder db) newEpoch
                liftFsError $ do
                  hClose (_currentIndexWriteHandle oldState)
                  `finally`
                  hClose (_currentEpochWriteHandle oldState)
                action ns
            False -> action oldState

getBinaryBlob :: forall m. (HasFSE m, MonadSTM m)
              => ImmutableDB m
              -> (Epoch, RelativeSlot)
              -> m (Either ImmutableDBError ByteString)
getBinaryBlob ImmutableDB{..} (epoch, RelativeSlot relativeSlot) = do
    InternalState{..} <- atomically (readTMVar _dbInternalState)

    -- Check whether or not the requested slot can be accessed.
    withSlotGuard _currentEpoch _currentNextExpectedRelativeSlot $ do
        runExceptT $ liftFsError $ do
            withFile (_dbFolder <> renderFile "epoch" epoch) ReadMode $ \eHnd ->
                withFile (_dbFolder <> renderFile "index" epoch) ReadMode $ \iHnd -> do
                    -- Step 1: grab the offset in bytes of the requested slot.
                    let indexSeekPosition = fromEnum relativeSlot * indexEntrySizeBytes
                    _ <- hSeek iHnd AbsoluteSeek (toEnum indexSeekPosition)

                    -- Step 1: computes the offset on disk and the blob size.
                    (blobOffset, !blobSize) <- do
                        bytes <- hGet iHnd (indexEntrySizeBytes * 2)
                        let !start = decodeIndexEntry   bytes
                        let !end   = decodeIndexEntryAt indexEntrySizeBytes bytes
                        return (start, end - start)

                    -- Step 2: Seek in the epoch file
                    _ <- hSeek eHnd AbsoluteSeek blobOffset
                    hGet eHnd (fromEnum blobSize)
  where
    -- Checks that the slot we are trying to access is valid.
    withSlotGuard :: Epoch
                  -> RelativeSlot
                  -> m (Either ImmutableDBError a)
                  -> m (Either ImmutableDBError a)
    withSlotGuard currentEpoch (RelativeSlot nextExpectedSlot) action
      | epoch > currentEpoch || relativeSlot >= nextExpectedSlot =
          return $ Left
                 $ SlotDoesNotExistError (currentEpoch, RelativeSlot nextExpectedSlot)
                                         (epoch, RelativeSlot relativeSlot)
                                         callStack
      | otherwise = action


-- | The size of each entry in the index file, namely 8 bytes for the offset
-- (represented as a Word64).
indexEntrySizeBytes :: Int
indexEntrySizeBytes = 8
{-# INLINE indexEntrySizeBytes #-}


-- | Appends a binary blob at the end of the epoch file relative to the input
-- 'Slot', updating its index file accordingly.
appendBinaryBlob :: (HasCallStack, HasFSE m, MonadSTM m, MonadMask m)
                 => ImmutableDB m
                 -> (Epoch, RelativeSlot)
                 -> BS.Builder
                 -> m (Either ImmutableDBError ())
appendBinaryBlob db (epoch, relativeSlot) builder = runExceptT $ do
    modifyInternalState db epoch $ \st@InternalState{..} -> do
        let eHnd = _currentEpochWriteHandle
        let iHnd = _currentIndexWriteHandle

        -- Step 0: Check the consistency of the slot being written and, if
        -- necessary, backfill the index file for any slot we missed.
        checkSlotConsistency relativeSlot _currentNextExpectedRelativeSlot
        let (gap, backfill) = indexBackfill relativeSlot
                                            _currentNextExpectedRelativeSlot
                                            _currentLastIndexOffset
        liftFsError $ do
            if gap /= 0
               then void $ hPut iHnd backfill
               else return ()

            -- Step 1: Append to the end of the epoch file
            bytesWritten <- hPut eHnd builder

            -- Step 2: Update the index file
            let newOffset = _currentLastIndexOffset + bytesWritten

            void $ hPut iHnd (I.encodeIndexEntry newOffset)

            return $ (st { _currentLastIndexOffset          = newOffset
                        , _currentNextExpectedRelativeSlot = relativeSlot + 1
                        }, ())

-- Enforce that the input slot we receive during a new append operation is
-- greater or equal to the next expected one.
checkSlotConsistency :: (MonadError ImmutableDBError m, HasCallStack)
                     => RelativeSlot
                     -> RelativeSlot
                     -> m ()
checkSlotConsistency cs@(RelativeSlot currentSlot) es@(RelativeSlot expectedNext) =
    when (currentSlot < expectedNext) $
       throwError $ InconsistentSlotError cs es callStack

-- | Backfills the index file. Such situation may arise in case we \"skip\"
-- some slots, and we write into the DB, say, for example, every other slot.
-- In this case, we do need to backfill the index file to take into account
-- all the missed slot. The question is: backfill with what? The easiest choice
-- is to backfill with the last seen offset, so that when getting data out of
-- the DB, the difference between these two offset is 0, signalling there is
-- no data to be fetched for that slot.
-- There are edge cases, however. First of all, we need to store inside the
-- 'InternalState' the \"last absolute slot seen\", because we need to check it
-- against the input one to determine whether or not we missed a slot.
-- However, the \"last absolute slot seen\" must be
-- a 'Maybe', because any other default value (like 0 for example) would be
-- incorrect. But this poses another problem:
-- Suppose we never wrote into this epoch file yet, and the user decides to
-- write in relative slot 3. Now we have a problem: we do not have any previously
-- stored slot, yet we do need to backfill, because we missed 0 1 and 2. This
-- complicates a bit the logic as we have to handle this case separately.
-- Returns the size of the \"gap\" alongise the (potentially empty) 'Builder'
-- to write.
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
