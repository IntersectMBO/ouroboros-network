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
  , streamBinaryBlob
  , appendBinaryBlob
  -- * Low-level Iterator
  , Iterator -- opaque
  , iteratorNext
  , iteratorClose
  -- * Utility functions
  , liftFsError
  -- * Pretty-printing things
  , prettyImmutableDBError
  ) where

import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as U
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
                       deriving (Eq, Ord, Num, Show)

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
  | UnexpectedEndOfIterator CallStack
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
    (UnexpectedEndOfIterator _, UnexpectedEndOfIterator _) -> True
    (UnexpectedEndOfIterator _, _) -> False

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
    UnexpectedEndOfIterator cs   ->
           "UnexpectedEndOfIterator: "
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


-- | Loads an index file in memory.
loadIndex :: forall m. HasFSE m
          => ImmutableDB m
          -> Epoch
          -> ExceptT FsError m (U.Vector (Word64, Word64))
loadIndex ImmutableDB{..} epoch = do
    let indexFile = _dbFolder <> renderFile "index" epoch
    fullIndex <- withFile indexFile ReadMode $ \hnd ->
                     BL.toStrict . BS.toLazyByteString <$> readAll hnd mempty
    let fullLen    = fromIntegral (BS.length fullIndex)
        -- We need to divide by 2 at the end as we store tuples.
        -- Example: We have stored only 1 slot in our index, so we have stored
        -- an offset and an size for a total of 2 * sizeof(Word64) = 16 bytes.
        -- To get the right size back we need to divide 16 by sizeof(Word64)
        -- and finally by 2.
        vectorSize = ceiling $
            (fullLen / (fromIntegral indexEntrySizeBytes) :: Double) / 2.0
        mkEntry ix =
          let start = decodeIndexEntryAt (ix * indexEntrySizeBytes) fullIndex
              end   = decodeIndexEntryAt ((ix + 1) * indexEntrySizeBytes) fullIndex
              in (start, end - start)
    return $ U.generate vectorSize mkEntry
  where
    -- Reads the data from the index file 64k bytes at the time.
    readAll :: FsHandleE m
            -> BS.Builder
            -> ExceptT FsError m BS.Builder
    readAll hnd acc = do
        bytesRead <- hGet hnd 64000
        let acc' = acc <> BS.byteString bytesRead
        case BS.length bytesRead < 64000 of
             True  -> return acc'
             False -> readAll hnd acc'

-- | An Iterator is a read handle which can be used to efficiently stream
-- binary blobs across multiple epochs.
data Iterator m = Iterator
                { _it_current      :: TVar m (Epoch, RelativeSlot)
                , _it_end          :: (Epoch, RelativeSlot)
                , _it_epoch_handle :: TVar m (FsHandleE m)
                , _it_epoch_index  :: TVar m (U.Vector (Word64, Word64))
                -- ^ We load in memory the index file for the epoch we are
                -- currently iterating on, as it's going to be small anyway
                -- (usually ~150kb).
                }

type IteratorResult = Maybe (Epoch, RelativeSlot, ByteString)

-- | Steps an 'Iterator, yielding either an 'ImmutableDBError' or some data
-- alongside with the 'RelativeSlot' and 'Epoch' this data refers to.
-- Returns Nothing when there is no more data to be fetched.
-- NOTE(adn): This works under the assumption that the user is the exclusive
-- owner of the iterator.
iteratorNext :: forall m. (MonadSTM m, MonadMask m, HasFSE m)
             => ImmutableDB m
             -> Iterator m
             -> m (Either ImmutableDBError IteratorResult)
iteratorNext db it@Iterator{..} = do
  current <- atomically $ readTVar _it_current
  case outOfBounds current _it_end of
      True  -> runExceptT $ do
          ExceptT $ iteratorClose it
          return Nothing
      False -> do
          -- Read more data and step the iterator
          (eHnd, fullIndex) <- atomically $ do
              e <- readTVar _it_epoch_handle
              i <- readTVar _it_epoch_index
              return (e,i)
          r <- readNext eHnd fullIndex (snd current)
          runExceptT $ case r of
              Left err -> liftFsError $ do
                  hClose eHnd
                  throwError err
              Right bs -> do
                  -- Step the iterator
                  iteratorStep
                  return  $ Just (fst current, snd current, bs)
  where
    outOfBounds :: (Epoch, RelativeSlot) -> (Epoch, RelativeSlot) -> Bool
    outOfBounds (currentEpoch, currentSlot) (endEpoch, endSlot)
      | currentEpoch > endEpoch                           = True
      | currentEpoch == endEpoch && currentSlot > endSlot = True
      | otherwise                                         = False

    readNext :: (Monad m, HasFSE m)
             => FsHandleE m
             -> U.Vector (Word64, Word64)
             -> RelativeSlot
             -> m (Either FsError ByteString)
    readNext eHnd fullIndex (RelativeSlot slot) = runExceptT $ do
          -- Step 1: grab from the cached index the blobSize.
          let (_, !blobSize) = fullIndex U.! (fromIntegral slot)

          -- Step 2: Read from the epoch file. No need for seeking: as we
          -- are streaming, we are already positioned at the correct place.
          hGet eHnd (fromEnum blobSize)

    -- Steps the internal state of an 'Iterator'. Internal use only.
    iteratorStep :: (MonadSTM m, MonadMask m, HasFSE m)
                 => ExceptT ImmutableDBError m ()
    iteratorStep = liftFsError $ do
        ((epoch, RelativeSlot slot), fullIndex) <- atomically $ do
            (,) <$> readTVar _it_current <*> readTVar _it_epoch_index

        let epoch' = epoch + 1
        -- If the current relative slot is equal to the size of the index, it
        -- means we reached the end of the epoch and we need to open the next
        -- epoch file.
        case fromIntegral (slot + 1) == U.length fullIndex &&
             not (outOfBounds (epoch', 0) _it_end) of
             True  -> do
                 eHnd <- atomically $ readTVar _it_epoch_handle
                 eHnd' <- hOpen (_dbFolder db <> renderFile "epoch" epoch') ReadMode
                          `finally` hClose eHnd
                 index' <- loadIndex db epoch'
                 atomically $ do
                     writeTVar _it_epoch_handle eHnd'
                     writeTVar _it_epoch_index index'
                     writeTVar _it_current (epoch', RelativeSlot 0)
             False -> atomically $ writeTVar _it_current (epoch, RelativeSlot $ slot + 1)

-- | Dispose of the 'Iterator'.
iteratorClose :: (MonadSTM m, MonadMask m, HasFSE m)
              => Iterator m
              -> m (Either ImmutableDBError ())
iteratorClose Iterator{..} = runExceptT $ liftFsError $ do
    atomically (readTVar _it_epoch_handle) >>= hClose

-- | When given a start and a stop position, this function returns an
-- 'Iterator' suitable to be used to efficiently stream binary blocks out of
-- the 'ImmutableDB'.
streamBinaryBlob :: forall m. (HasFSE m, MonadSTM m)
                 => ImmutableDB m
                 -> (Epoch, RelativeSlot)
                 -- ^ When to start streaming.
                 -> (Epoch, RelativeSlot)
                 -- ^ When to stop streaming.
                 -> m (Either ImmutableDBError (Iterator m))
streamBinaryBlob db@ImmutableDB{..} start@(epoch,RelativeSlot slot) end = do
    runExceptT $ liftFsError $ do
        eHnd      <- hOpen (_dbFolder <> renderFile "epoch" epoch) ReadMode
        fullIndex <- loadIndex db epoch

        -- Position the epoch handle at the right place
        _ <- hSeek eHnd AbsoluteSeek (fst $ fullIndex U.! fromIntegral slot)

        (current, eVar, iVar) <- atomically $ do
            (,,) <$> newTVar start <*> newTVar eHnd <*> newTVar fullIndex

        return $ Iterator {
            _it_current      = current
          , _it_end          = end
          , _it_epoch_handle = eVar
          , _it_epoch_index  = iVar
          }

getBinaryBlob :: forall m. (HasFSE m, MonadSTM m, MonadMask m)
              => ImmutableDB m
              -> (Epoch, RelativeSlot)
              -> m (Either ImmutableDBError ByteString)
getBinaryBlob db@ImmutableDB{..} start@(epoch, RelativeSlot relativeSlot) = do
    InternalState{..} <- atomically (readTMVar _dbInternalState)
    runExceptT $ do
        withSlotGuard _currentEpoch _currentNextExpectedRelativeSlot $ do
            it  <- ExceptT $ streamBinaryBlob db start start
            res <- ExceptT $ iteratorNext db it
            case res of
                Nothing -> throwError (UnexpectedEndOfIterator callStack)
                Just (e,s,bs) ->
                    assert (e == fst start && s == snd start) $ return bs
    where
      -- Checks that the slot we are trying to access is valid.
      withSlotGuard :: Epoch
                    -> RelativeSlot
                    -> ExceptT ImmutableDBError m a
                    -> ExceptT ImmutableDBError m a
      withSlotGuard currentEpoch (RelativeSlot nextExpectedSlot) action
        | epoch > currentEpoch || relativeSlot >= nextExpectedSlot =
            throwError
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
