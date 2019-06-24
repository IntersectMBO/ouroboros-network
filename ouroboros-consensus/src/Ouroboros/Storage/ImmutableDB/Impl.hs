{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Immutable on-disk database of binary blobs
--
-- = Internal format
--
-- The API of the ImmutableDB uses 'SlotNo' to indicate a location in the
-- chain\/immutable database. The contents of the database are not stored in
-- one big file that is appended to in eternity, but a separate file is
-- created for each 'EpochNo'.
--
-- Within each 'EpochNo', the entries are numbered by 'RelativeSlot's. Each
-- 'SlotNo' can be converted to a combination of an 'EpochNo' and a 'RelativeSlot'
-- (= 'EpochSlot') and vice versa. This conversion depends on the size of the
-- epochs: 'EpochSize'. This size will not be the same for each epoch. When
-- opening the database, the user must give a function of type 'EpochNo -> m
-- EpochSize' that will be used to find out (and cache using
-- 'CumulEpochSizes') the size of each epoch.
--
-- For example:
--
-- > Epochs:         <──────── 0 ────────> <────── 1 ──────>
-- > Epoch size:               4                   3
-- >                 ┌───┬───┬───┬───┬───┐ ┌───┬───┬───┬───┐
-- >                 │   │   │   │   │   │ │   │   │   │   │
-- >                 └───┴───┴───┴───┴───┘ └───┴───┴───┴───┘
-- > 'RelativeSlot':   0   1   2   3   4     0   1   2   3
-- > 'SlotNo':        EBB  0   1   2   3    EBB  4   5   6
--
-- = Errors
--
-- Whenever an 'Ouroboros.Storage.ImmutableDB.Types.UnexpectedError' is thrown
-- during an operation, e.g., 'appendBinaryBlob', the database will be
-- automatically closed because we can not guarantee a consistent state in the
-- face of file system errors. See the 'reopen' operation and the paragraph
-- below about reopening the database for more information.
--
-- = (Re)opening the database
--
-- The database can be closed and reopened. In case the database was closed
-- because of an unexpected error, the same database can be reopened again
-- with 'reopen' using a 'ValidationPolicy', which will truncate invalid data
-- from the database until a valid prefix is recovered.
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
--   * An \"index file\" that stores the offsets of the binary blobs and the
--     hash of the EBB. These are used to efficiently seek within the epoch
--     file. Index files are only written for \"finalised\" epochs, i.e. an
--     epoch that can no longer be appended to because a new epoch was
--     started.
--
-- == Index file layout
--
-- The index file has the following layout:
--
-- > ┌────────┬────────┬────────┬┄┄┄┄┄┬────────┐
-- > │offset 0│offset 1│offset 2│ ... │EBB hash│
-- > └────────┴────────┴────────┴┄┄┄┄┄┴────────┘
--
-- Where each @offset i@ is the offset in the epoch file where relative slot
-- @i@ starts. Each @offset i@ is a 'Data.Word.Word64' (8 bytes).
--
-- For example, say we have written \"a\" to relative slot 0 (the EBB),
-- \"bravo\" to relative slot 1, and \"haskell\" to slot 4. We get the
-- following index file (the row @offset@ is what is stored in the index
-- file) (the hash of the EBB was omitted):
--
-- > relative slot:   0   1   2   3   4
-- >                ┌───┬───┬───┬───┬───┬────┐
-- > offset:        │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >                └───┴───┴───┴───┴───┴────┘
--
-- Note that the last (relative) slot we appended to was the 5th slot, the
-- slot with index 4, but there are 6 offsets in the index. In other words,
-- the index contains @slots + 1@ entries (or @lastSlot + 2@). The last offset
-- is the offset for the next slot that can be appended to and can also be
-- used to find out the size of the last binary blob.
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
-- padded to @epochSize + 2@ (1 extra for the EBB and 1 extra for the final
-- offset) offsets to record that the slots after the last slot that was
-- appended to are empty.
--
-- For example, continuing with the index above, assuming the @epochSize@ is
-- 6, after starting a new epoch, the index file will be padded to the
-- following:
--
-- > relative slot:   0   1   2   3   4    5    6
-- >                ┌───┬───┬───┬───┬───┬────┬────┬────┐
-- > offset:        │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │ 13 │ 13 │
-- >                └───┴───┴───┴───┴───┴────┴────┴────┘
--
-- The last offset was repeated twice to indicate that slots 5 and 6 were
-- unfilled.
module Ouroboros.Storage.ImmutableDB.Impl
  ( openDB
  ) where

import           Prelude hiding (truncate)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Exception (assert)
import           Control.Monad (forM_, replicateM_, unless, when)
import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import           Control.Monad.Class.MonadThrow (ExitCase (..),
                     MonadCatch (generalBracket), MonadThrow, finally)
import           Control.Monad.State.Strict (StateT (..), get, lift, modify,
                     put, runStateT, state)

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Either (isRight)
import           Data.Functor (($>), (<&>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word

import           GHC.Stack (HasCallStack, callStack)

import           Ouroboros.Consensus.Util (SomePair (..))

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Index
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Util

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}


-- | The environment used by the immutable database.
data ImmutableDBEnv m hash = forall h e. ImmutableDBEnv
    { _dbHasFS           :: !(HasFS m h)
    , _dbErr             :: !(ErrorHandling ImmutableDBError m)
    , _dbInternalState   :: !(TMVar m (Either (ClosedState m) (OpenState m hash h)))
    , _dbEpochFileParser :: !(EpochFileParser e hash m (Word64, SlotNo))
    , _dbHashDecoder     :: !(forall s . Decoder s hash)
    , _dbHashEncoder     :: !(hash -> Encoding)
    }

-- | Internal state when the database is open.
data OpenState m hash h = OpenState
    { _currentEpoch            :: !EpochNo
    -- ^ The current 'EpochNo' the immutable store is writing to.
    , _currentEpochWriteHandle :: !h
    -- ^ The write handle for the current epoch file.
    , _currentEpochOffsets     :: !(NonEmpty SlotOffset)
    -- ^ The offsets to which blobs have been written in the current epoch
    -- file, stored from last to first.
    , _currentEBBHash          :: !(Maybe hash)
    -- ^ The hash of the EBB of the current epoch that must be appended to the
    -- index file when finalising the current epoch.
    , _currentTip              :: !ImmTip
    -- ^ The current tip of the database.
    , _epochInfo               :: !(EpochInfo m)
    -- ^ Function to get the size of an epoch.
    , _nextIteratorID          :: !BaseIteratorID
    -- ^ The ID of the next iterator that will be created.
    }


-- | Internal state when the database is closed. This contains data that
-- should be restored when the database is reopened. Data not present here
-- will be recovered when reopening.
data ClosedState m = ClosedState
    { _closedEpochInfo      :: !(EpochInfo m)
    -- ^ See '_epochInfo'.
    , _closedNextIteratorID :: !BaseIteratorID
    -- ^ See '_nextIteratorID'.
    }


-- | Variant of 'Tip' that uses 'EpochSlot' instead of 'EpochNo' or 'SlotNo'.
data TipEpochSlot
  = TipEpochSlotGenesis
  | TipEpochSlot EpochSlot
  deriving (Eq)

instance Ord TipEpochSlot where
  compare te1 te2 = case (te1, te2) of
    (TipEpochSlotGenesis, TipEpochSlotGenesis) -> EQ
    (TipEpochSlotGenesis, _)                   -> LT
    (_,                   TipEpochSlotGenesis) -> GT
    (TipEpochSlot es1,    TipEpochSlot es2)    -> compare es1 es2


{------------------------------------------------------------------------------
  ImmutableDB API
------------------------------------------------------------------------------}

-- | Open the database, creating it from scratch if necessary or reopening an
-- existing one using the given 'ValidationPolicy'.
--
-- A function that can be used to look up the size of an epoch must be passed.
-- This function must:
--
-- * For each epoch, return a strictly positive (> 0) epoch size,
-- * Always return the same epoch size for the same given epoch.
--
-- The results of this function will be cached.
--
-- See 'ValidationPolicy' for more details on the different validation
-- policies.
--
-- An 'EpochFileParser' must be passed in order to reconstruct indices from
-- epoch files. The 'Word' that the 'EpochFileParser' must return for each
-- 'SlotNo' is the size (in bytes) occupied by the (non-empty) block
-- corresponding to the 'SlotNo'. The only reason we need to know the size of
-- the blocks is to compute the offset of the end of the last block, so we can
-- know where to truncate the file to in case of invalid trailing data. For
-- all other blocks, we can derive this from the offset of the next block, but
-- there is of course no block after the last one.
--
-- __Note__: To be used in conjunction with 'withDB'.
openDB
  :: (HasCallStack, MonadSTM m, MonadCatch m, Eq hash)
  => (forall s . Decoder s hash)
  -> (hash -> Encoding)
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> ValidationPolicy
  -> EpochFileParser e hash m (Word64, SlotNo)
  -> m (ImmutableDB hash m)
openDB = openDBImpl

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

mkDBRecord :: (MonadSTM m, MonadCatch m, Eq hash)
           => ImmutableDBEnv m hash
           -> ImmutableDB hash m
mkDBRecord dbEnv = ImmutableDB
    { closeDB           = closeDBImpl           dbEnv
    , isOpen            = isOpenImpl            dbEnv
    , reopen            = reopenImpl            dbEnv
    , deleteAfter       = deleteAfterImpl       dbEnv
    , getTip            = getTipImpl            dbEnv
    , getBinaryBlob     = getBinaryBlobImpl     dbEnv
    , getEBB            = getEBBImpl            dbEnv
    , appendBinaryBlob  = appendBinaryBlobImpl  dbEnv
    , appendEBB         = appendEBBImpl         dbEnv
    , streamBinaryBlobs = streamBinaryBlobsImpl dbEnv
    , immutableDBErr    = _dbErr dbEnv
    }

openDBImpl :: forall m h hash e.
              (HasCallStack, MonadSTM m, MonadCatch m, Eq hash)
           => (forall s . Decoder s hash)
           -> (hash -> Encoding)
           -> HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> EpochInfo m
           -> ValidationPolicy
           -> EpochFileParser e hash m (Word64, SlotNo)
           -> m (ImmutableDB hash m)
openDBImpl hashDecoder hashEncoder hasFS@HasFS{..} err epochInfo valPol epochFileParser = do
    !ost  <- validateAndReopen hashDecoder hashEncoder hasFS err epochInfo
      valPol epochFileParser initialIteratorID

    stVar <- atomically $ newTMVar (Right ost)

    let dbEnv = ImmutableDBEnv hasFS err stVar epochFileParser hashDecoder hashEncoder
        db    = mkDBRecord dbEnv
    return db


closeDBImpl :: (HasCallStack, MonadSTM m)
            => ImmutableDBEnv m hash
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

isOpenImpl :: MonadSTM m => ImmutableDBEnv m hash -> m Bool
isOpenImpl ImmutableDBEnv {..} =
    isRight <$> atomically (readTMVar _dbInternalState)

reopenImpl :: forall m hash.
              (HasCallStack, MonadSTM m, MonadThrow m, Eq hash)
           => ImmutableDBEnv m hash
           -> ValidationPolicy
           -> m ()
reopenImpl ImmutableDBEnv {..} valPol = do
    internalState <- atomically $ takeTMVar _dbInternalState
    case internalState of
      -- When still open,
      Right _ -> do
        atomically $ putTMVar _dbInternalState internalState
        throwUserError _dbErr OpenDBError

      -- Closed, so we can try to reopen
      Left ClosedState {..} ->
        -- Important: put back the state when an error is thrown, otherwise we
        -- have an empty TMVar.
        onException hasFsErr _dbErr
          (atomically $ putTMVar _dbInternalState internalState) $ do

            !ost  <- validateAndReopen _dbHashDecoder _dbHashEncoder _dbHasFS
              _dbErr _closedEpochInfo valPol _dbEpochFileParser
              _closedNextIteratorID

            atomically $ putTMVar _dbInternalState (Right ost)
  where
    HasFS{..} = _dbHasFS

-- TODO close all iterators
deleteAfterImpl :: forall m hash.
                   (HasCallStack, MonadSTM m, MonadCatch m)
                => ImmutableDBEnv m hash
                -> ImmTip
                -> m ()
deleteAfterImpl dbEnv tip = modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do
    st@OpenState {..} <- get
    currentEpochSlot <- lift $ tipToEpochSlot _epochInfo _currentTip
    newEpochSlot     <- lift $ tipToEpochSlot _epochInfo tip

    case (currentEpochSlot, newEpochSlot) of
      -- If we're already at genesis we don't have to do anything
      (TipEpochSlotGenesis, _)      -> return ()
      (TipEpochSlot cur, new)
        -- No truncation needed either
        | new >= TipEpochSlot cur   -> return ()
        | otherwise                 -> do
          -- Close the current epoch file, as we might have to remove it
          lift $ hClose _currentEpochWriteHandle
          mbNewTipAndIndex <- lift $ truncateToTipLEQ hasFS st new
          !ost <- lift $ case mbNewTipAndIndex of
            Nothing -> mkOpenStateNewEpoch hasFS 0 _epochInfo
              _nextIteratorID TipGen
            Just (epochSlot@(EpochSlot epoch _), index) -> do
              newTip <- epochSlotToTip _epochInfo epochSlot
              mkOpenState hasFS epoch _epochInfo
                _nextIteratorID newTip index
          put ost
  where
    ImmutableDBEnv { _dbErr } = dbEnv

    -- | The current tip as a 'TipEpochSlot'
    tipToEpochSlot :: EpochInfo m -> ImmTip -> m TipEpochSlot
    tipToEpochSlot epochInfo currentTip = case currentTip of
      TipGen           -> return $ TipEpochSlotGenesis
      Tip (Left epoch) -> return $ TipEpochSlot (EpochSlot epoch 0)
      Tip (Right slot) -> TipEpochSlot <$> epochInfoBlockRelative epochInfo slot

    -- | Truncate to the last valid (filled slot or EBB) tip <= the given tip.
    truncateToTipLEQ :: HasFS m h
                     -> OpenState m hash h
                     -> TipEpochSlot
                        -- ^ The returned epoch slot will be the last valid
                        -- tip <= this tip.
                     -> m (Maybe (EpochSlot, Index hash))
                        -- ^ 'Nothing': no valid tip found, we have truncated
                        -- to genesis. Otherwise, we have truncated so that
                        -- the tip is at the given 'EpochSlot', and the index
                        -- of the epoch is returned.
    truncateToTipLEQ hasFS@HasFS{..} OpenState{..} = \case
        TipEpochSlotGenesis -> removeFilesStartingFrom hasFS 0 $> Nothing
        TipEpochSlot (EpochSlot epoch relSlot) -> do
          index <- truncateIndex relSlot <$> openIndex epoch
          go epoch index
      where
        -- | Look for the last filled relative slot in the given epoch (the
        -- index of the epoch is the second argument). When found, return it
        -- and the index that has the relative slot as its last slot. Removes
        -- the files corresponding to later epochs and the index file of the
        -- index. Even when the epoch is complete, i.e., the last slot is
        -- filled, will its index file be removed, since the epoch will be
        -- reopened as the current epoch.
        go :: EpochNo -> Index hash -> m (Maybe (EpochSlot, Index hash))
        go epoch index
          | Just relSlot <- lastFilledSlot index = do
            let truncatedIndex = truncateIndex relSlot index
            withFile hasFS (renderFile "epoch" epoch) (AppendMode AllowExisting) $ \eHnd ->
              hTruncate eHnd (lastSlotOffset truncatedIndex)
            removeIndex epoch
            removeFilesStartingFrom hasFS (succ epoch)
            return $ Just (EpochSlot epoch relSlot, truncatedIndex)
          | epoch == 0
            -- We come to the first epoch, without finding any empty filled
            -- slots or EBBs, so remove all files
          = removeFilesStartingFrom hasFS 0 $> Nothing
          | otherwise
          = let epoch' = epoch - 1 in openIndex epoch' >>= go epoch'

        -- | Helper function to open the index file of an epoch.
        openIndex :: EpochNo -> m (Index hash)
        openIndex epoch
          | epoch == _currentEpoch
          = return $ indexFromSlotOffsets _currentEpochOffsets _currentEBBHash
          | otherwise
          = epochInfoSize _epochInfo epoch >>= \size ->
            loadIndex (_dbHashDecoder dbEnv) hasFS _dbErr epoch (succ size)

        -- | Remove the index file of the given epoch, if it exists.
        removeIndex :: EpochNo -> m ()
        removeIndex epoch = do
            indexFileExists <- doesFileExist indexFile
            when indexFileExists $ removeFile indexFile
          where
            indexFile = renderFile "index" epoch

        -- | Truncate the index so that the given relative slot is the last
        -- relative slot in the index.
        truncateIndex :: RelativeSlot -> Index hash -> Index hash
        truncateIndex relSlot =
            truncateToSlots (EpochSize (unRelativeSlot (succ relSlot)))

getTipImpl :: (HasCallStack, MonadSTM m)
           => ImmutableDBEnv m hash
           -> m ImmTip
getTipImpl dbEnv = do
    SomePair _hasFS OpenState { _currentTip } <- getOpenState dbEnv
    return _currentTip

getBinaryBlobImpl
  :: forall m hash. (HasCallStack, MonadSTM m, MonadCatch m)
  => ImmutableDBEnv m hash
  -> SlotNo
  -> m (Maybe ByteString)
getBinaryBlobImpl dbEnv slot = withOpenState dbEnv $ \_dbHasFS st@OpenState{..} -> do
    inTheFuture <- case _currentTip of
          TipGen                  -> return $ True
          Tip (Right lastSlot')   -> return $ slot > lastSlot'
          -- The slot (that pointing to a regular block) corresponding to this
          -- EBB will be empty, as the EBB is the last thing in the database.
          -- So if @slot@ is equal to this slot, it is also refering to the
          -- future.
          Tip (Left lastEBBEpoch) -> do
            ebbSlot <- epochInfoAbsolute _epochInfo (EpochSlot lastEBBEpoch 0)
            return $ slot >= ebbSlot

    when inTheFuture $
      throwUserError _dbErr $ ReadFutureSlotError slot _currentTip

    epochSlot <- epochInfoBlockRelative _epochInfo slot
    snd <$> getEpochSlot _dbHasFS (_dbHashDecoder dbEnv) st _dbErr epochSlot
  where
    ImmutableDBEnv { _dbErr } = dbEnv

getEBBImpl
  :: forall m hash. (HasCallStack, MonadSTM m, MonadCatch m)
  => ImmutableDBEnv m hash
  -> EpochNo
  -> m (Maybe (hash, ByteString))
getEBBImpl dbEnv epoch = withOpenState dbEnv $ \_dbHasFS st@OpenState{..} -> do
    let inTheFuture = case _currentTip of
          TipGen        -> True
          Tip (Right _) -> epoch > _currentEpoch
          Tip (Left _)  -> epoch > _currentEpoch

    when inTheFuture $
      throwUserError _dbErr $ ReadFutureEBBError epoch _currentEpoch

    (mbEBBHash, mbBlob) <- getEpochSlot _dbHasFS (_dbHashDecoder dbEnv) st _dbErr (EpochSlot epoch 0)
    return $ (,) <$> mbEBBHash <*> mbBlob
  where
    ImmutableDBEnv { _dbErr } = dbEnv

-- Preconditions: the given 'EpochSlot' is in the past.
getEpochSlot
  :: forall m hash h. (HasCallStack, MonadSTM m, MonadThrow m)
  => HasFS m h
  -> (forall s . Decoder s hash)
  -> OpenState m hash h
  -> ErrorHandling ImmutableDBError m
  -> EpochSlot
  -> m (Maybe hash, Maybe ByteString)
getEpochSlot _dbHasFS hashDecoder OpenState {..} _dbErr epochSlot = do
    let epochFile = renderFile "epoch" epoch
        indexFile = renderFile "index" epoch

    lastRelativeSlot <- case _currentTip of
      TipGen           -> error "Postcondition violated: EpochSlot must be in the past"
      Tip (Left  _)    -> return 0
      Tip (Right slot) -> _relativeSlot <$> epochInfoBlockRelative _epochInfo slot

    (blobOffset, blobSize, mbEBBHash) <- case epoch == _currentEpoch of
      -- If the requested epoch is the current epoch, the offsets are still in
      -- memory
      True -> assert (lastRelativeSlot >= relativeSlot) $
        case NE.drop toDrop _currentEpochOffsets of
            (offsetAfter:offset:_) ->
              return (offset, offsetAfter - offset, _currentEBBHash)
            [_] ->
              -- We requested the EBB, but no EBB has been written yet.
              return $ assert (relativeSlot == 0) (0, 0, Nothing)
            [] -> error "impossible: _currentEpochOffsets out of sync"
          where
            -- The substraction below cannot underflow, in other words:
            -- @lastRelativeSlot >= relativeSlot@. This is guaranteed by the
            -- precondition.
            toDrop = fromEnum (lastRelativeSlot - relativeSlot)

      -- Otherwise, the offsets will have to be read from an index file
      False -> withFile _dbHasFS indexFile ReadMode $ \iHnd -> do
        -- Grab the offset in bytes of the requested slot.
        let indexSeekPosition =
              (fromIntegral (unRelativeSlot relativeSlot)) *
              fromIntegral indexEntrySizeBytes
        hSeek iHnd AbsoluteSeek indexSeekPosition
        -- Compute the offset on disk and the blob size.
        let nbBytesToGet = fromIntegral indexEntrySizeBytes * 2
        -- Note the use of hGetExactly: we must get enough bytes from the
        -- index file, otherwise 'decodeIndexEntry' (and its variant) would
        -- fail.
        bytes <- toStrict <$> hGetExactly _dbHasFS iHnd nbBytesToGet
        let !start = decodeIndexEntry   bytes
            !end   = decodeIndexEntryAt indexEntrySizeBytes bytes

        mbEBBHash <- if relativeSlot == 0 && end > start
          then do
            -- Seek till after the offsets so we can read the hash
            epochSize <- epochInfoSize _epochInfo epoch
            let hashOffset = (fromIntegral epochSize + 2) * indexEntrySizeBytes
            hSeek iHnd AbsoluteSeek (fromIntegral hashOffset)
            deserialiseHash' =<< hGetAll _dbHasFS iHnd
          else return Nothing

        return (start, end - start, mbEBBHash)

    -- In case the requested is still the current epoch, we will be reading
    -- from the epoch file while we're also writing to it. Are we guaranteed
    -- to read what have written? Duncan says: this is guaranteed at the OS
    -- level (POSIX), but not for Haskell handles, which might perform other
    -- buffering. However, the 'HasFS' implementation we're using uses POSIX
    -- file handles ("Ouroboros.Storage.IO") so we're safe (other
    -- implementations of the 'HasFS' API guarantee this too).
    mbBlob <- case blobSize of
      0 -> return Nothing
      _ -> withFile _dbHasFS epochFile ReadMode $ \eHnd -> do
        -- Seek in the epoch file
        hSeek eHnd AbsoluteSeek (fromIntegral blobOffset)
        Just <$> hGetExactly _dbHasFS eHnd (fromIntegral blobSize)

    return (mbEBBHash, mbBlob)
  where
    HasFS{..}                    = _dbHasFS
    EpochSlot epoch relativeSlot = epochSlot

    deserialiseHash' :: HasCallStack => ByteString -> m (Maybe hash)
    deserialiseHash' bs = case deserialiseHash hashDecoder bs of
      Right (_, hash) -> return hash
      Left df         -> throwUnexpectedError _dbErr $
        DeserialisationError df callStack


appendBinaryBlobImpl :: forall m hash.
                        (HasCallStack, MonadSTM m, MonadCatch m)
                     => ImmutableDBEnv m hash
                     -> SlotNo
                     -> Builder
                     -> m ()
appendBinaryBlobImpl dbEnv@ImmutableDBEnv{..} slot builder =
    modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do

      OpenState { _currentEpoch, _currentTip, _epochInfo } <- get

      EpochSlot epoch relSlot <- lift $ epochInfoBlockRelative _epochInfo slot

      -- Check that we're not appending to the past
      let inThePast = case _currentTip of
            Tip (Right lastSlot')   -> slot  <= lastSlot'
            Tip (Left lastEBBEpoch) -> epoch <  lastEBBEpoch
            TipGen                  -> False

      when inThePast $ lift $ throwUserError _dbErr $
        AppendToSlotInThePastError slot _currentTip

      -- If the slot is in an epoch > the current one, we have to finalise the
      -- current one and start a new epoch file, possibly skipping some
      -- epochs.
      when (epoch > _currentEpoch) $ do
        let newEpochsToStart :: Int
            newEpochsToStart = fromIntegral . unEpochNo $ epoch - _currentEpoch
        -- Start as many new epochs as needed.
        replicateM_ newEpochsToStart (startNewEpoch _dbHashEncoder hasFS)

      let initialEpoch = _currentEpoch

      -- We may have updated the state with 'startNewEpoch', so get the
      -- (possibly) updated state.
      OpenState {..} <- get
      -- If necessary, backfill for any slots we skipped in the current epoch
      nextFreeRelSlot <- lift $
          if epoch > initialEpoch
            -- If we had to start a new epoch, we start with slot 0. Note that
            -- in this case the _currentTip will refer to something in an
            -- epoch before _currentEpoch.
            then return 0
            else case _currentTip of
                   TipGen                -> return $ 0
                   Tip (Left _ebb)       -> return $ 1
                   Tip (Right lastSlot') -> succ . _relativeSlot <$>
                     epochInfoBlockRelative _epochInfo lastSlot'
      let lastEpochOffset = NE.head _currentEpochOffsets
          backfillOffsets =
            indexBackfill relSlot nextFreeRelSlot lastEpochOffset

      -- Append to the end of the epoch file.
      bytesWritten <- lift $ onException hasFsErr _dbErr
        (hClose _currentEpochWriteHandle)
        (hPut hasFS _currentEpochWriteHandle builder)
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
        , _currentTip = Tip (Right slot)
        }

startNewEpoch :: (HasCallStack, MonadSTM m, MonadCatch m)
              => (hash -> Encoding)
              -> HasFS m h
              -> StateT (OpenState m hash h) m ()
startNewEpoch hashEncoder hasFS@HasFS{..} = do
    OpenState {..} <- get
    -- We can close the epoch file
    lift $ hClose _currentEpochWriteHandle

    -- Find out the size of the current epoch, so we can pad the
    -- _currentEpochOffsets to match the size before writing them to the index
    -- file. When looking at the index file, it will then be clear that the
    -- epoch is finalised.
    epochSize <- lift $ epochInfoSize _epochInfo _currentEpoch

    -- Calculate what to pad the file with
    nextFreeRelSlot <- lift $
        if null (NE.tail _currentEpochOffsets)
          -- We have to take care when starting multiple new epochs in a
          -- row. In the first call the tip will be in the current epoch,
          -- but in subsequent calls, the tip will still be in an epoch in
          -- the past, not the '_currentEpoch'. In that case, we can't use
          -- the relative slot of the tip, since it will point to a relative
          -- slot in a past epoch. So when the current epoch is empty
          -- (detected by looking at the offsets), we use relative slot 0 to
          -- calculate how much to pad.
          then return 0
          else case _currentTip of
                 TipGen                -> return $ 0
                 Tip (Left _ebb)       -> return $ 1
                 Tip (Right lastSlot') -> succ . _relativeSlot <$>
                   epochInfoBlockRelative _epochInfo lastSlot'

    -- The above calls may have modified the _cumulEpochSizes, so get it
    -- again.
    OpenState {..} <- get
    let lastEpochOffset = NE.head _currentEpochOffsets
        -- The last relative slot in the file
        lastRelSlot     = maxRelativeSlot epochSize
        backfillOffsets =
          indexBackfill (succ lastRelSlot) nextFreeRelSlot lastEpochOffset
        -- Prepend the backfillOffsets to the current offsets to get a
        -- non-empty list of all the offsets. Note that this list is stored in
        -- reverse order.
        allOffsets = foldr NE.cons _currentEpochOffsets backfillOffsets

    -- Now write the offsets and the EBB hash to the index file
    lift $ writeSlotOffsets hashEncoder hasFS _currentEpoch allOffsets _currentEBBHash

    st <- lift $ mkOpenStateNewEpoch hasFS (succ _currentEpoch) _epochInfo
      _nextIteratorID _currentTip

    put st

appendEBBImpl :: forall m hash.
                 (HasCallStack, MonadSTM m, MonadCatch m)
              => ImmutableDBEnv m hash
              -> EpochNo
              -> hash
              -> Builder
              -> m ()
appendEBBImpl dbEnv@ImmutableDBEnv{..} epoch hash builder =
    modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do
      OpenState { _currentEpoch, _currentTip, _currentEBBHash } <- get

      -- Check that we're not appending to the past
      let inThePast = case _currentTip of
            -- There is already a block in this epoch, so the EBB can no
            -- longer be appended in this epoch
            Tip (Right _) -> epoch <= _currentEpoch
            -- There is already an EBB in this epoch
            Tip (Left _)  -> epoch <= _currentEpoch
            TipGen        -> False

      when inThePast $ lift $ throwUserError _dbErr $
        AppendToEBBInThePastError epoch _currentEpoch

      -- It must be that epoch > _currentEpoch or epoch == _currentEpoch == 0.
      -- In the former case, one or more new epochs must be started.
      let newEpochsToStart :: Int
          newEpochsToStart = fromIntegral . unEpochNo $ epoch - _currentEpoch
      -- Start as many new epochs as needed.
      replicateM_ newEpochsToStart (startNewEpoch _dbHashEncoder hasFS)

      -- We may have updated the state with 'startNewEpoch', so get the
      -- (possibly) updated state.
      OpenState {..} <- get

      -- Append to the epoch file.
      bytesWritten <- lift $ onException hasFsErr _dbErr
        (hClose _currentEpochWriteHandle)
        (hPut hasFS _currentEpochWriteHandle builder)
        -- In 'modifyOpenState': when an exception occurs, we close the handle
        -- if there is one in the initial open state. However, we might have
        -- opened a new one when we called 'startNewEpoch', and this handle
        -- will be different from the one in the initial state, so
        -- 'modifyOpenState' cannot close it in case of an exception. So take
        -- care of it here.

      -- The EBB is always the first blob in the file
      let newOffset = bytesWritten

      modify $ \st -> st
        { _currentEpochOffsets = newOffset NE.<| _currentEpochOffsets
        , _currentEBBHash      = Just hash
        , _currentTip          = Tip (Left epoch)
        }

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}


-- | Internal handle to an iterator
data IteratorHandle hash m = forall h. IteratorHandle
  { _it_hasFS    :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , _it_state    :: !(TVar m (Maybe (IteratorState hash h)))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , _it_end      :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  , _it_end_hash :: !(Maybe hash)
    -- ^ The @hash@ of the last block the iterator should return. 'Nothing'
    -- when no @hash@ was specified, then only '_it_end' will be used to
    -- determine when to stop streaming.
  }

data IteratorState hash h = IteratorState
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
  , _it_epoch_index  :: Index hash
    -- ^ We load the index file for the epoch we are currently iterating over
    -- in-memory, as it's going to be small anyway (usually ~150kb).
  }

streamBinaryBlobsImpl :: forall m hash.
                         (HasCallStack, MonadSTM m, MonadCatch m, Eq hash)
                      => ImmutableDBEnv m hash
                      -> Maybe (SlotNo, hash)
                      -- ^ When to start streaming (inclusive).
                      -> Maybe (SlotNo, hash)
                      -- ^ When to stop streaming (inclusive).
                      -> m (Iterator hash m ByteString)
streamBinaryBlobsImpl dbEnv mbStart mbEnd = withOpenState dbEnv $ \hasFS st -> do
    let ImmutableDBEnv { _dbErr } = dbEnv
        HasFS {..}                = hasFS
        OpenState {..}            = st

    validateIteratorRange _dbErr _epochInfo _currentTip mbStart mbEnd

    emptyOrEndBound <- case _currentTip of
          TipGen -> return $ Nothing
          Tip (Left epoch)
            | Just (endSlot, endHash) <- mbEnd
            -> do -- We don't really know if the upper bound points at a
                  -- regular block or an EBB here. We conservatively assume it
                  -- must a regular block (which would come /after/ the EBB),
                  -- and then check this when we actually reach the end.
                  endEpochSlot <- epochInfoBlockRelative _epochInfo endSlot
                  return $ Just (endEpochSlot, Just endHash)
            | otherwise
            -> return $ Just (EpochSlot epoch 0, Nothing)
          Tip (Right lastSlot')
            | Just (endSlot, endHash) <- mbEnd
            -> do endEpochSlot <- epochInfoBlockRelative _epochInfo endSlot
                  return $ Just (endEpochSlot, Just endHash)
            | otherwise
            -> do endEpochSlot <- epochInfoBlockRelative _epochInfo lastSlot'
                  return $ Just (endEpochSlot, Nothing)

    case emptyOrEndBound of
      -- The database is empty, just return an empty iterator (directly
      -- exhausted)
      Nothing -> mkEmptyIterator
      Just (end, mbEndHash) -> do
        -- Fill in missing start bound
        (start@(EpochSlot startEpoch startRelSlot), mbStartHash) <-
          case mbStart of
            Just (startSlot, startHash) -> do
              startEpochSlot <- epochInfoBlockRelative _epochInfo startSlot
              return $ case startEpochSlot of
                  -- Include the EBB by setting the start relative slot to 0
                  EpochSlot epoch 1 -> (EpochSlot epoch 0, Just startHash)
                  epochSlot         -> (epochSlot,         Just startHash)
            _otherwise ->
              return (EpochSlot 0 0, Nothing)

        -- Helper function to open the index file of an epoch.
        let openIndex epoch
              | epoch == _currentEpoch
              = return $ indexFromSlotOffsets _currentEpochOffsets
                  _currentEBBHash
              | otherwise
              = epochInfoSize _epochInfo epoch >>= \size ->
                loadIndex (_dbHashDecoder dbEnv) hasFS _dbErr epoch (succ size)

        startIndex <- openIndex startEpoch

        -- True: use @start@ as the first 'EpochSlot' to start streaming from.
        --
        -- False: start searching after @start@ for an 'EpochSlot' to start
        -- streaming from.
        let useStartOtherwiseSearch :: Bool
            useStartOtherwiseSearch
              | containsSlot startIndex startRelSlot
                -- The above 'containsSlot' condition is needed because we do
                -- not know whether the index has the right size, which is a
                -- precondition for 'isFilledSlot'.
              , isFilledSlot startIndex startRelSlot
              = case startRelSlot of
                  -- If the startSlot refers to the first relative slot (0) of
                  -- the epoch and the hash doesn't match the EBB hash, then
                  -- skip the EBB and start from the block after it
                  0 | Just startHash <- mbStartHash
                      -- If slot 0 is filled, there must be an EBB hash
                    , let ebbHash = fromMaybe (error "missing EBB hash") $
                            getEBBHash startIndex
                    , ebbHash /= startHash
                    -> False
                    | otherwise
                      -- The startSlot refers to the first relative slot (0),
                      -- but either no start hash was defined or it matched
                      -- that of the EBB, so start from the EBB.
                    -> True
                  -- The startSlot refers to a filled relative slot other than
                  -- the first (0), so start from that relative slot. We don't
                  -- care about hashes, as only the EBB in relative slot 0 has
                  -- a hash.
                  _ -> True
              | otherwise
                -- The slot was not filled, so indicate that we should search
                -- for a filled after it
              = False

        -- If we can't start from @start@, find the next filled 'EpochSlot' to
        -- start from. If there is none in this epoch, open the next epoch
        -- until you find one. If we didn't find a filled slot before reaching
        -- @end@, return Nothing.
        mbIndexAndNext <- if useStartOtherwiseSearch
          then return $ Just (startIndex, start)
          else case nextFilledSlot startIndex startRelSlot of
            -- We no longer case about the start hash, as we are starting at a
            -- later slot anyway. We don't care for end hash either, as we're
            -- still in the same epoch so there can be no more EBB that we
            -- would have to check the hash of.
            Just relSlot
              -- There is a filled slot, but we've gone too far
              | EpochSlot startEpoch relSlot > end
              -> return Nothing
              -- There is a filled slot after startSlot in this epoch
              | otherwise
              -> return $ Just (startIndex, EpochSlot startEpoch relSlot)
            -- No filled slot in the start epoch, open the next
            Nothing -> lookInLaterEpochs (startEpoch + 1)
              where
                lookInLaterEpochs epoch
                  -- Because we have checked that @end@ is valid, this check
                  -- is enough to guarantee that we will never open the index
                  -- of a future epoch, i.e. try to open a non-existing index
                  -- file.
                  | epoch > _epoch end = return Nothing
                  | otherwise = do
                    index <- openIndex epoch
                    case firstFilledSlot index of
                      Just relSlot
                        -- We've gone too far
                        | EpochSlot epoch relSlot > end
                        -> return Nothing
                        | otherwise
                        -- This @relSlot@ might refer to the first relative
                        -- slot (0) of an epoch, so it might refer to an EBB.
                        -- However, we don't have to check the EBB hash, as
                        -- the EBB must be included in the stream whether the
                        -- hash matches or not, because the EBB comes before
                        -- the block stored at the same 'SlotNo'. When
                        -- advancing the iterator, we will check whether we
                        -- should stop after the EBB or include the next
                        -- block.
                        -> return $ Just (index, EpochSlot epoch relSlot)
                      Nothing -> lookInLaterEpochs (epoch + 1)

        mbIteratorState <- case mbIndexAndNext of
          -- No filled slot found, so just create a closed iterator
          Nothing -> return Nothing
          Just (index, next@(EpochSlot nextEpoch nextRelSlot)) -> do
            -- Invariant 1 = OK by the search above for a filled slot

            eHnd <- hOpen (renderFile "epoch" nextEpoch) ReadMode
            -- Invariant 2 = OK

            -- Invariant 3 = OK by the search above for a filled slot

            -- Position the epoch handle at the right place. Invariant 4 = OK
            let offset = fromIntegral (offsetOfSlot index nextRelSlot)
            -- Close the handle if the seek fails
            onException hasFsErr _dbErr (hClose eHnd) $
              hSeek eHnd AbsoluteSeek offset

            return $ Just IteratorState
              { _it_next         = next
              , _it_epoch_handle = eHnd
              , _it_epoch_index  = index
              }

        itState <- atomically $ newTVar mbIteratorState

        let ith = IteratorHandle
              { _it_hasFS    = hasFS
              , _it_state    = itState
              , _it_end      = end
              , _it_end_hash = mbEndHash
              }
        -- Safely increment '_nextIteratorID' in the 'OpenState'.
        modifyOpenState dbEnv $ \_hasFS -> state $ \st'@OpenState {..} ->
          let it = Iterator
                { iteratorNext    = iteratorNextImpl  dbEnv ith True
                , iteratorPeek    = iteratorNextImpl  dbEnv ith False
                , iteratorHasNext = iteratorHasNextImpl     ith
                , iteratorClose   = iteratorCloseImpl       ith
                , iteratorID      = BaseIteratorID _nextIteratorID
                }
          in (it, st' { _nextIteratorID = succ _nextIteratorID })
  where
    mkEmptyIterator :: m (Iterator hash m ByteString)
    mkEmptyIterator =
      modifyOpenState dbEnv $ \_hasFS -> state $ \st@OpenState {..} ->
        let it = Iterator
              { iteratorNext    = return IteratorExhausted
              , iteratorPeek    = return IteratorExhausted
              , iteratorHasNext = return False
              , iteratorClose   = return ()
              , iteratorID      = BaseIteratorID _nextIteratorID
              }
        in (it, st { _nextIteratorID = succ _nextIteratorID })


iteratorNextImpl :: forall m hash.
                    (MonadSTM m, MonadCatch m, Eq hash)
                 => ImmutableDBEnv m hash
                 -> IteratorHandle hash m
                 -> Bool  -- ^ Step the iterator after reading iff True
                 -> m (IteratorResult hash ByteString)
iteratorNextImpl dbEnv it@IteratorHandle {_it_hasFS = hasFS :: HasFS m h, ..} step = do
    -- The idea is that if the state is not Nothing, then '_it_next' is always
    -- ready to be read. After reading it with 'readNext', 'stepIterator' will
    -- advance the iterator to the next valid epoch slot if @step@ is True.
    mbIteratorState <- atomically $ readTVar _it_state
    case mbIteratorState of
      -- Iterator already closed
      Nothing -> return IteratorExhausted
      -- Valid @next@ thanks to Invariant 1, so go ahead and read it
      Just iteratorState@IteratorState{..} -> withOpenState dbEnv $ \_ st -> do
        slot <- epochInfoAbsolute (_epochInfo st) _it_next
        blob <- readNext iteratorState
        case _it_next of
          -- It's an EBB
          EpochSlot epoch 0
            | let ebbHash = fromMaybe (error "missing EBB hash") $
                    getEBBHash _it_epoch_index
            -> do
              when step $ case (_it_end, _it_end_hash) of
                -- Special case: if the thing we are returning is an EBB and
                -- its 'EpochSlot' matches '_it_end' and its EBB hash matches
                -- '_it_end_hash', then we must stop after this EBB. Note that
                -- the '_it_end' will refer to relative slot 1, even though
                -- the EBB is stored at relative slot 0, because at the time
                -- we calculate '_it_end"", we don't know yet whether to stop
                -- at the EBB or the block stored in the same slot (after the
                -- EBB).
                (EpochSlot endEpoch 1, Just endHash)
                  | epoch == endEpoch, endHash == ebbHash
                  -> iteratorCloseImpl it
                _ -> stepIterator st iteratorState
              return $ IteratorEBB epoch ebbHash blob
          _ -> do
            -- If @step@ is True, advance the iterator before returning the
            -- read blob, so it has a valid @next@ to read the next time.
            when step $ stepIterator st iteratorState
            return $ IteratorResult slot blob
  where
    HasFS{..} = hasFS

    readNext :: IteratorState hash h -> m ByteString
    readNext IteratorState { _it_epoch_handle = eHnd
                           , _it_next = EpochSlot _ relSlot
                           , _it_epoch_index = index } = do
      -- Grab the blob size from the cached index
      let blobSize = sizeOfSlot index relSlot

      -- Read from the epoch file. No need for seeking: as we are streaming,
      -- we are already positioned at the correct place (Invariant 4).
      hGetExactly hasFS eHnd (fromIntegral blobSize)
        `finally`
        -- Seek to the previous position if we shouldn't step to the next.
        unless step (hSeek eHnd RelativeSeek (negate (fromIntegral blobSize)))
    -- Move the iterator to the next position that can be read from, advancing
    -- epochs if necessary. If no next position can be found, the iterator is
    -- closed.
    stepIterator :: OpenState m hash h' -> IteratorState hash h -> m ()
    stepIterator st its@IteratorState { _it_epoch_handle = eHnd
                                      , _it_next = EpochSlot epoch currentRelSlot
                                      , _it_epoch_index = index } =
      case nextFilledSlot index currentRelSlot of
        -- We're still in the same epoch
        Just nextRelSlot
          | next <= _it_end
            -- We don't have to look at the end hash, because the next filled
            -- slot can never refer to an EBB (only stored at slot 0), and
            -- only when looking at an EBB can we check the hash.
          -> atomically $ writeTVar _it_state $ Just its { _it_next = next }
             -- Invariant 1 is OK (see condition), Invariant 2 is unchanged,
             -- Invariant 3 is OK (thanks to nextFilledSlot), Invariant 4 is
             -- OK (readNext moved the handle + nextFilledSlot).
          | otherwise
          -> iteratorCloseImpl it
          where
            next = EpochSlot epoch nextRelSlot

        -- EpochNo exhausted, open the next epoch
        Nothing -> do
          hClose eHnd
          openNextNonEmptyEpoch (epoch + 1) st

    -- Start opening epochs (starting from the given epoch number) until we
    -- encounter a non-empty one, then update the iterator state accordingly.
    -- If no non-empty epoch can be found, the iterator is closed.
    openNextNonEmptyEpoch :: EpochNo -> OpenState m hash h' -> m ()
    openNextNonEmptyEpoch epoch st@OpenState {..}
      | epoch > _epoch _it_end
      = iteratorCloseImpl it
      | otherwise = do
        -- Thanks to the guard we know that epoch <= _epoch _it_end. We also
        -- know that _epoch _it_end is <= _currentEpoch, so we know that epoch
        -- <= _currentEpoch.
        index <- case epoch == _currentEpoch of
          False -> epochInfoSize _epochInfo epoch >>= \size ->
                   loadIndex (_dbHashDecoder dbEnv) hasFS (_dbErr dbEnv) epoch (succ size)
          True  -> return $
            indexFromSlotOffsets _currentEpochOffsets _currentEBBHash

        case firstFilledSlot index of
          -- Empty epoch -> try the next one
          Nothing -> openNextNonEmptyEpoch (epoch + 1) st
          Just relSlot
            -- Slot is after the end -> stop
            | EpochSlot epoch relSlot > _it_end -> iteratorCloseImpl it
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
                { _it_next = EpochSlot epoch relSlot
                , _it_epoch_handle = eHnd
                , _it_epoch_index = index
                }

iteratorHasNextImpl :: (HasCallStack, MonadSTM m)
                    => IteratorHandle hash m
                    -> m Bool
iteratorHasNextImpl IteratorHandle { _it_state } =
    fmap isJust $ atomically $ readTVar _it_state

iteratorCloseImpl :: (HasCallStack, MonadSTM m)
                  => IteratorHandle hash m
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

-- | Perform validation as per the 'ValidationPolicy' using 'validate' and
-- create an 'OpenState' corresponding to its outcome.
validateAndReopen :: forall m hash h e.
                     (HasCallStack, MonadThrow m, Eq hash)
                  => (forall s . Decoder s hash)
                  -> (hash -> Encoding)
                  -> HasFS m h
                  -> ErrorHandling ImmutableDBError m
                  -> EpochInfo m
                  -> ValidationPolicy
                  -> EpochFileParser e hash m (Word64, SlotNo)
                  -> BaseIteratorID
                  -> m (OpenState m hash h)
validateAndReopen hashDecoder hashEncoder hasFS err epochInfo valPol epochFileParser nextIteratorID = do
    mbLastValidLocationAndIndex <-
      validate hashDecoder hashEncoder hasFS err epochInfo valPol epochFileParser

    case mbLastValidLocationAndIndex of
      Nothing ->
        mkOpenStateNewEpoch hasFS 0 epochInfo nextIteratorID TipGen
      Just (lastValidLocation, index) -> do
        tip <- epochSlotToTip epochInfo lastValidLocation
        let epoch = _epoch lastValidLocation
        mkOpenState hasFS epoch epochInfo nextIteratorID tip index

-- | Create the internal open state based on an epoch with the given 'Index'.
--
-- Open the epoch file for appending.
mkOpenState :: (HasCallStack, MonadThrow m)
            => HasFS m h
            -> EpochNo
            -> EpochInfo m
            -> BaseIteratorID
            -> ImmTip
            -> Index hash
            -> m (OpenState m hash h)
mkOpenState HasFS{..} epoch epochInfo nextIteratorID tip index = do
    let epochFile     = renderFile "epoch" epoch
        epochOffsets  = indexToSlotOffsets index

    eHnd <- hOpen epochFile (AppendMode AllowExisting)

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _currentEBBHash          = getEBBHash index
      , _currentTip              = tip
      , _epochInfo               = epochInfo
      , _nextIteratorID          = nextIteratorID
      }

-- | Create the internal open state for a new empty epoch.
--
-- Create the epoch file for appending.
mkOpenStateNewEpoch :: (HasCallStack, MonadThrow m)
                    => HasFS m h
                    -> EpochNo
                    -> EpochInfo m
                    -> BaseIteratorID
                    -> ImmTip
                    -> m (OpenState m hash h)
mkOpenStateNewEpoch HasFS{..} epoch epochInfo nextIteratorID tip = do
    let epochFile    = renderFile "epoch" epoch
        epochOffsets = 0 NE.:| []

    eHnd <- hOpen epochFile (AppendMode MustBeNew)

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _currentEBBHash          = Nothing
      , _currentTip              = tip
      , _epochInfo               = epochInfo
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
             => ImmutableDBEnv m hash
             -> m (SomePair (HasFS m) (OpenState m hash))
getOpenState ImmutableDBEnv {..} = do
    internalState <- atomically (readTMVar _dbInternalState)
    case internalState of
       Left  _         -> throwUserError _dbErr ClosedDBError
       Right openState -> return (SomePair _dbHasFS openState)

-- | Modify the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedError' is thrown, the database is closed to prevent
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
modifyOpenState :: forall m hash r. (HasCallStack, MonadSTM m, MonadCatch m)
                => ImmutableDBEnv m hash
                -> (forall h. HasFS m h -> StateT (OpenState m hash h) m r)
                -> m r
modifyOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open close (tryImmDB hasFsErr _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (r, _) -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    -- We use @m (Either e a)@ instead of @EitherT e m a@ for 'generalBracket'
    -- so that 'close' knows which error is thrown (@Either e (s, r)@ vs. @(s,
    -- r)@).

    open :: m (Either (ClosedState m) (OpenState m hash h))
    open = atomically $ takeTMVar _dbInternalState

    close :: Either (ClosedState m) (OpenState m hash h)
          -> ExitCase (Either ImmutableDBError (r, OpenState m hash h))
          -> m ()
    close !st ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> atomically $ putTMVar _dbInternalState st
      -- In case of an exception, close the DB for safety.
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
             => Either (ClosedState m) (OpenState m hash h)
             -> m (r, OpenState m hash h)
    mutation (Left _)    = throwUserError _dbErr ClosedDBError
    mutation (Right ost) = runStateT (action hasFS) ost

    -- TODO what if this fails?
    closeOpenHandles :: Either (ClosedState m) (OpenState m hash h) -> m ()
    closeOpenHandles (Left _)               = return ()
    closeOpenHandles (Right OpenState {..}) = hClose _currentEpochWriteHandle

-- | Perform an action that accesses the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedError' is thrown while the action is being run, the
-- database is closed to prevent further appending to a database in a
-- potentially inconsistent state.
withOpenState :: forall m hash r. (HasCallStack, MonadSTM m, MonadCatch m)
              => ImmutableDBEnv m hash
              -> (forall h. HasFS m h -> OpenState m hash h -> m r)
              -> m r
withOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open (const close) (tryImmDB hasFsErr _dbErr . access)
    case mr of
      Left  e -> throwError e
      Right r -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    open :: m (Either (ClosedState m) (OpenState m hash h))
    open = atomically $ readTMVar _dbInternalState

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readTMVar' here, 'takeTMVar'). So we need to get the most recent
    -- state anyway.
    close :: ExitCase (Either ImmutableDBError r)
          -> m ()
    close ec = case ec of
      ExitCaseAbort         -> return ()
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        st <- atomically $ do
          st <- takeTMVar _dbInternalState
          let !cst = closedStateFromInternalState st
          putTMVar _dbInternalState (Left cst)
          return st
        closeOpenHandles st
      ExitCaseSuccess (Right _) -> return ()
      -- In case of an ImmutableDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- We need to get the most recent state because it might have changed
        -- behind our back
        st <- atomically $ do
          st <- takeTMVar _dbInternalState
          let !cst = closedStateFromInternalState st
          putTMVar _dbInternalState (Left cst)
          return st
        closeOpenHandles st
      ExitCaseSuccess (Left (UserError {})) -> return ()

    access :: HasCallStack
           => Either (ClosedState m) (OpenState m hash h)
           -> m r
    access (Left _)    = throwUserError _dbErr ClosedDBError
    access (Right ost) = action hasFS ost

    -- TODO what if this fails?
    closeOpenHandles :: Either (ClosedState m) (OpenState m hash h) -> m ()
    closeOpenHandles (Left _)               = return ()
    closeOpenHandles (Right OpenState {..}) = hClose _currentEpochWriteHandle


-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: Either (ClosedState m) (OpenState m hash h)
                             -> ClosedState m
closedStateFromInternalState (Left cst) = cst
closedStateFromInternalState (Right OpenState {..}) = ClosedState
  { _closedEpochInfo       = _epochInfo
  , _closedNextIteratorID  = _nextIteratorID
  }

-- | Convert an 'EpochSlot' to a 'Tip'
epochSlotToTip :: Monad m => EpochInfo m -> EpochSlot -> m ImmTip
epochSlotToTip _         (EpochSlot epoch 0) = return $ Tip (Left epoch)
epochSlotToTip epochInfo epochSlot           = Tip . Right <$>
    epochInfoAbsolute epochInfo epochSlot

-- | Go through all files, making two sets: the set of epoch-xxx.dat
-- files, and the set of index-xxx.dat files, discarding all others.
dbFilesOnDisk :: Set String -> (Set EpochNo, Set EpochNo)
dbFilesOnDisk = foldr categorise mempty
  where
    categorise file fs@(epochFiles, indexFiles) = case parseDBFile file of
      Just ("epoch", n) -> (Set.insert n epochFiles, indexFiles)
      Just ("index", n) -> (epochFiles, Set.insert n indexFiles)
      _                 -> fs

-- | Remove all epoch and index starting from the given epoch (included).
removeFilesStartingFrom :: (HasCallStack, Monad m)
                        => HasFS m h
                        -> EpochNo
                        -> m ()
removeFilesStartingFrom HasFS{..} epoch = do
    filesInDBFolder <- listDirectory []
    let (epochFiles, indexFiles) = dbFilesOnDisk filesInDBFolder
    forM_ (takeWhile (>= epoch) (Set.toDescList epochFiles)) $ \e ->
      removeFile (renderFile "epoch" e)
    forM_ (takeWhile (>= epoch) (Set.toDescList indexFiles)) $ \i ->
      removeFile (renderFile "index" i)

-- | Internal data type used as the result of @validateEpoch@.
data ValidateResult hash
  = Missing
    -- ^ The epoch file is missing. The epoch and index files corresponding to
    -- the epoch are guaranteed to be no longer on disk.
  | Complete   (Index hash)
    -- ^ There is a valid epoch file and a valid index file on disk (this may
    -- be the result of recovery). The index is complete, i.e. finalised,
    -- according to the index or because the last slot of the epoch was
    -- filled.
    --
    -- The index may end with an empty slot.
  | Incomplete (Index hash)
    -- ^ There is a valid epoch file on disk. There is no index file on disk
    -- (this may have been removed during recovery). The index is incomplete.
    --
    -- Either the index ends with a filled slot or it is empty.


-- | Execute the 'ValidationPolicy'.
--
-- * Invalid or missing files will cause truncation. Later epoch and index
--   files are removed. Trailing empty slots are truncated so that the tip of
--   the database will always point to a valid block or EBB.
--
-- * Epoch files are the main source of truth. Index files can be
--   reconstructed from the epoch files using the 'EpochFileParser'.
--
-- * Only complete index files (with the same number of slots as the epoch
--   size) are valid.
--
-- * The last, unfinalised epoch will not have an index file. We do our best
--   to only reconstruct its index once.
--
-- * Index files are checked against the indices reconstructed from the epoch
--   files. Reconstructed indices are unaware of empty trailing slots. Special
--   case: when the last slot of an epoch is filled, the reconstructed index
--   gives us all the information we need, because there can't be any trailing
--   empty slots that only the index file could know about. In this case, we
--   overwrite the index file if it is missing or invalid instead of
--   truncating the database. This means that if all index files are missing,
--   but the last slot of each epoch is filled, we can reconstruct all index
--   files from the epochs without needing any truncation.
validate :: forall m hash h e.
            (HasCallStack, MonadThrow m, Eq hash)
         => (forall s . Decoder s hash)
         -> (hash -> Encoding)
         -> HasFS m h
         -> ErrorHandling ImmutableDBError m
         -> EpochInfo m
         -> ValidationPolicy
         -> EpochFileParser e hash m (Word64, SlotNo)
         -> m (Maybe (EpochSlot, Index hash))
            -- ^ The 'EpochSlot' pointing at the last valid block or EBB on
            -- disk and the 'Index' of the corresponding epoch. 'Nothing' if
            -- the database is empty.
validate hashDecoder hashEncoder hasFS@HasFS{..} err epochInfo valPol epochFileParser = do
    filesInDBFolder <- listDirectory []
    let epochFiles = fst $ dbFilesOnDisk filesInDBFolder
    case Set.lookupMax epochFiles of
      Nothing              -> do
        -- Remove left-over index files
        removeFilesStartingFrom hasFS 0
        -- TODO calls listDirectory again
        return Nothing

      Just lastEpochOnDisk -> case valPol of
        ValidateMostRecentEpoch -> validateMostRecentEpoch lastEpochOnDisk
        ValidateAllEpochs       -> validateAllEpochs       lastEpochOnDisk
  where
    -- | Validate the most recent (given) epoch using 'validateEpoch'.
    --
    -- Starts from the given epoch, if that is invalid or empty, it is
    -- truncated and the epoch before it is validated, and so on.
    --
    -- Validation stops as soon as we have found a valid non-empty epoch.
    --
    -- The location of the last valid block or EBB, along with the index of
    -- the corresponding epoch, is returned.
    --
    -- All data after the last valid block or EBB is truncated.
    validateMostRecentEpoch :: HasCallStack
                            => EpochNo
                            -> m (Maybe (EpochSlot, Index hash))
    validateMostRecentEpoch = go
      where
        go epoch = do
          validateRes <- validateEpoch epoch
          let continueIfPossible | epoch == 0 = return Nothing
                                 | otherwise  = go (epoch - 1)
          case validateRes of
            Missing
              -> continueIfPossible
            Incomplete index
              | Just lastRelativeSlot <- lastFilledSlot index
              -> return $ Just (EpochSlot epoch lastRelativeSlot, index)
              | otherwise
              -> do
                removeFile (renderFile "epoch" epoch)
                continueIfPossible
            Complete index
              | Just lastRelativeSlot <- lastFilledSlot index
              -> do
                index' <- if
                  | lastSlot index == lastRelativeSlot -> return index
                    -- If the index contains empty trailing slots, truncate
                    -- them.
                  | otherwise                          -> do
                    -- As the epoch will no longer be complete, remove the
                    -- index file.
                    removeFile (renderFile "index" epoch)
                    let newIndexSize = EpochSize . unRelativeSlot
                                     $ succ lastRelativeSlot
                    return $ truncateToSlots newIndexSize index
                return $ Just (EpochSlot epoch lastRelativeSlot, index')
              | otherwise
              -> do
                removeFile (renderFile "epoch" epoch)
                removeFile (renderFile "index" epoch)
                continueIfPossible

    -- | Validate all the epochs using @validateEpoch@, starting from the most
    -- recent (given) epoch.
    --
    -- Starts from the given epoch, if that is invalid or empty, it is
    -- truncated and the epoch before it is validated, and so on.
    --
    -- When a valid non-empty epoch is encountered, the location of the last
    -- valid block or EBB in it is remembered, but validation continues until
    -- all epochs are validated. Epoch 0 will be the last epoch to validate.
    --
    -- The location of the last valid block or EBB that was remembered, along
    -- with the index of the corresponding epoch, is returned. All data before
    -- this location will have been validated.
    --
    -- All data after the last valid block or EBB is truncated.
    validateAllEpochs :: HasCallStack
                      => EpochNo
                      -> m (Maybe (EpochSlot, Index hash))
    validateAllEpochs = go Nothing
      where
        go lastValid epoch = do
          validateRes <- validateEpoch epoch
          let continueIfPossible lastValid'
                | epoch == 0 = return lastValid'
                | otherwise  = go lastValid' (epoch - 1)
          case validateRes of
            Missing -> do
              -- Remove all valid files that may come after it. Note that
              -- 'Invalid' guarantees that there is no epoch or index file for
              -- this epoch.
              removeFilesStartingFrom hasFS (succ epoch)
              continueIfPossible Nothing
            Incomplete index -> do
              case firstFilledSlot index of
                -- If the index is empty, remove the index and epoch file too
                Nothing -> removeFilesStartingFrom hasFS epoch
                Just _  -> removeFilesStartingFrom hasFS (succ epoch)
              let lastValid' = lastFilledSlot index <&> \lastRelativeSlot ->
                    (EpochSlot epoch lastRelativeSlot, index)
              continueIfPossible lastValid'
            Complete index
              | Just _ <- lastValid
                -- If we have a valid epoch after this epoch to start at (and
                -- all epochs in between are also valid), just continue
                -- validating.
              -> continueIfPossible lastValid
              | Just lastRelativeSlot <- lastFilledSlot index
                -- If there are no valid epochs after this one, and this one
                -- is not empty, use it as lastValid
              -> do
                index' <- if
                  | lastSlot index == lastRelativeSlot -> return index
                    -- If the index contains empty trailing slots, truncate
                    -- them.
                  | otherwise                          -> do
                    -- As the epoch will no longer be complete, remove the
                    -- index file.
                    removeFile (renderFile "index" epoch)
                    let newIndexSize = EpochSize . unRelativeSlot
                                     $ succ lastRelativeSlot
                    return $ truncateToSlots newIndexSize index
                continueIfPossible $ Just (EpochSlot epoch lastRelativeSlot, index')
              | otherwise
                -- If there are no valid epochs after this one, and this one
                -- is empty, we can't use it as lastValid, so remove it and
                -- continue.
              -> do
                removeFile (renderFile "epoch" epoch)
                removeFile (renderFile "index" epoch)
                continueIfPossible Nothing

    -- | Validates the epoch and index file of the given epoch.
    --
    -- Reconstructs the index by parsing the epoch file. If there remains
    -- unparsed data, the epoch file is truncated.
    --
    -- If there is no epoch file, the result will be 'Missing'. An empty epoch
    -- file will result in 'Incomplete'.
    --
    -- Reads the index from the index file.
    --
    -- The epoch is 'Complete' when the index file is valid (remember that we
    -- only write index files for complete epochs).
    --
    -- Special case: if the last slot of the epoch is filled, the epoch is
    -- 'Complete' without there having to be a valid index file. As the index
    -- file wouldn't be able to give us more information than the
    -- reconstructed index already gives us, e.g., trailing empty slots. The
    -- index file will be overwritten with the reconstructed index when
    -- invalid or missing.
    --
    -- An invalid index file is deleted when the epoch is 'Incomplete'.
    --
    -- Note that an index file can tell us more than the reconstructed index,
    -- i.e. the presence of trailing empty slots, which we will accept as the
    -- truth.
    validateEpoch :: HasCallStack
                  => EpochNo
                  -> m (ValidateResult hash)
    validateEpoch epoch = do
      epochSize <- epochInfoSize epochInfo epoch

      let indexSize = succ epochSize  -- One extra slot for the EBB
          indexFile = renderFile "index" epoch
          epochFile = renderFile "epoch" epoch

      epochFileExists <- doesFileExist epochFile
      indexFileExists <- doesFileExist indexFile
      if not epochFileExists
        then do
          when indexFileExists $ removeFile indexFile
          return Missing
        else do

          -- Read the epoch file and reconstruct an index from it.
          (reconstructedIndex, mbErr) <- reconstructIndex epochFile
            epochFileParser epochInfo

          -- If there was an error parsing the epoch file, truncate it
          case mbErr of
            -- If there was an error parsing the epoch file, truncate it
            Just _ ->
              withFile hasFS epochFile (AppendMode AllowExisting) $ \eHnd ->
                hTruncate eHnd (lastSlotOffset reconstructedIndex)
            -- If not, check that the last offset matches the epoch file size.
            -- If it does not, it means the 'EpochFileParser' is incorrect. We
            -- can't recover from this.
            Nothing -> do
              epochFileSize <- withFile hasFS epochFile ReadMode hGetSize
              -- TODO assert?
              when (lastSlotOffset reconstructedIndex /= epochFileSize) $
                error $ "EpochFileParser incorrect: expected last offset = " <>
                        show (lastSlotOffset reconstructedIndex) <>
                        ", actual last offset = " <> show epochFileSize

          -- If the last slot of the epoch is filled, we don't need an index
          -- file. We can reconstruct it and don't have to throw an error.
          let lastSlotFilled = indexSlots reconstructedIndex == indexSize
              -- Handle only InvalidFileError and DeserialisationError
              loadErr :: ErrorHandling UnexpectedError m
              loadErr = EH.embed UnexpectedError
                (\case
                  UnexpectedError (e@InvalidFileError {})     -> Just e
                  UnexpectedError (e@DeserialisationError {}) -> Just e
                  _ -> Nothing) err

          if
            | lastSlotFilled -> do
              -- If the last slot of the epoch is filled, we know all we need
              -- to know from the reconstructed index, as there can't be any
              -- trailing empty slots that the reconstructed index will be
              -- unaware of. Write the reconstructed index to disk if needed.
              overwrite <- if indexFileExists
                then do
                  indexFromFileOrError <- EH.try loadErr $
                    loadIndex hashDecoder hasFS err epoch indexSize
                  case indexFromFileOrError of
                    Left _              -> return True
                    Right indexFromFile ->
                      return $ indexFromFile /= reconstructedIndex
                else return True
              when overwrite $
                -- TODO log
                writeIndex hashEncoder hasFS epoch reconstructedIndex
              return $ Complete reconstructedIndex

            | indexFileExists -> do
              indexFromFileOrError <- EH.try loadErr $
                loadIndex hashDecoder hasFS err epoch indexSize
              case indexFromFileOrError of
                Left _              -> return $ Incomplete reconstructedIndex
                Right indexFromFile
                  | reconstructedIndex `isPrefixOf` indexFromFile -> do
                    -- A reconstructed index knows nothing about trailing
                    -- empty slots whereas an index from an index file may be
                    -- aware of trailing empty slots in the epoch.
                    --
                    -- If the index from the index file pads the end of the
                    -- reconstructed index with empty slots so that the epoch
                    -- is full, we accept it, otherwise it is incomplete and
                    -- thus invalid.
                    --
                    -- We don't want an index that ends with empty slots
                    -- unless it is a finalised epoch, as such an index cannot
                    -- be the result of regular operations.
                    let extendedIndex = extendWithTrailingUnfilledSlotsFrom
                          reconstructedIndex indexFromFile
                    if indexSlots extendedIndex /= indexSize ||
                       indexSlots indexFromFile > indexSlots extendedIndex
                      then do
                        removeFile indexFile
                        return $ Incomplete reconstructedIndex
                      else return $ Complete extendedIndex

                  | otherwise -> do
                    -- No prefix: the index file is invalid
                    removeFile indexFile
                    return $ Incomplete reconstructedIndex

            -- No index file, either because it is missing or because the
            -- epoch was not finalised
            | otherwise -> return $ Incomplete reconstructedIndex


-- | Reconstruct an 'Index' from the given epoch file using the
-- 'EpochFileParser'.
--
-- Also returns the error returned by the 'EpochFileParser'.
reconstructIndex :: forall m e hash. MonadThrow m
                 => FsPath
                 -> EpochFileParser e hash m (Word64, SlotNo)
                 -> EpochInfo m
                 -> m (Index hash, Maybe e)
reconstructIndex epochFile epochFileParser epochInfo = do
    (offsetsAndSizesAndSlots, ebbHash, mbErr) <-
      runEpochFileParser epochFileParser epochFile
    offsetsAndSizesAndRelSlots <- case offsetsAndSizesAndSlots of
      [] -> return []
      (offset0, (size0, _slot0)) : offsetsAndSizesAndSlots'
        | Just _ <- ebbHash
          -- If there is an EBB, then the first entry in the list must
          -- correspond to the EBB
        -> ((offset0, (size0, 0)) :) <$> slotsToRelSlots offsetsAndSizesAndSlots'
        | otherwise
        -> slotsToRelSlots offsetsAndSizesAndSlots

    let slotOffsets = reconstructSlotOffsets offsetsAndSizesAndRelSlots
        index       = indexFromSlotOffsets slotOffsets ebbHash
    return (index, mbErr)
  where
    slotsToRelSlots :: [(SlotOffset, (Word64, SlotNo))]
                    -> m [(SlotOffset, (Word64, RelativeSlot))]
    slotsToRelSlots = mapM $ \(offset, (size, slot)) -> do
      relSlot <- _relativeSlot <$> epochInfoBlockRelative epochInfo slot
      return (offset, (size, relSlot))
