{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Control.Monad (replicateM_, when)
import           Control.Monad.State.Strict (StateT (..), get, lift, modify,
                     put)
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Functor (($>))
import           Data.Word

import           GHC.Stack (HasCallStack, callStack)

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index
import           Ouroboros.Storage.ImmutableDB.Impl.Iterator
import           Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets
                     (SlotOffsets (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets as SlotOffsets
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Impl.Validation
import           Ouroboros.Storage.ImmutableDB.Layout

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
  :: (HasCallStack, IOLike m, Eq hash)
  => (forall s . Decoder s hash)
  -> (hash -> Encoding)
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> ValidationPolicy
  -> EpochFileParser e hash m (Word64, SlotNo)
  -> Tracer m (TraceEvent e)
  -> m (ImmutableDB hash m)
openDB = openDBImpl

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

mkDBRecord :: (IOLike m, Eq hash)
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
              (HasCallStack, IOLike m, Eq hash)
           => (forall s . Decoder s hash)
           -> (hash -> Encoding)
           -> HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> EpochInfo m
           -> ValidationPolicy
           -> EpochFileParser e hash m (Word64, SlotNo)
           -> Tracer m (TraceEvent e)
           -> m (ImmutableDB hash m)
openDBImpl hashDecoder hashEncoder hasFS@HasFS{..} err epochInfo valPol parser tracer = do
    !ost  <- validateAndReopen hashDecoder hashEncoder hasFS err epochInfo
      valPol parser initialIteratorID tracer

    stVar <- newMVar (DbOpen ost)

    let dbEnv = ImmutableDBEnv hasFS err stVar parser hashDecoder hashEncoder tracer
        db    = mkDBRecord dbEnv
    return db


closeDBImpl :: (HasCallStack, IOLike m)
            => ImmutableDBEnv m hash
            -> m ()
closeDBImpl ImmutableDBEnv {..} = do
    internalState <- takeMVar _dbInternalState
    case internalState of
      -- Already closed
      DbClosed  _ -> do
        traceWith _dbTracer $ DBAlreadyClosed
        putMVar _dbInternalState internalState
      DbOpen OpenState {..} -> do
        let !closedState = closedStateFromInternalState internalState
        -- Close the database before doing the file-system operations so that
        -- in case these fail, we don't leave the database open.
        putMVar _dbInternalState (DbClosed closedState)
        hClose _currentEpochWriteHandle
        traceWith _dbTracer DBClosed
  where
    HasFS{..} = _dbHasFS

isOpenImpl :: IOLike m => ImmutableDBEnv m hash -> m Bool
isOpenImpl ImmutableDBEnv {..} =
    dbIsOpen <$> readMVar _dbInternalState

reopenImpl :: forall m hash.
              (HasCallStack, IOLike m, Eq hash)
           => ImmutableDBEnv m hash
           -> ValidationPolicy
           -> m ()
reopenImpl ImmutableDBEnv {..} valPol = do
    internalState <- takeMVar _dbInternalState
    case internalState of
      -- When still open,
      DbOpen _ -> do
        putMVar _dbInternalState internalState
        throwUserError _dbErr OpenDBError

      -- Closed, so we can try to reopen
      DbClosed ClosedState {..} ->
        -- Important: put back the state when an error is thrown, otherwise we
        -- have an empty TMVar.
        onException hasFsErr _dbErr
          (putMVar _dbInternalState internalState) $ do

            ost <- validateAndReopen _dbHashDecoder _dbHashEncoder _dbHasFS
              _dbErr _closedEpochInfo valPol _dbEpochFileParser
              _closedNextIteratorID _dbTracer

            putMVar _dbInternalState (DbOpen ost)
  where
    HasFS{..} = _dbHasFS

-- TODO close all iterators
deleteAfterImpl :: forall m hash.
                   (HasCallStack, IOLike m)
                => ImmutableDBEnv m hash
                -> ImmTip
                -> m ()
deleteAfterImpl dbEnv@ImmutableDBEnv { _dbErr, _dbTracer } tip =
  modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do
    st@OpenState {..} <- get
    currentEpochSlot <- lift $ tipToEpochSlot _epochInfo _currentTip
    newEpochSlot     <- lift $ tipToEpochSlot _epochInfo tip

    lift $ traceWith _dbTracer $ DeletingAfter currentEpochSlot newEpochSlot

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
    -- | The current tip as a 'TipEpochSlot'
    tipToEpochSlot :: EpochInfo m -> ImmTip -> m TipEpochSlot
    tipToEpochSlot epochInfo currentTip = case currentTip of
      TipGen           -> return $ TipEpochSlotGenesis
      Tip (EBB  epoch) -> return $ TipEpochSlot (EpochSlot epoch 0)
      Tip (Block slot) -> TipEpochSlot <$> epochInfoBlockRelative epochInfo slot

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

getTipImpl :: (HasCallStack, IOLike m)
           => ImmutableDBEnv m hash
           -> m ImmTip
getTipImpl dbEnv = do
    SomePair _hasFS OpenState { _currentTip } <- getOpenState dbEnv
    return _currentTip

getBinaryBlobImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> SlotNo
  -> m (Maybe ByteString)
getBinaryBlobImpl dbEnv slot = withOpenState dbEnv $ \_dbHasFS st@OpenState{..} -> do
    inTheFuture <- case _currentTip of
          TipGen                  -> return $ True
          Tip (Block lastSlot')   -> return $ slot > lastSlot'
          -- The slot (that pointing to a regular block) corresponding to this
          -- EBB will be empty, as the EBB is the last thing in the database.
          -- So if @slot@ is equal to this slot, it is also refering to the
          -- future.
          Tip (EBB lastEBBEpoch)  -> do
            ebbSlot <- epochInfoAbsolute _epochInfo (EpochSlot lastEBBEpoch 0)
            return $ slot >= ebbSlot

    when inTheFuture $
      throwUserError _dbErr $ ReadFutureSlotError slot _currentTip

    epochSlot <- epochInfoBlockRelative _epochInfo slot
    snd <$> getEpochSlot _dbHasFS (_dbHashDecoder dbEnv) st _dbErr epochSlot
  where
    ImmutableDBEnv { _dbErr } = dbEnv

getEBBImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> EpochNo
  -> m (Maybe (hash, ByteString))
getEBBImpl dbEnv epoch = withOpenState dbEnv $ \_dbHasFS st@OpenState{..} -> do
    let inTheFuture = case _currentTip of
          TipGen        -> True
          Tip (Block _) -> epoch > _currentEpoch
          Tip (EBB _)   -> epoch > _currentEpoch

    when inTheFuture $
      throwUserError _dbErr $ ReadFutureEBBError epoch _currentEpoch

    (mbEBBHash, mbBlob) <- getEpochSlot _dbHasFS (_dbHashDecoder dbEnv) st _dbErr (EpochSlot epoch 0)
    return $ (,) <$> getCurrentEBB mbEBBHash <*> mbBlob
  where
    ImmutableDBEnv { _dbErr } = dbEnv

-- Preconditions: the given 'EpochSlot' is in the past.
getEpochSlot
  :: forall m hash h. (HasCallStack, IOLike m)
  => HasFS m h
  -> (forall s . Decoder s hash)
  -> OpenState m hash h
  -> ErrorHandling ImmutableDBError m
  -> EpochSlot
  -> m (CurrentEBB hash, Maybe ByteString)
getEpochSlot _dbHasFS hashDecoder OpenState {..} _dbErr epochSlot = do
    let epochFile = renderFile "epoch" epoch
        indexFile = renderFile "index" epoch

    lastRelativeSlot <- case _currentTip of
      TipGen           -> error "Postcondition violated: EpochSlot must be in the past"
      Tip (EBB  _)     -> return 0
      Tip (Block slot) -> _relativeSlot <$> epochInfoBlockRelative _epochInfo slot

    (blobOffset, blobSize, mbEBBHash) <- case epoch == _currentEpoch of
      -- If the requested epoch is the current epoch, the offsets are still in
      -- memory
      True -> assert (lastRelativeSlot >= relativeSlot) $
        case SlotOffsets.drop toDrop _currentEpochOffsets of
            (_ `Snoc` offset) `Snoc` offsetAfter ->
              return (offset, offsetAfter - offset, _currentEBBHash)
            First `Snoc` offsetAfter ->
              return (0, offsetAfter, _currentEBBHash)
            First ->
              -- We requested the EBB, but no EBB has been written yet.
              return $ assert (relativeSlot == 0) (0, 0, NoCurrentEBB)
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
        -- Compute the offset on disk and the blob size.
        let nbBytesToGet = fromIntegral indexEntrySizeBytes * 2
        -- Note the use of hGetExactlyAt: we must get enough bytes from the
        -- index file, otherwise 'decodeIndexEntry' (and its variant) would
        -- fail.
        bytes <- toStrict <$> hGetExactlyAt _dbHasFS iHnd nbBytesToGet
                                                          indexSeekPosition
        let !start = decodeIndexEntry   bytes
            !end   = decodeIndexEntryAt indexEntrySizeBytes bytes

        mbEBBHash <- if relativeSlot == 0 && end > start
          then do
            -- TODO: This case seems to never appear in tests.
            -- Seek till after the offsets so we can read the hash
            epochSize <- epochInfoSize _epochInfo epoch
            let hashOffset = (fromIntegral epochSize + 2) * indexEntrySizeBytes
            deserialiseHash' =<< hGetAllAt _dbHasFS iHnd (fromIntegral hashOffset)
          else return NoCurrentEBB

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
        -- Read from the epoch file
        Just <$> hGetExactlyAt _dbHasFS eHnd (fromIntegral blobSize)
                                             (fromIntegral blobOffset)

    return (mbEBBHash, mbBlob)
  where
    HasFS{..}                    = _dbHasFS
    EpochSlot epoch relativeSlot = epochSlot

    deserialiseHash' :: HasCallStack => ByteString -> m (CurrentEBB hash)
    deserialiseHash' bs = case deserialiseHash hashDecoder bs of
      Right (_, hash) -> return hash
      Left df         -> throwUnexpectedError _dbErr $
        DeserialisationError df callStack


appendBinaryBlobImpl :: forall m hash.
                        (HasCallStack, IOLike m)
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
            Tip (Block lastSlot')  -> slot  <= lastSlot'
            Tip (EBB lastEBBEpoch) -> epoch <  lastEBBEpoch
            TipGen                 -> False

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
                   Tip (EBB _ebb)        -> return $ 1
                   Tip (Block lastSlot') -> succ . _relativeSlot <$>
                     epochInfoBlockRelative _epochInfo lastSlot'
      let lastEpochOffset = SlotOffsets.last _currentEpochOffsets
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
           (_currentEpochOffsets `SlotOffsets.append` backfillOffsets)
             `Snoc` newOffset
        , _currentTip = Tip (Block slot)
        }

startNewEpoch :: (HasCallStack, IOLike m)
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
        if SlotOffsets.isFirst _currentEpochOffsets
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
                 Tip (EBB _ebb)        -> return $ 1
                 Tip (Block lastSlot') -> succ . _relativeSlot <$>
                   epochInfoBlockRelative _epochInfo lastSlot'

    -- The above calls may have modified the _cumulEpochSizes, so get it
    -- again.
    OpenState {..} <- get
    let lastEpochOffset = SlotOffsets.last _currentEpochOffsets
        -- The last relative slot in the file
        lastRelSlot     = maxRelativeSlot epochSize
        backfillOffsets =
          indexBackfill (succ lastRelSlot) nextFreeRelSlot lastEpochOffset
        -- Prepend the backfillOffsets to the current offsets to get a
        -- non-empty list of all the offsets. Note that this list is stored in
        -- reverse order.
        allOffsets = _currentEpochOffsets `SlotOffsets.append` backfillOffsets

    -- Now write the offsets and the EBB hash to the index file
    lift $ SlotOffsets.write hashEncoder hasFS _currentEpoch allOffsets _currentEBBHash

    st <- lift $ mkOpenStateNewEpoch hasFS (succ _currentEpoch) _epochInfo
      _nextIteratorID _currentTip

    put st

appendEBBImpl :: forall m hash.
                 (HasCallStack, IOLike m)
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
            Tip (Block _) -> epoch <= _currentEpoch
            -- There is already an EBB in this epoch
            Tip (EBB _)   -> epoch <= _currentEpoch
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
        { _currentEpochOffsets = _currentEpochOffsets `Snoc` newOffset
        , _currentEBBHash      = CurrentEBB hash
        , _currentTip          = Tip (EBB epoch)
        }
