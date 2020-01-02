{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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
-- >   00000.epoch
-- >   00000.primary
-- >   00000.secondary
-- >   ..
-- >   00008.epoch
-- >   00008.primary
-- >   00008.secondary
--
-- For each epoch, there are three files on disk:
--
--   * An \"epoch file\" that stores the actual binary blobs. But nothing
--     more, so nothing is stored for empty slots.
--
--   * A \"secondary index file\" that stores information about each block:
--     its hash, the slot number or epoch number in case of an EBB, a checksum
--     of the block, the offset of the block in the epoch file, and more. This
--     index is sparse to save space.
--
--   * A \"primary index file\" that maps slots to offsets in the secondary
--     index file.
module Ouroboros.Storage.ImmutableDB.Impl
  ( withDB
    -- * Internals for testing purposes
  , openDBInternal
  , Internal (..)
  ) where

import           Prelude hiding (truncate)

import           Control.Monad (replicateM_, when)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (StateT (..), get, lift, modify,
                     put)
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>), (<&>))

import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow (bracket, finally)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     releaseAll, withRegistry)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types hiding (allowExisting)
import           Ouroboros.Storage.FS.CRC
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index as Index
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..), BlockSize, HeaderOffset (..),
                     HeaderSize (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.Iterator
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
withDB
  :: forall m h hash e a.
     (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> HashInfo hash
  -> ValidationPolicy
  -> EpochFileParser e m (Secondary.Entry hash) hash
  -> Tracer m (TraceEvent e hash)
  -> Index.CacheConfig
  -> (ImmutableDB hash m -> m a)
  -> m a
withDB hasFS err epochInfo hashInfo valPol parser tracer cacheConfig k =
    withRegistry $ \registry -> bracket (open registry) closeDB k
  where
    open registry = fst <$>
      openDBInternal registry hasFS err epochInfo hashInfo valPol parser tracer
        cacheConfig

{------------------------------------------------------------------------------
  Exposed internals and/or extra functionality for testing purposes
------------------------------------------------------------------------------}

data Internal hash m = Internal
  { -- | Delete everything in the database after 'ImmTip'.
    --
    -- PRECONDITION: 'ImmTip' must correspond to an existing block (unless it
    -- is 'TipGen').
    --
    -- The correctness of open iterators is not guaranteed, they should be
    -- closed before calling this operation.
    --
    -- Throws a 'ClosedDBError' if the database is closed.
    deleteAfter
      :: HasCallStack => ImmTip -> m ()
  }

{------------------------------------------------------------------------------
  ImmutableDB Implementation
------------------------------------------------------------------------------}

mkDBRecord :: (IOLike m, Eq hash, NoUnexpectedThunks hash)
           => ImmutableDBEnv m hash -> ImmutableDB hash m
mkDBRecord dbEnv = ImmutableDB
    { closeDB             = closeDBImpl       dbEnv
    , isOpen              = isOpenImpl        dbEnv
    , reopen              = reopenImpl        dbEnv
    , getTip              = getTipImpl        dbEnv
    , getBlock            = getBlockImpl      dbEnv GetBlock
    , getBlockHeader      = getBlockImpl      dbEnv GetHeader
    , getBlockHash        = getBlockImpl      dbEnv GetHash
    , getEBB              = getEBBImpl        dbEnv GetBlock
    , getEBBHeader        = getEBBImpl        dbEnv GetHeader
    , getEBBHash          = getEBBImpl        dbEnv GetHash
    , getBlockOrEBB       = getBlockOrEBBImpl dbEnv GetBlock
    , getBlockOrEBBHeader = getBlockOrEBBImpl dbEnv GetHeader
    , appendBlock         = appendBlockImpl   dbEnv
    , appendEBB           = appendEBBImpl     dbEnv
    , streamBlocks        = streamImpl        dbEnv Blocks
    , streamHeaders       = streamImpl        dbEnv Headers
    , immutableDBErr      = _dbErr            dbEnv
    }

-- | For testing purposes:
--
-- * Exposes internal via 'Internal'
-- * Non-bracketed, as @quickcheck-state-machine@ doesn't support that.
openDBInternal
  :: forall m h hash e.
     (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
  => ResourceRegistry m  -- ^ The ImmutableDB will be in total control of
                         -- this, not to be used for other resources.
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> HashInfo hash
  -> ValidationPolicy
  -> EpochFileParser e m (Secondary.Entry hash) hash
  -> Tracer m (TraceEvent e hash)
  -> Index.CacheConfig
  -> m (ImmutableDB hash m, Internal hash m)
openDBInternal registry hasFS@HasFS{..} err epochInfo hashInfo valPol parser
               tracer cacheConfig = do
    let validateEnv = ValidateEnv
          { hasFS
          , err
          , epochInfo
          , hashInfo
          , parser
          , tracer
          , registry
          , cacheConfig
          }
    !ost  <- validateAndReopen validateEnv valPol initialIteratorID

    stVar <- newMVar (DbOpen ost)

    let dbEnv = ImmutableDBEnv
          { _dbHasFS           = hasFS
          , _dbErr             = err
          , _dbInternalState   = stVar
          , _dbEpochFileParser = parser
          , _dbEpochInfo       = epochInfo
          , _dbHashInfo        = hashInfo
          , _dbTracer          = tracer
          , _dbRegistry        = registry
          , _dbCacheConfig     = cacheConfig
          }
        db = mkDBRecord dbEnv
        internal = Internal
          { deleteAfter = deleteAfterImpl dbEnv
          }
    return (db, internal)

closeDBImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> m ()
closeDBImpl ImmutableDBEnv {..} = releaseAll _dbRegistry `finally` do
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
        traceWith _dbTracer DBClosed
  where
    HasFS{..} = _dbHasFS

isOpenImpl :: IOLike m => ImmutableDBEnv m hash -> m Bool
isOpenImpl ImmutableDBEnv {..} =
    dbIsOpen <$> readMVar _dbInternalState

reopenImpl
  :: forall m hash. (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
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
            let validateEnv = ValidateEnv
                  { hasFS       = _dbHasFS
                  , err         = _dbErr
                  , epochInfo   = _dbEpochInfo
                  , hashInfo    = _dbHashInfo
                  , parser      = _dbEpochFileParser
                  , tracer      = _dbTracer
                  , registry    = _dbRegistry
                  , cacheConfig = _dbCacheConfig
                  }
            ost <- validateAndReopen validateEnv valPol _closedNextIteratorID
            putMVar _dbInternalState (DbOpen ost)
  where
    HasFS{..} = _dbHasFS

deleteAfterImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> ImmTip
  -> m ()
deleteAfterImpl dbEnv@ImmutableDBEnv { _dbTracer } newTip =
  -- We're not using 'Index' in this function but truncating the index files
  -- directly.
  modifyOpenState dbEnv $ \hasFS@HasFS{..} -> do
    st@OpenState {..} <- get
    currentTipEpochSlot <- lift $ mapM blockOrEBBEpochSlot (forgetHash <$> _currentTip)
    newTipEpochSlot     <- lift $ mapM blockOrEBBEpochSlot newTip

    when (newTipEpochSlot < currentTipEpochSlot) $ do
      !ost <- lift $ do
        traceWith _dbTracer $ DeletingAfter newTip
        -- Release the open handles and terminate the running threads, as we
        -- might have to remove files that are currently opened.
        releaseAll _dbRegistry
        newTipWithHash <- truncateTo hasFS st newTipEpochSlot
        let (newEpoch, allowExisting) = case newTipEpochSlot of
              TipGen                  -> (0, MustBeNew)
              Tip (EpochSlot epoch _) -> (epoch, AllowExisting)
        -- Reset the index, as it can contain stale information. Also restarts
        -- the background thread expiring unused past epochs.
        Index.reset _index newEpoch
        mkOpenState _dbRegistry hasFS _dbErr _index newEpoch _nextIteratorID
          newTipWithHash allowExisting
      put ost
  where
    ImmutableDBEnv { _dbErr, _dbEpochInfo, _dbHashInfo, _dbRegistry } = dbEnv

    -- | The current tip as a 'TipEpochSlot'
    blockOrEBBEpochSlot :: BlockOrEBB -> m EpochSlot
    blockOrEBBEpochSlot = \case
      EBB  epoch -> return (EpochSlot epoch 0)
      Block slot -> epochInfoBlockRelative _dbEpochInfo slot

    truncateTo
      :: HasFS m h
      -> OpenState m hash h
      -> Tip EpochSlot
      -> m (ImmTipWithHash hash)
    truncateTo hasFS OpenState {} = \case
      TipGen                        ->
        removeFilesStartingFrom hasFS 0 $> TipGen
      Tip (EpochSlot epoch relSlot) -> do
        removeFilesStartingFrom hasFS (epoch + 1)

        -- Retrieve the needed info from the primary index file and then
        -- truncate it.
        primaryIndex <- Primary.load hasFS _dbErr epoch
        Primary.truncateToSlotFS hasFS epoch relSlot
        let lastSecondaryOffset = Primary.offsetOfSlot primaryIndex relSlot
            isEBB | relSlot == 0 = IsEBB
                  | otherwise    = IsNotEBB

        -- Retrieve the needed info from the secondary index file and then
        -- truncate it.
        (entry, blockSize) <- Secondary.readEntry hasFS _dbErr _dbHashInfo
          epoch isEBB lastSecondaryOffset
        Secondary.truncateToEntry hasFS _dbHashInfo epoch lastSecondaryOffset

        -- Truncate the epoch file.
        case blockSize of
          -- The block is the last block in the epoch file, so no need to
          -- truncate
          Secondary.LastEntry      -> return ()
          Secondary.BlockSize size ->
              withFile hasFS epochFile (AppendMode AllowExisting) $ \eHnd ->
                hTruncate hasFS eHnd offset
            where
              epochFile = renderFile "epoch" epoch
              offset    = unBlockOffset (Secondary.blockOffset entry)
                        + fromIntegral size

        return $ WithHash (Secondary.headerHash entry) <$> newTip

getTipImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> m (ImmTipWithHash hash)
getTipImpl dbEnv = do
    SomePair _hasFS OpenState { _currentTip } <- getOpenState dbEnv
    return _currentTip

-- | Whether to read the whole block, its header, or the hash
data BlockComponent hash res where
  GetBlock  :: BlockComponent hash (hash, ByteString)
  GetHeader :: BlockComponent hash (hash, ByteString)
  GetHash   :: BlockComponent hash hash

getBlockImpl
  :: forall m hash res. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> BlockComponent hash res
  -> SlotNo
  -> m (Maybe res)
getBlockImpl dbEnv blockComponent slot =
    withOpenState dbEnv $ \_dbHasFS OpenState{..} -> do
      inTheFuture <- case forgetHash <$> _currentTip of
        TipGen                 -> return $ True
        Tip (Block lastSlot')  -> return $ slot > lastSlot'
        -- The slot (that's pointing to a regular block) corresponding to this
        -- EBB will be empty, as the EBB is the last thing in the database. So
        -- if @slot@ is equal to this slot, it is also referring to the future.
        Tip (EBB lastEBBEpoch) -> do
          ebbSlot <- epochInfoAbsolute _dbEpochInfo (EpochSlot lastEBBEpoch 0)
          return $ slot >= ebbSlot

      when inTheFuture $
        throwUserError _dbErr $
          ReadFutureSlotError slot (forgetHash <$> _currentTip)

      let curEpochInfo = CurrentEpochInfo _currentEpoch _currentEpochOffset
      epochSlot <- epochInfoBlockRelative _dbEpochInfo slot
      getEpochSlot _dbHasFS _dbErr _index curEpochInfo blockComponent epochSlot
  where
    ImmutableDBEnv { _dbEpochInfo, _dbErr, _dbHashInfo } = dbEnv

getEBBImpl
  :: forall m hash res. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> BlockComponent hash res
  -> EpochNo
  -> m (Maybe res)
getEBBImpl dbEnv blockComponent epoch =
    withOpenState dbEnv $ \_dbHasFS OpenState{..} -> do
      let inTheFuture = case forgetHash <$> _currentTip of
            TipGen        -> True
            Tip (Block _) -> epoch > _currentEpoch
            Tip (EBB _)   -> epoch > _currentEpoch

      when inTheFuture $
        throwUserError _dbErr $ ReadFutureEBBError epoch _currentEpoch

      let curEpochInfo = CurrentEpochInfo _currentEpoch _currentEpochOffset
      getEpochSlot _dbHasFS _dbErr _index curEpochInfo blockComponent
        (EpochSlot epoch 0)
  where
    ImmutableDBEnv { _dbEpochInfo, _dbErr, _dbHashInfo } = dbEnv

getBlockComponent
  :: forall m h hash res. (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> CurrentEpochInfo
  -> (Secondary.Entry hash, BlockSize)
  -> BlockComponent hash res
  -> m res
getBlockComponent hasFS err epoch curEpochInfo (entry, blockSize) = \case
    GetHash  -> return headerHash

    -- In case the requested epoch is the current epoch, we will be reading
    -- from the epoch file while we're also writing to it. Are we guaranteed
    -- to read what have written? Duncan says: this is guaranteed at the OS
    -- level (POSIX), but not for Haskell handles, which might perform other
    -- buffering. However, the 'HasFS' implementation we're using uses POSIX
    -- file handles ("Ouroboros.Storage.IO") so we're safe (other
    -- implementations of the 'HasFS' API guarantee this too).

    GetBlock -> do
      -- Get the whole block
      let offset = AbsOffset $ unBlockOffset blockOffset
      (bl, checksum') <- withFile hasFS epochFile ReadMode $ \eHnd ->
        case blockSize of
          -- It is the last entry in the file, so we don't know the size
          -- of the block.
          Secondary.LastEntry
            | epoch == curEpoch
              -- Even though it was the last block in the secondary
              -- index file (and thus in the epoch file) when we read
              -- the secondary index file, it is possible that more
              -- blocks have been appended in the meantime. For this
              -- reason, we cannot simply read the until the end of the
              -- epoch file, because we would read the newly appended
              -- blocks too.
              --
              -- Instead, we derive the size of the block from
              -- @curEpochOffset@, which corresponds to the qoffset at
              -- the end of that block /at the time we read the state/.
              -- Note that we don't allow reading a block newer than the
              -- tip, which we obtained from the /same state/.
            -> let size = curEpochOffset - blockOffset in
               hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
            | otherwise
              -- If it is in an epoch in the past, it is immutable,
              -- so no blocks can have been appended since we retrieved
              -- the entry. We can simply read all remaining bytes, as
              -- it is the last block in the file.
            -> hGetAllAtCRC     hasFS eHnd                     offset
          Secondary.BlockSize size
            -> hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
      checkChecksum err epochFile blockOrEBB checksum checksum'
      return (headerHash, bl)

    GetHeader -> fmap (headerHash,) $
        -- Get just the header
        withFile hasFS epochFile ReadMode $ \eHnd ->
          -- We cannot check the checksum in this case, as we're not reading
          -- the whole block
          hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ unHeaderSize headerSize
        offset = AbsOffset $
          unBlockOffset blockOffset +
          fromIntegral (unHeaderOffset headerOffset)
  where
    Secondary.Entry
      { blockOffset, headerOffset, headerSize, headerHash, checksum
      , blockOrEBB
      } = entry
    CurrentEpochInfo curEpoch curEpochOffset = curEpochInfo
    epochFile = renderFile "epoch" epoch

getBlockOrEBBImpl
  :: forall m hash. (HasCallStack, IOLike m, Eq hash)
  => ImmutableDBEnv m hash
  -> BlockComponent hash (hash, ByteString)
  -> SlotNo
  -> hash
  -> m (Maybe (Either EpochNo SlotNo, ByteString))
getBlockOrEBBImpl dbEnv blockComponent slot hash =
    withOpenState dbEnv $ \_dbHasFS OpenState{..} -> do

      inTheFuture <- case forgetHash <$> _currentTip of
        TipGen                 -> return True
        Tip (Block lastSlot)   -> return $ slot > lastSlot
        Tip (EBB lastEBBEpoch) -> do
          ebbSlot <- epochInfoFirst _dbEpochInfo lastEBBEpoch
          return $ slot > ebbSlot

      when inTheFuture $
        throwUserError _dbErr $ ReadFutureSlotError slot (forgetHash <$> _currentTip)

      let curEpochInfo = CurrentEpochInfo _currentEpoch _currentEpochOffset

      errOrRes <- runExceptT $
        getSlotInfo _dbEpochInfo _index (slot, hash)
      case errOrRes of
        Left _ ->
          return Nothing
        Right (EpochSlot epoch relSlot, (entry, blockSize), _secondaryOffset) ->
            Just . (epochOrSlot, ) . snd <$>
              getBlockComponent _dbHasFS _dbErr epoch curEpochInfo
                (entry, blockSize) blockComponent
          where
            epochOrSlot | relSlot == 0 = Left epoch
                        | otherwise    = Right slot
  where
    ImmutableDBEnv { _dbEpochInfo, _dbErr, _dbHashInfo } = dbEnv

-- | Get the block, header, or hash (depending on 'BlockComponent') corresponding to
-- the given 'EpochSlot'.
--
-- Preconditions: the given 'EpochSlot' is in the past.
getEpochSlot
  :: forall m h hash res. (HasCallStack, IOLike m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> Index m hash h
  -> CurrentEpochInfo
  -> BlockComponent hash res
  -> EpochSlot
  -> m (Maybe res)
getEpochSlot hasFS err index curEpochInfo blockComponent epochSlot =
    -- Check the primary index first
    Index.readOffset index epoch relativeSlot >>= \case
      -- Empty slot
      Nothing              -> return Nothing
      -- Filled slot; read the corresponding entry from the sparse secondary
      -- index
      Just secondaryOffset -> do
        -- TODO only read the hash in case of 'GetHash'?
        (entry, blockSize) <- Index.readEntry index epoch isEBB secondaryOffset
        Just <$>
          getBlockComponent hasFS err epoch curEpochInfo (entry, blockSize)
            blockComponent
  where
    EpochSlot epoch relativeSlot = epochSlot
    isEBB | relativeSlot == 0    = IsEBB
          | otherwise            = IsNotEBB

appendBlockImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> SlotNo
  -> hash
  -> BinaryInfo Builder
  -> m ()
appendBlockImpl dbEnv slot headerHash binaryInfo =
    modifyOpenState dbEnv $ \_dbHasFS@HasFS{..} -> do
      OpenState { _currentEpoch, _currentTip, _index } <- get

      epochSlot@(EpochSlot epoch _) <- lift $
        epochInfoBlockRelative _dbEpochInfo slot

      -- Check that we're not appending to the past
      let inThePast = case forgetHash <$> _currentTip of
            Tip (Block lastSlot)   -> slot  <= lastSlot
            Tip (EBB lastEBBEpoch) -> epoch <  lastEBBEpoch
            TipGen                 -> False

      when inThePast $ lift $
        throwUserError _dbErr $
          AppendToSlotInThePastError slot (forgetHash <$> _currentTip)

      appendEpochSlot _dbRegistry _dbHasFS _dbErr _dbEpochInfo _index
        epochSlot (Block slot) headerHash binaryInfo
  where
    ImmutableDBEnv { _dbEpochInfo, _dbErr, _dbHashInfo, _dbRegistry } = dbEnv

appendEBBImpl
  :: forall m hash. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> EpochNo
  -> hash
  -> BinaryInfo Builder
  -> m ()
appendEBBImpl dbEnv epoch headerHash binaryInfo =
    modifyOpenState dbEnv $ \_dbHasFS@HasFS{..} -> do
      OpenState { _currentEpoch, _currentTip, _index } <- get

      -- Check that we're not appending to the past
      let inThePast = case forgetHash <$> _currentTip of
            -- There is already a block in this epoch, so the EBB can no
            -- longer be appended in this epoch
            Tip (Block _) -> epoch <= _currentEpoch
            -- There is already an EBB in this epoch
            Tip (EBB _)   -> epoch <= _currentEpoch
            TipGen        -> False

      when inThePast $ lift $ throwUserError _dbErr $
        AppendToEBBInThePastError epoch _currentEpoch

      appendEpochSlot _dbRegistry _dbHasFS _dbErr _dbEpochInfo _index
        (EpochSlot epoch 0) (EBB epoch) headerHash binaryInfo
  where
    ImmutableDBEnv { _dbEpochInfo, _dbErr, _dbHashInfo, _dbRegistry } = dbEnv

appendEpochSlot
  :: forall m h hash. (HasCallStack, IOLike m)
  => ResourceRegistry m
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> Index m hash h
  -> EpochSlot  -- ^ The 'EpochSlot' of the new block or EBB
  -> BlockOrEBB -- ^ Corresponds to the new block, will be installed as the
                -- new tip
  -> hash
  -> BinaryInfo Builder
  -> StateT (OpenState m hash h) m ()
appendEpochSlot registry hasFS err epochInfo index epochSlot blockOrEBB headerHash
                BinaryInfo { binaryBlob, headerOffset, headerSize } = do
    OpenState { _currentEpoch = initialEpoch } <- get

    -- If the slot is in an epoch > the current one, we have to finalise the
    -- current one and start a new epoch file, possibly skipping some
    -- epochs.
    when (epoch > initialEpoch) $ do
      let newEpochsToStart :: Int
          newEpochsToStart = fromIntegral . unEpochNo $ epoch - initialEpoch
      replicateM_ newEpochsToStart (startNewEpoch registry hasFS err index epochInfo)

    -- We may have updated the state with 'startNewEpoch', so get the
    -- (possibly) updated state, but first remember the current epoch
    OpenState {..} <- get

    -- Compute the next empty slot @m@, if we need to write to slot @n@, we
    -- will need to backfill @n - m@ slots.
    nextFreeRelSlot <- lift $
        if epoch > initialEpoch
          -- If we had to start a new epoch, we start with slot 0. Note that
          -- in this case the _currentTip will refer to something in an epoch
          -- before _currentEpoch.
          then return 0
          else case forgetHash <$> _currentTip of
            TipGen               -> return 0
            Tip (EBB _ebb)       -> return 1
            Tip (Block lastSlot) -> succ . _relativeSlot <$>
              epochInfoBlockRelative epochInfo lastSlot

    -- Append to the end of the epoch file.
    (blockSize, entrySize) <- lift $ do

        -- Write to the epoch file
        (blockSize, crc) <- hPutCRC hasFS _currentEpochHandle binaryBlob

        -- Write to the secondary index file
        let entry = Secondary.Entry
              { blockOffset  = _currentEpochOffset
              , headerOffset = HeaderOffset headerOffset
              , headerSize   = HeaderSize headerSize
              , checksum     = crc
              , headerHash   = headerHash
              , blockOrEBB   = blockOrEBB
              }
        entrySize <- fromIntegral <$> Index.appendEntry index epoch
          _currentSecondaryHandle (WithBlockSize (fromIntegral blockSize) entry)

        -- Write to the primary index file
        let backfillOffsets = Primary.backfill
              relSlot nextFreeRelSlot _currentSecondaryOffset
            offsets = backfillOffsets <> [_currentSecondaryOffset + entrySize]
        Index.appendOffsets index _currentPrimaryHandle offsets

        return (blockSize, entrySize)

    modify $ \st -> st
      { _currentEpochOffset     = _currentEpochOffset + fromIntegral blockSize
      , _currentSecondaryOffset = _currentSecondaryOffset + entrySize
      , _currentTip             = Tip (WithHash headerHash blockOrEBB)
      }
  where
    EpochSlot epoch relSlot = epochSlot

startNewEpoch
  :: forall m h hash. (HasCallStack, IOLike m)
  => ResourceRegistry m
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> Index m hash h
  -> EpochInfo m
  -> StateT (OpenState m hash h) m ()
startNewEpoch registry hasFS@HasFS{..} err index epochInfo = do
    st@OpenState {..} <- get

    -- Find out the size of the current epoch, so we can pad the primary
    -- index.
    epochSize <- lift $ epochInfoSize epochInfo _currentEpoch

    -- We have to take care when starting multiple new epochs in a row. In the
    -- first call the tip will be in the current epoch, but in subsequent
    -- calls, the tip will still be in an epoch in the past, not the
    -- '_currentEpoch'. In that case, we can't use the relative slot of the
    -- tip, since it will point to a relative slot in a past epoch. So when
    -- the current (empty) epoch is not the epoch containing the tip, we use
    -- relative slot 0 to calculate how much to pad.
    nextFreeRelSlot <- lift $ case forgetHash <$> _currentTip of
      TipGen                     -> return 0
      Tip (EBB epoch)
        | epoch == _currentEpoch -> return 1
          -- The @_currentEpoch > epoch@: we're in an empty epoch and the tip
          -- was an EBB of an older epoch. So the first relative slot of this
          -- epoch is empty
        | otherwise              -> return 0
      Tip (Block lastSlot)       ->
        epochInfoBlockRelative epochInfo lastSlot <&> \(EpochSlot epoch relSlot) ->
          if epoch == _currentEpoch then succ relSlot else 0

    let backfillOffsets = Primary.backfillEpoch epochSize nextFreeRelSlot
          _currentSecondaryOffset

    lift $
      Index.appendOffsets index _currentPrimaryHandle backfillOffsets
      `finally` closeOpenStateHandles hasFS st

    st' <- lift $ mkOpenState registry hasFS err index (succ _currentEpoch)
      _nextIteratorID _currentTip MustBeNew

    put st'
