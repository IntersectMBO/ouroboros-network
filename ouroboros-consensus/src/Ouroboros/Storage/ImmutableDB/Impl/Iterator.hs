{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Storage.ImmutableDB.Impl.Iterator
  ( streamImpl
  , getSlotInfo
  , BlocksOrHeaders (..)
  , CurrentEpochInfo (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.State.Strict (state)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (find)
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isNothing)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks, forceElemsToWHNF)

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.CRC
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..), BlockSize (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}

-- | Internal handle to an iterator
data IteratorHandle hash m = forall h. IteratorHandle
  { itHasFS   :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , itState   :: !(StrictTVar m (IteratorStateOrExhausted hash h))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , itEnd     :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  , itEndHash :: !hash
    -- ^ The @hash@ of the last block the iterator should return.
  }

data IteratorStateOrExhausted hash h =
    IteratorStateOpen !(IteratorState hash h)
  | IteratorStateExhausted
  deriving (Generic, NoUnexpectedThunks)

-- Existential type; we can't use generics
instance ( forall a. NoUnexpectedThunks (StrictTVar m a)
         , NoUnexpectedThunks hash
         ) => NoUnexpectedThunks (IteratorHandle hash m) where
  showTypeOf _ = "IteratorHandle"
  whnfNoUnexpectedThunks ctxt IteratorHandle{..} =
      allNoUnexpectedThunks [
          noUnexpectedThunks ctxt itHasFS
        , noUnexpectedThunks ctxt itState
        , noUnexpectedThunks ctxt itEnd
        , noUnexpectedThunks ctxt itEndHash
        ]

data IteratorState hash h = IteratorState
  { itEpoch        :: !EpochNo
    -- ^ The current epoch the iterator is streaming from.
  , itEpochHandle  :: !(Handle h)
    -- ^ A handle to the epoch file corresponding with 'itEpoch'.
  , itEpochEntries :: !(NonEmpty (WithBlockSize (Secondary.Entry hash)))
    -- ^ The entries from the secondary index corresponding to the current
    -- epoch. The first entry in the list is the next one to stream.
    --
    -- Invariant: all the entries in this list must be included in the stream.
    -- In other words, entries corresponding to blocks after the end bound are
    -- not included in this list.
  }
  deriving (Generic, NoUnexpectedThunks)

-- | Used by 'streamImpl' to choose between streaming blocks or headers.
data BlocksOrHeaders = Blocks | Headers

-- | Auxiliary data type that combines the '_currentEpoch' and
-- '_currentEpochOffset' fields from 'OpenState'. This is used to avoid
-- passing the whole state around, and moreover, it avoids issues with
-- existential @h@ type parameter.
data CurrentEpochInfo = CurrentEpochInfo !EpochNo !BlockOffset

streamImpl
  :: forall m hash. (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
  => ImmutableDBEnv m hash
  -> BlocksOrHeaders
  -> Maybe (SlotNo, hash)
     -- ^ When to start streaming (inclusive).
  -> Maybe (SlotNo, hash)
     -- ^ When to stop streaming (inclusive).
  -> m (Either (WrongBoundError hash)
               (Iterator hash m ByteString))
streamImpl dbEnv blocksOrHeaders mbStart mbEnd =
    withOpenState dbEnv $ \hasFS OpenState{..} -> runExceptT $ do
      lift $ validateIteratorRange _dbErr _dbEpochInfo
        (forgetHash <$> _currentTip) mbStart mbEnd

      -- TODO cache index files: we might open the same primary and secondary
      -- indices to validate the end bound as for the start bound

      case _currentTip of
        TipGen ->
          -- If any of the two bounds were specified, 'validateIteratorRange'
          -- would have thrown a 'ReadFutureSlotError'.
          assert (isNothing mbStart && isNothing mbEnd) $ lift mkEmptyIterator
        Tip tip -> do
          WithHash endHash endEpochSlot <- fillInEndBound   hasFS tip mbEnd
          (secondaryOffset, start)      <- fillInStartBound hasFS     mbStart

          lift $ do
            -- 'validateIteratorRange' will catch nearly all invalid ranges,
            -- except for one: streaming from the regular block to the EBB in
            -- the same slot. The EBB comes before the regular block, so these
            -- bounds are invalid. However, to distinguish the EBB from the
            -- regular block, as both have the same slot number, we need to
            -- look at the hashes. 'validateIteratorRange' doesn't have enough
            -- information to do that.
            let WithHash _startHash startEpochSlot = start
            when (startEpochSlot > endEpochSlot) $ do
              startSlot <- epochInfoAbsolute _dbEpochInfo startEpochSlot
              endSlot   <- epochInfoAbsolute _dbEpochInfo endEpochSlot
              throwUserError _dbErr $ InvalidIteratorRangeError startSlot endSlot

            let EpochSlot startEpoch startRelSlot = startEpochSlot
                startIsEBB | startRelSlot == 0 = IsEBB
                           | otherwise         = IsNotEBB
                curEpochInfo = CurrentEpochInfo _currentEpoch _currentEpochOffset

            -- TODO avoid rereading the indices of the start epoch. We read
            -- from both the primary and secondary index in 'fillInStartBound'

            iteratorState <- iteratorStateForEpoch hasFS _dbErr _dbHashInfo
              curEpochInfo endHash startEpoch secondaryOffset startIsEBB

            varIteratorState <- newTVarM $ IteratorStateOpen iteratorState

            mkIterator IteratorHandle
              { itHasFS   = hasFS
              , itState   = varIteratorState
              , itEnd     = endEpochSlot
              , itEndHash = endHash
              }
  where
    ImmutableDBEnv { _dbErr, _dbEpochInfo, _dbHashInfo } = dbEnv

    -- | Fill in the end bound: if 'Nothing', use the current tip. Otherwise,
    -- check whether the bound exists in the database and return the
    -- corresponding 'EpochSlot'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInEndBound
      :: HasCallStack
      => HasFS m h
      -> WithHash hash BlockOrEBB  -- ^ Current tip
      -> Maybe (SlotNo, hash)      -- ^ End bound
      -> ExceptT (WrongBoundError hash) m (WithHash hash EpochSlot)
    fillInEndBound hasFS currentTip = \case
      -- End bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'EpochSlot' accordingly.
      Just end -> do
        (epochSlot, (entry, _blockSize), _secondaryOffset) <-
          getSlotInfo hasFS _dbErr _dbEpochInfo _dbHashInfo end
        return (WithHash (Secondary.headerHash entry) epochSlot)

      -- No end bound given, use the current tip, but convert the 'BlockOrEBB'
      -- to an 'EpochSlot'.
      Nothing  -> lift $ forM currentTip $ \case
        EBB epoch      -> return (EpochSlot epoch 0)
        Block lastSlot -> epochInfoBlockRelative _dbEpochInfo lastSlot

    -- | Fill in the start bound: if 'Nothing', use the first block in the
    -- database. Otherwise, check whether the bound exists in the database and
    -- return the corresponding 'EpochSlot' and 'SecondaryOffset'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInStartBound
      :: HasCallStack
      => HasFS m h
      -> Maybe (SlotNo, hash)  -- ^ Start bound
      -> ExceptT (WrongBoundError hash)
                  m
                  (SecondaryOffset, WithHash hash EpochSlot)
    fillInStartBound hasFS = \case
      -- Start bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'EpochSlot' accordingly.
      Just start -> do
        (epochSlot, (entry, _blockSize), secondaryOffset) <-
          getSlotInfo hasFS _dbErr _dbEpochInfo _dbHashInfo start
        return (secondaryOffset, WithHash (Secondary.headerHash entry) epochSlot)

      -- No start bound given, use the first block in the ImmutableDB as the
      -- start bound.
      Nothing -> lift $ findFirstFilledSlot 0
        where
          findFirstFilledSlot epoch =
            Primary.readFirstFilledSlot hasFS _dbErr epoch >>= \case
              -- We know the database is not empty, so this loop must end
              -- before we reach an epoch that doesn't yet exist (which would
              -- result in an error).
              Nothing      -> findFirstFilledSlot (epoch + 1)
              Just relSlot -> do
                  (Secondary.Entry { headerHash }, _) <-
                    Secondary.readEntry hasFS _dbErr _dbHashInfo epoch isEBB
                      secondaryOffset
                  return (secondaryOffset, WithHash headerHash epochSlot)
                where
                  -- The first entry in the secondary index file (i.e. the
                  -- first filled slot in the primary index) always starts at
                  -- 0.
                  secondaryOffset = 0
                  isEBB | relSlot == 0 = IsEBB
                        | otherwise    = IsNotEBB
                  epochSlot = EpochSlot epoch relSlot

    -- TODO we're calling 'modifyOpenState' from within 'withOpenState', ok?
    withNewIteratorID
      :: (IteratorID -> Iterator hash m ByteString)
      -> m (Iterator hash m ByteString)
    withNewIteratorID mkIter = modifyOpenState dbEnv $ \_hasFS ->
      state $ \st@OpenState { _nextIteratorID = itID } ->
        (mkIter (BaseIteratorID itID), st { _nextIteratorID = succ itID })

    mkEmptyIterator :: m (Iterator hash m ByteString)
    mkEmptyIterator = withNewIteratorID $ \itID -> Iterator
      { iteratorNext    = return IteratorExhausted
      , iteratorPeek    = return IteratorExhausted
      , iteratorHasNext = return Nothing
      , iteratorClose   = return ()
      , iteratorID      = itID
      }

    mkIterator :: IteratorHandle hash m -> m (Iterator hash m ByteString)
    mkIterator ith = withNewIteratorID $ \itID -> Iterator
      { iteratorNext    = iteratorNextImpl dbEnv ith blocksOrHeaders True
      , iteratorPeek    = iteratorNextImpl dbEnv ith blocksOrHeaders False
      , iteratorHasNext = iteratorHasNextImpl    ith
      , iteratorClose   = iteratorCloseImpl      ith
      , iteratorID      = itID
      }

-- | Get information about the block or EBB at the given slot with the given
-- hash. If no such block exists, because the slot is empty or it contains a
-- block and/or EBB with a different hash, return a 'WrongBoundError'.
--
-- Return the 'EpochSlot' corresponding to the block or EBB, the corresponding
-- entry (and 'BlockSize') from the secondary index file, and the
-- 'SecondaryOffset' of that entry.
--
-- The primary index is read to find out whether the slot is filled and what
-- the 'SecondaryOffset' is for the slot. The secondary index is read to check
-- the hash and to return the 'Secondary.Entry'.
--
-- PRECONDITION: the bound is in the past.
--
-- PRECONDITION: the database is not empty.
getSlotInfo
  :: (HasCallStack, IOLike m, Eq hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochInfo m
  -> HashInfo hash
  -> (SlotNo, hash)
  -> ExceptT (WrongBoundError hash) m
             (EpochSlot, (Secondary.Entry hash, BlockSize), SecondaryOffset)
getSlotInfo hasFS err epochInfo hashInfo (slot, hash) = do
    epochSlot@(EpochSlot epoch relSlot) <- lift $
      epochInfoBlockRelative epochInfo slot
    -- 'epochInfoBlockRelative' always assumes the given 'SlotNo' refers to a
    -- regular block and will return 1 as the relative slot number when given
    -- an EBB.
    let couldBeEBB = relSlot == 1

    -- Obtain the offsets in the secondary index file from the primary index
    -- file. The block /could/ still correspond to an EBB, a regular block or
    -- both. We will know which one it is when we can check the hashes from
    -- the secondary index file with the hash we have.
    toRead :: NonEmpty (IsEBB, SecondaryOffset) <- if couldBeEBB then
        lift (Primary.readOffsets hasFS err epoch (Two 0 1)) >>= \case
          Two Nothing Nothing                   ->
            throwError $ EmptySlotError slot
          Two (Just ebbOffset) (Just blkOffset) ->
            return ((IsEBB, ebbOffset) NE.:| [(IsNotEBB, blkOffset)])
          Two (Just ebbOffset) Nothing          ->
            return ((IsEBB, ebbOffset) NE.:| [])
          Two Nothing (Just blkOffset)          ->
            return ((IsNotEBB, blkOffset) NE.:| [])
      else
        lift (Primary.readOffset hasFS err epoch relSlot) >>= \case
          Nothing        ->
            throwError $ EmptySlotError slot
          Just blkOffset ->
            return ((IsNotEBB, blkOffset) NE.:| [])

    entriesWithBlockSizes :: NonEmpty (Secondary.Entry hash, BlockSize) <- lift $
      Secondary.readEntries hasFS err hashInfo epoch toRead

    -- Return the entry from the secondary index file that matches the
    -- expected hash.
    (secondaryOffset, (entry, blockSize))
      :: (SecondaryOffset, (Secondary.Entry hash, BlockSize)) <-
      case find ((== hash) . Secondary.headerHash . fst . snd)
                (NE.zip (fmap snd toRead) entriesWithBlockSizes) of
        Just found -> return found
        Nothing    -> throwError $ WrongHashError slot hash hashes
          where
            hashes = Secondary.headerHash . fst <$> entriesWithBlockSizes

    -- Use the secondary index entry to determine whether the slot + hash
    -- correspond to an EBB or a regular block.
    let epochSlot' = case Secondary.blockOrEBB entry of
          Block _ -> epochSlot
          EBB   _ -> EpochSlot epoch 0
    return (epochSlot', (entry, blockSize), secondaryOffset)

iteratorNextImpl
  :: forall m hash. (IOLike m, Eq hash)
  => ImmutableDBEnv m hash
  -> IteratorHandle hash m
  -> BlocksOrHeaders
  -> Bool  -- ^ Step the iterator after reading iff True
  -> m (IteratorResult hash ByteString)
iteratorNextImpl dbEnv it@IteratorHandle {itHasFS = hasFS :: HasFS m h, ..}
                 blocksOrHeaders step = do
    -- The idea is that if the state is not 'IteratorStateExhausted, then the
    -- head of 'itEpochEntries' is always ready to be read. After reading it
    -- with 'readNextBlock' or 'readNextHeader', 'stepIterator' will advance
    -- the iterator to the next valid epoch slot if @step@ is True.
    atomically (readTVar itState) >>= \case
      -- Iterator already closed
      IteratorStateExhausted -> return IteratorExhausted
      IteratorStateOpen iteratorState@IteratorState{..} ->
        withOpenState dbEnv $ \_ st -> do
          let entryWithBlockSize@(WithBlockSize _ entry) = NE.head itEpochEntries
              hash = Secondary.headerHash entry
              curEpochInfo = CurrentEpochInfo
                (_currentEpoch       st)
                (_currentEpochOffset st)
          blob <- case blocksOrHeaders of
            Blocks  -> readNextBlock  itEpochHandle entryWithBlockSize itEpoch
            Headers -> readNextHeader itEpochHandle entry
          when step $ stepIterator curEpochInfo iteratorState
          return $ case Secondary.blockOrEBB entry of
            Block slot  -> IteratorResult slot  hash blob
            EBB   epoch -> IteratorEBB    epoch hash blob
  where
    ImmutableDBEnv { _dbErr, _dbEpochInfo, _dbHashInfo } = dbEnv
    HasFS { hClose } = hasFS

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAtCRC', i.e. @pread@ for reading from a given offset.
    readNextBlock
      :: Handle h
      -> WithBlockSize (Secondary.Entry hash)
      -> EpochNo
      -> m ByteString
    readNextBlock eHnd (WithBlockSize size entry) epoch = do
        (bl, checksum') <- hGetExactlyAtCRC hasFS eHnd (fromIntegral size) offset
        checkChecksum _dbErr epochFile blockOrEBB checksum checksum'
        return bl
      where
        Secondary.Entry { blockOffset, checksum, blockOrEBB } = entry
        offset    = AbsOffset $ Secondary.unBlockOffset blockOffset
        epochFile = renderFile "epoch" epoch

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAt', i.e. @pread@ for reading from a given offset.
    readNextHeader
      :: Handle h
      -> Secondary.Entry hash
      -> m ByteString
    readNextHeader eHnd Secondary.Entry { blockOffset, headerOffset, headerSize } =
        -- We cannot check the checksum in this case, as we're not reading the
        -- whole block
        hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ Secondary.unHeaderSize headerSize
        offset = AbsOffset $
          (Secondary.unBlockOffset blockOffset) +
          fromIntegral (Secondary.unHeaderOffset headerOffset)

    -- | Move the iterator to the next position that can be read from,
    -- advancing epochs if necessary. If no next position can be found, the
    -- iterator is closed.
    stepIterator :: CurrentEpochInfo -> IteratorState hash h -> m ()
    stepIterator curEpochInfo iteratorState@IteratorState {..} =
      case NE.nonEmpty (NE.tail itEpochEntries) of
        -- There are entries left in this epoch, so continue. See the
        -- invariant on 'itEpochEntries'
        Just itEpochEntries' -> atomically $ writeTVar itState $
          IteratorStateOpen iteratorState { itEpochEntries = itEpochEntries' }

        -- No more entries in this epoch, so open the next.
        Nothing -> do
          hClose itEpochHandle
          -- If this was the final epoch, close the iterator
          if itEpoch >= _epoch itEnd then
            iteratorCloseImpl it

          else
            openNextEpoch curEpochInfo itEnd (itEpoch + 1) >>= \iteratorState' ->
            atomically $ writeTVar itState $ IteratorStateOpen iteratorState'

    openNextEpoch
      :: CurrentEpochInfo
      -> EpochSlot  -- ^ The end bound
      -> EpochNo    -- ^ The epoch to open
      -> m (IteratorState hash h)
    openNextEpoch curEpochInfo end epoch =
      Primary.readFirstFilledSlot hasFS _dbErr epoch >>= \case
        -- This epoch is empty, look in the next one.
        --
        -- We still haven't encountered the end bound, so this loop must end
        -- when we reach the non-empty epoch containing the end bound. This
        -- cannot loop forever as an error would be thrown when opening the
        -- index file(s) of a non-existing epoch.
        Nothing      -> openNextEpoch curEpochInfo end (epoch + 1)
        Just relSlot -> do
          -- Note that the only reason we actually open the primary index file
          -- is to see whether the first block in the epoch is an EBB or not.
          -- To see whether the epoch is empty, we could open the secondary
          -- index file directly and see whether it contains any blocks. The
          -- 'secondaryOffset' will be 0, as the first entry in the secondary
          -- index file always starts at offset 0. The same is true for
          -- 'findFirstFilledSlot'.
          let firstIsEBB | relSlot == 0 = IsEBB
                         | otherwise    = IsNotEBB
              secondaryOffset = 0

          iteratorStateForEpoch hasFS _dbErr _dbHashInfo curEpochInfo itEndHash
            epoch secondaryOffset firstIsEBB

iteratorHasNextImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m (Maybe (Either EpochNo SlotNo, hash))
iteratorHasNextImpl IteratorHandle { itState } =
    atomically $ readTVar itState <&> \case
      IteratorStateExhausted -> Nothing
      IteratorStateOpen IteratorState { itEpochEntries } ->
          Just (epochOrSlot, Secondary.headerHash nextEntry)
        where
          WithBlockSize _ nextEntry NE.:| _ = itEpochEntries
          epochOrSlot = case Secondary.blockOrEBB nextEntry of
            EBB epoch  -> Left epoch
            Block slot -> Right slot

iteratorCloseImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m ()
iteratorCloseImpl IteratorHandle {..} = do
    atomically (readTVar itState) >>= \case
      -- Already closed
      IteratorStateExhausted -> return ()
      IteratorStateOpen IteratorState {..} -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- 'hClose' might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar itState IteratorStateExhausted
        hClose itEpochHandle
  where
    HasFS { hClose } = itHasFS

iteratorStateForEpoch
  :: (HasCallStack, IOLike m, Eq hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> CurrentEpochInfo
  -> hash
     -- ^ Hash of the end bound
  -> EpochNo
  -> SecondaryOffset
     -- ^ Where to start in the secondary index
  -> IsEBB
     -- ^ Whether the first expected block will be an EBB or not.
  -> m (IteratorState hash h)
iteratorStateForEpoch hasFS err hashInfo
                      (CurrentEpochInfo curEpoch curEpochOffset) endHash
                      epoch secondaryOffset firstIsEBB = do
    -- Open the epoch file
    eHnd <- hOpen (renderFile "epoch" epoch) ReadMode

    -- If we don't close the handle when an exception is thrown, we leak a
    -- file handle, since this handle is not stored in a state that can be
    -- closed yet.
    onException hasFsErr err (hClose eHnd) $ do

      -- If the last entry in @entries@ corresponds to the last block in the
      -- epoch, we cannot calculate the block size based on the next block.
      -- Instead, we calculate it based on the size of the epoch file.
      --
      -- IMPORTANT: for older epochs, this is fine, as the secondary index
      -- (entries) and the epoch file (size) are immutable. However, when
      -- doing this for the current epoch, there is a potential race condition
      -- between reading of the entries from the secondary index and obtaining
      -- the epoch file size: what if a new block was appended after reading
      -- the entries but before obtaining the epoch file size? Then the epoch
      -- file size will not correspond to the last entry we read, but to the
      -- block after it. Similarly if we switch the order of the two
      -- operations.
      --
      -- To avoid this race condition, we use the value of
      -- '_currentEpochOffset' from the state as the file size of the current
      -- epoch (stored in 'CurrentEpochInfo'). This value corresponds to the
      -- epoch file size at the time we /read the state/. We also know that
      -- the end bound of our iterator is always <= the tip from that same
      -- state, so all @entries@ must be <= the tip from that state because
      -- we'll never stream beyond the tip. Remember that we only actually use
      -- the current epoch file size if the last entry we have read from the
      -- secondary index is the last entry in the file, in which case it would
      -- correspond to the tip from the state. In this case, the epoch file
      -- size (@curEpochOffset@) we are passed is consistent with the tip, as
      -- it was obtained from the same consistent state.
      epochFileSize <- if epoch == curEpoch
        then return (unBlockOffset curEpochOffset)
        else hGetSize eHnd

      entries <- Secondary.readAllEntries hasFS err hashInfo secondaryOffset
        epoch ((== endHash) . Secondary.headerHash) epochFileSize firstIsEBB

      case NE.nonEmpty entries of
        -- We still haven't encountered the end bound, so it cannot be
        -- that this non-empty epoch contains no entries <= the end bound.
        Nothing             -> error
          "impossible: there must be entries according to the primary index"

        Just itEpochEntries -> return IteratorState
          { itEpoch        = epoch
          , itEpochHandle  = eHnd
            -- Force so we don't store any thunks in the state
          , itEpochEntries = forceElemsToWHNF itEpochEntries
          }
  where
    HasFS { hOpen, hClose, hGetSize, hasFsErr } = hasFS
