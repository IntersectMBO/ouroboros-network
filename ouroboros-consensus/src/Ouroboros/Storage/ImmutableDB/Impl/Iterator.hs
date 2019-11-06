{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
module Ouroboros.Storage.ImmutableDB.Impl.Iterator
  ( streamBinaryBlobsImpl
  ) where

import           Control.Monad (unless, when)
import           Control.Monad.State.Strict (state)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     UseIsNormalFormNamed (..), allNoUnexpectedThunks)

import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow hiding (onException)

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}


-- | Internal handle to an iterator
data IteratorHandle hash m = forall h. IteratorHandle
  { _it_hasFS    :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , _it_state    :: !(StrictTVar m (IteratorStateOrExhausted hash h))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , _it_end      :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  , _it_end_hash :: !(IteratorEndHash hash)
    -- ^ The @hash@ of the last block the iterator should return. 'Nothing'
    -- when no @hash@ was specified, then only '_it_end' will be used to
    -- determine when to stop streaming.
  }

data IteratorStateOrExhausted hash h =
    IteratorStateOpen !(IteratorState hash h)
  | IteratorStateExhausted
  deriving (Generic, NoUnexpectedThunks)

iteratorStateIsOpen :: IteratorStateOrExhausted hash h -> Bool
iteratorStateIsOpen (IteratorStateOpen _)  = True
iteratorStateIsOpen IteratorStateExhausted = False

data IteratorEndHash hash =
    ItrNoEndHash
  | ItrEndHash !hash
  deriving NoUnexpectedThunks via UseIsNormalFormNamed "IteratorEndHash" (IteratorEndHash hash)

-- Existential type; we can't use generics
instance ( forall a. NoUnexpectedThunks (StrictTVar m a)
         ) => NoUnexpectedThunks (IteratorHandle hash m) where
  showTypeOf _ = "IteratorHandle"
  whnfNoUnexpectedThunks ctxt IteratorHandle{..} =
      allNoUnexpectedThunks [
          noUnexpectedThunks ctxt _it_hasFS
        , noUnexpectedThunks ctxt _it_state
        , noUnexpectedThunks ctxt _it_end
        , noUnexpectedThunks ctxt _it_end_hash
        ]

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
  , _it_epoch_handle :: !(Handle h)
    -- ^ A handle to the epoch file corresponding with '_it_next'.
  , _it_epoch_index  :: Index hash
    -- ^ We load the index file for the epoch we are currently iterating over
    -- in-memory, as it's going to be small anyway (usually ~150kb).
  }
  deriving (Generic, NoUnexpectedThunks)

streamBinaryBlobsImpl :: forall m hash.
                         (HasCallStack, IOLike m, Eq hash)
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
          Tip (EBB epoch)
            | Just (endSlot, endHash) <- mbEnd
            -> do -- We don't really know if the upper bound points at a
                  -- regular block or an EBB here. We conservatively assume it
                  -- must a regular block (which would come /after/ the EBB),
                  -- and then check this when we actually reach the end.
                  endEpochSlot <- epochInfoBlockRelative _epochInfo endSlot
                  return $ Just (endEpochSlot, ItrEndHash endHash)
            | otherwise
            -> return $ Just (EpochSlot epoch 0, ItrNoEndHash)
          Tip (Block lastSlot')
            | Just (endSlot, endHash) <- mbEnd
            -> do endEpochSlot <- epochInfoBlockRelative _epochInfo endSlot
                  return $ Just (endEpochSlot, ItrEndHash endHash)
            | otherwise
            -> do endEpochSlot <- epochInfoBlockRelative _epochInfo lastSlot'
                  return $ Just (endEpochSlot, ItrNoEndHash)

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
                            getCurrentEBB (getEBBHash startIndex)
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
          Nothing -> return IteratorStateExhausted
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

            return $ IteratorStateOpen IteratorState
              { _it_next         = next
              , _it_epoch_handle = eHnd
              , _it_epoch_index  = index
              }

        itState <- newTVarM mbIteratorState

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
                    (IOLike m, Eq hash)
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
      IteratorStateExhausted -> return IteratorExhausted
      -- Valid @next@ thanks to Invariant 1, so go ahead and read it
      IteratorStateOpen iteratorState@IteratorState{..} -> withOpenState dbEnv $ \_ st -> do
        slot <- epochInfoAbsolute (_epochInfo st) _it_next
        blob <- readNext iteratorState
        case _it_next of
          -- It's an EBB
          EpochSlot epoch 0
            | let ebbHash = fromMaybe (error "missing EBB hash") $
                    getCurrentEBB (getEBBHash _it_epoch_index)
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
                (EpochSlot endEpoch 1, ItrEndHash endHash)
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
          -> atomically $ writeTVar _it_state $ IteratorStateOpen its { _it_next = next }
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
              atomically $ writeTVar _it_state $ IteratorStateOpen IteratorState
                { _it_next = EpochSlot epoch relSlot
                , _it_epoch_handle = eHnd
                , _it_epoch_index = index
                }

iteratorHasNextImpl :: (HasCallStack, IOLike m)
                    => IteratorHandle hash m
                    -> m Bool
iteratorHasNextImpl IteratorHandle { _it_state } =
    fmap iteratorStateIsOpen $ atomically $ readTVar _it_state

iteratorCloseImpl :: (HasCallStack, IOLike m)
                  => IteratorHandle hash m
                  -> m ()
iteratorCloseImpl IteratorHandle {..} = do
    mbIteratorState <- atomically $ readTVar _it_state
    case mbIteratorState of
      -- Already closed
      IteratorStateExhausted -> return ()
      IteratorStateOpen IteratorState {..} -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- hClose might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar _it_state IteratorStateExhausted
        hClose _it_epoch_handle
  where
    HasFS{..} = _it_hasFS
