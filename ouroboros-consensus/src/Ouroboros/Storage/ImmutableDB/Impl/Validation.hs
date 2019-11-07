{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Storage.ImmutableDB.Impl.Validation
  ( validateAndReopen
  , reconstructIndex
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad (when)
import           Control.Tracer (Tracer, traceWith)

import           Data.Functor ((<&>))
import qualified Data.Set as Set
import           Data.Word

import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow hiding (onException)


import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index
import qualified Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets as SlotOffsets
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

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
                  -> Tracer m (TraceEvent e)
                  -> m (OpenState hash h)
validateAndReopen
  hashDecoder
  hashEncoder
  hasFS
  err
  epochInfo
  valPol
  parser
  nextIteratorID
  tracer = do
    mbLastValidLocationAndIndex <-
      validate hashDecoder hashEncoder hasFS err epochInfo valPol parser tracer

    case mbLastValidLocationAndIndex of
      Nothing -> do
        traceWith tracer $ NoValidLastLocation
        mkOpenStateNewEpoch hasFS 0 nextIteratorID TipGen
      Just (lastValidLocation, index) -> do
        tip <- epochSlotToTip epochInfo lastValidLocation
        let epoch = _epoch lastValidLocation
        traceWith tracer $ ValidatedLastLocation epoch tip
        mkOpenState hasFS epoch nextIteratorID tip index

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
         -> Tracer m (TraceEvent e)
         -> m (Maybe (EpochSlot, Index hash))
            -- ^ The 'EpochSlot' pointing at the last valid block or EBB on
            -- disk and the 'Index' of the corresponding epoch. 'Nothing' if
            -- the database is empty.
validate hashDecoder hashEncoder hasFS@HasFS{..} err epochInfo valPol parser tracer = do
    filesInDBFolder <- listDirectory (mkFsPath [])
    let epochFiles = fst $ dbFilesOnDisk filesInDBFolder
    case Set.lookupMax epochFiles of
      Nothing              -> do
        -- Remove left-over index files
        removeFilesStartingFrom hasFS 0
        -- TODO calls listDirectory again
        return Nothing

      Just lastEpochOnDisk -> case valPol of
        ValidateMostRecentEpoch -> validateMostRecentEpoch lastEpochOnDisk
        ValidateAllEpochs       -> validateAllEpochs lastEpochOnDisk
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
          traceWith tracer $ ValidatingEpoch epoch
          validateRes <- validateEpoch epoch

          let continueIfPossible lastValid'
                | epoch == 0 = return lastValid'
                | otherwise  = go lastValid' (epoch - 1)
          case validateRes of
            Missing -> do
              traceWith tracer $ ValidatingEpochMissing epoch
              -- Remove all valid files that may come after it. Note that
              -- 'Invalid' guarantees that there is no epoch or index file for
              -- this epoch.
              removeFilesStartingFrom hasFS (succ epoch)
              continueIfPossible Nothing
            Incomplete index -> do
              traceWith tracer $ ValidatingEpochIndexIncomplete epoch
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
            parser epochInfo

          -- If there was an error parsing the epoch file, truncate it
          case mbErr of
            -- If there was an error parsing the epoch file, truncate it
            Just _err -> do
              traceWith tracer $ ValidatingEpochErrorParsing _err
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
            | otherwise -> do
              traceWith tracer ReconstructIndexLastSlotMissing
              return $ Incomplete reconstructedIndex

-- | Reconstruct an 'Index' from the given epoch file using the
-- 'EpochFileParser'.
--
-- Also returns the error returned by the 'EpochFileParser'.
reconstructIndex :: forall m e hash. MonadThrow m
                 => FsPath
                 -> EpochFileParser e hash m (Word64, SlotNo)
                 -> EpochInfo m
                 -> m (Index hash, Maybe e)
reconstructIndex epochFile parser epochInfo = do
    (offsetsAndSizesAndSlots, ebbHash, mbErr) <-
      runEpochFileParser parser epochFile
    offsetsAndSizesAndRelSlots <- case offsetsAndSizesAndSlots of
      [] -> return []
      (offset0, (size0, _slot0)) : offsetsAndSizesAndSlots'
        | CurrentEBB _ <- ebbHash
          -- If there is an EBB, then the first entry in the list must
          -- correspond to the EBB
        -> ((offset0, (size0, 0)) :) <$> slotsToRelSlots offsetsAndSizesAndSlots'
        | otherwise
        -> slotsToRelSlots offsetsAndSizesAndSlots

    let slotOffsets = SlotOffsets.reconstruct offsetsAndSizesAndRelSlots
        index       = indexFromSlotOffsets slotOffsets ebbHash
    return (index, mbErr)
  where
    slotsToRelSlots :: [(SlotOffset, (Word64, SlotNo))]
                    -> m [(SlotOffset, (Word64, RelativeSlot))]
    slotsToRelSlots = mapM $ \(offset, (size, slot)) -> do
      relSlot <- _relativeSlot <$> epochInfoBlockRelative epochInfo slot
      return (offset, (size, relSlot))
