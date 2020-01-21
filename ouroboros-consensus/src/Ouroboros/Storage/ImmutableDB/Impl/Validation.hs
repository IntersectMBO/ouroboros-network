{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Storage.ImmutableDB.Impl.Validation
  ( validateAndReopen
  , ValidateEnv (..)
    -- * Exported for testing purposes
  , reconstructPrimaryIndex
  , ShouldBeFinalised (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad (unless, when)
import           Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.Coerce (coerce)
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)
import           Streaming (Of (..))
import qualified Streaming.Prelude as S

import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util (lastMaybe, whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index (cachedIndex)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex,
                     SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

-- | Bundle of arguments used most validation functions.
--
-- Note that we don't use "Ouroboros.Storage.ImmutableDB.Impl.Index" because
-- we are reading and manipulating index files in different ways, e.g.,
-- truncating them.
data ValidateEnv m hash h e = ValidateEnv
  { hasFS       :: !(HasFS m h)
  , err         :: !(ErrorHandling ImmutableDBError m)
  , epochInfo   :: !(EpochInfo m)
  , hashInfo    :: !(HashInfo hash)
  , parser      :: !(EpochFileParser e m (Secondary.Entry hash) hash)
  , tracer      :: !(Tracer m (TraceEvent e hash))
  , registry    :: !(ResourceRegistry m)
  , cacheConfig :: !Index.CacheConfig
  }

-- | Perform validation as per the 'ValidationPolicy' using 'validate' and
-- create an 'OpenState' corresponding to its outcome using 'mkOpenState'.
validateAndReopen
  :: forall m hash h e. (IOLike m, Eq hash, NoUnexpectedThunks hash, HasCallStack)
  => ValidateEnv m hash h e
  -> ValidationPolicy
  -> m (OpenState m hash h)
validateAndReopen validateEnv valPol = do
    (epoch, tip) <- validate validateEnv valPol
    index        <- cachedIndex hasFS err hashInfo registry cacheTracer cacheConfig epoch
    case tip of
      TipGen -> assert (epoch == 0) $ do
        traceWith tracer NoValidLastLocation
        mkOpenState registry hasFS err index epoch TipGen MustBeNew
      _     -> do
        traceWith tracer $ ValidatedLastLocation epoch (forgetHash <$> tip)
        mkOpenState registry hasFS err index epoch tip    AllowExisting
  where
    ValidateEnv { hasFS, err, hashInfo, tracer, registry, cacheConfig } = validateEnv
    cacheTracer = contramap TraceCacheEvent tracer

-- | Execute the 'ValidationPolicy'.
validate
  :: forall m hash h e. (IOLike m, Eq hash, HasCallStack)
  => ValidateEnv m hash h e
  -> ValidationPolicy
  -> m (EpochNo, ImmTipWithHash hash)
validate validateEnv@ValidateEnv{ hasFS } valPol = do
    filesInDBFolder <- listDirectory (mkFsPath [])
    let (epochFiles, _, _) = dbFilesOnDisk filesInDBFolder
    case Set.lookupMax epochFiles of
      Nothing              -> do
        -- Remove left-over index files
        -- TODO calls listDirectory again
        removeFilesStartingFrom hasFS 0
        return (0, TipGen)

      Just lastEpochOnDisk -> case valPol of
        ValidateAllEpochs       ->
          validateAllEpochs       validateEnv lastEpochOnDisk
        ValidateMostRecentEpoch ->
          validateMostRecentEpoch validateEnv lastEpochOnDisk
  where
    HasFS { listDirectory } = hasFS

-- | Validate epochs from oldest to newest, stop after the most recent epoch
-- on disk. During this validation, keep track of the last valid block we
-- encountered. If at the end, that block is not in the last epoch on disk,
-- remove the epoch and index files after that epoch.
validateAllEpochs
  :: forall m hash h e. (IOLike m, Eq hash, HasCallStack)
  => ValidateEnv m hash h e
  -> EpochNo
     -- ^ Most recent epoch on disk
  -> m (EpochNo, ImmTipWithHash hash)
validateAllEpochs validateEnv@ValidateEnv { hasFS, err, epochInfo } lastEpoch =
    go (0, TipGen) 0 Origin
  where
    go
      :: (EpochNo, ImmTipWithHash hash)  -- ^ The last valid epoch and tip
      -> EpochNo                         -- ^ The epoch to validate now
      -> WithOrigin hash                 -- ^ The hash of the last block of
                                         -- the previous epoch
      -> m (EpochNo, ImmTipWithHash hash)
    go lastValid epoch prevHash = do
      shouldBeFinalised <- if epoch == lastEpoch
        then return ShouldNotBeFinalised
        else ShouldBeFinalised <$> epochInfoSize epochInfo epoch
      runExceptT
        (validateEpoch validateEnv shouldBeFinalised epoch (Just prevHash)) >>= \case
          Left  ()              -> cleanup lastValid epoch $> lastValid
          Right Nothing         -> continueOrStop lastValid             epoch prevHash
          Right (Just validBlk) -> continueOrStop (epoch, Tip validBlk) epoch prevHash'
            where
              prevHash' = At (theHash validBlk)

    -- | Validate the next epoch, unless the epoch just validated is the last
    -- epoch to validate. Cleanup files corresponding to epochs after the
    -- epoch in which we found the last valid block. Return that epoch and the
    -- tip corresponding to that block.
    continueOrStop
      :: (EpochNo, ImmTipWithHash hash)
      -> EpochNo         -- ^ The epoch just validated
      -> WithOrigin hash -- ^ The hash of the last block of the previous epoch
      -> m (EpochNo, ImmTipWithHash hash)
    continueOrStop lastValid epoch prevHash
      | epoch < lastEpoch
      = go lastValid (epoch + 1) prevHash
      | otherwise
      = assert (epoch == lastEpoch) $ do
        -- Cleanup is only needed when the final epoch was empty, yet valid.
        cleanup lastValid epoch
        return lastValid

    -- | Remove left over files from epochs newer than the last epoch
    -- containing a valid file. Also unfinalise it if necessary.
    cleanup
      :: (EpochNo, ImmTipWithHash hash)  -- ^ The last valid epoch and tip
      -> EpochNo  -- ^ The last validated epoch, could have been invalid or
                  -- empty
      -> m ()
    cleanup (lastValidEpoch, tip) lastValidatedEpoch = case tip of
      TipGen ->
        removeFilesStartingFrom hasFS 0
      Tip _  -> do
        removeFilesStartingFrom hasFS (lastValidEpoch + 1)
        when (lastValidEpoch < lastValidatedEpoch) $
          Primary.unfinalise hasFS err lastValidEpoch

-- | Validate the given most recent epoch. If that epoch contains no valid
-- block, try the epoch before it, and so on. Stop as soon as an epoch with a
-- valid block is found, returning that epoch and the tip corresponding to
-- that block. If no valid blocks are found, epoch 0 and 'TipGen' is returned.
validateMostRecentEpoch
  :: forall m hash h e. (IOLike m, Eq hash, HasCallStack)
  => ValidateEnv m hash h e
  -> EpochNo
     -- ^ Most recent epoch on disk, the epoch to validate
  -> m (EpochNo, ImmTipWithHash hash)
validateMostRecentEpoch validateEnv@ValidateEnv { hasFS } = go
  where
    go :: EpochNo -> m (EpochNo, ImmTipWithHash hash)
    go epoch = runExceptT
      (validateEpoch validateEnv ShouldNotBeFinalised epoch Nothing) >>= \case
        Right (Just validBlk) -> do
            -- Found a valid block, we can stop now.
            removeFilesStartingFrom hasFS (epoch + 1)
            return (epoch, Tip validBlk)
        _  -- This epoch file is unusable: either the epoch is empty or
           -- everything after it should be truncated.
          | epoch > 0 -> go (epoch - 1)
          | otherwise -> do
            -- Found no valid blocks on disk.
            -- TODO be more precise in which cases we need which cleanup.
            removeFilesStartingFrom hasFS 0
            return (0, TipGen)

-- | Iff the epoch is the most recent epoch, it should not be finalised.
--
-- With finalising, we mean: if there are one or more empty slots at the end
-- of the epoch, the primary index should be padded with offsets to indicate
-- that these slots are empty. See 'Primary.backfill'.
data ShouldBeFinalised
  = ShouldBeFinalised EpochSize
  | ShouldNotBeFinalised
  deriving (Show)

-- | Validate the given epoch
--
-- * Invalid or missing epoch files will cause truncation. All blocks after a
--   gap in blocks (due to a missing blocks or invalid block(s)) are
--   truncated.
--
-- * Epoch files are the main source of truth. Primary and secondary index
--   files can be reconstructed from the epoch files using the
--   'EpochFileParser'. If index files are missing, corrupt, or do not match
--   the epoch files, they are overwritten.
--
-- * The 'EpochFileParser' checks whether the hashes (header hash) line up
--   within an epoch. When they do not, we truncate the epoch, including the
--   block of which its previous hash does not match the hash of the previous
--   block.
--
-- * For each block, the 'EpochFileParser' checks whether the checksum (and
--   other fields) from the secondary index file match the ones retrieved from
--   the actual block. If they do, the block has not been corrupted. If they
--   don't match or if the secondary index file is missing or corrupt, we have
--   to do the expensive integrity check of the block itself to determine
--   whether it is corrupt or not.
--
-- * This function checks whether the first block in the epoch fits onto the
--   last block of the previous epoch by checking the hashes. If they do not
--   fit, this epoch is truncated and @()@ is thrown.
--
-- * When an invalid block needs to be truncated, trailing empty slots are
--   also truncated so that the tip of the database will always point to a
--   valid block or EBB.
--
-- * All but the most recent epoch in the database should be finalised, i.e.
--   padded to the size of the epoch.
--
validateEpoch
  :: forall m hash h e. (IOLike m, Eq hash, HasCallStack)
  => ValidateEnv m hash h e
  -> ShouldBeFinalised
  -> EpochNo
  -> Maybe (WithOrigin hash)
     -- ^ The hash of the last block of the previous epoch. 'Nothing' if
     -- unknown. When this is the first epoch, it should be 'Just Origin'.
  -> ExceptT () m (Maybe (WithHash hash BlockOrEBB))
     -- ^ When non-empty, return the 'BlockOrEBB' and @hash@ of the last valid
     -- block in the epoch.
     --
     -- When the epoch file is missing or when we should truncate starting
     -- from this epoch because it doesn't fit onto the previous one, @()@ is
     -- thrown.
     --
     -- Note that when an invalid block is detected, we don't throw, but we
     -- truncate the epoch file. When validating the epoch file after it, we
     -- would notice it doesn't fit anymore, and then throw.
validateEpoch ValidateEnv{..} shouldBeFinalised epoch mbPrevHash = do
    trace $ ValidatingEpoch epoch
    epochFileExists <- lift $ doesFileExist epochFile
    unless epochFileExists $ do
      trace $ MissingEpochFile epoch
      throwError ()

    -- Read the entries from the secondary index file, if it exists.
    secondaryIndexFileExists  <- lift $ doesFileExist secondaryIndexFile
    entriesFromSecondaryIndex <- lift $ if secondaryIndexFileExists
      then EH.try errLoad
        -- Note the 'maxBound': it is used to calculate the block size for
        -- each entry, but we don't care about block sizes here, so we use
        -- some dummy value.
        (Secondary.readAllEntries hasFS err hashInfo 0 epoch (const False)
           maxBound IsEBB) >>= \case
          Left _                -> do
            traceWith tracer $ InvalidSecondaryIndex epoch
            return []
          Right entriesFromFile ->
            return $ fixupEBB (map withoutBlockSize entriesFromFile)
      else do
        traceWith tracer $ MissingSecondaryIndex epoch
        return []

    -- Parse the epoch file using the checksums from the secondary index file
    -- as input. If the checksums match, the parser doesn't have to do the
    -- expensive integrity check of a block.
    let expectedChecksums = map Secondary.checksum entriesFromSecondaryIndex
    (entriesWithPrevHashes, mbErr) <- lift $
        runEpochFileParser parser epochFile expectedChecksums $ \stream ->
          (\(es :> mbErr) -> (es, mbErr)) <$> S.toList stream

    -- Check whether the first block of this epoch fits onto the last block of
    -- the previous epoch.
    case entriesWithPrevHashes of
      (_, actualPrevHash) : _
        | Just expectedPrevHash <- mbPrevHash
        , expectedPrevHash /= actualPrevHash
          -- The previous hash of the first block in the epoch does not match
          -- the hash of the last block of the previous epoch. There must be a
          -- gap. This epoch should be truncated.
        -> do
          trace $ EpochFileDoesntFit expectedPrevHash actualPrevHash
          throwError ()
      _ -> return ()

    lift $ do

      -- If the parser returneds a deserialisation error, truncate the epoch
      -- file. Don't truncate the database just yet, because the
      -- deserialisation error may be due to some extra random bytes that
      -- shouldn't have been there in the first place.
      whenJust mbErr $ \(parseErr, endOfLastValidBlock) -> do
        traceWith tracer $ InvalidEpochFile epoch parseErr
        withFile hasFS epochFile (AppendMode AllowExisting) $ \eHnd ->
          hTruncate eHnd endOfLastValidBlock

      -- If the secondary index file is missing, parsing it failed, or it does
      -- not match the entries from the epoch file, overwrite it using those
      -- (truncate first).
      let entries = map fst entriesWithPrevHashes
      when (entriesFromSecondaryIndex /= entries ||
            not secondaryIndexFileExists) $ do
        traceWith tracer $ RewriteSecondaryIndex epoch
        Secondary.writeAllEntries hasFS hashInfo epoch entries

      -- Reconstruct the primary index from the 'Secondary.Entry's.
      --
      -- Read the primary index file, if it is missing, parsing fails, or it
      -- does not match the reconstructed primary index, overwrite it using
      -- the reconstructed index (truncate first).
      primaryIndex            <- reconstructPrimaryIndex epochInfo hashInfo
        shouldBeFinalised (map Secondary.blockOrEBB entries)
      primaryIndexFileExists  <- doesFileExist primaryIndexFile
      primaryIndexFileMatches <- if primaryIndexFileExists
        then EH.try errLoad (Primary.load hasFS err epoch) >>= \case
          Left _                     -> do
            traceWith tracer $ InvalidPrimaryIndex epoch
            return False
          Right primaryIndexFromFile ->
            return $ primaryIndexFromFile == primaryIndex
        else do
          traceWith tracer $ MissingPrimaryIndex epoch
          return False
      unless primaryIndexFileMatches $ do
        traceWith tracer $ RewritePrimaryIndex epoch
        Primary.write hasFS epoch primaryIndex

      return $ entryToWithHash <$> lastMaybe entries
  where
    epochFile          = renderFile "epoch"     epoch
    primaryIndexFile   = renderFile "primary"   epoch
    secondaryIndexFile = renderFile "secondary" epoch

    HasFS { hTruncate, doesFileExist } = hasFS

    trace = lift . traceWith tracer

    entryToWithHash :: Secondary.Entry hash -> WithHash hash BlockOrEBB
    entryToWithHash entry =
      WithHash (Secondary.headerHash entry) (Secondary.blockOrEBB entry)

    -- | Handle only 'InvalidFileError', which is the only error that can be
    -- thrown while load a primary or a secondary index file
    errLoad :: ErrorHandling UnexpectedError m
    errLoad = EH.embed UnexpectedError
      (\case
        UnexpectedError (e@InvalidFileError {}) -> Just e
        _ -> Nothing) err

    -- | When reading the entries from the secondary index file, we need to
    -- pass in a value of type 'IsEBB' so we know whether the first entry
    -- corresponds to an EBB or a regular block. We need this information to
    -- correctly interpret the deserialised 'Word64' as a 'BlockOrEBB': if
    -- it's an EBB, it's the 'EpochNo' ('Word64'), if it's a regular block,
    -- it's a 'SlotNo' ('Word64').
    --
    -- However, at the point we are reading the secondary index file, we don't
    -- yet know whether the first block will be an EBB or a regular block. We
    -- will find that out when we read the actual block from the epoch file.
    --
    -- Fortunately, we can make a /very/ good guess: if the 'Word64' of the
    -- 'BlockOrEBB' matches the epoch number, it is almost certainly an EBB,
    -- as the slot numbers increase @10k@ times faster than epoch numbers.
    -- Property: for every epoch @e > 0@, for all slot numbers @s@ in epoch
    -- @e@ we have @s > e@. The only exception is epoch 0, which contains a
    -- slot number 0. From this follows that it's an EBB if and only if the
    -- 'Word64' matches the epoch number.
    --
    -- E.g., the first slot number in epoch 1 will be 21600 if @k = 2160@. We
    -- could only make the wrong guess in the first very first epoch, i.e.,
    -- epoch 0, as the first slot number is also 0. However, we know that the
    -- real blockchain starts with an EBB, so even in that case we're fine.
    --
    -- If the epoch size were 1, then we would make the wrong guess for each
    -- epoch that contains an EBB, which is a rather unrealistic scenario.
    --
    -- Note that even making the wrong guess is not a problem. The (CRC)
    -- checksums are the only thing we extract from the secondary index file.
    -- These are passed to the 'EpochFileParser'. We then reconstruct the
    -- secondary index using the output of the 'EpochFileParser'. If that
    -- output doesn't match the parsed secondary index file, we will overwrite
    -- the secondary index file.
    --
    -- So the only thing that wouldn't go according to plan is that we will
    -- needlessly overwrite the secondary index file.
    fixupEBB :: [Secondary.Entry hash] -> [Secondary.Entry hash]
    fixupEBB = \case
      entry@Secondary.Entry { blockOrEBB = EBB epoch' }:rest
        | epoch' /= epoch
        -> entry { Secondary.blockOrEBB = Block (coerce epoch') }:rest
      entries -> entries

-- | Reconstruct a 'PrimaryIndex' based on a list of 'Secondary.Entry's.
reconstructPrimaryIndex
  :: forall m hash. Monad m
  => EpochInfo m
  -> HashInfo hash
  -> ShouldBeFinalised
  -> [BlockOrEBB]
  -> m PrimaryIndex
reconstructPrimaryIndex epochInfo HashInfo { hashSize } shouldBeFinalised
                        blockOrEBBs = do
    relSlots <- mapM toRelativeSlot blockOrEBBs
    let secondaryOffsets = 0 : go 0 0 relSlots

    -- This can only fail if the slot numbers of the entries are not
    -- monotonically increasing.
    return $ fromMaybe (error msg) $ Primary.mk secondaryOffsets
  where
    msg = "blocks have non-increasing slot numbers"

    toRelativeSlot :: BlockOrEBB -> m RelativeSlot
    toRelativeSlot (EBB _)      = return 0
    toRelativeSlot (Block slot) = _relativeSlot <$>
      epochInfoBlockRelative epochInfo slot

    go
      :: HasCallStack
      => RelativeSlot
      -> SecondaryOffset
      -> [RelativeSlot]
      -> [SecondaryOffset]
    go nextExpectedRelSlot lastSecondaryOffset = \case
      [] -> case shouldBeFinalised of
        ShouldNotBeFinalised        -> []
        ShouldBeFinalised epochSize ->
          Primary.backfillEpoch epochSize nextExpectedRelSlot lastSecondaryOffset

      relSlot:relSlots'
        | relSlot < nextExpectedRelSlot
        -> error msg
        | otherwise
        -> let backfilled = Primary.backfill relSlot nextExpectedRelSlot
                 lastSecondaryOffset
               secondaryOffset = lastSecondaryOffset
                               + Secondary.entrySize hashSize
           in backfilled ++ secondaryOffset :
              go (succ relSlot) secondaryOffset relSlots'
