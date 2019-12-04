{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import           Control.Tracer (Tracer, traceWith)
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

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex,
                     SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

-- | Bundle of arguments used most validation functions.
data ValidateEnv m hash h e = ValidateEnv
  { hasFS     :: !(HasFS m h)
  , err       :: !(ErrorHandling ImmutableDBError m)
  , epochInfo :: !(EpochInfo m)
  , hashInfo  :: !(HashInfo hash)
  , parser    :: !(EpochFileParser e m (Secondary.Entry hash, WithOrigin hash))
  , tracer    :: !(Tracer m (TraceEvent e hash))
  }

-- | Perform validation as per the 'ValidationPolicy' using 'validate' and
-- create an 'OpenState' corresponding to its outcome using 'mkOpenState'.
validateAndReopen
  :: forall m hash h e. (IOLike m, Eq hash, HasCallStack)
  => ValidateEnv m hash h e
  -> ValidationPolicy
  -> BaseIteratorID
  -> m (OpenState hash h)
validateAndReopen validateEnv valPol nextIteratorID =
    validate validateEnv valPol >>= \case
      (epoch, TipGen) -> assert (epoch == 0) $ do
        traceWith tracer NoValidLastLocation
        mkOpenState hasFS epoch nextIteratorID TipGen MustBeNew
      (epoch, tip)    -> do
        traceWith tracer $ ValidatedLastLocation epoch (forgetHash <$> tip)
        mkOpenState hasFS epoch nextIteratorID tip    AllowExisting
  where
    ValidateEnv { hasFS, tracer } = validateEnv

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
-- TODO currently, we focus on detecting partial writes, not on detecting all
-- kinds of corruptions, i.e. bitflips. In reality, the likeliness of a
-- bitflip happening in an epoch file is much larger than one happening in a
-- secondary index file. So when the checksums don't match, it will probably
-- be because the block is corrupt, not because the checksum got corrupted in
-- the secondary index file. We currently assume the epoch file is correct and
-- overwrite the secondary index file with the new checksum.
--
-- In the future, we will be able to check the integrity of a block on its own
-- so we can determine whether the block or the checksum got corrupted. See
-- #1253.
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
    --  Parse the epoch file, return a list of 'Secondary.Entry's with one for
    --  each block in the file. If the parser returns a deserialisation error,
    --  truncate the epoch file. Don't truncate the database just yet, because
    --  the deserialisation error may be due to some extra random bytes that
    --  shouldn't have been there in the first place.
    epochFileExists <- lift $ doesFileExist epochFile
    unless epochFileExists $ do
      trace $ MissingEpochFile epoch
      throwError ()

    (entriesWithPrevHashes, mbErr) <- lift $
        runEpochFileParser parser epochFile $ \stream ->
          (\(es :> mbErr) -> (es, mbErr)) <$> S.toList stream

    case entriesWithPrevHashes of
      (_, actualPrevHash) : _
        | Just expectedPrevHash <- mbPrevHash
        , expectedPrevHash /= actualPrevHash
          -- The previous hash of the first block in the epoch does not match
          -- the hash of the last block of the previous epoch. There must be a
          -- gap. This epoch should be truncated
        -> do
          trace $ EpochFileDoesntFit expectedPrevHash actualPrevHash
          throwError ()
      _ -> return ()

    lift $ do

      whenJust mbErr $ \(parseErr, endOfLastValidBlock) -> do
        -- If there was an error parsing the epoch file, truncate it
        traceWith tracer $ InvalidEpochFile epoch parseErr
        withFile hasFS epochFile (AppendMode AllowExisting) $ \eHnd ->
          hTruncate eHnd endOfLastValidBlock

      -- Read the secondary index file, if it is missing, parsing fails, or it
      -- does not match the 'Secondary.Entry's from the epoch file, overwrite
      -- it using those (truncate first).
      let entries = map fst entriesWithPrevHashes
          isEBB
            | entry:_ <- entries, EBB _ <- Secondary.blockOrEBB entry
            = IsEBB
            | otherwise
            = IsNotEBB

      secondaryIndexFileExists  <- doesFileExist secondaryIndexFile
      secondaryIndexFileMatches <- if secondaryIndexFileExists
        then EH.try errLoad
          (Secondary.readAllEntries hasFS err hashInfo 0 epoch (const False) isEBB) >>= \case
            Left _                -> do
              traceWith tracer $ InvalidSecondaryIndex epoch
              return False
            -- TODO what if a hash or a checksum doesn't match? Who's speaking
            -- the truth? If the checksums match, then the hash of the epoch
            -- file is the right one. If the checksums don't match, we'd have
            -- to check the signature and whether the body of the block
            -- matches the body hash in the header to know which checksum is
            -- correct. If the signature check and so on pass, then the
            -- checksum of the block is correct, not the checksum in the
            -- secondary index entry. If they don't, the the block is corrupt
            -- and we should truncate it.
            --
            -- CURRENT APPROACH: we assume the contents of the epoch file are
            -- correct (however unrealistic that may be, as the chance of a
            -- bit flip in an epoch file is much greater than in an index
            -- file). We will do the right thing in the future.
            Right entriesFromFile -> return $ map fst entriesFromFile == entries
        else do
          traceWith tracer $ MissingSecondaryIndex epoch
          return False
      unless secondaryIndexFileMatches $ do
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
