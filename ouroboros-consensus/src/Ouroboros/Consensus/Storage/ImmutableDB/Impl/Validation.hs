{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Validation (
    ValidateEnv (..)
  , validateAndReopen
    -- * Exported for testing purposes
  , ShouldBeFinalised (..)
  , reconstructPrimaryIndex
  ) where

import           Control.Exception (assert)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import           Control.Tracer (Tracer, contramap, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)
import           Streaming (Of (..))
import qualified Streaming.Prelude as S

import           Ouroboros.Consensus.Block hiding (hashSize)
import           Ouroboros.Consensus.Util (lastMaybe, whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     WithTempRegistry)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unChunkNo, unsafeEpochNoToChunkNo)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index
                     (cachedIndex)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (PrimaryIndex, SecondaryOffset)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Parser
                     (BlockSummary (..), parseChunkFile)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..),
                     HasBinaryBlockInfo (..))

-- | Bundle of arguments used most validation functions.
--
-- Note that we don't use "Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index"
-- because we are reading and manipulating index files in different ways, e.g.,
-- truncating them.
data ValidateEnv m blk h = ValidateEnv {
      hasFS          :: !(HasFS m h)
    , chunkInfo      :: !ChunkInfo
    , tracer         :: !(Tracer m (TraceEvent blk))
    , cacheConfig    :: !Index.CacheConfig
    , codecConfig    :: !(CodecConfig blk)
    , checkIntegrity :: !(blk -> Bool)
    }

-- | Perform validation as per the 'ValidationPolicy' using 'validate' and
-- create an 'OpenState' corresponding to its outcome using 'mkOpenState'.
validateAndReopen ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ConvertRawHash blk
     , Eq h
     , HasCallStack
     )
  => ValidateEnv m blk h
  -> ResourceRegistry m
     -- ^ Not used for validation, only used to open a new index
  -> ValidationPolicy
  -> WithTempRegistry (OpenState m blk h) m (OpenState m blk h)
validateAndReopen validateEnv registry valPol = wrapFsError (Proxy @blk) $ do
    (chunk, tip) <- lift $ validate validateEnv valPol
    index        <- lift $ cachedIndex
                      hasFS
                      registry
                      cacheTracer
                      cacheConfig
                      chunkInfo
                      chunk
    case tip of
      Origin -> assert (chunk == firstChunkNo) $ do
        lift $ traceWith tracer NoValidLastLocation
        mkOpenState hasFS index chunk Origin MustBeNew
      NotOrigin tip' -> do
        lift $ traceWith tracer $ ValidatedLastLocation chunk tip'
        mkOpenState hasFS index chunk tip AllowExisting
  where
    ValidateEnv { hasFS, tracer, cacheConfig, chunkInfo } = validateEnv
    cacheTracer = contramap TraceCacheEvent tracer

-- | Execute the 'ValidationPolicy'.
--
-- Migrates first.
--
-- NOTE: we don't use a 'ResourceRegistry' to allocate file handles in,
-- because validation happens on startup, so when an exception is thrown, the
-- database hasn't even been opened and the node will shut down. In which case
-- we don't have to worry about leaking handles, they will be closed when the
-- process terminates.
validate ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ConvertRawHash blk
     , HasCallStack
     )
  => ValidateEnv m blk h
  -> ValidationPolicy
  -> m (ChunkNo, WithOrigin (Tip blk))
validate validateEnv@ValidateEnv{ hasFS } valPol = do

    -- First migrate any old files before validating them
    migrate validateEnv

    filesInDBFolder <- listDirectory (mkFsPath [])
    let (chunkFiles, _, _) = dbFilesOnDisk filesInDBFolder
    case Set.lookupMax chunkFiles of
      Nothing              -> do
        -- Remove left-over index files
        -- TODO calls listDirectory again
        removeFilesStartingFrom hasFS firstChunkNo
        return (firstChunkNo, Origin)

      Just lastChunkOnDisk -> case valPol of
        ValidateAllChunks       ->
          validateAllChunks       validateEnv lastChunkOnDisk
        ValidateMostRecentChunk ->
          validateMostRecentChunk validateEnv lastChunkOnDisk
  where
    HasFS { listDirectory } = hasFS

-- | Validate chunks from oldest to newest, stop after the most recent chunk
-- on disk. During this validation, keep track of the last valid block we
-- encountered. If at the end, that block is not in the last chunk on disk,
-- remove the chunk and index files after that chunk.
validateAllChunks ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ConvertRawHash blk
     , HasCallStack
     )
  => ValidateEnv m blk h
  -> ChunkNo
     -- ^ Most recent chunk on disk
  -> m (ChunkNo, WithOrigin (Tip blk))
validateAllChunks validateEnv@ValidateEnv { hasFS, chunkInfo } lastChunk =
    go (firstChunkNo, Origin) firstChunkNo GenesisHash
  where
    go ::
         (ChunkNo, WithOrigin (Tip blk))  -- ^ The last valid chunk and tip
      -> ChunkNo                          -- ^ The chunk to validate now
      -> ChainHash blk                    -- ^ The hash of the last block of
                                          -- the previous chunk
      -> m (ChunkNo, WithOrigin (Tip blk))
    go lastValid chunk prevHash = do
      let shouldBeFinalised =
            if chunk == lastChunk
              then ShouldNotBeFinalised
              else ShouldBeFinalised
      runExceptT
        (validateChunk validateEnv shouldBeFinalised chunk (Just prevHash)) >>= \case
          Left  ()              -> cleanup lastValid chunk $> lastValid
          Right Nothing         -> continueOrStop lastValid                   chunk prevHash
          Right (Just validBlk) -> continueOrStop (chunk, NotOrigin validBlk) chunk prevHash'
            where
              prevHash' = BlockHash (tipHash validBlk)

    -- | Validate the next chunk, unless the chunk just validated is the last
    -- chunk to validate. Cleanup files corresponding to chunks after the
    -- chunk in which we found the last valid block. Return that chunk and the
    -- tip corresponding to that block.
    continueOrStop ::
         (ChunkNo, WithOrigin (Tip blk))
      -> ChunkNo        -- ^ The chunk just validated
      -> ChainHash blk  -- ^ The hash of the last block of the previous chunk
      -> m (ChunkNo, WithOrigin (Tip blk))
    continueOrStop lastValid chunk prevHash
      | chunk < lastChunk
      = go lastValid (nextChunkNo chunk) prevHash
      | otherwise
      = assert (chunk == lastChunk) $ do
        -- Cleanup is only needed when the final chunk was empty, yet valid.
        cleanup lastValid chunk
        return lastValid

    -- | Remove left over files from chunks newer than the last chunk
    -- containing a valid file. Also unfinalise it if necessary.
    cleanup ::
         (ChunkNo, WithOrigin (Tip blk))  -- ^ The last valid chunk and tip
      -> ChunkNo  -- ^ The last validated chunk, could have been invalid or
                  -- empty
      -> m ()
    cleanup (lastValidChunk, tip) lastValidatedChunk = case tip of
      Origin ->
        removeFilesStartingFrom hasFS firstChunkNo
      NotOrigin _ -> do
        removeFilesStartingFrom hasFS (nextChunkNo lastValidChunk)
        when (lastValidChunk < lastValidatedChunk) $
          Primary.unfinalise (Proxy @blk) hasFS chunkInfo lastValidChunk

-- | Validate the given most recent chunk. If that chunk contains no valid
-- block, try the chunk before it, and so on. Stop as soon as an chunk with a
-- valid block is found, returning that chunk and the tip corresponding to
-- that block. If no valid blocks are found, chunk 0 and 'TipGen' is returned.
validateMostRecentChunk ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ConvertRawHash blk
     , HasCallStack
     )
  => ValidateEnv m blk h
  -> ChunkNo
     -- ^ Most recent chunk on disk, the chunk to validate
  -> m (ChunkNo, WithOrigin (Tip blk))
validateMostRecentChunk validateEnv@ValidateEnv { hasFS } = go
  where
    go :: ChunkNo -> m (ChunkNo, WithOrigin (Tip blk))
    go chunk = runExceptT
      (validateChunk validateEnv ShouldNotBeFinalised chunk Nothing) >>= \case
        Right (Just validBlk) -> do
            -- Found a valid block, we can stop now.
            removeFilesStartingFrom hasFS (nextChunkNo chunk)
            return (chunk, NotOrigin validBlk)
        _  -- This chunk file is unusable: either the chunk is empty or
           -- everything after it should be truncated.
          | Just chunk' <- prevChunkNo chunk -> go chunk'
          | otherwise -> do
            -- Found no valid blocks on disk.
            -- TODO be more precise in which cases we need which cleanup.
            removeFilesStartingFrom hasFS firstChunkNo
            return (firstChunkNo, Origin)

-- | Iff the chunk is the most recent chunk, it should not be finalised.
--
-- With finalising, we mean: if there are one or more empty slots at the end
-- of the chunk, the primary index should be padded with offsets to indicate
-- that these slots are empty. See 'Primary.backfill'.
data ShouldBeFinalised =
    ShouldBeFinalised
  | ShouldNotBeFinalised
  deriving (Show)

-- | Validate the given chunk
--
-- * Invalid or missing chunk files will cause truncation. All blocks after a
--   gap in blocks (due to a missing blocks or invalid block(s)) are
--   truncated.
--
-- * Chunk files are the main source of truth. Primary and secondary index
--   files can be reconstructed from the chunk files using the
--   'ChunkFileParser'. If index files are missing, corrupt, or do not match
--   the chunk files, they are overwritten.
--
-- * The 'ChunkFileParser' checks whether the hashes (header hash) line up
--   within an chunk. When they do not, we truncate the chunk, including the
--   block of which its previous hash does not match the hash of the previous
--   block.
--
-- * For each block, the 'ChunkFileParser' checks whether the checksum (and
--   other fields) from the secondary index file match the ones retrieved from
--   the actual block. If they do, the block has not been corrupted. If they
--   don't match or if the secondary index file is missing or corrupt, we have
--   to do the expensive integrity check of the block itself to determine
--   whether it is corrupt or not.
--
-- * This function checks whether the first block in the chunk fits onto the
--   last block of the previous chunk by checking the hashes. If they do not
--   fit, this chunk is truncated and @()@ is thrown.
--
-- * When an invalid block needs to be truncated, trailing empty slots are
--   also truncated so that the tip of the database will always point to a
--   valid block or EBB.
--
-- * All but the most recent chunk in the database should be finalised, i.e.
--   padded to the size of the chunk.
--
validateChunk ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , ConvertRawHash blk
     , HasCallStack
     )
  => ValidateEnv m blk h
  -> ShouldBeFinalised
  -> ChunkNo
  -> Maybe (ChainHash blk)
     -- ^ The hash of the last block of the previous chunk. 'Nothing' if
     -- unknown. When this is the first chunk, it should be 'Just Origin'.
  -> ExceptT () m (Maybe (Tip blk))
     -- ^ When non-empty, the 'Tip' corresponds to the last valid block in the
     -- chunk.
     --
     -- When the chunk file is missing or when we should truncate starting from
     -- this chunk because it doesn't fit onto the previous one, @()@ is thrown.
     --
     -- Note that when an invalid block is detected, we don't throw, but we
     -- truncate the chunk file. When validating the chunk file after it, we
     -- would notice it doesn't fit anymore, and then throw.
validateChunk ValidateEnv{..} shouldBeFinalised chunk mbPrevHash = do
    trace $ ValidatingChunk chunk
    chunkFileExists <- lift $ doesFileExist chunkFile
    unless chunkFileExists $ do
      trace $ MissingChunkFile chunk
      throwError ()

    -- Read the entries from the secondary index file, if it exists.
    secondaryIndexFileExists  <- lift $ doesFileExist secondaryIndexFile
    entriesFromSecondaryIndex <- lift $ if secondaryIndexFileExists
      then tryJust isInvalidFileError
        -- Note the 'maxBound': it is used to calculate the block size for
        -- each entry, but we don't care about block sizes here, so we use
        -- some dummy value.
        (Secondary.readAllEntries hasFS 0 chunk (const False) maxBound IsEBB) >>= \case
          Left _                -> do
            traceWith tracer $ InvalidSecondaryIndex chunk
            return []
          Right entriesFromFile ->
            return $ fixupEBB (map withoutBlockSize entriesFromFile)
      else do
        traceWith tracer $ MissingSecondaryIndex chunk
        return []

    -- Parse the chunk file using the checksums from the secondary index file
    -- as input. If the checksums match, the parser doesn't have to do the
    -- expensive integrity check of a block.
    let expectedChecksums = map Secondary.checksum entriesFromSecondaryIndex
    (entriesWithPrevHashes, mbErr) <- lift $
        parseChunkFile
          codecConfig
          hasFS
          checkIntegrity
          chunkFile
          expectedChecksums
          (\entries -> (\(es :> mbErr) -> (es, mbErr)) <$> S.toList entries)

    -- Check whether the first block of this chunk fits onto the last block of
    -- the previous chunk.
    case entriesWithPrevHashes of
      (_, actualPrevHash) : _
        | Just expectedPrevHash <- mbPrevHash
        , expectedPrevHash /= actualPrevHash
          -- The previous hash of the first block in the chunk does not match
          -- the hash of the last block of the previous chunk. There must be a
          -- gap. This chunk should be truncated.
        -> do
          trace $ ChunkFileDoesntFit expectedPrevHash actualPrevHash
          throwError ()
      _ -> return ()

    lift $ do

      -- If the parser returneds a deserialisation error, truncate the chunk
      -- file. Don't truncate the database just yet, because the
      -- deserialisation error may be due to some extra random bytes that
      -- shouldn't have been there in the first place.
      whenJust mbErr $ \(parseErr, endOfLastValidBlock) -> do
        traceWith tracer $ InvalidChunkFile chunk parseErr
        withFile hasFS chunkFile (AppendMode AllowExisting) $ \eHnd ->
          hTruncate eHnd endOfLastValidBlock

      -- If the secondary index file is missing, parsing it failed, or it does
      -- not match the entries from the chunk file, overwrite it using those
      -- (truncate first).
      let summary = map fst entriesWithPrevHashes
          entries = map summaryEntry summary
      when (entriesFromSecondaryIndex /= entries ||
            not secondaryIndexFileExists) $ do
        traceWith tracer $ RewriteSecondaryIndex chunk
        Secondary.writeAllEntries hasFS chunk entries

      -- Reconstruct the primary index from the 'Secondary.Entry's.
      --
      -- Read the primary index file, if it is missing, parsing fails, or it
      -- does not match the reconstructed primary index, overwrite it using
      -- the reconstructed index (truncate first).
      let primaryIndex = reconstructPrimaryIndex
                           (Proxy @blk)
                           chunkInfo
                           shouldBeFinalised
                           chunk
                           (map Secondary.blockOrEBB entries)
      primaryIndexFileExists  <- doesFileExist primaryIndexFile
      primaryIndexFileMatches <- if primaryIndexFileExists
        then tryJust isInvalidFileError (Primary.load (Proxy @blk) hasFS chunk) >>= \case
          Left ()                    -> do
            traceWith tracer $ InvalidPrimaryIndex chunk
            return False
          Right primaryIndexFromFile ->
            return $ primaryIndexFromFile == primaryIndex
        else do
          traceWith tracer $ MissingPrimaryIndex chunk
          return False
      unless primaryIndexFileMatches $ do
        traceWith tracer $ RewritePrimaryIndex chunk
        Primary.write hasFS chunk primaryIndex

      return $ summaryToTipInfo <$> lastMaybe summary
  where
    chunkFile          = fsPathChunkFile          chunk
    primaryIndexFile   = fsPathPrimaryIndexFile   chunk
    secondaryIndexFile = fsPathSecondaryIndexFile chunk

    HasFS { hTruncate, doesFileExist } = hasFS

    trace = lift . traceWith tracer

    summaryToTipInfo :: BlockSummary blk -> Tip blk
    summaryToTipInfo BlockSummary {..} = Tip {
          tipSlotNo  = summarySlotNo
        , tipIsEBB   = isBlockOrEBB $ Secondary.blockOrEBB summaryEntry
        , tipBlockNo = summaryBlockNo
        , tipHash    = Secondary.headerHash summaryEntry
        }

    -- | 'InvalidFileError' is the only error that can be thrown while loading
    -- a primary or a secondary index file
    isInvalidFileError :: ImmutableDBError blk -> Maybe ()
    isInvalidFileError = \case
      UnexpectedFailure (InvalidFileError {}) -> Just ()
      _                                       -> Nothing

    -- | When reading the entries from the secondary index file, we need to
    -- pass in a value of type 'IsEBB' so we know whether the first entry
    -- corresponds to an EBB or a regular block. We need this information to
    -- correctly interpret the deserialised 'Word64' as a 'BlockOrEBB': if
    -- it's an EBB, it's the 'EpochNo' ('Word64'), if it's a regular block,
    -- it's a 'SlotNo' ('Word64').
    --
    -- However, at the point we are reading the secondary index file, we don't
    -- yet know whether the first block will be an EBB or a regular block. We
    -- will find that out when we read the actual block from the chunk file.
    --
    -- Fortunately, we can make a /very/ good guess: if the 'Word64' of the
    -- 'BlockOrEBB' matches the chunk number, it is almost certainly an EBB,
    -- as the slot numbers increase @10k@ times faster than chunk numbers
    -- (remember that for EBBs, chunk numbers and epoch numbers must line up).
    -- Property: for every chunk @e > 0@, for all slot numbers @s@ in chunk
    -- @e@ we have @s > e@. The only exception is chunk 0, which contains a
    -- slot number 0. From this follows that it's an EBB if and only if the
    -- 'Word64' matches the chunk number.
    --
    -- E.g., the first slot number in chunk 1 will be 21600 if @k = 2160@. We
    -- could only make the wrong guess in the first very first chunk, i.e.,
    -- chunk 0, as the first slot number is also 0. However, we know that the
    -- real blockchain starts with an EBB, so even in that case we're fine.
    --
    -- If the chunk size were 1, then we would make the wrong guess for each
    -- chunk that contains an EBB, which is a rather unrealistic scenario.
    --
    -- Note that even making the wrong guess is not a problem. The (CRC)
    -- checksums are the only thing we extract from the secondary index file.
    -- These are passed to the 'ChunkFileParser'. We then reconstruct the
    -- secondary index using the output of the 'ChunkFileParser'. If that
    -- output doesn't match the parsed secondary index file, we will overwrite
    -- the secondary index file.
    --
    -- So the only thing that wouldn't go according to plan is that we will
    -- needlessly overwrite the secondary index file.
    fixupEBB :: forall hash. [Secondary.Entry hash] -> [Secondary.Entry hash]
    fixupEBB = \case
      entry@Secondary.Entry { blockOrEBB = EBB epoch' }:rest
        | let chunk' = unsafeEpochNoToChunkNo epoch'
        , chunk' /= chunk
        -> entry { Secondary.blockOrEBB = Block (SlotNo (unChunkNo chunk')) }:rest
      entries -> entries

-- | Reconstruct a 'PrimaryIndex' based on a list of 'Secondary.Entry's.
reconstructPrimaryIndex ::
     forall blk. (ConvertRawHash blk, HasCallStack)
  => Proxy blk
  -> ChunkInfo
  -> ShouldBeFinalised
  -> ChunkNo
  -> [BlockOrEBB]
  -> PrimaryIndex
reconstructPrimaryIndex pb chunkInfo shouldBeFinalised chunk blockOrEBBs =
    fromMaybe (error nonIncreasing) $
      Primary.mk chunk . (0:) $
        go (NextRelativeSlot (firstBlockOrEBB chunkInfo chunk)) 0 $
          map (chunkRelative . chunkSlotForBlockOrEBB chunkInfo) blockOrEBBs
  where
    nonIncreasing :: String
    nonIncreasing = "blocks have non-increasing slot numbers"

    go :: HasCallStack
       => NextRelativeSlot
       -> SecondaryOffset
       -> [RelativeSlot]
       -> [SecondaryOffset]
    go expected lastSecondaryOffset relSlots =
        case (expected, relSlots) of
          (_, []) ->
            case shouldBeFinalised of
              ShouldNotBeFinalised -> []
              ShouldBeFinalised    -> Primary.backfillChunk
                                        chunkInfo
                                        chunk
                                        expected
                                        lastSecondaryOffset
          (NoMoreRelativeSlots, _) ->
            -- Assumption: when we validate the chunk file, we check its size
            error "reconstructPrimaryIndex: too many entries"
          (NextRelativeSlot nextExpectedRelSlot, relSlot:relSlots') ->
            if compareRelativeSlot relSlot nextExpectedRelSlot == LT then
              error nonIncreasing
            else
              let backfilled      = Primary.backfill
                                      relSlot
                                      nextExpectedRelSlot
                                      lastSecondaryOffset
                  secondaryOffset = lastSecondaryOffset
                                  + Secondary.entrySize pb
              in backfilled ++ secondaryOffset
               : go (nextRelativeSlot relSlot) secondaryOffset relSlots'


{------------------------------------------------------------------------------
  Migration
------------------------------------------------------------------------------}

-- | Migrate the files in the database to the latest version.
--
-- We always migrate the database to the latest version before opening it. If
-- a migration was unsuccessful, an error is thrown and the database is not
-- opened. User intervention will be needed before the database can be
-- reopened, as without it, the same error will be thrown when reopening the
-- database the next time.
--
-- For example, when during a migration we have to rename a file A to B, but
-- we don't have permissions to do so, we require user intervention.
--
-- We have the following versions, from current to oldest:
--
-- * Current version:
--
--   - Chunk files are named "XXXXX.chunk" where "XXXXX" is the chunk/epoch
--     number padded with zeroes to five decimals. A chunk file stores the
--     blocks in that chunk sequentially. Empty slots are skipped.
--
--   - Primary index files are named "XXXXX.primary". See 'PrimaryIndex' for
--     more information.
--
--   - Secondary index files are named "XXXXX.secondary". See
--     'Secondary.Entry' for more information.
--
-- * The only difference with the version after it was that chunk files were
--   named "XXXXX.epoch" instead of "XXXXX.chunk". The contents of all files
--   remain identical because we chose the chunk size to be equal to the Byron
--   epoch size and allowed EBBs in the chunk.
--
-- We don't include versions before the first release, as we don't have to
-- migrate from them.
--
-- Note that primary index files also contain a version number, but since the
-- binary format hasn't changed yet, this version number hasn't been changed
-- yet.
--
-- Implementation note: as currently the sole migration we need to be able to
-- perform only requires renaming files, we keep it simple for now.
migrate :: (IOLike m, HasCallStack) => ValidateEnv m blk h -> m ()
migrate ValidateEnv { hasFS, tracer } = do
    filesInDBFolder <- listDirectory (mkFsPath [])
    -- Any old "XXXXX.epoch" files
    let epochFileChunkNos :: [(FsPath, ChunkNo)]
        epochFileChunkNos =
          mapMaybe
            (\file -> (mkFsPath [file],) <$> isEpochFile file)
            (Set.toAscList filesInDBFolder)

    unless (null epochFileChunkNos) $ do
      traceWith tracer $ Migrating ".epoch files to .chunk files"
      forM_ epochFileChunkNos $ \(epochFile, chunk) ->
        renameFile epochFile (fsPathChunkFile chunk)
  where
    HasFS { listDirectory, renameFile } = hasFS

    isEpochFile :: String -> Maybe ChunkNo
    isEpochFile s = case parseDBFile s of
      Just (prefix, chunk)
        | prefix == "epoch"
        -> Just chunk
      _ -> Nothing
