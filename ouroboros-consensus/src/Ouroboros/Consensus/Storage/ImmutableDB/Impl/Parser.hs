{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeFamilies             #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Parser (
    BlockSummary (..)
  , ChunkFileError (..)
  , parseChunkFile
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor ((<&>))
import           Data.Word (Word64)
import           Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

import           Ouroboros.Consensus.Block hiding (headerHash)
import           Ouroboros.Consensus.Util.CBOR (withStreamIncrementalOffsets)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import           Ouroboros.Consensus.Storage.FS.CRC

import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..),
                     HasBinaryBlockInfo (..))

-- | Information about a block returned by the parser.
--
-- The fields of this record are strict to make sure that by evaluating this
-- record to WHNF, we no longer hold on to the entire block. Otherwise, we might
-- accidentally keep all blocks in a single file in memory during parsing.
data BlockSummary blk = BlockSummary {
      summaryEntry   :: !(Secondary.Entry blk)
    , summaryBlockNo :: !BlockNo
    , summarySlotNo  :: !SlotNo
    }

-- | Parse the contents of a chunk file.
--
-- * The parser decodes each block in the chunk. When one of them fails to
--   decode, a 'ChunkErrRead' error is returned.
--
-- * Each block's checksum is checked against its given expected checksum
--   (coming from the secondary index). When a checksum doesn't match, a
--   'ChunkErrCorrupt' error is returned. When the secondary index is missing or
--   corrupt, and there are no or fewer expected checksums, we use the given
--   (more expensive) integrity checking function instead of checksum
--   comparison.
--
-- * We check that each block fits onto the previous one by checking the hashes.
--   If not, we return a 'ChunkErrHashMismatch' error.
--
-- * An error is returned in the form of:
--
--   > 'Maybe' ('ChunkFileError' blk, 'Word64')
--
--   The 'Word64' corresponds to the offset in the file where the last valid
--   entry ends. Truncating to this offset will remove all invalid data from the
--   file and just leave the valid entries before it. Note that we are not using
--   'Either' because the error might occur after some valid entries have been
--   parsed successfully, in which case we still want these valid entries, but
--   also want to know about the error so we can truncate the file to get rid of
--   the unparseable data.
--
parseChunkFile ::
     forall m blk h r.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     )
  => CodecConfig blk
  -> HasFS m h
  -> (blk -> Bool)   -- ^ Check integrity of the block. 'False' = corrupt.
  -> FsPath
  -> [CRC]
  -> (   Stream (Of (BlockSummary blk, ChainHash blk))
                m
                (Maybe (ChunkFileError blk, Word64))
      -> m r
     )
  -> m r
parseChunkFile ccfg hasFS isNotCorrupt fsPath expectedChecksums k =
      withStreamIncrementalOffsets hasFS decoder fsPath
        ( k
        . checkIfHashesLineUp
        . checkEntries expectedChecksums
        . fmap (fmap (first ChunkErrRead))
        )
  where
    decoder :: forall s. Decoder s (Lazy.ByteString -> (blk, CRC))
    decoder = decodeDisk ccfg <&> \mkBlk bs ->
      let !blk      = mkBlk bs
          !checksum = computeCRC bs
      in (blk, checksum)

    -- | Go over the expected checksums and blocks in parallel. Stop with an
    -- error when a block is corrupt. Yield correct entries along the way.
    --
    -- If there's an expected checksum and it matches the block's checksum,
    -- then the block is correct. Continue with the next.
    --
    -- If they do not match or if there's no expected checksum in the stream,
    -- check the integrity of the block (expensive). When corrupt, stop
    -- parsing blocks and return an error that the block is corrupt. When not
    -- corrupt, continue with the next.
    checkEntries
      :: [CRC]
         -- ^ Expected checksums
      -> Stream (Of (Word64, (Word64, (blk, CRC))))
                m
                (Maybe (ChunkFileError blk, Word64))
         -- ^ Input stream of blocks (with additional info)
      -> Stream (Of (BlockSummary blk, ChainHash blk))
                m
                (Maybe (ChunkFileError blk, Word64))
    checkEntries = \expected -> mapAccumS expected updateAcc
      where
        updateAcc
          :: [CRC]
          -> (Word64, (Word64, (blk, CRC)))
          -> Either (Maybe (ChunkFileError blk, Word64))
                    ( (BlockSummary blk, ChainHash blk)
                    , [CRC]
                    )
        updateAcc expected blkAndInfo@(offset, (_, (blk, checksum))) =
            case expected of
              expectedChecksum:expected'
                | expectedChecksum == checksum
                -> Right (entryAndPrevHash, expected')
              -- No expected entry or a mismatch
              _ | isNotCorrupt blk
                  -- The (expensive) integrity check passed, so continue
                -> Right (entryAndPrevHash, drop 1 expected)
                | otherwise
                  -- The block is corrupt, stop
                -> Left $ Just (ChunkErrCorrupt (blockPoint blk), offset)
          where
            entryAndPrevHash = entryForBlockAndInfo blkAndInfo

    entryForBlockAndInfo
      :: (Word64, (Word64, (blk, CRC)))
      -> (BlockSummary blk, ChainHash blk)
    entryForBlockAndInfo (offset, (_size, (blk, checksum))) =
        (blockSummary, prevHash)
      where
        -- Don't accidentally hold on to the block!
        !prevHash = blockPrevHash blk
        entry = Secondary.Entry {
              blockOffset  = Secondary.BlockOffset  offset
            , headerOffset = Secondary.HeaderOffset headerOffset
            , headerSize   = Secondary.HeaderSize   headerSize
            , checksum     = checksum
            , headerHash   = blockHash blk
            , blockOrEBB   = case blockIsEBB blk of
                Just epoch -> EBB epoch
                Nothing    -> Block (blockSlot blk)
            }
        !blockSummary = BlockSummary {
              summaryEntry   = entry
            , summaryBlockNo = blockNo blk
            , summarySlotNo  = blockSlot blk

          }
        BinaryBlockInfo { headerOffset, headerSize } = getBinaryBlockInfo blk


    checkIfHashesLineUp
      :: Stream (Of (BlockSummary blk, ChainHash blk))
                m
                (Maybe (ChunkFileError blk, Word64))
      -> Stream (Of (BlockSummary blk, ChainHash blk))
                m
                (Maybe (ChunkFileError blk, Word64))
    checkIfHashesLineUp = mapAccumS0 checkFirst checkNext
      where
        -- We pass the hash of the previous block around as the state (@s@).
        checkFirst x@(BlockSummary { summaryEntry }, _) =
            Right (x, Secondary.headerHash summaryEntry)

        checkNext hashOfPrevBlock x@(BlockSummary { summaryEntry }, prevHash)
          | prevHash == BlockHash hashOfPrevBlock
          = Right (x, Secondary.headerHash summaryEntry)
          | otherwise
          = Left (Just (err, offset))
            where
              err = ChunkErrHashMismatch hashOfPrevBlock prevHash
              offset = Secondary.unBlockOffset $ Secondary.blockOffset summaryEntry

{-------------------------------------------------------------------------------
  Streaming utilities
-------------------------------------------------------------------------------}

-- | Thread some state through a 'Stream'. An early return is possible by
-- returning 'Left'.
mapAccumS
  :: Monad m
  => s  -- ^ Initial state
  -> (s -> a -> Either r (b, s))
  -> Stream (Of a) m r
  -> Stream (Of b) m r
mapAccumS st0 updateAcc = go st0
  where
    go st input = S.lift (S.next input) >>= \case
      Left  r           -> return r
      Right (a, input') -> case updateAcc st a of
        Left r         -> return r
        Right (b, st') -> S.yield b *> go st' input'

-- | Variant of 'mapAccumS' that calls the first function argument on the
-- first element in the stream to construct the initial state. For all
-- elements in the stream after the first one, the second function argument is
-- used.
mapAccumS0
  :: forall m a b r s. Monad m
  => (a -> Either r (b, s))
  -> (s -> a -> Either r (b, s))
  -> Stream (Of a) m r
  -> Stream (Of b) m r
mapAccumS0 initAcc updateAcc = mapAccumS Nothing updateAcc'
  where
    updateAcc' :: Maybe s -> a -> Either r (b, Maybe s)
    updateAcc' mbSt = fmap (fmap Just) . maybe initAcc updateAcc mbSt
