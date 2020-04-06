{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
-- | The ImmutableDB doesn't care about the serialisation format, but in
-- practice we use CBOR. If we were to change the serialisation format, we
-- would have to write a new 'ChunkFileParser' implementation, but the rest of
-- the ImmutableDB would be unaffected.
module Ouroboros.Consensus.Storage.ImmutableDB.Parser
  ( -- * ChunkFileParser
    ChunkFileError (..)
  , BlockSummary(..)
  , chunkFileParser
  , chunkFileParser'
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<&>))
import           Data.Word (Word64)
import           Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import           Ouroboros.Consensus.Storage.FS.CRC

import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Types

{-------------------------------------------------------------------------------
  ChunkFileParser
-------------------------------------------------------------------------------}

data ChunkFileError hash =
    ChunkErrRead Util.CBOR.ReadIncrementalErr

    -- | The previous hash of a block did not match the hash of the previous
    -- block.
  | ChunkErrHashMismatch
      (WithOrigin hash)  -- ^ The hash of the previous block
      (WithOrigin hash)  -- ^ The previous hash of the block

    -- | The integrity verification of the block with the given hash and
    -- 'BlockOrEBB' number returned 'False', indicating that the block got
    -- corrupted.
  | ChunkErrCorrupt hash BlockOrEBB
  deriving (Eq, Show)

-- | Information about a block returned by the parser
data BlockSummary hash = BlockSummary {
      summaryEntry   :: !(Secondary.Entry hash)
    , summaryBlockNo :: !BlockNo
    }

chunkFileParser'
  :: forall m blk hash h. (IOLike m, Eq hash)
  => (blk -> SlotNo)
  -> (blk -> BlockNo)
  -> (blk -> hash)
  -> (blk -> WithOrigin hash)  -- ^ Previous hash
  -> HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)    -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> (blk -> Bool)             -- ^ Check integrity of the block. 'False' =
                               -- corrupt.
  -> ChunkFileParser
       (ChunkFileError hash)
       m
       (BlockSummary hash)
       hash
chunkFileParser' getSlotNo getBlockNo getHash getPrevHash hasFS decodeBlock isEBB
                 getBinaryInfo isNotCorrupt =
    ChunkFileParser $ \fsPath expectedChecksums k ->
      Util.CBOR.withStreamIncrementalOffsets hasFS decoder fsPath
        ( k
        . checkIfHashesLineUp
        . checkEntries expectedChecksums
        . fmap (fmap (first ChunkErrRead))
        )
  where
    decoder :: forall s. Decoder s (BL.ByteString -> (blk, CRC))
    decoder = decodeBlock <&> \mkBlk bs ->
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
                (Maybe (ChunkFileError hash, Word64))
         -- ^ Input stream of blocks (with additional info)
      -> Stream (Of (BlockSummary hash, WithOrigin hash))
                m
                (Maybe (ChunkFileError hash, Word64))
    checkEntries = \expected -> mapAccumS expected updateAcc
      where
        updateAcc
          :: [CRC]
          -> (Word64, (Word64, (blk, CRC)))
          -> Either (Maybe (ChunkFileError hash, Word64))
                    ( (BlockSummary hash, WithOrigin hash)
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
                -> Left $ Just (ChunkErrCorrupt headerHash blockOrEBB, offset)
          where
            entryAndPrevHash@(BlockSummary actualEntry _, _) =
              entryForBlockAndInfo blkAndInfo
            Secondary.Entry { headerHash, blockOrEBB } = actualEntry

    entryForBlockAndInfo
      :: (Word64, (Word64, (blk, CRC)))
      -> (BlockSummary hash, WithOrigin hash)
    entryForBlockAndInfo (offset, (_size, (blk, checksum))) =
        (BlockSummary entry (getBlockNo blk), prevHash)
      where
        -- Don't accidentally hold on to the block!
        !prevHash = getPrevHash blk
        !entry    = Secondary.Entry
          { blockOffset  = Secondary.BlockOffset  offset
          , headerOffset = Secondary.HeaderOffset headerOffset
          , headerSize   = Secondary.HeaderSize   headerSize
          , checksum     = checksum
          , headerHash   = getHash blk
          , blockOrEBB   = case isEBB blk of
              Just epoch -> EBB epoch
              Nothing    -> Block (getSlotNo blk)
          }
        BinaryInfo { headerOffset, headerSize } = getBinaryInfo blk

    checkIfHashesLineUp
      :: Stream (Of (BlockSummary hash, WithOrigin hash))
                m
                (Maybe (ChunkFileError hash, Word64))
      -> Stream (Of (BlockSummary hash, WithOrigin hash))
                m
                (Maybe (ChunkFileError hash, Word64))
    checkIfHashesLineUp = mapAccumS0 checkFirst checkNext
      where
        -- We pass the hash of the previous block around as the state (@s@).
        checkFirst x@(BlockSummary entry _, _) =
            Right (x, Secondary.headerHash entry)

        checkNext hashOfPrevBlock x@(BlockSummary entry _blockNo, prevHash)
          | prevHash == At hashOfPrevBlock
          = Right (x, Secondary.headerHash entry)
          | otherwise
          = Left (Just (err, offset))
            where
              err = ChunkErrHashMismatch (At hashOfPrevBlock) prevHash
              offset = Secondary.unBlockOffset $ Secondary.blockOffset entry

-- | A version of 'chunkFileParser'' for blocks that implement 'HasHeader'.
chunkFileParser
  :: forall m blk h. (IOLike m, HasHeader blk)
  => HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)  -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> (blk -> Bool)           -- ^ Check integrity of the block. 'False' =
                             -- corrupt.
  -> ChunkFileParser
       (ChunkFileError (HeaderHash blk))
       m
       (BlockSummary (HeaderHash blk))
       (HeaderHash blk)
chunkFileParser =
    chunkFileParser' blockSlot blockNo blockHash (convertPrevHash . blockPrevHash)
  where
    convertPrevHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
    convertPrevHash GenesisHash   = Origin
    convertPrevHash (BlockHash h) = At h

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
