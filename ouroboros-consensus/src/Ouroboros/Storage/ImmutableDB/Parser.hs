{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
-- | The ImmutableDB doesn't care about the serialisation format, but in
-- practice we use CBOR. If we were to change the serialisation format, we
-- would have to write a new 'EpochFileParser' implementation, but the rest of
-- the ImmutableDB would be unaffected.
module Ouroboros.Storage.ImmutableDB.Parser
  ( -- * EpochFileParser
    EpochFileError (..)
  , epochFileParser
  , epochFileParser'
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<&>))
import           Data.Word (Word64)
import           Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.CRC

import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Types

{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}

data EpochFileError hash =
    EpochErrRead Util.CBOR.ReadIncrementalErr

    -- | The previous hash of a block did not match the hash of the previous
    -- block.
  | EpochErrHashMismatch
      (WithOrigin hash)  -- ^ The hash of the previous block
      (WithOrigin hash)  -- ^ The previous hash of the block

    -- | The integrity verification of the block with the given hash and
    -- 'BlockOrEBB' number returned 'False', indicating that the block got
    -- corrupted.
  | EpochErrCorrupt hash BlockOrEBB
  deriving (Eq, Show)

epochFileParser'
  :: forall m blk hash h. (IOLike m, Eq hash)
  => (blk -> SlotNo)
  -> (blk -> hash)
  -> (blk -> WithOrigin hash)  -- ^ Previous hash
  -> HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)    -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> (blk -> Bool)             -- ^ Check integrity of the block. 'False' =
                               -- corrupt.
  -> EpochFileParser
       (EpochFileError hash)
       m
       (Secondary.Entry hash)
       hash
epochFileParser' getSlotNo getHash getPrevHash hasFS decodeBlock isEBB
                 getBinaryInfo isNotCorrupt =
    EpochFileParser $ \fsPath expectedChecksums k ->
      Util.CBOR.withStreamIncrementalOffsets hasFS decoder fsPath
        (k . checkIfHashesLineUp . checkEntries expectedChecksums)
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
                (Maybe (Util.CBOR.ReadIncrementalErr, Word64))
         -- ^ Input stream of blocks (with additional info)
      -> Stream (Of (Secondary.Entry hash, WithOrigin hash))
                m
                (Maybe (EpochFileError hash, Word64))
    checkEntries = go
      where
        go expected blkAndInfos = S.lift (S.next blkAndInfos) >>= \case
          -- No more blocks, but maybe some expected entries. We ignore them.
          Left mbErr -> return $ first EpochErrRead <$> mbErr
          -- A block
          Right (blkAndInfo@(offset, (_, (blk, checksum))), blkAndInfos') ->
              case expected of
                expectedChecksum:expected'
                  | expectedChecksum == checksum
                  -> S.yield entryAndPrevHash *> go expected' blkAndInfos'
                -- No expected entry or a mismatch
                _ | isNotCorrupt blk
                    -- The (expensive) integrity check passed, so continue
                  -> S.yield entryAndPrevHash *> go (drop 1 expected) blkAndInfos'
                  | otherwise
                    -- The block is corrupt, stop
                  -> return $ Just (EpochErrCorrupt headerHash blockOrEBB, offset)
            where
              entryAndPrevHash@(actualEntry, _) =
                entryForBlockAndInfo blkAndInfo
              Secondary.Entry { headerHash, blockOrEBB } = actualEntry

    entryForBlockAndInfo
      :: (Word64, (Word64, (blk, CRC)))
      -> (Secondary.Entry hash, WithOrigin hash)
    entryForBlockAndInfo (offset, (_size, (blk, checksum))) = (entry, prevHash)
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
      :: Stream (Of (Secondary.Entry hash, WithOrigin hash))
                m
                (Maybe (EpochFileError hash, Word64))
      -> Stream (Of (Secondary.Entry hash, WithOrigin hash))
                m
                (Maybe (EpochFileError hash, Word64))
    checkIfHashesLineUp = \input -> S.lift (S.next input) >>= \case
        Left mbErr ->
          return mbErr
        Right ((entry, prevHash), input') ->
          S.yield (entry, prevHash) *>
          go (At (Secondary.headerHash entry)) input'
      where
        -- Loop invariant: the @hashOfPrevBlock@ is the hash of the most
        -- recently checked block.
        go hashOfPrevBlock input = S.lift (S.next input) >>= \case
          Left mbErr
            -> return mbErr
          Right ((entry, prevHash), input')
            | prevHash == hashOfPrevBlock
            -> S.yield (entry, prevHash) *>
               go (At (Secondary.headerHash entry)) input'
            | otherwise
            -> let err = EpochErrHashMismatch hashOfPrevBlock prevHash
                   offset = Secondary.unBlockOffset $ Secondary.blockOffset entry
               in return $ Just (err, offset)

-- | A version of 'epochFileParser'' for blocks that implement 'HasHeader'.
epochFileParser
  :: forall m blk h. (IOLike m, HasHeader blk)
  => HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)  -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> (blk -> Bool)           -- ^ Check integrity of the block. 'False' =
                             -- corrupt.
  -> EpochFileParser
       (EpochFileError (HeaderHash blk))
       m
       (Secondary.Entry (HeaderHash blk))
       (HeaderHash blk)
epochFileParser =
    epochFileParser' blockSlot blockHash (convertPrevHash . blockPrevHash)
  where
    convertPrevHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
    convertPrevHash GenesisHash   = Origin
    convertPrevHash (BlockHash h) = At h
