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
import           Data.Functor (($>), (<&>))
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

-- | Type used internally in 'epochFileParser''.
data BlockInfo hash = BlockInfo
  { hash         :: !hash
  , prevHash     :: !(WithOrigin hash)
  , checksum     :: !CRC
  , blockOrEBB   :: !BlockOrEBB
  , notCorrupted :: !Bool
  }

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
       (Secondary.Entry hash, WithOrigin hash)
epochFileParser' getSlotNo getHash getPrevHash hasFS decodeBlock isEBB
                 getBinaryInfo isNotCorrupt =
    EpochFileParser $ \fsPath k ->
      Util.CBOR.withStreamIncrementalOffsets hasFS decoder fsPath
        (k . checkIfHashesLineUp . S.map extractEntry . stopAtCorruption)
  where
    decoder :: forall s. Decoder s (BL.ByteString -> BinaryInfo (BlockInfo hash))
    decoder = extractBlockInfo <$> decodeBlock

    -- | It is important that we don't first parse all blocks, storing them
    -- all in memory, and only /then/ extract the information we need. So
    -- make sure we don't create thunks refering to the whole block.
    extractBlockInfo :: (BL.ByteString -> blk)
                     -> (BL.ByteString -> BinaryInfo (BlockInfo hash))
    extractBlockInfo f bs =
        getBinaryInfo blk $> BlockInfo
          { hash         = getHash blk
          , prevHash     = getPrevHash blk
          , checksum     = computeCRC bs
          , blockOrEBB   = case isEBB blk of
            Just epoch -> EBB epoch
            Nothing    -> Block (getSlotNo blk)
          -- TODO do this only when the checksum doesn't match the known one
          , notCorrupted = isNotCorrupt blk
           }
      where
        blk = f bs

    stopAtCorruption
      :: Stream (Of (Word64, (Word64, BinaryInfo (BlockInfo hash))))
                m
                (Maybe (Util.CBOR.ReadIncrementalErr, Word64))
      -> Stream (Of (Word64, (Word64, BinaryInfo (BlockInfo hash))))
                m
                (Maybe (EpochFileError hash, Word64))
    stopAtCorruption input = do
      -- Stop streaming as soon as we encounter a corrupted one (or exhaust
      -- the stream)
      rest <- S.span
        (\(_, (_, BinaryInfo blockInfo _ _)) -> notCorrupted blockInfo)
        input
      S.lift (S.next rest) <&> \case
        Left mbErr ->
           first EpochErrRead <$> mbErr
        Right ((offset, (_, BinaryInfo blockInfo _ _)), _) ->
            Just (EpochErrCorrupt hash blockOrEBB, offset)
          where
            BlockInfo { hash, blockOrEBB } = blockInfo

    extractEntry
      :: (Word64, (Word64, BinaryInfo (BlockInfo hash)))
      -> (Secondary.Entry hash, WithOrigin hash)
    extractEntry (offset, (_size, blockInfo)) = (entry, prevHash)
      where
        BinaryInfo
          { binaryBlob = BlockInfo { hash, prevHash, checksum, blockOrEBB }
          , headerOffset
          , headerSize
          } = blockInfo
        entry = Secondary.Entry
          { blockOffset  = Secondary.BlockOffset  offset
          , headerOffset = Secondary.HeaderOffset headerOffset
          , headerSize   = Secondary.HeaderSize   headerSize
          , checksum
          , headerHash   = hash
          , blockOrEBB
          }

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
       (Secondary.Entry (HeaderHash blk), WithOrigin (HeaderHash blk))
epochFileParser =
    epochFileParser' blockSlot blockHash (convertPrevHash . blockPrevHash)
  where
    convertPrevHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
    convertPrevHash GenesisHash   = Origin
    convertPrevHash (BlockHash h) = At h
