{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser (
    ParseError (..)
  , ParsedBlockInfo (..)
  , parseBlockFile
    -- * Auxiliary
  , extractBlockInfo
  ) where

import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word (Word64)
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr (..),
                     withStreamIncrementalOffsets)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types

-- | Information returned by the parser about a single block.
--
-- The parser returns for each block, its offset, its size and its 'BlockInfo'
--
-- The fields of this record are strict to make sure that by evaluating this
-- record to WHNF, we no longer hold on to the entire block. Otherwise, we might
-- accidentally keep all blocks in a single file in memory during parsing.
data ParsedBlockInfo blk = ParsedBlockInfo {
      pbiBlockOffset :: !BlockOffset
    , pbiBlockSize   :: !BlockSize
    , pbiBlockInfo   :: !(BlockInfo blk)
    , pbiNestedCtxt  :: !(SomeSecond (NestedCtxt Header) blk)
    }

-- | Parse the given file containing blocks.
--
-- Return the 'ParsedBlockInfo' for all the valid blocks in the file. Stop
-- when encountering an error and include the offset to truncate to.
parseBlockFile ::
     forall m blk h.
     ( IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , HasNestedContent Header blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     )
  => CodecConfig blk
  -> HasFS m h
  -> (blk -> Bool)
  -> BlockValidationPolicy
  -> FsPath
  -> m ( [ParsedBlockInfo blk]
       , Maybe (ParseError blk, BlockOffset)
       )
parseBlockFile ccfg hasFS isNotCorrupt validationPolicy fsPath =
    withStreamIncrementalOffsets hasFS (decodeDisk ccfg) fsPath $
      checkEntries []
  where
    noValidation :: Bool
    noValidation = validationPolicy == NoValidation

    checkEntries ::
         [ParsedBlockInfo blk]
      -> Stream (Of (Word64, (Word64, blk)))
                m
                (Maybe (ReadIncrementalErr, Word64))
      -> m ( [ParsedBlockInfo blk]
           , Maybe (ParseError blk, BlockOffset)
           )
    checkEntries parsed stream = S.next stream >>= \case
      Left mbErr
        -> return (reverse parsed, bimap BlockReadErr BlockOffset <$> mbErr)
      Right ((offset, (size, blk)), stream')
        | noValidation || isNotCorrupt blk
        -> let !blockInfo = extractBlockInfo blk
               !newParsed = ParsedBlockInfo  {
                   pbiBlockOffset = BlockOffset offset
                 , pbiBlockSize   = BlockSize $ fromIntegral size
                 , pbiBlockInfo   = blockInfo
                 , pbiNestedCtxt  = case unnest (getHeader blk) of
                                      DepPair nestedCtxt _ -> SomeSecond nestedCtxt
                 }
           in checkEntries (newParsed : parsed) stream'
        | otherwise  -- The block was invalid
        -> let !hash = blockHash blk
           in return ( reverse parsed
                     , Just (BlockCorruptedErr hash, BlockOffset offset)
                     )

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractBlockInfo ::
     (GetPrevHash blk, HasBinaryBlockInfo blk)
  => blk
  -> BlockInfo blk
extractBlockInfo blk = BlockInfo {
      biHash         = blockHash     blk
    , biSlotNo       = blockSlot     blk
    , biBlockNo      = blockNo       blk
    , biIsEBB        = blockToIsEBB  blk
    , biPrevHash     = blockPrevHash blk
    , biHeaderOffset = headerOffset
    , biHeaderSize   = headerSize
    }
  where
    BinaryBlockInfo { headerOffset, headerSize } = getBinaryBlockInfo blk
