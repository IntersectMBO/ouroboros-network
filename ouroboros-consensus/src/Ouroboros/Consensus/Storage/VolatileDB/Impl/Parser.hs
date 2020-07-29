{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser (
    Parser (..)
  , ParsedBlockInfo (..)
  , ParseError (..)
  , blockFileParser
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

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import           Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types

{------------------------------------------------------------------------------
  API
------------------------------------------------------------------------------}

-- | Parse the given file containing blocks.
--
-- Return the 'ParsedBlockInfo' for all the valid blocks in the file. Stop
-- when encountering an error and include the offset to truncate to.
newtype Parser m blk = Parser {
    parse :: FsPath
          -> m ( [ParsedBlockInfo blk]
               , Maybe (ParseError blk, BlockOffset)
               )
  }

-- | Information returned by the parser about a single block.
--
-- The parser returns for each block, its offset, its size and its 'BlockInfo'
data ParsedBlockInfo blk = ParsedBlockInfo {
      pbiBlockOffset :: !BlockOffset
    , pbiBlockSize   :: !BlockSize
    , pbiBlockInfo   :: !(BlockInfo blk)
    , pbiNestedCtxt  :: !(SomeBlock (NestedCtxt Header) blk)
    }

{------------------------------------------------------------------------------
  Implementation
------------------------------------------------------------------------------}

blockFileParser ::
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
  -> Parser m blk
blockFileParser ccfg hasFS isNotCorrupt validationPolicy =
    Parser $ \fsPath ->
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
        -> let !blockInfo = extractBlockInfo ccfg blk
               !newParsed = ParsedBlockInfo  {
                   pbiBlockOffset = BlockOffset offset
                 , pbiBlockSize   = BlockSize $ fromIntegral size
                 , pbiBlockInfo   = blockInfo
                 , pbiNestedCtxt  = case unnest (getHeader blk) of
                                      DepPair nestedCtxt _ -> SomeBlock nestedCtxt
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
  => CodecConfig blk
  -> blk
  -> BlockInfo blk
extractBlockInfo ccfg blk = BlockInfo {
      biHash         = blockHash          blk
    , biSlotNo       = blockSlot          blk
    , biBlockNo      = blockNo            blk
    , biIsEBB        = blockToIsEBB       blk
    , biPrevHash     = blockPrevHash ccfg blk
    , biHeaderOffset = headerOffset
    , biHeaderSize   = headerSize
    }
  where
    BinaryBlockInfo { headerOffset, headerSize } = getBinaryBlockInfo blk
