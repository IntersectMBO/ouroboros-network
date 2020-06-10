{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.Storage.ChainDB.Impl.BlockComponent
  ( BlockComponent (..)
  , translateToRawDB
  , Parser
  , BlockOrHeader (..)
  ) where

import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Word (Word8)

import           Ouroboros.Network.Block (pattern BlockPoint, HeaderHash,
                     SlotNo)

import           Ouroboros.Consensus.Block (GetHeader (..), IsEBB (..))

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockRef (..),
                     ChainDB)
import           Ouroboros.Consensus.Storage.Common

-- | Translate a ChainDB 'BlockComponent' into a 'BlockComponent' known by the
-- ImmutableDB and the VolatileDB.
translateToRawDB
  :: forall m blk b db. DBHeaderHash db ~ HeaderHash blk
  => (forall b'. Parser m blk b')
  -> Word8
     -- ^ Length of the 'ShortByteString' below, returned by 'GetNestedCtxt'
  -> (ShortByteString -> SizeInBytes -> Lazy.ByteString -> Lazy.ByteString)
     -- ^ Add header envelope
  -> BlockComponent (ChainDB m blk) b
  -> BlockComponent db b
translateToRawDB parse prefixLen addHdrEnv = \case
    GetBlock        ->
      parse Block <$> getBlockRef <*> GetRawBlock
    GetRawBlock     -> GetRawBlock
    GetHeader       ->
      (\prefix blockSize blockRef bs ->
        parse Header blockRef (addHdrEnv prefix blockSize bs)) <$>
      GetNestedCtxt prefixLen <*> GetBlockSize <*> getBlockRef <*> GetRawHeader
    GetRawHeader    -> addHdrEnv <$> GetNestedCtxt prefixLen <*> GetBlockSize <*> GetRawHeader
    GetHash         -> GetHash
    GetSlot         -> GetSlot
    GetIsEBB        -> GetIsEBB
    GetBlockSize    -> GetBlockSize
    GetHeaderSize   -> GetHeaderSize
    GetNestedCtxt n -> GetNestedCtxt n
    GetPure a       -> GetPure a
    GetApply f bc   -> GetApply
      (translateToRawDB parse prefixLen addHdrEnv f)
      (translateToRawDB parse prefixLen addHdrEnv bc)

-- | Block or header parser
type Parser m blk b =
     BlockOrHeader blk b
  -> BlockRef blk
  -> Lazy.ByteString
  -> m b

-- | Either a block (@blk@) or a header (@'Header' blk@). Both have the same
-- @HeaderHash blk@.
data BlockOrHeader blk b where
  Block  :: BlockOrHeader blk blk
  Header :: BlockOrHeader blk (Header blk)

getBlockRef :: DBHeaderHash db ~ HeaderHash blk
            => BlockComponent db (BlockRef blk)
getBlockRef = mkBlockRef <$> GetSlot <*> GetHash <*> GetIsEBB
  where
    mkBlockRef :: SlotNo -> HeaderHash blk -> IsEBB -> BlockRef blk
    mkBlockRef slot hash = BlockRef (BlockPoint slot hash)
