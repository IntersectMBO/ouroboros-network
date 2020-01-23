{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Storage.ChainDB.Impl.BlockComponent
  ( BlockComponent (..)
  , translateToRawDB
  , Parser
  , BlockOrHeader (..)
  ) where

import qualified Data.ByteString.Lazy as Lazy

import           Ouroboros.Network.Block (pattern BlockPoint, HeaderHash,
                     SlotNo)

import           Ouroboros.Consensus.Block (GetHeader (..), IsEBB (..))

import           Ouroboros.Storage.ChainDB.API (BlockRef (..), ChainDB)
import           Ouroboros.Storage.Common

-- | Translate a ChainDB 'BlockComponent' into a 'BlockComponent' known by the
-- ImmutableDB and the VolatileDB.
translateToRawDB
  :: forall m blk b db. DBHeaderHash db ~ HeaderHash blk
  => (forall b'. Parser m blk b')
  -> (IsEBB -> Lazy.ByteString -> Lazy.ByteString)
     -- ^ Add header envelope
  -> BlockComponent (ChainDB m blk) b
  -> BlockComponent db b
translateToRawDB parse addHdrEnv = \case
    GetBlock ->
      parse Block <$> getBlockRef <*> GetRawBlock
    GetRawBlock   -> GetRawBlock
    GetHeader ->
      (\isEBB blockRef bs -> parse Header blockRef (addHdrEnv isEBB bs)) <$>
      GetIsEBB <*> getBlockRef <*> GetRawHeader
    GetRawHeader  -> addHdrEnv <$> GetIsEBB <*> GetRawHeader
    GetHash       -> GetHash
    GetSlot       -> GetSlot
    GetIsEBB      -> GetIsEBB
    GetBlockSize  -> GetBlockSize
    GetHeaderSize -> GetHeaderSize
    GetPure a     -> GetPure a
    GetApply f bc -> GetApply
      (translateToRawDB parse addHdrEnv f)
      (translateToRawDB parse addHdrEnv bc)

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
