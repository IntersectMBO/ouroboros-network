{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Consensus.Config.SupportsNode (
    ConfigSupportsNode (..)
  ) where

import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block.Abstract (BlockConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)

-- | The 'BlockConfig' needs to contain some information in order to support
-- running a node.
class ConfigSupportsNode blk where

  -- | Static configuration required for serialisation and deserialisation of
  -- types pertaining to this type of block.
  --
  -- Data family instead of type family to get better type inference.
  data family CodecConfig blk :: *

  getCodecConfig     :: BlockConfig blk -> CodecConfig blk
  getSystemStart     :: BlockConfig blk -> SystemStart
  getNetworkMagic    :: BlockConfig blk -> NetworkMagic
  getProtocolMagicId :: BlockConfig blk -> ProtocolMagicId
