module Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..)) where

import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block.Abstract (BlockConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)

-- | The 'BlockConfig' needs to contain some information in order to support
-- running a node.
class ConfigSupportsNode blk where
  getSystemStart  :: BlockConfig blk -> SystemStart
  getNetworkMagic :: BlockConfig blk -> NetworkMagic
