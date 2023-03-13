module Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..)) where

import           Ouroboros.Consensus.Block.Abstract (BlockConfig)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Network.Magic (NetworkMagic)

-- | The 'BlockConfig' needs to contain some information in order to support
-- running a node.
class ConfigSupportsNode blk where
  getSystemStart  :: BlockConfig blk -> SystemStart
  getNetworkMagic :: BlockConfig blk -> NetworkMagic
