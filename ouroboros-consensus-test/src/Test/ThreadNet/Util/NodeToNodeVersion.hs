module Test.ThreadNet.Util.NodeToNodeVersion (
    genVersion
  , genVersionFiltered
  , newestVersion
  ) where

import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Test.QuickCheck (Gen)
import           Test.Util.QuickCheck

genVersion ::
     SupportedNetworkProtocolVersion blk
  => Proxy blk -> Gen (NodeToNodeVersion, BlockNodeToNodeVersion blk)
genVersion = genVersionFiltered (const True)

genVersionFiltered ::
    SupportedNetworkProtocolVersion blk
  => (BlockNodeToNodeVersion blk -> Bool)
  -> Proxy blk
  -> Gen (NodeToNodeVersion, BlockNodeToNodeVersion blk)
genVersionFiltered f =
      elements
    . filter (f . snd)
    . Map.toList
    . supportedNodeToNodeVersions

-- | Return the newest version, i.e., the version with the highest
-- 'NodeToNodeVersion'. This can be used when you don't care about the
-- versioning of a block.
newestVersion ::
     SupportedNetworkProtocolVersion blk
  => Proxy blk -> (NodeToNodeVersion, BlockNodeToNodeVersion blk)
newestVersion = Map.findMax . supportedNodeToNodeVersions
