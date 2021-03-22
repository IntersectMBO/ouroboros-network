-- | In tests we want to be able to map a block to the core node that produced
-- it.
module Test.ThreadNet.Util.HasCreator (
    HasCreator (..)
  ) where

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

class HasCreator b where
    getCreator :: b -> CoreNodeId
