
module Ouroboros.Consensus.Ledger.Query.Version (
    QueryVersion (..)
  , nodeToClientVersionToQueryVersion
  ) where

import           Ouroboros.Network.NodeToClient.Version

-- | Version of the `Query blk` type.
data QueryVersion
  = TopLevelQueryDisabled
  -- ^ Only the @BlockQuery@ constructor of @Query@ is supported.
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the @QueryVersion@ supported by this @NodeToClientVersion@.
nodeToClientVersionToQueryVersion :: NodeToClientVersion -> QueryVersion
nodeToClientVersionToQueryVersion x = case x of
  NodeToClientV_1 -> TopLevelQueryDisabled
  NodeToClientV_2 -> TopLevelQueryDisabled
  NodeToClientV_3 -> TopLevelQueryDisabled
  NodeToClientV_4 -> TopLevelQueryDisabled
  NodeToClientV_5 -> TopLevelQueryDisabled
  NodeToClientV_6 -> TopLevelQueryDisabled
  NodeToClientV_7 -> TopLevelQueryDisabled
  NodeToClientV_8 -> TopLevelQueryDisabled
  NodeToClientV_9 -> TopLevelQueryDisabled
