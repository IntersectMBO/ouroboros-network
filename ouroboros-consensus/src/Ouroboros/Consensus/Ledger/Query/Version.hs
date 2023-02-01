
module Ouroboros.Consensus.Ledger.Query.Version (
    QueryVersion (..)
  , nodeToClientVersionToQueryVersion
  ) where

import           Ouroboros.Network.NodeToClient.Version

-- | Version of the `Query blk` type.
--
-- Multiple top level queries are now supported. The encoding now has
-- constructor tags for the different top level queries for QueryVersion1 onwards.
data QueryVersion
  -- Adds support for 'GetSystemStart'.
  = QueryVersion1

  -- Adds support for 'GetChainBlockNo' and 'GetChainPoint'.
  | QueryVersion2
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the @QueryVersion@ supported by this @NodeToClientVersion@.
nodeToClientVersionToQueryVersion :: NodeToClientVersion -> QueryVersion
nodeToClientVersionToQueryVersion x = case x of
  NodeToClientV_9  -> QueryVersion1
  NodeToClientV_10 -> QueryVersion2
  NodeToClientV_11 -> QueryVersion2
  NodeToClientV_12 -> QueryVersion2
  NodeToClientV_13 -> QueryVersion2
  NodeToClientV_14 -> QueryVersion2
  NodeToClientV_15 -> QueryVersion2
