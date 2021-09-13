
module Ouroboros.Consensus.Ledger.Query.Version (
    QueryVersion (..)
  , nodeToClientVersionToQueryVersion
  ) where

import           Ouroboros.Network.NodeToClient.Version

-- | Version of the `Query blk` type.
data QueryVersion
    -- | Only the 'BlockQuery' constructor of 'Query' is supported. The binary
    -- encoding is backwards compatible: it does not introduce any constructor
    -- tag around the 'BlockQuery'.
  = TopLevelQueryDisabled

    -- | Multiple top level queries are now supported. The encoding now has
    -- constructor tags for the different top level queries. Specifically V1
    -- adds support for 'GetSystemStart'.
  | QueryVersion1
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
  NodeToClientV_9 -> QueryVersion1
