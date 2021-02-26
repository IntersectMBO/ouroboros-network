-- | See 'BlockSupportsMetrics'.
module Ouroboros.Consensus.Block.SupportsMetrics (
    BlockSupportsMetrics (..)
  , WhetherSelfIssued (..)
  , isSelfIssuedConstUnknown
  ) where

import           Ouroboros.Consensus.Block.Abstract

{-------------------------------------------------------------------------------
  Supported blocks
-------------------------------------------------------------------------------}

-- | Whether a block was issued by a stakeholder currently forging on this node
data WhetherSelfIssued =
    IsSelfIssued
  | IsNotSelfIssued
    -- | We are unable to determine
  | UnknownSelfIssued
  deriving (Show, Eq)

-- | Evidence that a block supports the metrics needed for business requirements
--
-- For example, we use 'isSelfIssued' to help Stake Pool Operators monitor how
-- many of their forged blocks have not ended up on their chain.
class BlockSupportsMetrics blk where
  -- | See 'WhetherSelfIssued'.
  isSelfIssued :: BlockConfig blk -> Header blk -> WhetherSelfIssued

-- | Use this default for block types that cannot or do not yet support the
-- predicate
isSelfIssuedConstUnknown :: BlockConfig blk -> Header blk -> WhetherSelfIssued
isSelfIssuedConstUnknown _cfg _hdr = UnknownSelfIssued
