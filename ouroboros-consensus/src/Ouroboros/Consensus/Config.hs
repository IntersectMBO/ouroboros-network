{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.Config (
    TopLevelConfig(..)
  , configSecurityParam
  ) where

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | The top-level node configuration
--
-- TODOs:
--
-- * Replace @NodeConfig (BlockProtocol blk)@ with 'TopLevelConfig' throughout
-- * Remove uses of 'ExtConfig' where possible (since we have the ledger
--   config separately here).
-- * Add separate 'topLevelBlockConfig :: BlockConfig blk', and change use
--   sites of 'TopLevelConfig' to 'BlockConfig' where appropriate
-- * Rename 'NodeConfig' to 'ConsensusConfig'
data TopLevelConfig blk = TopLevelConfig {
      configConsensus :: NodeConfig (BlockProtocol blk)
    , configLedger    :: LedgerConfig blk
    }

configSecurityParam :: OuroborosTag (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus
