{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    TopLevelConfig(..)
  , configSecurityParam
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | The top-level node configuration
--
-- TODOs:
--
-- * Remove uses of 'ExtConfig' where possible (since we have the ledger
--   config separately here).
-- * Add separate 'topLevelBlockConfig :: BlockConfig blk', and change use
--   sites of 'TopLevelConfig' to 'BlockConfig' where appropriate
-- * Rename 'NodeConfig' to 'ConsensusConfig'
data TopLevelConfig blk = TopLevelConfig {
      configConsensus :: !(NodeConfig (BlockProtocol blk))
    , configLedger    :: !(LedgerConfig blk)
    }
  deriving (Generic)

instance ( OuroborosTag (BlockProtocol blk)
         , UpdateLedger blk
         ) => NoUnexpectedThunks (TopLevelConfig blk)

configSecurityParam :: OuroborosTag (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus
