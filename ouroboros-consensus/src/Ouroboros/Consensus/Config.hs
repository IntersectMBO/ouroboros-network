{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    TopLevelConfig(..)
  , configSecurityParam
  , configCodec
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | The top-level node configuration
data TopLevelConfig blk = TopLevelConfig {
      configConsensus :: !(ConsensusConfig (BlockProtocol blk))
    , configLedger    :: !(LedgerConfig                   blk)
    , configBlock     :: !(BlockConfig                    blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol  (BlockProtocol blk)
         , UpdateLedger                      blk
         , NoUnexpectedThunks (BlockConfig   blk)
         ) => NoUnexpectedThunks (TopLevelConfig blk)

configSecurityParam :: ConsensusProtocol (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus

configCodec
  :: BlockHasCodecConfig blk
  => TopLevelConfig blk
  -> CodecConfig blk
configCodec = getCodecConfig . configBlock
