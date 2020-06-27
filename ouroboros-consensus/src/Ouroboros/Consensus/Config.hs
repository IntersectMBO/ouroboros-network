{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    TopLevelConfig(..)
  , configSecurityParam
  , castTopLevelConfig
  ) where

import           Data.Coerce
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Protocol.Abstract

-- | The top-level node configuration
data TopLevelConfig blk = TopLevelConfig {
      configConsensus :: !(ConsensusConfig       (BlockProtocol blk))
    , configIndep     :: !(ChainIndepStateConfig (BlockProtocol blk))
    , configLedger    :: !(LedgerConfig                         blk)
    , configBlock     :: !(BlockConfig                          blk)
    , configCodec     :: !(CodecConfig                          blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol  (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig  blk)
         , NoUnexpectedThunks (BlockConfig   blk)
         , NoUnexpectedThunks (CodecConfig   blk)
         ) => NoUnexpectedThunks (TopLevelConfig blk)

configSecurityParam :: ConsensusProtocol (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus

castTopLevelConfig ::
     ( Coercible (ConsensusConfig (BlockProtocol blk))
                 (ConsensusConfig (BlockProtocol blk'))
     ,   ChainIndepStateConfig (BlockProtocol blk)
       ~ ChainIndepStateConfig (BlockProtocol blk')
     , LedgerConfig blk ~ LedgerConfig blk'
     , Coercible (BlockConfig blk) (BlockConfig blk')
     , Coercible (CodecConfig blk) (CodecConfig blk')
     )
  => TopLevelConfig blk -> TopLevelConfig blk'
castTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus = coerce configConsensus
    , configIndep     = configIndep
    , configLedger    = configLedger
    , configBlock     = coerce configBlock
    , configCodec     = coerce configCodec
    }
