{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    -- * The top-level node configuration
    TopLevelConfig(..)
  , mkTopLevelConfig
  , castTopLevelConfig
    -- ** Derived extraction functions
  , configConsensus
  , configIndep
  , configLedger
  , configBlock
  , configCodec
    -- ** Additional convenience functions
  , configSecurityParam
    -- * Protocol config
  , FullProtocolConfig(..)
  , castFullProtocolConfig
    -- * Block config
  , FullBlockConfig(..)
  , castFullBlockConfig
  ) where

import           Data.Coerce
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Top-level config
-------------------------------------------------------------------------------}

-- | The top-level node configuration
data TopLevelConfig blk = TopLevelConfig {
      topLevelConfigProtocol :: !(FullProtocolConfig (BlockProtocol blk))
    , topLevelConfigBlock    :: !(FullBlockConfig (LedgerState blk) blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig blk)
         , NoUnexpectedThunks (BlockConfig  blk)
         , NoUnexpectedThunks (CodecConfig  blk)
         ) => NoUnexpectedThunks (TopLevelConfig blk)

-- | Convenience constructor for 'TopLevelConfig'
mkTopLevelConfig :: ConsensusConfig       (BlockProtocol blk)
                 -> ChainIndepStateConfig (BlockProtocol blk)
                 -> LedgerConfig blk
                 -> BlockConfig  blk
                 -> CodecConfig  blk
                 -> TopLevelConfig blk
mkTopLevelConfig protocolConfigConsensus
                 protocolConfigIndep
                 blockConfigLedger
                 blockConfigBlock
                 blockConfigCodec =
    TopLevelConfig {
        topLevelConfigProtocol = FullProtocolConfig{..}
      , topLevelConfigBlock    = FullBlockConfig{..}
      }

configConsensus :: TopLevelConfig blk -> ConsensusConfig (BlockProtocol blk)
configConsensus = protocolConfigConsensus . topLevelConfigProtocol

configIndep :: TopLevelConfig blk -> ChainIndepStateConfig (BlockProtocol blk)
configIndep = protocolConfigIndep . topLevelConfigProtocol

configLedger :: TopLevelConfig blk -> LedgerConfig blk
configLedger = blockConfigLedger . topLevelConfigBlock

configBlock  :: TopLevelConfig blk -> BlockConfig  blk
configBlock = blockConfigBlock . topLevelConfigBlock

configCodec  :: TopLevelConfig blk -> CodecConfig  blk
configCodec = blockConfigCodec . topLevelConfigBlock

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
      topLevelConfigProtocol = castFullProtocolConfig topLevelConfigProtocol
    , topLevelConfigBlock    = castFullBlockConfig    topLevelConfigBlock
    }

{-------------------------------------------------------------------------------
  Protocol config
-------------------------------------------------------------------------------}

data FullProtocolConfig p = FullProtocolConfig {
      protocolConfigConsensus :: !(ConsensusConfig       p)
    , protocolConfigIndep     :: !(ChainIndepStateConfig p)
    }
  deriving (Generic)

instance ConsensusProtocol p => NoUnexpectedThunks (FullProtocolConfig p)

castFullProtocolConfig ::
     ( Coercible (ConsensusConfig p) (ConsensusConfig p')
     , ChainIndepStateConfig p ~ ChainIndepStateConfig p'
     )
  => FullProtocolConfig p -> FullProtocolConfig p'
castFullProtocolConfig FullProtocolConfig{..} = FullProtocolConfig{
      protocolConfigConsensus = coerce protocolConfigConsensus
    , protocolConfigIndep     = protocolConfigIndep
    }

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

data FullBlockConfig l blk = FullBlockConfig {
      blockConfigLedger :: !(LedgerCfg l)
    , blockConfigBlock  :: !(BlockConfig blk)
    , blockConfigCodec  :: !(CodecConfig blk)
    }
  deriving (Generic)

instance ( NoUnexpectedThunks (LedgerCfg l)
         , NoUnexpectedThunks (BlockConfig blk)
         , NoUnexpectedThunks (CodecConfig blk)
         ) => NoUnexpectedThunks (FullBlockConfig l blk)

castFullBlockConfig ::
     ( LedgerCfg l ~ LedgerCfg l'
     , Coercible (BlockConfig blk) (BlockConfig blk')
     , Coercible (CodecConfig blk) (CodecConfig blk')
     )
  => FullBlockConfig l blk -> FullBlockConfig l' blk'
castFullBlockConfig FullBlockConfig{..} = FullBlockConfig{
      blockConfigLedger = blockConfigLedger
    , blockConfigBlock  = coerce blockConfigBlock
    , blockConfigCodec  = coerce blockConfigCodec
    }
