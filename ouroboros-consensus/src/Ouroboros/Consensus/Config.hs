{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    -- * The top-level node configuration
    TopLevelConfig(..)
  , mkTopLevelConfig
  , castTopLevelConfig
    -- ** Derived extraction functions
  , configConsensus
  , configLedger
  , configBlock
  , configCodec
    -- ** Additional convenience functions
  , configSecurityParam
    -- * Block config
  , FullBlockConfig(..)
  , castFullBlockConfig
  , mapLedgerCfg
    -- * Re-exports
  , module Ouroboros.Consensus.Config.SecurityParam
  ) where

import           Data.Coerce
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Top-level config
-------------------------------------------------------------------------------}

-- | The top-level node configuration
data TopLevelConfig blk = TopLevelConfig {
      topLevelConfigProtocol :: !(ConsensusConfig (BlockProtocol blk))
    , topLevelConfigBlock    :: !(FullBlockConfig (LedgerState blk) blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig blk)
         , NoUnexpectedThunks (BlockConfig  blk)
         , NoUnexpectedThunks (CodecConfig  blk)
         ) => NoUnexpectedThunks (TopLevelConfig blk)

-- | Convenience constructor for 'TopLevelConfig'
mkTopLevelConfig :: ConsensusConfig (BlockProtocol blk)
                 -> LedgerConfig blk
                 -> BlockConfig  blk
                 -> CodecConfig  blk
                 -> TopLevelConfig blk
mkTopLevelConfig configProtocol
                 blockConfigLedger
                 blockConfigBlock
                 blockConfigCodec =
    TopLevelConfig {
        topLevelConfigProtocol = configProtocol
      , topLevelConfigBlock    = FullBlockConfig{..}
      }

configConsensus :: TopLevelConfig blk -> ConsensusConfig (BlockProtocol blk)
configConsensus = topLevelConfigProtocol

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
     , LedgerConfig blk ~ LedgerConfig blk'
     , Coercible (BlockConfig blk) (BlockConfig blk')
     , Coercible (CodecConfig blk) (CodecConfig blk')
     )
  => TopLevelConfig blk -> TopLevelConfig blk'
castTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      topLevelConfigProtocol = coerce topLevelConfigProtocol
    , topLevelConfigBlock    = castFullBlockConfig topLevelConfigBlock
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
deriving instance ( Show (LedgerCfg l)
                  , Show (BlockConfig blk)
                  , Show (CodecConfig blk)
                  ) => Show (FullBlockConfig l blk)

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

mapLedgerCfg :: (LedgerCfg l -> LedgerCfg l')
             -> FullBlockConfig l blk -> FullBlockConfig l' blk
mapLedgerCfg f FullBlockConfig{..} = FullBlockConfig {
      blockConfigLedger = f blockConfigLedger
    , blockConfigBlock  = blockConfigBlock
    , blockConfigCodec  = blockConfigCodec
    }
