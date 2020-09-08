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
    , topLevelConfigLedger   :: !(LedgerConfig blk)
    , topLevelConfigBlock    :: !(BlockConfig blk)
    , topLevelConfigCodec    :: !(CodecConfig blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig blk)
         , NoUnexpectedThunks (BlockConfig  blk)
         , NoUnexpectedThunks (CodecConfig  blk)
         ) => NoUnexpectedThunks (TopLevelConfig blk)

mkTopLevelConfig :: ConsensusConfig (BlockProtocol blk)
                 -> LedgerConfig blk
                 -> BlockConfig  blk
                 -> CodecConfig  blk
                 -> TopLevelConfig blk
mkTopLevelConfig = TopLevelConfig

configConsensus :: TopLevelConfig blk -> ConsensusConfig (BlockProtocol blk)
configConsensus = topLevelConfigProtocol

configLedger :: TopLevelConfig blk -> LedgerConfig blk
configLedger = topLevelConfigLedger

configBlock  :: TopLevelConfig blk -> BlockConfig  blk
configBlock = topLevelConfigBlock

configCodec  :: TopLevelConfig blk -> CodecConfig  blk
configCodec = topLevelConfigCodec

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
    , topLevelConfigLedger   = topLevelConfigLedger
    , topLevelConfigBlock    = coerce topLevelConfigBlock
    , topLevelConfigCodec    = coerce topLevelConfigCodec
    }
