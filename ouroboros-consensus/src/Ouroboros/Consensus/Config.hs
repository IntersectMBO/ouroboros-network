{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Config (
    -- * The top-level node configuration
    TopLevelConfig (..)
  , castTopLevelConfig
  , mkTopLevelConfig
    -- ** Derived extraction functions
  , configBlock
  , configCodec
  , configConsensus
  , configLedger
  , configStorage
    -- ** Additional convenience functions
  , configSecurityParam
    -- * Re-exports
  , module Ouroboros.Consensus.Config.SecurityParam
  ) where

import           Data.Coerce
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

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
    , topLevelConfigStorage  :: !(StorageConfig blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoThunks (LedgerConfig  blk)
         , NoThunks (BlockConfig   blk)
         , NoThunks (CodecConfig   blk)
         , NoThunks (StorageConfig blk)
         ) => NoThunks (TopLevelConfig blk)

mkTopLevelConfig ::
     ConsensusConfig (BlockProtocol blk)
  -> LedgerConfig   blk
  -> BlockConfig    blk
  -> CodecConfig    blk
  -> StorageConfig  blk
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

configStorage  :: TopLevelConfig blk -> StorageConfig blk
configStorage = topLevelConfigStorage

configSecurityParam :: ConsensusProtocol (BlockProtocol blk)
                    => TopLevelConfig blk -> SecurityParam
configSecurityParam = protocolSecurityParam . configConsensus

castTopLevelConfig ::
     ( Coercible (ConsensusConfig (BlockProtocol blk))
                 (ConsensusConfig (BlockProtocol blk'))
     , LedgerConfig blk ~ LedgerConfig blk'
     , Coercible (BlockConfig   blk) (BlockConfig   blk')
     , Coercible (CodecConfig   blk) (CodecConfig   blk')
     , Coercible (StorageConfig blk) (StorageConfig blk')
     )
  => TopLevelConfig blk -> TopLevelConfig blk'
castTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      topLevelConfigProtocol = coerce topLevelConfigProtocol
    , topLevelConfigLedger   = topLevelConfigLedger
    , topLevelConfigBlock    = coerce topLevelConfigBlock
    , topLevelConfigCodec    = coerce topLevelConfigCodec
    , topLevelConfigStorage  = coerce topLevelConfigStorage
    }
