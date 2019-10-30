{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Ledger.Byron.ContainsGenesis (
    ConfigContainsGenesis(..)
  ) where

import qualified Cardano.Chain.Genesis as CC.Genesis

import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

class ConfigContainsGenesis cfg where
  genesisConfig :: cfg -> CC.Genesis.Config

instance ConfigContainsGenesis ByronConfig where
  genesisConfig = pbftGenesisConfig

instance ConfigContainsGenesis cfg
      => ConfigContainsGenesis (NodeConfig (PBft cfg c)) where
  genesisConfig = genesisConfig . pbftExtConfig

instance ConfigContainsGenesis (NodeConfig p)
      => ConfigContainsGenesis (NodeConfig (WithEBBs p)) where
  genesisConfig = genesisConfig . unWithEBBNodeConfig
