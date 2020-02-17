{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Byron.Ledger.ContainsGenesis (
    ConfigContainsGenesis(..)
  ) where

import qualified Cardano.Chain.Genesis as CC.Genesis

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtConfig

import           Ouroboros.Consensus.Byron.Ledger.Config

class ConfigContainsGenesis cfg where
  getGenesisConfig :: cfg -> CC.Genesis.Config

instance ConfigContainsGenesis ByronConfig where
  getGenesisConfig = pbftGenesisConfig

instance ConfigContainsGenesis cfg
      => ConfigContainsGenesis (NodeConfig (ExtConfig p cfg)) where
  getGenesisConfig = getGenesisConfig . extNodeConfig
