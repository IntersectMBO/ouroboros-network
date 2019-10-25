{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Ledger.Byron.Config (
    ByronConfig(..)
  , ByronExtNodeConfig
  , ByronEBBExtNodeConfig
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.WithEBBs

-- | Extended configuration we need for Byron
data ByronConfig = ByronConfig {
      pbftProtocolMagic   :: !Crypto.ProtocolMagic
    , pbftProtocolVersion :: !CC.Update.ProtocolVersion
    , pbftSoftwareVersion :: !CC.Update.SoftwareVersion
    , pbftEpochSlots      :: !CC.Slot.EpochSlots
    , pbftGenesisConfig   :: !CC.Genesis.Config
    , pbftGenesisHash     :: !CC.Genesis.GenesisHash
    }
  deriving (Generic, NoUnexpectedThunks)

type ByronExtNodeConfig = ExtNodeConfig ByronConfig (PBft PBftCardanoCrypto)
type ByronEBBExtNodeConfig = WithEBBs ByronExtNodeConfig

instance ConfigContainsGenesis ByronConfig where
  genesisConfig = pbftGenesisConfig

instance ConfigContainsGenesis (NodeConfig ByronExtNodeConfig) where
  genesisConfig = genesisConfig . encNodeConfigExt

instance ConfigContainsGenesis (NodeConfig ByronEBBExtNodeConfig) where
  genesisConfig = genesisConfig . unWithEBBNodeConfig
