{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Ledger.Byron.Config (
    ByronConfig(..)
  , ByronExtNodeConfig
  , ByronEBBExtNodeConfig
  ) where

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
      pbftProtocolMagic   :: Crypto.ProtocolMagic
    , pbftProtocolVersion :: CC.Update.ProtocolVersion
    , pbftSoftwareVersion :: CC.Update.SoftwareVersion
    , pbftEpochSlots      :: CC.Slot.EpochSlots
    , pbftGenesisConfig   :: CC.Genesis.Config
    , pbftGenesisHash     :: CC.Genesis.GenesisHash
      -- | This is only needed by "Ouroboros.Consensus.Demo.Byron.Elaborate"
      -- to elaborate from mock transactions to real ones. This obviously only
      -- works for demos. This can be removed as soon as the elaboration is
      -- removed (or moved into the tx submission tool for demos).
      --
    , pbftSecrets         :: CC.Genesis.GeneratedSecrets
      -- TODO: remove this ^^
    }

type ByronExtNodeConfig = ExtNodeConfig ByronConfig (PBft PBftCardanoCrypto)
type ByronEBBExtNodeConfig = WithEBBs ByronExtNodeConfig

instance ConfigContainsGenesis ByronConfig where
  genesisConfig = pbftGenesisConfig
