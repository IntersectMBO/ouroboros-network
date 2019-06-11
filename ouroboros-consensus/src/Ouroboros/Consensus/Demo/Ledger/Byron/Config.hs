{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Ouroboros.Consensus.Demo.Ledger.Byron.Config (
    ByronDemoConfig(..)
  , ByronExtNodeConfig
  ) where

import           Data.Bimap (Bimap)

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT

-- | Extended configuration we need for the demo
data ByronDemoConfig = ByronDemoConfig {
      -- | Mapping from generic keys to core node IDs
      --
      -- The keys in this map are the verification keys of the core nodes - that
      -- is, the delegates of the genesis keys.
      pbftCoreNodes       :: Bimap Crypto.VerificationKey CoreNodeId
    , pbftProtocolMagic   :: Crypto.ProtocolMagic
    , pbftProtocolVersion :: CC.Update.ProtocolVersion
    , pbftSoftwareVersion :: CC.Update.SoftwareVersion
    , pbftEpochSlots      :: CC.Slot.EpochSlots
    , pbftGenesisConfig   :: CC.Genesis.Config
    , pbftGenesisHash     :: CC.Genesis.GenesisHash
    , pbftGenesisDlg      :: CC.Genesis.GenesisDelegation
    , pbftSecrets         :: CC.Genesis.GeneratedSecrets
    }

type ByronExtNodeConfig = ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto)

instance ConfigContainsGenesis ByronDemoConfig where
  genesisConfig = pbftGenesisConfig
