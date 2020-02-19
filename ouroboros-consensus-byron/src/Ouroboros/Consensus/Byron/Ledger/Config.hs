{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Config (
    BlockConfig(..)
  , pbftProtocolMagicId
  , pbftProtocolMagic
  , pbftGenesisHash
  , pbftEpochSlots
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger.Block

-- | Extended configuration we need for Byron
data instance BlockConfig ByronBlock = ByronConfig {
      -- | Genesis configuration
      pbftGenesisConfig   :: !CC.Genesis.Config

      -- | Node protocol version
      --
      -- NOTE: This is /static/ for the node, and may not correspond to what's
      -- on the chain. It's the protocol supported by /this/ node; to change it,
      -- you'd have to change the software.
    , pbftProtocolVersion :: !CC.Update.ProtocolVersion

      -- | Node software version
      --
      -- Like 'pbftProtocolVersion', this is independent from the chain.
    , pbftSoftwareVersion :: !CC.Update.SoftwareVersion
    }
  deriving (Generic, NoUnexpectedThunks)

pbftProtocolMagicId :: BlockConfig ByronBlock -> Crypto.ProtocolMagicId
pbftProtocolMagicId = Crypto.getProtocolMagicId . pbftProtocolMagic

pbftProtocolMagic :: BlockConfig ByronBlock -> Crypto.ProtocolMagic
pbftProtocolMagic = CC.Genesis.configProtocolMagic . pbftGenesisConfig

pbftGenesisHash :: BlockConfig ByronBlock -> CC.Genesis.GenesisHash
pbftGenesisHash = CC.Genesis.configGenesisHash . pbftGenesisConfig

pbftEpochSlots :: BlockConfig ByronBlock -> CC.Slot.EpochSlots
pbftEpochSlots = CC.Genesis.configEpochSlots . pbftGenesisConfig
