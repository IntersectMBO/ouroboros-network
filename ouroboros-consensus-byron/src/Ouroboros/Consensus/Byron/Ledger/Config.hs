{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Config (
    BlockConfig(..)
  , byronProtocolMagicId
  , byronProtocolMagic
  , byronGenesisHash
  , byronEpochSlots
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
      byronGenesisConfig   :: !CC.Genesis.Config

      -- | Node protocol version
      --
      -- NOTE: This is /static/ for the node, and may not correspond to what's
      -- on the chain. It's the protocol supported by /this/ node; to change it,
      -- you'd have to change the software.
    , byronProtocolVersion :: !CC.Update.ProtocolVersion

      -- | Node software version
      --
      -- Like 'byronProtocolVersion', this is independent from the chain.
    , byronSoftwareVersion :: !CC.Update.SoftwareVersion
    }
  deriving (Generic, NoUnexpectedThunks)

byronProtocolMagicId :: BlockConfig ByronBlock -> Crypto.ProtocolMagicId
byronProtocolMagicId = Crypto.getProtocolMagicId . byronProtocolMagic

byronProtocolMagic :: BlockConfig ByronBlock -> Crypto.ProtocolMagic
byronProtocolMagic = CC.Genesis.configProtocolMagic . byronGenesisConfig

byronGenesisHash :: BlockConfig ByronBlock -> CC.Genesis.GenesisHash
byronGenesisHash = CC.Genesis.configGenesisHash . byronGenesisConfig

byronEpochSlots :: BlockConfig ByronBlock -> CC.Slot.EpochSlots
byronEpochSlots = CC.Genesis.configEpochSlots . byronGenesisConfig
