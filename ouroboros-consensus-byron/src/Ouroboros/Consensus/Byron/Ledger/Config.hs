{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Config (
    ByronConfig(..)
  , pbftProtocolMagicId
  , BlockConfig(..)
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
data ByronConfig = ByronConfig {
      pbftProtocolMagic   :: !Crypto.ProtocolMagic
    , pbftEpochSlots      :: !CC.Slot.EpochSlots
    , pbftGenesisConfig   :: !CC.Genesis.Config
    , pbftGenesisHash     :: !CC.Genesis.GenesisHash

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

pbftProtocolMagicId :: ByronConfig -> Crypto.ProtocolMagicId
pbftProtocolMagicId = Crypto.getProtocolMagicId . pbftProtocolMagic

-- | TODO: This can probably replace ByronConfig
data instance BlockConfig ByronBlock = ByronBlockConfig
  deriving (Generic, NoUnexpectedThunks)
