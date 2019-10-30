{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Ouroboros.Consensus.Ledger.Byron.Config (
    ByronConfig(..)
  , pbftProtocolMagicId
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Crypto as Crypto

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

pbftProtocolMagicId :: ByronConfig -> Crypto.ProtocolMagicId
pbftProtocolMagicId = Crypto.getProtocolMagicId . pbftProtocolMagic
