{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Config (
    -- * Block config
    BlockConfig (..)
  , byronEpochSlots
  , byronGenesisHash
  , byronProtocolMagic
  , byronProtocolMagicId
    -- * Codec config
  , CodecConfig (..)
  , mkByronCodecConfig
    -- * Storage config
  , StorageConfig (..)
    -- * Compact genesis config
  , compactGenesisConfig
  ) where

import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.Byron.Ledger.Block

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

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
  deriving (Generic, NoThunks)

byronGenesisHash :: BlockConfig ByronBlock -> CC.Genesis.GenesisHash
byronGenesisHash = CC.Genesis.configGenesisHash . byronGenesisConfig

byronProtocolMagicId :: BlockConfig ByronBlock -> Crypto.ProtocolMagicId
byronProtocolMagicId = Crypto.getProtocolMagicId . byronProtocolMagic

byronProtocolMagic :: BlockConfig ByronBlock -> Crypto.ProtocolMagic
byronProtocolMagic = CC.Genesis.configProtocolMagic . byronGenesisConfig

byronEpochSlots :: BlockConfig ByronBlock -> CC.Slot.EpochSlots
byronEpochSlots = CC.Genesis.configEpochSlots . byronGenesisConfig

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig ByronBlock = ByronCodecConfig {
      getByronEpochSlots :: CC.Slot.EpochSlots
    }
  deriving (Generic, NoThunks)

mkByronCodecConfig :: CC.Genesis.Config -> CodecConfig ByronBlock
mkByronCodecConfig cfg = ByronCodecConfig {
      getByronEpochSlots = CC.Genesis.configEpochSlots cfg
    }

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

newtype instance StorageConfig ByronBlock = ByronStorageConfig {
      -- | We need the 'BlockConfig' to be able to forge an EBB in
      -- 'nodeInitChainDB'.
      getByronBlockConfig :: BlockConfig ByronBlock
    }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Compact genesis config
-------------------------------------------------------------------------------}

-- | Byron's genesis config contains the AVVM balances, of which there are +14k
-- in mainnet's genesis config. These balances are only used to create the
-- initial ledger state, there is no reason to keep them in memory afterwards.
--
-- This function empties the 'gdAvvmDistr' field in the genesis config. As we
-- keep Byron's genesis config in memory (even in later eras), this can save us
-- a bit of memory.
compactGenesisConfig :: CC.Genesis.Config -> CC.Genesis.Config
compactGenesisConfig cfg = cfg {
      CC.Genesis.configGenesisData = (CC.Genesis.configGenesisData cfg) {
          CC.Genesis.gdAvvmDistr = CC.Genesis.GenesisAvvmBalances Map.empty
        }
    }
