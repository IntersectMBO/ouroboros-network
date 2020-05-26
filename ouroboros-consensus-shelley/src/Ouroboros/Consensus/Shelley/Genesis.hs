{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Consensus.Shelley.Genesis (
    ShelleyGenesisStaking(..)
  , emptyGenesisStaking
  , ShelleyGenesis(..)
  , sgActiveSlotCoeff
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Crypto (ProtocolMagicId)
import           Cardano.Prelude (Natural, NoUnexpectedThunks)
import           Cardano.Slotting.Slot (EpochSize)

import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.BlockchainTime (SlotLength, SystemStart)
import           Ouroboros.Consensus.Config.SecurityParam

import qualified Shelley.Spec.Ledger.Address as SL
import           Shelley.Spec.Ledger.BaseTypes (ActiveSlotCoeff)
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.TxData as SL

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking {
      -- | Pools to register
      --
      --   The key in this map is the hash of the public key of the _pool_. This
      --   need not correspond to any payment or staking key, but must correspond
      --   to the cold key held by 'TPraosIsCoreNode'.
      sgsPools :: !(Map (SL.KeyHash 'SL.StakePool c) (SL.PoolParams c))
      -- | Stake-holding key hash credentials and the pools to delegate that stake
      -- to. We require the raw staking key hash in order to:
      --
      -- - Avoid pointer addresses, which would be tricky when there's no slot or
      --   transaction to point to.
      -- - Avoid script credentials.
    , sgsStake :: !(Map (SL.KeyHash 'SL.Staking c) (SL.KeyHash 'SL.StakePool c))
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking = ShelleyGenesisStaking
  { sgsPools = Map.empty
  , sgsStake = Map.empty
  }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis {
      sgStartTime             :: !SystemStart
    , sgNetworkMagic          :: !NetworkMagic
    , sgNetworkId             :: !SL.Network
    , sgProtocolMagicId       :: !ProtocolMagicId
    , sgActiveSlotsCoeff      :: !Double
    , sgSecurityParam         :: !SecurityParam
    , sgEpochLength           :: !EpochSize
    , sgSlotsPerKESPeriod     :: !Word64
    , sgMaxKESEvolutions      :: !Word64
    , sgSlotLength            :: !SlotLength
    , sgUpdateQuorum          :: !Word64
    , sgMaxMajorPV            :: !Natural
    , sgMaxLovelaceSupply     :: !Word64
    , sgProtocolParams        :: !SL.PParams
    , sgGenDelegs             :: !(Map (SL.KeyHash 'SL.Genesis c) (SL.KeyHash 'SL.GenesisDelegate c))
    , sgInitialFunds          :: !(Map (SL.Addr c) SL.Coin)
    , sgStaking               :: !(ShelleyGenesisStaking c)
    }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

sgActiveSlotCoeff :: ShelleyGenesis c -> ActiveSlotCoeff
sgActiveSlotCoeff =
      SL.mkActiveSlotCoeff
    . SL.truncateUnitInterval
    . toRational
    . sgActiveSlotsCoeff
