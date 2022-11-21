{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockConfig (..)
  , CodecConfig (..)
  , StorageConfig (..)
  , compactGenesis
  , getCompactGenesis
  , mkShelleyBlockConfig
    -- * opaque
  , CompactGenesis
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (FromCBOR, ToCBOR)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config

import qualified Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (ShelleyBlock proto era) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion  :: !SL.ProtVer
    , shelleySystemStart      :: !SystemStart
    , shelleyNetworkMagic     :: !NetworkMagic
      -- | For nodes that can produce blocks, this should be set to the
      -- verification key(s) corresponding to the node's signing key(s). For non
      -- block producing nodes, this can be set to the empty map.
    , shelleyBlockIssuerVKeys :: !(Map (SL.KeyHash 'SL.BlockIssuer (EraCrypto era))
                                       (SL.VKey 'SL.BlockIssuer (EraCrypto era)))
    , shelleyGenesis          :: SL.ShelleyGenesis era
    }
  deriving stock (Generic)

deriving instance ShelleyBasedEra era => Show     (BlockConfig (ShelleyBlock proto era))
deriving instance ShelleyBasedEra era => NoThunks (BlockConfig (ShelleyBlock proto era))

mkShelleyBlockConfig ::
     ShelleyBasedEra era
  => SL.ProtVer
  -> SL.ShelleyGenesis era
  -> [SL.VKey 'SL.BlockIssuer (EraCrypto era)]
  -> BlockConfig (ShelleyBlock proto era)
mkShelleyBlockConfig protVer genesis blockIssuerVKeys = ShelleyConfig {
      shelleyProtocolVersion  = protVer
    , shelleySystemStart      = SystemStart  $ SL.sgSystemStart  genesis
    , shelleyNetworkMagic     = NetworkMagic $ SL.sgNetworkMagic genesis
    , shelleyBlockIssuerVKeys = Map.fromList
        [ (SL.hashKey k, k)
        | k <- blockIssuerVKeys
        ]
    , shelleyGenesis          = genesis
    }

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | No particular codec configuration is needed for Shelley
data instance CodecConfig (ShelleyBlock proto era) = ShelleyCodecConfig
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

data instance StorageConfig (ShelleyBlock proto era) = ShelleyStorageConfig {
      -- | Needed for 'nodeCheckIntegrity'
      shelleyStorageConfigSlotsPerKESPeriod :: !Word64
      -- | Needed for 'nodeImmutableDbChunkInfo'
    , shelleyStorageConfigSecurityParam     :: !SecurityParam
    }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Compact genesis
-------------------------------------------------------------------------------}

-- | Compact variant of 'SL.ShelleyGenesis' with some fields erased that are
-- only used on start-up and that should not be kept in memory forever.
--
-- Concretely:
--
-- * The 'sgInitialFunds' field is erased. It is only used to set up the initial
--   UTxO in tests and testnets.
--
-- * The 'sgStaking' field is erased. It is only used to register initial stake
--   pools in tests and benchmarks.
newtype CompactGenesis era = CompactGenesis {
      getCompactGenesis :: SL.ShelleyGenesis era
    }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromCBOR, ToCBOR)

deriving anyclass instance ShelleyBasedEra era => NoThunks (CompactGenesis era)

-- | Compacts the given 'SL.ShelleyGenesis'.
compactGenesis :: SL.ShelleyGenesis era -> CompactGenesis era
compactGenesis genesis = CompactGenesis $
    genesis {
        SL.sgInitialFunds = mempty
      , SL.sgStaking      = SL.emptyGenesisStaking
      }
