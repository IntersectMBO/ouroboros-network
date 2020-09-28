{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockConfig (..)
  , mkShelleyBlockConfig
  , CodecConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (ShelleyBlock era) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion  :: !SL.ProtVer
    , shelleySystemStart      :: !SystemStart
    , shelleyNetworkMagic     :: !NetworkMagic
      -- | When chain selection is comparing two fragments, it will prefer the
      -- fragment with a tip signed by (one of) its own key(s) (provided that
      -- the 'BlockNo's and 'SlotNo's of the two tips are equal). For nodes that
      -- can produce blocks, this should be set to the verification key(s)
      -- corresponding to the node's signing key(s), to make sure we prefer
      -- self-issued blocks. For non block producing nodes, this can be set to
      -- the empty map.
    , shelleyBlockIssuerVKeys :: !(Map (SL.KeyHash 'SL.BlockIssuer era)
                                       (SL.VKey 'SL.BlockIssuer era))
    }
  deriving stock (Show, Generic)
  deriving anyclass NoUnexpectedThunks

mkShelleyBlockConfig ::
     (Era era)
  => SL.ProtVer
  -> SL.ShelleyGenesis era
  -> [SL.VKey 'SL.BlockIssuer era]
  -> BlockConfig (ShelleyBlock era)
mkShelleyBlockConfig protVer genesis blockIssuerVKeys = ShelleyConfig {
      shelleyProtocolVersion  = protVer
    , shelleySystemStart      = SystemStart  $ SL.sgSystemStart  genesis
    , shelleyNetworkMagic     = NetworkMagic $ SL.sgNetworkMagic genesis
    , shelleyBlockIssuerVKeys = Map.fromList
        [ (SL.hashKey k, k)
        | k <- blockIssuerVKeys
        ]
    }

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | No particular codec configuration is needed for Shelley
data instance CodecConfig (ShelleyBlock era) = ShelleyCodecConfig
  deriving (Generic, NoUnexpectedThunks)
