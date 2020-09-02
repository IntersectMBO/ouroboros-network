{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockIssuerVKey (..)
  , BlockConfig (..)
  , mkShelleyBlockConfig
  , CodecConfig (..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.PParams as SL (ProtVer)

import           Ouroboros.Consensus.Shelley.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

-- | In case we're a block issuer, the verification key we use.
data BlockIssuerVKey era =
    BlockIssuerVKey !(SL.VKey 'SL.BlockIssuer era)
  | NotABlockIssuer
  deriving stock (Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

data instance BlockConfig (ShelleyBlock era) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion :: !SL.ProtVer
    , shelleySystemStart     :: !SystemStart
    , shelleyNetworkMagic    :: !NetworkMagic
      -- | When chain selection is comparing two fragments, it will prefer the
      -- fragment with a tip signed by this key (provided that the 'BlockNo's
      -- and 'SlotNo's of the two tips are equal). For nodes that can produce
      -- blocks, this should be set to the verification key corresponding to
      -- the node's signing key, to make sure we prefer self-issued blocks.
      -- For non block producing nodes, this can be set to 'NotABlockIssuer'.
    , shelleyBlockIssuerVKey :: !(BlockIssuerVKey era)
    }
  deriving stock (Show, Generic)
  deriving anyclass NoUnexpectedThunks

mkShelleyBlockConfig ::
     SL.ProtVer
  -> SL.ShelleyGenesis era
  -> BlockIssuerVKey era
  -> BlockConfig (ShelleyBlock era)
mkShelleyBlockConfig protVer genesis blockIssuerVKey = ShelleyConfig {
      shelleyProtocolVersion = protVer
    , shelleySystemStart     = SystemStart     $ SL.sgSystemStart  genesis
    , shelleyNetworkMagic    = NetworkMagic    $ SL.sgNetworkMagic genesis
    , shelleyBlockIssuerVKey = blockIssuerVKey
    }

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | No particular codec configuration is needed for Shelley
data instance CodecConfig (ShelleyBlock era) = ShelleyCodecConfig
  deriving (Generic, NoUnexpectedThunks)
