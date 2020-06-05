{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockConfig (..)
  , CodecConfig (..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Crypto (ProtocolMagicId)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime

import qualified Shelley.Spec.Ledger.PParams as SL (ProtVer)

import           Ouroboros.Consensus.Shelley.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (ShelleyBlock c) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion :: !SL.ProtVer
    , shelleySystemStart     :: !SystemStart
    , shelleyNetworkMagic    :: !NetworkMagic
    , shelleyProtocolMagicId :: !ProtocolMagicId
    }
  deriving stock (Show, Generic)
  deriving anyclass NoUnexpectedThunks

instance HasCodecConfig (ShelleyBlock c) where
  -- | No particular codec configuration is needed for Shelley
  data CodecConfig (ShelleyBlock c) = ShelleyCodecConfig
    deriving (Generic, NoUnexpectedThunks)

  getCodecConfig = const ShelleyCodecConfig
