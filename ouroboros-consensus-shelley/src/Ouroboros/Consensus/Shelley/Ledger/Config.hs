{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockConfig (..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Crypto (ProtocolMagicId)
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import qualified Ouroboros.Consensus.HardFork.History as HardFork

import qualified Shelley.Spec.Ledger.PParams as SL (ProtVer)

import           Ouroboros.Consensus.Shelley.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (ShelleyBlock c) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion :: !SL.ProtVer
    , shelleyStartTime       :: !SystemStart
    , shelleyNetworkMagic    :: !NetworkMagic
    , shelleyProtocolMagicId :: !ProtocolMagicId
    , shelleyEraParams       :: !HardFork.EraParams
    }
  deriving stock (Show, Generic)
  deriving anyclass NoUnexpectedThunks
