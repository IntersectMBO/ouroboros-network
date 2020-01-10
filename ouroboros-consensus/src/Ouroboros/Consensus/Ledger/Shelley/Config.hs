{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE StrictData     #-}

module Ouroboros.Consensus.Ledger.Shelley.Config where

import           BlockChain (ProtVer)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Cardano.Crypto (ProtocolMagicId)

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data ShelleyNodeConfig = ShelleyNodeConfig
  { sncProtocolVersion :: ProtVer
  , sncStartTime :: SystemStart
  , sncNetworkMagic :: NetworkMagic
  , sncProtocolMagicId :: ProtocolMagicId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoUnexpectedThunks
