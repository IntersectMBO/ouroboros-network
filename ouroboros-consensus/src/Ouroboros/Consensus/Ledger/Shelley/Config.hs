{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
module Ouroboros.Consensus.Ledger.Shelley.Config where

import           BlockChain (ProtVer)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data ShelleyNodeConfig = ShelleyNodeConfig
  { sncProtocolVersion :: ProtVer }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoUnexpectedThunks
