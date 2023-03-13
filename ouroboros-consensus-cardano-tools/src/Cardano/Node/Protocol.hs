-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol.hs

module Cardano.Node.Protocol (
    ProtocolInstantiationError (..)
  , SomeConsensusProtocol (..)
  , mkConsensusProtocol
  ) where

import           Cardano.Api.Any
import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol ncProtocolConfig mProtocolFiles =
    case ncProtocolConfig of

      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config mProtocolFiles

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config mProtocolFiles

      NodeProtocolConfigurationCardano byronConfig
                                       shelleyConfig
                                       alonzoConfig
                                       conwayConfig
                                       hardForkConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano
            byronConfig
            shelleyConfig
            alonzoConfig
            conwayConfig
            hardForkConfig
            mProtocolFiles

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


instance Error ProtocolInstantiationError where
  displayError (ByronProtocolInstantiationError   err) = displayError err
  displayError (ShelleyProtocolInstantiationError err) = displayError err
  displayError (CardanoProtocolInstantiationError err) = displayError err
