{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolByron
  , ProtocolCardano
  , ProtocolShelley
    -- * Abstract over the various protocols
  , ProtocolParamsAllegra (..)
  , ProtocolParamsAlonzo (..)
  , ProtocolParamsByron (..)
  , ProtocolParamsMary (..)
  , ProtocolParamsShelley (..)
  , ProtocolTransitionParamsShelleyBased (..)
  , module X
  ) where

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X
import           Ouroboros.Consensus.Shelley.ShelleyHFC

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron   = HardForkProtocol '[ ByronBlock ]
type ProtocolCardano = HardForkProtocol '[ ByronBlock
                                         , ShelleyBlock (TPraos StandardCrypto) StandardShelley
                                         , ShelleyBlock (TPraos StandardCrypto) StandardAllegra
                                         , ShelleyBlock (TPraos StandardCrypto) StandardMary
                                         , ShelleyBlock (TPraos StandardCrypto) StandardAlonzo
                                         , ShelleyBlock (Praos StandardCrypto)  StandardBabbage
                                         , ShelleyBlock (Praos StandardCrypto)  StandardConway
                                         ]
