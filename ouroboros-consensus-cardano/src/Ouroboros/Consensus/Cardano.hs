{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolByron
  , ProtocolShelley
  , ProtocolCardano
    -- * Abstract over the various protocols
  , ProtocolParamsByron(..)
  , ProtocolParamsShelley(..)
  , ProtocolParamsAllegra(..)
  , ProtocolParamsMary(..)
  , ProtocolParamsTransition(..)
  , Protocol(..)
  , module X
    -- * Client support for nodes running a protocol
  , ProtocolClient(..)
  , RunProtocol(..)
  , RunProtocolClient(..)
  ) where

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Protocol as X
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Shelley.ShelleyHFC

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron   = HardForkProtocol '[ ByronBlock ]
type ProtocolCardano = HardForkProtocol '[ ByronBlock
                                         , ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardAllegra
                                         , ShelleyBlock StandardMary
                                         ]

-- | Run PBFT against the Byron ledger
instance IOLike m => Protocol m ByronBlockHFC ProtocolByron where
  data RunProtocol m ByronBlockHFC ProtocolByron = RunProtocolByron ProtocolParamsByron
  protocolInfo (RunProtocolByron params) = inject $ protocolInfoByron params

instance IOLike m => Protocol m (CardanoBlock StandardCrypto) ProtocolCardano where
  data RunProtocol m (CardanoBlock StandardCrypto) ProtocolCardano = RunProtocolCardano
    ProtocolParamsByron
    (ProtocolParamsShelleyBased StandardShelley)
    ProtocolParamsShelley
    ProtocolParamsAllegra
    ProtocolParamsMary
    (ProtocolParamsTransition ByronBlock (ShelleyBlock StandardShelley))
    (ProtocolParamsTransition (ShelleyBlock StandardShelley) (ShelleyBlock StandardAllegra))
    (ProtocolParamsTransition (ShelleyBlock StandardAllegra) (ShelleyBlock StandardMary))
  protocolInfo (RunProtocolCardano
               paramsByron
               paramsShelleyBased
               paramsShelley
               paramsAllegra
               paramsMary
               paramsByronShelley
               paramsShelleyAllegra
               paramsAllegraMary) =
    protocolInfoCardano
      paramsByron
      paramsShelleyBased
      paramsShelley
      paramsAllegra
      paramsMary
      paramsByronShelley
      paramsShelleyAllegra
      paramsAllegraMary

instance ProtocolClient ByronBlockHFC ProtocolByron where
  data RunProtocolClient ByronBlockHFC ProtocolByron =
    RunProtocolClientByron EpochSlots
  protocolClientInfo (RunProtocolClientByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

instance ProtocolClient (CardanoBlock StandardCrypto) ProtocolCardano where
  data RunProtocolClient (CardanoBlock StandardCrypto) ProtocolCardano =
    RunProtocolClientCardano EpochSlots
  protocolClientInfo (RunProtocolClientCardano epochSlots) =
    protocolClientInfoCardano epochSlots
