{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instances required to support PBFT
module Ouroboros.Consensus.Ledger.Byron.PBFT (
    ByronConsensusProtocol
  , toPBftLedgerView
  , fromPBftLedgerView
  , encodeByronChainState
  , decodeByronChainState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.ByteString (ByteString)

import           Cardano.Binary (Annotated)
import           Cardano.Crypto.DSIGN

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation

import           Ouroboros.Network.Block (HasHeader (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron.Block
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.Serialisation ()
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

type ByronConsensusProtocol = ExtConfig (PBft PBftCardanoCrypto) ByronConfig
type instance BlockProtocol ByronBlock = ByronConsensusProtocol

instance SupportedBlock ByronBlock where
  validateView cfg hdr@ByronHeader{..} =
      case byronHeaderRaw of
        CC.ABOBBoundaryHdr _    -> pbftValidateBoundary hdr
        CC.ABOBBlockHdr regular ->
          let pbftFields :: PBftFields PBftCardanoCrypto
                                       (Annotated CC.ToSign ByteString)
              pbftFields = PBftFields {
                  pbftIssuer    = VerKeyCardanoDSIGN
                                . Delegation.delegateVK
                                . CC.delegationCertificate
                                . CC.headerSignature
                                $ regular
                , pbftGenKey    = VerKeyCardanoDSIGN
                                . CC.headerGenesisKey
                                $ regular
                , pbftSignature = SignedDSIGN
                                . SigCardanoDSIGN
                                . CC.signature
                                . CC.headerSignature
                                $ regular
                }

          in PBftValidateRegular
               (blockSlot hdr)
               pbftFields
               (CC.recoverSignedBytes epochSlots regular)
               (extNodeConfig cfg, pbftGenKey pbftFields)
    where
      epochSlots = pbftEpochSlots (extNodeConfig cfg)

  selectView _ hdr = (blockNo hdr, byronHeaderIsEBB hdr)

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftCardanoCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftCardanoCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeByronChainState :: ChainState (BlockProtocol ByronBlock) -> Encoding
encodeByronChainState = CS.encodePBftChainState

decodeByronChainState :: SecurityParam
                      -> Decoder s (ChainState (BlockProtocol ByronBlock))
decodeByronChainState k = CS.decodePBftChainState k (pbftWindowSize k)
