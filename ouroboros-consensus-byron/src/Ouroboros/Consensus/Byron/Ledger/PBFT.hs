{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | Instances required to support PBFT
module Ouroboros.Consensus.Byron.Ledger.PBFT (
    toPBftLedgerView
  , fromPBftLedgerView
  , encodeByronConsensusState
  , decodeByronConsensusState
  , mkByronContextDSIGN
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
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

type instance NodeState     ByronBlock = ()
type instance BlockProtocol ByronBlock = PBft PBftByronCrypto

-- | Construct DSIGN required for Byron crypto
mkByronContextDSIGN :: BlockConfig  ByronBlock
                    -> VerKeyDSIGN  ByronDSIGN
                    -> ContextDSIGN ByronDSIGN
mkByronContextDSIGN = (,) . byronProtocolMagicId

instance BlockSupportsProtocol ByronBlock where
  validateView cfg hdr@ByronHeader{..} =
      case byronHeaderRaw of
        CC.ABOBBoundaryHdr _    -> pbftValidateBoundary hdr
        CC.ABOBBlockHdr regular ->
          let pbftFields :: PBftFields PBftByronCrypto
                                       (Annotated CC.ToSign ByteString)
              pbftFields = PBftFields {
                  pbftIssuer    = VerKeyByronDSIGN
                                . Delegation.delegateVK
                                . CC.delegationCertificate
                                . CC.headerSignature
                                $ regular
                , pbftGenKey    = VerKeyByronDSIGN
                                . CC.headerGenesisKey
                                $ regular
                , pbftSignature = SignedDSIGN
                                . SigByronDSIGN
                                . CC.signature
                                . CC.headerSignature
                                $ regular
                }

          in PBftValidateRegular
               (blockSlot hdr)
               pbftFields
               (CC.recoverSignedBytes epochSlots regular)
               (mkByronContextDSIGN cfg (pbftGenKey pbftFields))
    where
      epochSlots = byronEpochSlots cfg

  selectView _ hdr = (blockNo hdr, byronHeaderIsEBB hdr)

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftByronCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftByronCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeByronConsensusState
  :: ConsensusState (BlockProtocol ByronBlock)
  -> Encoding
encodeByronConsensusState = S.encodePBftState

decodeByronConsensusState
  :: SecurityParam
  -> Decoder s (ConsensusState (BlockProtocol ByronBlock))
decodeByronConsensusState k = S.decodePBftState k (pbftWindowSize k)
