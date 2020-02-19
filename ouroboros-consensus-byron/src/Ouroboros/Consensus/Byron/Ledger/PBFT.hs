{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instances required to support PBFT
module Ouroboros.Consensus.Byron.Ledger.PBFT (
    ByronConsensusProtocol
  , toPBftLedgerView
  , fromPBftLedgerView
  , encodeByronChainState
  , decodeByronChainState
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
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

import           Ouroboros.Consensus.Byron.Crypto.DSIGN
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

type ByronConsensusProtocol = ExtConfig (PBft PBftByronCrypto) (BlockConfig ByronBlock)
type instance BlockProtocol ByronBlock = ByronConsensusProtocol

-- | Construct DSIGN required for Byron crypto
mkByronContextDSIGN :: TopLevelConfig ByronBlock
                    -> VerKeyDSIGN ByronDSIGN
                    -> ContextDSIGN ByronDSIGN
mkByronContextDSIGN cfg genKey = (byronProtocolMagicId (extNodeConfig (configConsensus cfg)), genKey)

instance SupportedBlock ByronBlock where
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
      epochSlots = byronEpochSlots (extNodeConfig (configConsensus cfg))

  selectView _ hdr = (blockNo hdr, byronHeaderIsEBB hdr)

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftByronCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftByronCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeByronChainState :: ChainState (BlockProtocol ByronBlock) -> Encoding
encodeByronChainState = CS.encodePBftChainState

decodeByronChainState :: SecurityParam
                      -> Decoder s (ChainState (BlockProtocol ByronBlock))
decodeByronChainState k = CS.decodePBftChainState k (pbftWindowSize k)
