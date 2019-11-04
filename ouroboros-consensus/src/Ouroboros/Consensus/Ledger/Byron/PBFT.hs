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
import           Codec.Serialise (decode, encode)
import           Data.ByteString (ByteString)

import           Cardano.Binary (Annotated)
import           Cardano.Crypto.DSIGN

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron.Aux
import           Ouroboros.Consensus.Ledger.Byron.Block
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT

type ByronConsensusProtocol = PBft ByronConfig PBftCardanoCrypto
type instance BlockProtocol ByronBlock = ByronConsensusProtocol

instance HeaderSupportsPBft ByronConfig PBftCardanoCrypto (Header ByronBlock) where
  type OptSigned (Header ByronBlock) = Annotated CC.ToSign ByteString

  headerPBftFields cfg ByronHeader{..} =
      case byronHeaderRaw of
        ABOBBoundaryHdr _   -> Nothing
        ABOBBlockHdr    hdr -> Just (
            PBftFields {
              pbftIssuer    = VerKeyCardanoDSIGN
                            . Delegation.delegateVK
                            . CC.delegationCertificate
                            . CC.headerSignature
                            $ hdr
            , pbftGenKey    = VerKeyCardanoDSIGN
                            . CC.headerGenesisKey
                            $ hdr
            , pbftSignature = SignedDSIGN
                            . SigCardanoDSIGN
                            . CC.signature
                            . CC.headerSignature
                            $ hdr
            }
          , CC.recoverSignedBytes epochSlots hdr
          )
    where
      epochSlots = pbftEpochSlots $ pbftExtConfig cfg

instance SupportedBlock ByronBlock

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftCardanoCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftCardanoCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeByronChainState :: ChainState (BlockProtocol ByronBlock) -> Encoding
encodeByronChainState = encode

decodeByronChainState :: Decoder s (ChainState (BlockProtocol ByronBlock))
decodeByronChainState = decode
