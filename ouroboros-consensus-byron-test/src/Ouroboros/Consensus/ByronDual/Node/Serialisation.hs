{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.ByronDual.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger

import           Ouroboros.Consensus.ByronDual.Ledger

{-------------------------------------------------------------------------------
  HasNetworkProtocolVersion
-------------------------------------------------------------------------------}

pb :: Proxy ByronBlock
pb = Proxy

instance HasNetworkProtocolVersion DualByronBlock where
  type BlockNodeToNodeVersion   DualByronBlock = BlockNodeToNodeVersion   ByronBlock
  type BlockNodeToClientVersion DualByronBlock = BlockNodeToClientVersion ByronBlock

instance SupportedNetworkProtocolVersion DualByronBlock where
  supportedNodeToNodeVersions     _ = supportedNodeToNodeVersions     pb
  supportedNodeToClientVersions   _ = supportedNodeToClientVersions   pb

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance SerialiseDiskConstraints DualByronBlock

instance EncodeDisk DualByronBlock DualByronBlock where
  encodeDisk _ = encodeDualBlock encodeByronBlock
instance DecodeDisk DualByronBlock (Lazy.ByteString -> DualByronBlock) where
  decodeDisk ccfg = decodeDualBlock (decodeByronBlock epochSlots)
    where
      epochSlots = extractEpochSlots ccfg

instance DecodeDiskDep (NestedCtxt Header) DualByronBlock where
  decodeDiskDep (DualCodecConfig ccfg ByronSpecCodecConfig)
                (NestedCtxt (CtxtDual ctxt)) =
      decodeDiskDep ccfg (NestedCtxt ctxt)

instance EncodeDisk DualByronBlock (LedgerState DualByronBlock) where
  encodeDisk _ = encodeDualLedgerState encodeByronLedgerState
instance DecodeDisk DualByronBlock (LedgerState DualByronBlock) where
  decodeDisk _ = decodeDualLedgerState decodeByronLedgerState

-- | @'ChainDepState' ('BlockProtocol' 'DualByronBlock')@
instance EncodeDisk DualByronBlock (PBftState PBftByronCrypto) where
  encodeDisk _ = encodeByronChainDepState
-- | @'ChainDepState' ('BlockProtocol' 'DualByronBlock')@
instance DecodeDisk DualByronBlock (PBftState PBftByronCrypto) where
  decodeDisk _ = decodeByronChainDepState

instance EncodeDisk DualByronBlock (AnnTip DualByronBlock) where
  encodeDisk ccfg = encodeDisk (dualCodecConfigMain ccfg)
                  . (castAnnTip :: AnnTip DualByronBlock -> AnnTip ByronBlock)
instance DecodeDisk DualByronBlock (AnnTip DualByronBlock) where
  decodeDisk ccfg = (castAnnTip :: AnnTip ByronBlock -> AnnTip DualByronBlock)
                <$> decodeDisk (dualCodecConfigMain ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints DualByronBlock where
  -- We don't enforce this estimate, so we just reuse the estimate from the
  -- concrete header.
  estimateBlockSize = estimateBlockSize . dualHeaderMain

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualByronBlock DualByronBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeByronBlock)
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeByronBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualByronBlock (Serialised DualByronBlock)
  -- Default instance

-- | Forward to the Byron instance.
instance SerialiseNodeToNode DualByronBlock (Header DualByronBlock) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualHeaderMain
  decodeNodeToNode ccfg version =
          DualHeader
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

-- | Forward to the Byron instance.
instance SerialiseNodeToNode DualByronBlock (SerialisedHeader DualByronBlock) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualWrappedMain
  decodeNodeToNode ccfg version =
          rewrapMain
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

instance SerialiseNodeToNode DualByronBlock (GenTx DualByronBlock) where
  encodeNodeToNode _ _ = encodeDualGenTx encodeByronGenTx
  decodeNodeToNode _ _ = decodeDualGenTx decodeByronGenTx

instance SerialiseNodeToNode DualByronBlock (GenTxId DualByronBlock) where
  encodeNodeToNode _ _ = encodeDualGenTxId encodeByronGenTxId
  decodeNodeToNode _ _ = decodeDualGenTxId decodeByronGenTxId

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClientConstraints DualByronBlock

instance SerialiseNodeToClient DualByronBlock (DualLedgerConfig ByronBlock ByronSpecBlock) where
  encodeNodeToClient _ _ = encodeDualLedgerConfig toCBOR toCBOR
  decodeNodeToClient _ _ = decodeDualLedgerConfig fromCBOR fromCBOR

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualByronBlock DualByronBlock where
  encodeNodeToClient _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeByronBlock)
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeByronBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualByronBlock (Serialised DualByronBlock)
  -- Default instance

instance SerialiseNodeToClient DualByronBlock (GenTx DualByronBlock) where
  encodeNodeToClient _ _ = encodeDualGenTx encodeByronGenTx
  decodeNodeToClient _ _ = decodeDualGenTx decodeByronGenTx

-- | @'ApplyTxErr' 'DualByronBlock'@
instance SerialiseNodeToClient DualByronBlock (DualGenTxErr ByronBlock ByronSpecBlock) where
  encodeNodeToClient _ _ = encodeDualGenTxErr encodeByronApplyTxError
  decodeNodeToClient _ _ = decodeDualGenTxErr decodeByronApplyTxError

instance SerialiseNodeToClient DualByronBlock (SomeSecond BlockQuery DualByronBlock) where
  encodeNodeToClient _ _ = \case {}
  decodeNodeToClient _ _ = error "DualByron: no query to decode"

instance SerialiseResult DualByronBlock (BlockQuery DualByronBlock) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractEpochSlots :: CodecConfig DualByronBlock -> EpochSlots
extractEpochSlots = getByronEpochSlots . dualCodecConfigMain

-- | The headers for 'DualByronBlock' and 'ByronBlock' are identical, so we
-- can safely cast the serialised forms.
dualWrappedMain :: SerialisedHeader DualByronBlock
                -> SerialisedHeader ByronBlock
dualWrappedMain = castSerialisedHeader ctxtDualMain

-- | Inverse of 'dualWrappedMain'.
rewrapMain :: SerialisedHeader ByronBlock
           -> SerialisedHeader DualByronBlock
rewrapMain = castSerialisedHeader CtxtDual
