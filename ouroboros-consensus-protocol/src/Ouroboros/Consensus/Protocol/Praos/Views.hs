{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Protocol.Praos.Views (
    HeaderView (..)
  , LedgerView (..)
  ) where

import           Cardano.Crypto.KES (SignedKES)
import           Cardano.Crypto.VRF (CertifiedVRF, VRFAlgorithm (VerKeyVRF))
import           Cardano.Ledger.BaseTypes (ProtVer)
import           Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Protocol.HeaderCrypto (KES, VRF)
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Protocol.TPraos.OCert (OCert)
import           Cardano.Slotting.Slot (SlotNo)
import           Numeric.Natural (Natural)
import           Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | View of the block header required by the Praos protocol.
data HeaderView c hc = HeaderView
  { -- | Hash of the previous block
    hvPrevHash  :: !(PrevHash c),
    -- | verification key of block issuer
    hvVK        :: !(VKey 'BlockIssuer c),
    -- | VRF verification key for block issuer
    hvVrfVK     :: !(VerKeyVRF (VRF hc)),
    -- | VRF result
    hvVrfRes    :: !(CertifiedVRF (VRF hc) InputVRF),
    -- | operational certificate
    hvOCert     :: !(OCert c hc),
    -- | Slot
    hvSlotNo    :: !SlotNo,
    -- | Header which must be signed
    hvSigned    :: !(HeaderBody c hc),
    -- | KES Signature of the header
    hvSignature :: !(SignedKES (KES hc) (HeaderBody c hc))
  }

data LedgerView c = LedgerView
  { -- | Stake distribution
    lvPoolDistr       :: SL.PoolDistr c,
    -- | Maximum header size
    lvMaxHeaderSize   :: !Natural,
    -- | Maximum block body size
    lvMaxBodySize     :: !Natural,
    -- | Current protocol version
    lvProtocolVersion :: !ProtVer
  }
  deriving (Show)
