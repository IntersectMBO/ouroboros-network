{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Commonality between multiple protocols.
--
--   Everything in this module is indexed on the protocol (or the crypto),
--   rather than on the block type. This allows it to be imported in
--   @Ouroboros.Consensus.Shelley.Ledger.Block@.
module Ouroboros.Consensus.Shelley.Protocol.Abstract (
    ProtoCrypto
  , ProtocolHeaderSupportsEnvelope (..)
  , ProtocolHeaderSupportsKES (..)
  , ProtocolHeaderSupportsLedger (..)
  , ProtocolHeaderSupportsProtocol (..)
  , ShelleyHash (..)
  , ShelleyProtocol
  , ShelleyProtocolHeader
  ) where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Crypto.VRF (OutputVRF)
import           Cardano.Ledger.BHeaderView (BHeaderView)
import           Cardano.Ledger.BaseTypes (ProtVer)
import           Cardano.Ledger.Crypto (Crypto, HASH, VRF)
import           Cardano.Ledger.Hashes (EraIndependentBlockBody,
                     EraIndependentBlockHeader)
import           Cardano.Ledger.Keys (KeyRole (BlockIssuer), VKey)
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except (Except)
import           Data.Kind (Type)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)
import           Ouroboros.Consensus.Protocol.Abstract (CanBeLeader,
                     ChainDepState, ConsensusConfig, ConsensusProtocol,
                     IsLeader, LedgerView, ValidateView)
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import           Ouroboros.Consensus.Protocol.Signed (SignedHeader)
import           Ouroboros.Consensus.Ticked (Ticked)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

type family ProtoCrypto proto :: Type

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash crypto = ShelleyHash
  { unShelleyHash :: Hash (HASH crypto) EraIndependentBlockHeader
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NoThunks)

deriving newtype instance Crypto crypto => ToCBOR (ShelleyHash crypto)

deriving newtype instance Crypto crypto => FromCBOR (ShelleyHash crypto)

instance
  Crypto crypto =>
  Serialise (ShelleyHash crypto)
  where
  encode = toCBOR
  decode = fromCBOR


instance Condense (ShelleyHash crypto) where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Shelley header, determined by the associated protocol.
--
type family ShelleyProtocolHeader proto = (sh :: Type) | sh -> proto

-- | Indicates that the header (determined by the protocol) supports " Envelope
-- " functionality. Envelope functionality refers to the minimal functionality
-- required to construct a chain.
class
  ( Eq (EnvelopeCheckError proto),
    NoThunks (EnvelopeCheckError proto),
    Show (EnvelopeCheckError proto)
  ) =>
  ProtocolHeaderSupportsEnvelope proto
  where
  pHeaderHash :: ShelleyProtocolHeader proto -> ShelleyHash (ProtoCrypto proto)
  pHeaderPrevHash :: ShelleyProtocolHeader proto -> PrevHash (ProtoCrypto proto)
  pHeaderBodyHash :: ShelleyProtocolHeader proto -> Hash (HASH (ProtoCrypto proto)) EraIndependentBlockBody
  pHeaderSlot :: ShelleyProtocolHeader proto -> SlotNo
  pHeaderBlock :: ShelleyProtocolHeader proto -> BlockNo
  pHeaderSize :: ShelleyProtocolHeader proto -> Natural
  pHeaderBlockSize :: ShelleyProtocolHeader proto -> Natural

  type EnvelopeCheckError proto :: Type

  -- | Carry out any protocol-specific envelope checks. For example, this might
  -- check things like maximum header size.
  envelopeChecks ::
    ConsensusConfig proto ->
    Ticked (LedgerView proto) ->
    ShelleyProtocolHeader proto ->
    Except (EnvelopeCheckError proto) ()

-- | `ProtocolHeaderSupportsKES` describes functionality common to protocols
--    using key evolving signature schemes. This includes verifying the header
--    integrity (e.g. validating the KES signature), as well as constructing the
--    header (made specific to KES-using protocols through the need to handle
--    the hot key).
class ProtocolHeaderSupportsKES proto where

  -- | Extract the "slots per KES period" value from the protocol config.
  --
  --   Note that we do not require `ConsensusConfig` in 'verifyHeaderIntegrity'
  --   since that function is also invoked with 'StorageConfig'.
  configSlotsPerKESPeriod :: ConsensusConfig proto -> Word64

  -- | Verify that the signature on a header is correct and valid.
  verifyHeaderIntegrity ::
    -- | Slots per KES period
    Word64 ->
    ShelleyProtocolHeader proto ->
    Bool

  mkHeader ::
    forall crypto m.
    (Crypto crypto, Monad m, crypto ~ ProtoCrypto proto) =>
    HotKey crypto m ->
    CanBeLeader proto ->
    IsLeader proto ->
    -- | Slot no
    SlotNo ->
    -- | Block no
    BlockNo ->
    -- | Hash of the previous block
    PrevHash crypto ->
    -- | Hash of the block body to include in the header
    Hash (HASH crypto) EraIndependentBlockBody ->
    -- | Size of the block body
    Int ->
    -- | Protocol version
    ProtVer ->
    m (ShelleyProtocolHeader proto)

-- | ProtocolHeaderSupportsProtocol` provides support for the concrete
--   block header to support the `ConsensusProtocol` itself.
class ProtocolHeaderSupportsProtocol proto where

  type CannotForgeError proto :: Type

  protocolHeaderView ::
    ShelleyProtocolHeader proto -> ValidateView proto

  pHeaderIssuer ::
    ShelleyProtocolHeader proto -> VKey 'BlockIssuer (ProtoCrypto proto)
  pHeaderIssueNo ::
    ShelleyProtocolHeader proto -> Word64
  -- | A VRF value in the header, used to choose between otherwise equally
  -- preferable chains.
  pTieBreakVRFValue ::
    ShelleyProtocolHeader proto -> OutputVRF (VRF (ProtoCrypto proto))

-- | Indicates that the protocol header supports the Shelley ledger. We may need
-- to generalise this if, in the future, the ledger requires different things
-- from the protocol.
class ProtocolHeaderSupportsLedger proto where
  mkHeaderView :: ShelleyProtocolHeader proto -> BHeaderView (ProtoCrypto proto)


{-------------------------------------------------------------------------------
  Key constraints
-------------------------------------------------------------------------------}

class
  ( ConsensusProtocol proto,
    Typeable (ShelleyProtocolHeader proto),
    ProtocolHeaderSupportsEnvelope proto,
    ProtocolHeaderSupportsKES proto,
    ProtocolHeaderSupportsProtocol proto,
    ProtocolHeaderSupportsLedger proto,
    Serialise (ChainDepState proto),
    SignedHeader (ShelleyProtocolHeader proto)
  ) =>
  ShelleyProtocol proto
