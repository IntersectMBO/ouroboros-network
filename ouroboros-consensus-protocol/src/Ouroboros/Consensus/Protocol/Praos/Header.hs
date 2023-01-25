{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Block header associated with Praos.
--
-- The choice of whether to associate the header with the ledger era or the
-- protocol is a little artitrary. Functionally the header contains things which
-- are associated with both ledger and protocol, and which are used by both.
--
-- We choose to associate the header with the protocol, since it more strongly
-- binds in that direction, and to assist with the mental picture that the
-- protocol is concerned with the block header, while the ledger is concerned
-- with the block body. However, in order to more cleanly illustrate which parts
-- of the header are _strictly_ protocol concerns, we also provide a view of the
-- header (in 'Ouroboros.Consensus.Protocol.Praos.Views') which extracts just
-- the fields needed for the Praos protocol. This also allows us to hide the
-- more detailed construction of the header.
module Ouroboros.Consensus.Protocol.Praos.Header (
    Header (Header, headerBody, headerSig)
  , HeaderBody (..)
  , headerHash
  , headerSize
  ) where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     serialize')
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.Util
                     (SignableRepresentation (getSignableRepresentation))
import           Cardano.Ledger.BaseTypes (ProtVer)
import qualified Cardano.Ledger.Crypto as CC
import           Cardano.Ledger.Hashes (EraIndependentBlockBody,
                     EraIndependentBlockHeader)
import           Cardano.Ledger.Keys (Hash, KeyRole (BlockIssuer),
                     VKey, decodeSignedKES,
                     decodeVerKeyVRF, encodeSignedKES, encodeVerKeyVRF)
import           Cardano.Protocol.HeaderKeys (CertifiedVRF, SignedKES, VerKeyVRF)
import           Cardano.Ledger.Serialization (CBORGroup (unCBORGroup))
import qualified Cardano.Protocol.HeaderCrypto as CC
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Protocol.TPraos.OCert (OCert)
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString.Short as SBS
import           Data.Coders
import           Data.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | The body of the header is the part which gets hashed to form the hash
-- chain.
data HeaderBody c hc = HeaderBody
  { -- | block number
    hbBlockNo  :: !BlockNo,
    -- | block slot
    hbSlotNo   :: !SlotNo,
    -- | Hash of the previous block header
    hbPrev     :: !(PrevHash c),
    -- | verification key of block issuer
    hbVk       :: !(VKey 'BlockIssuer c),
    -- | VRF verification key for block issuer
    hbVrfVk    :: !(VerKeyVRF hc),
    -- | Certified VRF value
    hbVrfRes   :: !(CertifiedVRF hc InputVRF),
    -- | Size of the block body
    hbBodySize :: !Word32,
    -- | Hash of block body
    hbBodyHash :: !(Hash c EraIndependentBlockBody),
    -- | operational certificate
    hbOCert    :: !(OCert c hc),
    -- | protocol version
    hbProtVer  :: !ProtVer
  }
  deriving (Generic)

deriving instance (CC.Crypto c, CC.HeaderCrypto hc) => Show (HeaderBody c hc)

deriving instance (CC.Crypto c, CC.HeaderCrypto hc) => Eq (HeaderBody c hc)

instance
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  SignableRepresentation (HeaderBody c hc)
  where
  getSignableRepresentation = serialize'

instance
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  NoThunks (HeaderBody c hc)

data HeaderRaw c hc = HeaderRaw
  { headerRawBody :: !(HeaderBody c hc),
    headerRawSig  :: !(SignedKES hc (HeaderBody c hc))
  }
  deriving (Show, Generic)

instance
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  NoThunks (HeaderRaw c hc)

-- | Full header type, carrying its own memoised bytes.
newtype Header c hc = HeaderConstr (MemoBytes (HeaderRaw c hc))
  deriving newtype (Eq, Show, NoThunks, ToCBOR)

deriving via
  (Mem (HeaderRaw c hc))
  instance
    (CC.Crypto c, CC.HeaderCrypto hc) => (FromCBOR (Annotator (Header c hc)))

pattern Header ::
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  HeaderBody c hc ->
  SignedKES hc (HeaderBody c hc) ->
  Header c hc
pattern Header {headerBody, headerSig} <-
  HeaderConstr
    ( Memo
        HeaderRaw
          { headerRawBody = headerBody,
            headerRawSig = headerSig
          }
        _
      )
  where
    Header body sig =
      HeaderConstr $ memoBytes (encodeHeaderRaw $ HeaderRaw body sig)

{-# COMPLETE Header #-}

-- | Compute the size of the header
headerSize :: Header c hc -> Int
headerSize (HeaderConstr (Memo _ bytes)) = SBS.length bytes

-- | Hash a header
headerHash ::
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  Header c hc ->
  Hash.Hash (CC.HASH c) EraIndependentBlockHeader
headerHash = Hash.castHash . Hash.hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance (CC.Crypto c, CC.HeaderCrypto hc) => ToCBOR (HeaderBody c hc) where
  toCBOR
    HeaderBody
      { hbBlockNo,
        hbSlotNo,
        hbPrev,
        hbVk,
        hbVrfVk,
        hbVrfRes,
        hbBodySize,
        hbBodyHash,
        hbOCert,
        hbProtVer
      } =
      encode $
        Rec HeaderBody
          !> To hbBlockNo
          !> To hbSlotNo
          !> To hbPrev
          !> To hbVk
          !> E encodeVerKeyVRF hbVrfVk
          !> To hbVrfRes
          !> To hbBodySize
          !> To hbBodyHash
          !> To hbOCert
          !> To hbProtVer

instance (CC.Crypto c, CC.HeaderCrypto hc) => FromCBOR (HeaderBody c hc) where
  fromCBOR =
    decode $
      RecD HeaderBody
        <! From
        <! From
        <! From
        <! From
        <! D decodeVerKeyVRF
        <! From
        <! From
        <! From
        <! (unCBORGroup <$> From)
        <! From

encodeHeaderRaw ::
  (CC.Crypto c, CC.HeaderCrypto hc) =>
  HeaderRaw c hc ->
  Encode ('Closed 'Dense) (HeaderRaw c hc)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance (CC.Crypto c, CC.HeaderCrypto hc) => ToCBOR (HeaderRaw c hc) where
  toCBOR = encode . encodeHeaderRaw

instance (CC.Crypto c, CC.HeaderCrypto hc) => FromCBOR (HeaderRaw c hc) where
  fromCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance (CC.Crypto c, CC.HeaderCrypto hc) => FromCBOR (Annotator (HeaderRaw c hc)) where
  fromCBOR = pure <$> fromCBOR
