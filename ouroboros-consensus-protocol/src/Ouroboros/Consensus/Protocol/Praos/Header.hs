{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
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

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.Util
                     (SignableRepresentation (getSignableRepresentation))
import           Cardano.Ledger.BaseTypes (ProtVer (pvMajor))
import           Cardano.Ledger.Binary (Annotator (..), CBORGroup (unCBORGroup),
                     ToCBOR (..), DecCBOR (decCBOR), EncCBOR (..),
                     encodedSigKESSizeExpr, serialize', withSlice)
import           Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import           Cardano.Ledger.Crypto (Crypto (HASH))
import           Cardano.Ledger.Hashes (EraIndependentBlockBody,
                     EraIndependentBlockHeader)
import           Cardano.Ledger.Keys (CertifiedVRF, Hash, KeyRole (BlockIssuer),
                     SignedKES, VKey, VerKeyVRF, decodeSignedKES,
                     decodeVerKeyVRF, encodeSignedKES, encodeVerKeyVRF)
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Protocol.TPraos.OCert (OCert)
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | The body of the header is the part which gets hashed to form the hash
-- chain.
data HeaderBody crypto = HeaderBody
  { -- | block number
    hbBlockNo  :: !BlockNo,
    -- | block slot
    hbSlotNo   :: !SlotNo,
    -- | Hash of the previous block header
    hbPrev     :: !(PrevHash crypto),
    -- | verification key of block issuer
    hbVk       :: !(VKey 'BlockIssuer crypto),
    -- | VRF verification key for block issuer
    hbVrfVk    :: !(VerKeyVRF crypto),
    -- | Certified VRF value
    hbVrfRes   :: !(CertifiedVRF crypto InputVRF),
    -- | Size of the block body
    hbBodySize :: !Word32,
    -- | Hash of block body
    hbBodyHash :: !(Hash crypto EraIndependentBlockBody),
    -- | operational certificate
    hbOCert    :: !(OCert crypto),
    -- | protocol version
    hbProtVer  :: !ProtVer
  }
  deriving (Generic)

deriving instance Crypto crypto => Show (HeaderBody crypto)

deriving instance Crypto crypto => Eq (HeaderBody crypto)

instance
  Crypto crypto =>
  SignableRepresentation (HeaderBody crypto)
  where
  getSignableRepresentation hb = serialize' (pvMajor (hbProtVer hb)) hb

instance
  Crypto crypto =>
  NoThunks (HeaderBody crypto)

data HeaderRaw crypto = HeaderRaw
  { headerRawBody :: !(HeaderBody crypto),
    headerRawSig  :: !(SignedKES crypto (HeaderBody crypto))
  }
  deriving (Show, Generic)

instance Crypto c => Eq (HeaderRaw c) where
  h1 == h2 = headerRawSig h1 == headerRawSig h2
             && headerRawBody h1 == headerRawBody h2

-- | Checks the binary representation first.
instance Crypto c => Eq (Header c) where
  h1 == h2 = headerBytes h1 == headerBytes h2
             && headerRaw h1 == headerRaw h2

instance
  Crypto crypto =>
  NoThunks (HeaderRaw crypto)

-- | Full header type, carrying its own memoised bytes.
data Header crypto = HeaderConstr
  { headerRaw   :: !(HeaderRaw crypto)
  , headerBytes :: BS.ByteString -- lazy on purpose, constructed on demand
  }
  deriving (Show, Generic)
  deriving (NoThunks) via AllowThunksIn '["headerBytes"] (Header crypto)

pattern Header ::
  Crypto crypto =>
  HeaderBody crypto ->
  SignedKES crypto (HeaderBody crypto) ->
  Header crypto
pattern Header {headerBody, headerSig} <-
  HeaderConstr {
    headerRaw =
      HeaderRaw
        { headerRawBody = headerBody
        , headerRawSig = headerSig
        }
    }
  where
    Header body sig =
      let header = HeaderRaw
            { headerRawBody = body
            , headerRawSig = sig
            }
      in HeaderConstr
         { headerRaw = header
         , headerBytes = serialize' (pvMajor (hbProtVer body)) header
         }

{-# COMPLETE Header #-}

-- | Compute the size of the header
headerSize :: Header crypto -> Int
headerSize (HeaderConstr _ bytes) = BS.length bytes

-- | Hash a header
headerHash ::
  Crypto crypto =>
  Header crypto ->
  Hash.Hash (HASH crypto) EraIndependentBlockHeader
headerHash = Hash.castHash . Hash.hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance Crypto crypto => EncCBOR (HeaderBody crypto) where
  encCBOR
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

instance Crypto crypto => DecCBOR (HeaderBody crypto) where
  decCBOR =
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
  Crypto crypto =>
  HeaderRaw crypto ->
  Encode ('Closed 'Dense) (HeaderRaw crypto)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance Crypto crypto => EncCBOR (HeaderRaw crypto) where
  encCBOR = encode . encodeHeaderRaw

instance Crypto crypto => DecCBOR (HeaderRaw crypto) where
  decCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance Crypto crypto => DecCBOR (Annotator (HeaderRaw crypto)) where
  decCBOR = pure <$> decCBOR

instance Crypto c => Plain.ToCBOR (Header c) where
  toCBOR (HeaderConstr _ bytes) = Plain.encodePreEncoded bytes

instance Crypto c => EncCBOR (Header c) where
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (headerRawBody . headerRaw <$> proxy)
      + encodedSigKESSizeExpr (KES.getSig . headerRawSig . headerRaw <$> proxy)

instance Crypto c => DecCBOR (Annotator (Header c)) where
  decCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice decCBOR
    pure (Annotator (\fullbytes -> HeaderConstr (getT fullbytes) (BSL.toStrict (getBytes fullbytes))))
