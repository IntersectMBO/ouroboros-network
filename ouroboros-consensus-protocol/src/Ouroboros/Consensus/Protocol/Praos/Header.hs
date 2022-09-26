{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}

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
import           Cardano.Ledger.Core (Era, EraCrypto)
import           Cardano.Ledger.Hashes (EraIndependentBlockBody,
                     EraIndependentBlockHeader)
import           Cardano.Ledger.Keys (CertifiedVRF, Hash, KeyRole (BlockIssuer),
                     SignedKES, VKey, VerKeyVRF, decodeSignedKES,
                     decodeVerKeyVRF, encodeSignedKES, encodeVerKeyVRF)
import           Cardano.Ledger.Serialization (CBORGroup (unCBORGroup))
import           Cardano.Protocol.TPraos.BHeader (PrevHash)
import           Cardano.Protocol.TPraos.OCert (OCert)
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString.Short as SBS
import           Data.Coders
import           Cardano.Ledger.MemoBytes (Mem, MemoBytes (Memo), memoBytes)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF)

-- | The body of the header is the part which gets hashed to form the hash
-- chain.
data HeaderBody era = HeaderBody
  { -- | block number
    hbBlockNo  :: !BlockNo,
    -- | block slot
    hbSlotNo   :: !SlotNo,
    -- | Hash of the previous block header
    hbPrev     :: !(PrevHash (EraCrypto era)),
    -- | verification key of block issuer
    hbVk       :: !(VKey 'BlockIssuer (EraCrypto era)),
    -- | VRF verification key for block issuer
    hbVrfVk    :: !(VerKeyVRF (EraCrypto era)),
    -- | Certified VRF value
    hbVrfRes   :: !(CertifiedVRF (EraCrypto era) InputVRF),
    -- | Size of the block body
    hbBodySize :: !Word32,
    -- | Hash of block body
    hbBodyHash :: !(Hash (EraCrypto era) EraIndependentBlockBody),
    -- | operational certificate
    hbOCert    :: !(OCert (EraCrypto era)),
    -- | protocol version
    hbProtVer  :: !ProtVer
  }
  deriving (Generic)

deriving instance Era era => Show (HeaderBody era)

deriving instance Era era => Eq (HeaderBody era)

instance
  Era era =>
  SignableRepresentation (HeaderBody era)
  where
  getSignableRepresentation = serialize'

instance
  Era era =>
  NoThunks (HeaderBody era)

data HeaderRaw era = HeaderRaw
  { headerRawBody :: !(HeaderBody era),
    headerRawSig  :: !(SignedKES (EraCrypto era) (HeaderBody era))
  }
  deriving (Generic)

instance
  Era era =>
  NoThunks (HeaderRaw era)

deriving instance 
  Era era =>
  Show (HeaderRaw era)

-- | Full header type, carrying its own memoised bytes.
newtype Header era = HeaderConstr (MemoBytes HeaderRaw era)
  deriving newtype (Eq, NoThunks, Show, ToCBOR)

deriving via
  (Mem HeaderRaw era)
  instance
    Era era => (FromCBOR (Annotator (Header era)))

pattern Header ::
  Era era =>
  HeaderBody era ->
  SignedKES (EraCrypto era) (HeaderBody era) ->
  Header era
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
headerSize :: Era era => Header era -> Int
headerSize (HeaderConstr (Memo _ bytes)) = SBS.length bytes

-- | Hash a header
headerHash ::
  Era era =>
  Header era ->
  Hash.Hash (CC.HASH (EraCrypto era)) EraIndependentBlockHeader
headerHash = Hash.castHash . Hash.hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance Era era => ToCBOR (HeaderBody era) where
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

instance Era era => FromCBOR (HeaderBody era) where
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
  Era era =>
  HeaderRaw era ->
  Encode ('Closed 'Dense) (HeaderRaw era)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance Era era => ToCBOR (HeaderRaw era) where
  toCBOR = encode . encodeHeaderRaw

instance Era era => FromCBOR (HeaderRaw era) where
  fromCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance Era era => FromCBOR (Annotator (HeaderRaw era)) where
  fromCBOR = pure <$> fromCBOR
