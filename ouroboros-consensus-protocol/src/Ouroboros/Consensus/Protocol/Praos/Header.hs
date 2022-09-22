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

import           Cardano.Crypto.Hash.Class (HashAlgorithm)
import           Cardano.Ledger.Crypto (HASH)
import           Cardano.Ledger.Core (EraCrypto, Era)
import           Cardano.Crypto.KES.Class (KESAlgorithm)
-- | The body of the header is the part which gets hashed to form the hash
-- chain.
data HeaderBody crypto = HeaderBody
  { -- | block number
    hbBlockNo  :: !BlockNo,
    -- | block slot
    hbSlotNo   :: !SlotNo,
    -- | Hash of the previous block header
    hbPrev     :: !(PrevHash (EraCrypto crypto)),
    -- | verification key of block issuer
    hbVk       :: !(VKey 'BlockIssuer (EraCrypto crypto)),
    -- | VRF verification key for block issuer
    hbVrfVk    :: !(VerKeyVRF (EraCrypto crypto)),
    -- | Certified VRF value
    hbVrfRes   :: !(CertifiedVRF (EraCrypto crypto) InputVRF),
    -- | Size of the block body
    hbBodySize :: !Word32,
    -- | Hash of block body
    hbBodyHash :: !(Hash (EraCrypto crypto) EraIndependentBlockBody),
    -- | operational certificate
    hbOCert    :: !(OCert (EraCrypto crypto)),
    -- | protocol version
    hbProtVer  :: !ProtVer
  }
  deriving (Generic)

deriving instance Era crypto => Show (HeaderBody crypto)

deriving instance Era crypto => Eq (HeaderBody crypto)

instance
  Era crypto =>
  SignableRepresentation (HeaderBody crypto)
  where
  getSignableRepresentation = serialize'

instance
  Era crypto =>
  NoThunks (HeaderBody crypto)

data HeaderRaw crypto = HeaderRaw
  { headerRawBody :: !(HeaderBody crypto),
    headerRawSig  :: !(SignedKES crypto (HeaderBody crypto))
  }
  deriving (Generic)

instance
  Era crypto =>
  Show (HeaderRaw crypto)

instance
  (Era crypto, KESAlgorithm (CC.KES crypto)) =>
  NoThunks (HeaderRaw crypto)

-- | Full header type, carrying its own memoised bytes.
-- deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (HeaderRaw era)

newtype Header era = HeaderConstr (MemoBytes HeaderRaw era)
  deriving newtype (Eq, ToCBOR)

deriving instance (Era era, KESAlgorithm (CC.KES era)) => NoThunks (Header era)
deriving instance (Era era, HashAlgorithm (HASH (EraCrypto era))) => Show (Header era)

deriving via
  (Mem HeaderRaw crypto)
  instance
    (Era crypto, KESAlgorithm (CC.KES crypto)) => (FromCBOR (Annotator (Header crypto)))

pattern Header ::
  (Era crypto, KESAlgorithm (CC.KES crypto)) =>
  HeaderBody crypto ->
  SignedKES crypto (HeaderBody crypto) ->
  Header crypto
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
headerSize :: Era crypto => Header crypto -> Int
headerSize (HeaderConstr (Memo _ bytes)) = SBS.length bytes

-- | Hash a header
headerHash ::
  (Era crypto, HashAlgorithm (HASH crypto)) =>
  Header crypto ->
  Hash.Hash (CC.HASH crypto) EraIndependentBlockHeader
headerHash = Hash.castHash . Hash.hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance Era crypto => ToCBOR (HeaderBody crypto) where
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
      } = undefined
      -- encode $
      --   Rec HeaderBody
      --     !> To hbBlockNo
      --     !> To hbSlotNo
      --     !> To hbPrev
      --     !> To hbVk
      --     !> E encodeVerKeyVRF hbVrfVk
      --     !> To hbVrfRes
      --     !> To hbBodySize
      --     !> To hbBodyHash
      --     !> To hbOCert
      --     !> To hbProtVer

instance Era crypto => FromCBOR (HeaderBody crypto) where
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
  (Era crypto, KESAlgorithm (CC.KES crypto)) =>
  HeaderRaw crypto ->
  Encode ('Closed 'Dense) (HeaderRaw crypto)
encodeHeaderRaw (HeaderRaw body sig) =
  Rec HeaderRaw !> To body !> E encodeSignedKES sig

instance (Era crypto, KESAlgorithm (CC.KES crypto)) => ToCBOR (HeaderRaw crypto) where
  toCBOR = encode . encodeHeaderRaw

instance (Era crypto, KESAlgorithm (CC.KES crypto)) => FromCBOR (HeaderRaw crypto) where
  fromCBOR = decode $ RecD HeaderRaw <! From <! D decodeSignedKES

instance (Era crypto, KESAlgorithm (CC.KES crypto)) => FromCBOR (Annotator (HeaderRaw crypto)) where
  fromCBOR = pure <$> fromCBOR
