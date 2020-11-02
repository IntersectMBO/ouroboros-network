{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Chain.Serialised (
    Serialised (..)
  , wrapCBORinCBOR
  , unwrapCBORinCBOR
  , mkSerialised
  , fromSerialised
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.CBOR.Read as Read
import qualified Codec.CBOR.Write as Write
import           Codec.Serialise (Serialise (..))
import           Control.Monad (when)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as BSC

import           Ouroboros.Chain.HasHeader
import           Ouroboros.Chain.Util.ShowProxy

-- | An already serialised value
--
-- When streaming blocks/header from disk to the network, there is often no
-- need to deserialise them, as we'll just end up serialising them again when
-- putting them on the wire.
newtype Serialised a = Serialised {
      unSerialised :: Lazy.ByteString
    }
  deriving (Eq)

instance Show (Serialised a) where
  show (Serialised bytes) = BSC.unpack (B16.encode bytes)

instance ShowProxy a => ShowProxy (Serialised a) where
    showProxy _ = "Serialised " ++ showProxy (Proxy @a)

type instance HeaderHash (Serialised block) = HeaderHash block
instance StandardHash block => StandardHash (Serialised block)

-- | Wrap CBOR-in-CBOR
--
-- This is primarily useful for the /decoder/; see 'unwrapCBORinCBOR'
wrapCBORinCBOR :: (a -> Encoding) -> a -> Encoding
wrapCBORinCBOR enc = encode . mkSerialised enc

-- | Unwrap CBOR-in-CBOR
--
-- The CBOR-in-CBOR encoding gives us the 'ByteString' we need in order to
-- to construct annotations.
unwrapCBORinCBOR :: (forall s. Decoder s (Lazy.ByteString -> a))
                 -> (forall s. Decoder s a)
unwrapCBORinCBOR dec = fromSerialised dec =<< decode

-- | Construct 'Serialised' value from an unserialised value
mkSerialised :: (a -> Encoding) -> a -> Serialised a
mkSerialised enc = Serialised . Write.toLazyByteString . enc

-- | Decode a 'Serialised' value
--
-- Unlike a regular 'Decoder', which has an implicit input stream,
-- 'fromSerialised' takes the 'Serialised' value as an argument.
fromSerialised :: (forall s. Decoder s (Lazy.ByteString -> a))
               -> Serialised a -> (forall s. Decoder s a)
fromSerialised dec (Serialised payload) =
    case Read.deserialiseFromBytes dec payload of
      Left (Read.DeserialiseFailure _ reason) -> fail reason
      Right (trailing, mkA)
        | not (Lazy.null trailing) -> fail "trailing bytes in CBOR-in-CBOR"
        | otherwise                -> return (mkA payload)

-- | CBOR-in-CBOR
--
-- TODO: replace with encodeEmbeddedCBOR from cborg-0.2.4 once
-- it is available, since that will be faster.
--
-- TODO: Avoid converting to a strict ByteString, as that requires copying O(n)
-- in case the lazy ByteString consists of more than one chunks.
instance Serialise (Serialised a) where
  encode (Serialised bs) = mconcat [
        Enc.encodeTag 24
      , Enc.encodeBytes (Lazy.toStrict bs)
      ]

  decode = do
      tag <- Dec.decodeTag
      when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
      Serialised . Lazy.fromStrict <$> Dec.decodeBytes
