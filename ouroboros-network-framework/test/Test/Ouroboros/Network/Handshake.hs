{-# LANGUAGE NamedFieldPuns      #-}

-- | Helpers for test code that does not need to deal with protocol versioning.
--
module Test.Ouroboros.Network.Handshake where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise(..))

import           Ouroboros.Network.Protocol.Handshake.Version


data UnversionedProtocol = UnversionedProtocol
  deriving (Eq, Ord, Enum, Show)

instance Serialise UnversionedProtocol where
    encode UnversionedProtocol = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return UnversionedProtocol
        _ -> fail "decode UnversionedProtocol: expected version 1"


data UnversionedProtocolData = UnversionedProtocolData
  deriving (Eq, Show)

instance Acceptable UnversionedProtocolData where
  acceptableVersion UnversionedProtocolData
                    UnversionedProtocolData = Accept

unversionedProtocolDataCodec :: CodecCBORTerm Text UnversionedProtocolData
unversionedProtocolDataCodec = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: UnversionedProtocolData -> CBOR.Term
      encodeTerm UnversionedProtocolData = CBOR.TNull

      decodeTerm :: CBOR.Term -> Either Text UnversionedProtocolData
      decodeTerm CBOR.TNull = Right UnversionedProtocolData
      decodeTerm t          = Left $ T.pack $ "unexpected term: " ++ show t
      
unversionedProtocol :: app -> Versions UnversionedProtocol DictVersion app
unversionedProtocol =
    simpleSingletonVersions UnversionedProtocol UnversionedProtocolData
                            (DictVersion unversionedProtocolDataCodec)

