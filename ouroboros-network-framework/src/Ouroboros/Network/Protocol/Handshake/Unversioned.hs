{-# LANGUAGE NamedFieldPuns      #-}

-- | Unversioned protocol, used in tests and demo applications.
--
module Ouroboros.Network.Protocol.Handshake.Unversioned
  ( UnversionedProtocol (..)
  , UnversionedProtocolData (..)
  , unversionedHandshakeCodec
  , unversionedProtocolDataCodec
  , unversionedProtocol
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Codec

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Version negotiation for an unversioned protocol. We only use this for
-- tests and demos where proper versioning is excessive.
--
data UnversionedProtocol = UnversionedProtocol
  deriving (Eq, Ord, Show)


data UnversionedProtocolData = UnversionedProtocolData
  deriving (Eq, Show)


instance Acceptable UnversionedProtocolData where
  acceptableVersion UnversionedProtocolData
                    UnversionedProtocolData = Accept UnversionedProtocolData


unversionedProtocolDataCodec :: UnversionedProtocol -> CodecCBORTerm Text UnversionedProtocolData
unversionedProtocolDataCodec _ = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: UnversionedProtocolData -> CBOR.Term
      encodeTerm UnversionedProtocolData = CBOR.TNull

      decodeTerm :: CBOR.Term -> Either Text UnversionedProtocolData
      decodeTerm CBOR.TNull = Right UnversionedProtocolData
      decodeTerm t          = Left $ T.pack $ "unexpected term: " ++ show t


-- | Make a 'Versions' for an unversioned protocol. Only use this for
-- tests and demos where proper versioning is excessive.
--
unversionedProtocol :: app
                    -> Versions UnversionedProtocol
                                UnversionedProtocolData
                                app
unversionedProtocol = simpleSingletonVersions UnversionedProtocol UnversionedProtocolData


-- | 'Handshake' codec used in various tests.
--
unversionedHandshakeCodec :: MonadST m
                          => Codec (Handshake UnversionedProtocol CBOR.Term)
                                    CBOR.DeserialiseFailure m ByteString
unversionedHandshakeCodec = codecHandshake unversionedProtocolCodec
  where
    unversionedProtocolCodec :: CodecCBORTerm (String, Maybe Int) UnversionedProtocol
    unversionedProtocolCodec = CodecCBORTerm { encodeTerm, decodeTerm }
      where
        encodeTerm UnversionedProtocol = CBOR.TInt 1
        decodeTerm (CBOR.TInt 1) = Right UnversionedProtocol
        decodeTerm (CBOR.TInt n) = Left ("decode UnversionedProtocol: unknown tag", Just n)
        decodeTerm _             = Left ("decode UnversionedProtocol: deserialisation failure", Nothing)


