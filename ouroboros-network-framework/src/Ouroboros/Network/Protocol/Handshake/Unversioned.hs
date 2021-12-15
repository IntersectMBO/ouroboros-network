{-# LANGUAGE NamedFieldPuns #-}

-- | Unversioned protocol, used in tests and demo applications.
--
module Ouroboros.Network.Protocol.Handshake.Unversioned
  ( UnversionedProtocol (..)
  , UnversionedProtocolData (..)
  , unversionedHandshakeCodec
  , unversionedProtocolDataCodec
  , unversionedProtocol
  , DataFlowProtocolData (..)
  , dataFlowProtocolDataCodec
  , dataFlowProtocol
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.TypedProtocol.Codec

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
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


unversionedProtocolDataCodec :: VersionDataCodec CBOR.Term UnversionedProtocol
                                                           UnversionedProtocolData
unversionedProtocolDataCodec = cborTermVersionDataCodec
                                 (const CodecCBORTerm {encodeTerm, decodeTerm})
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


-- | Alternative for 'UnversionedProtocolData' which contains 'DataFlow'.
--
newtype DataFlowProtocolData =
    DataFlowProtocolData { getProtocolDataFlow :: DataFlow }
  deriving (Eq, Show)

instance Acceptable DataFlowProtocolData where
  acceptableVersion (DataFlowProtocolData local) (DataFlowProtocolData remote) =
    Accept (DataFlowProtocolData $ local `min` remote)

dataFlowProtocolDataCodec :: UnversionedProtocol -> CodecCBORTerm Text DataFlowProtocolData
dataFlowProtocolDataCodec _ = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: DataFlowProtocolData -> CBOR.Term
      encodeTerm (DataFlowProtocolData Unidirectional) = CBOR.TBool False
      encodeTerm (DataFlowProtocolData Duplex)         = CBOR.TBool True

      decodeTerm :: CBOR.Term -> Either Text DataFlowProtocolData
      decodeTerm (CBOR.TBool False) = Right (DataFlowProtocolData Unidirectional)
      decodeTerm (CBOR.TBool True)  = Right (DataFlowProtocolData Duplex)
      decodeTerm t                  = Left $ T.pack $ "unexpected term: " ++ show t

dataFlowProtocol :: DataFlow
                 -> app
                 -> Versions UnversionedProtocol
                             DataFlowProtocolData
                             app
dataFlowProtocol dataFlow =
    simpleSingletonVersions UnversionedProtocol (DataFlowProtocolData dataFlow)

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


