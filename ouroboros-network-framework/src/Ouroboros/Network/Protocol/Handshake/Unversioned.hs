{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad.Class.MonadST

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as T

import Network.TypedProtocol.Codec

import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version


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

instance Queryable UnversionedProtocolData where
  queryVersion UnversionedProtocolData = False


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
unversionedProtocol app =
    simpleSingletonVersions UnversionedProtocol
                            UnversionedProtocolData
                            (\_ -> app)


-- | Alternative for 'UnversionedProtocolData' which contains 'DataFlow'.
--
data DataFlowProtocolData =
    DataFlowProtocolData {
      getProtocolDataFlow    :: DataFlow,
      getProtocolPeerSharing :: PeerSharing
    }
  deriving (Eq, Show)

instance Acceptable DataFlowProtocolData where
  acceptableVersion (DataFlowProtocolData local lps) (DataFlowProtocolData remote rps) =
    Accept (DataFlowProtocolData (local `min` remote) (lps <> rps))

instance Queryable DataFlowProtocolData where
  queryVersion (DataFlowProtocolData _ _) = False

dataFlowProtocolDataCodec :: UnversionedProtocol -> CodecCBORTerm Text DataFlowProtocolData
dataFlowProtocolDataCodec _ = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: DataFlowProtocolData -> CBOR.Term
      encodeTerm (DataFlowProtocolData Unidirectional ps) =
        let peerSharing = case ps of
              PeerSharingDisabled -> 0
              PeerSharingEnabled  -> 1
         in CBOR.TList [CBOR.TBool False, CBOR.TInt peerSharing]
      encodeTerm (DataFlowProtocolData Duplex ps) =
        let peerSharing = case ps of
              PeerSharingDisabled -> 0
              PeerSharingEnabled  -> 1
         in CBOR.TList [CBOR.TBool True, CBOR.TInt peerSharing]

      toPeerSharing :: Int -> Either Text PeerSharing
      toPeerSharing 0 = Right PeerSharingDisabled
      toPeerSharing 1 = Right PeerSharingEnabled
      toPeerSharing _ = Left "toPeerSharing: out of bounds"

      decodeTerm :: CBOR.Term -> Either Text DataFlowProtocolData
      decodeTerm (CBOR.TList [CBOR.TBool False, CBOR.TInt a]) = DataFlowProtocolData Unidirectional <$> (toPeerSharing a)
      decodeTerm (CBOR.TList [CBOR.TBool True, CBOR.TInt a])  = DataFlowProtocolData Duplex <$> (toPeerSharing a)
      decodeTerm t                  = Left $ T.pack $ "unexpected term: " ++ show t

dataFlowProtocol :: DataFlow
                 -> app
                 -> Versions UnversionedProtocol
                             DataFlowProtocolData
                             app
dataFlowProtocol dataFlow app =
    simpleSingletonVersions UnversionedProtocol
                            (DataFlowProtocolData dataFlow PeerSharingDisabled)
                            (\_ -> app)

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


