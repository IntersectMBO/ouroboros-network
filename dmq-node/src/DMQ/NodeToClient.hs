{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module DMQ.NodeToClient
  ( module DMQ.NodeToClient.Version
  , Protocols (..)
  , HandshakeTr
  , Apps
  , dmqCodecs
  , ntcApps
  , ntcHandshakeArguments
  , responders
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Typeable (Typeable)
import Data.Void
import Data.Word

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, nullTracer)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Term qualified as CBOR

import Cardano.KESAgent.KES.Crypto (Crypto (..))

import Network.Mux qualified as Mx
import Network.TypedProtocol.Codec hiding (decode, encode)
import Network.TypedProtocol.Codec.CBOR qualified as CBOR

import DMQ.Configuration
import DMQ.NodeToClient.LocalMsgNotification
import DMQ.NodeToClient.LocalMsgSubmission
import DMQ.NodeToClient.Version
import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import DMQ.Protocol.SigSubmission.Type (Sig, SigId, sigId)
import DMQ.Tracer

import Ouroboros.Network.Context
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Mux
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, noTimeLimitsHandshake)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TxSubmissionMempoolWriter)
import Ouroboros.Network.TxSubmission.Mempool.Reader


type HandshakeTr ntcAddr = Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))

ntcHandshakeArguments
  :: MonadST m
  => Tracer m (HandshakeTr ntcAddr)
  -> HandshakeArguments
      (ConnectionId ntcAddr)
      NodeToClientVersion
      NodeToClientVersionData
      m
ntcHandshakeArguments tracer =
  HandshakeArguments {
    haHandshakeTracer  = tracer
  , haBearerTracer     = nullTracer -- TODO
  , haHandshakeCodec   = codecHandshake nodeToClientVersionCodec
  , haVersionDataCodec =
      cborTermVersionDataCodec
        nodeToClientCodecCBORTerm
  , haAcceptVersion = acceptableVersion
  , haQueryVersion  = queryVersion
  , haTimeLimits    = noTimeLimitsHandshake
  }


data Codecs crypto m =
  Codecs {
    msgSubmissionCodec
      :: !(AnnotatedCodec (LocalMsgSubmission (Sig crypto))
                 CBOR.DeserialiseFailure m ByteString)
  , msgNotificationCodec
      :: !(AnnotatedCodec (LocalMsgNotification (Sig crypto))
               CBOR.DeserialiseFailure m ByteString)
  }

dmqCodecs :: ( MonadST m
             , Crypto crypto
             )
          => (SigMempoolFail -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s SigMempoolFail)
          -> Codecs crypto m
dmqCodecs encodeReject' decodeReject' =
  Codecs {
    msgSubmissionCodec   = codecLocalMsgSubmission encodeReject' decodeReject'
  , msgNotificationCodec = codecLocalMsgNotification
  }


-- | A node-to-client application
--
type App ntcAddr m a =
     NodeToClientVersion
  -> ResponderContext ntcAddr
  -> Mx.Channel m ByteString
  -> m (a, Maybe ByteString)


data Apps ntcAddr m a =
  Apps {
    -- | Start a sig-submission client
    aLocalMsgSubmission   :: !(App ntcAddr m a)

    -- | Start a sig-submission server
  , aLocalMsgNotification :: !(App ntcAddr m a)
  }


-- | Construct applications for the node-to-client protocols
--
ntcApps
  :: forall crypto idx ntcAddr m.
     ( MonadThrow m
     , MonadThread m
     , MonadSTM m
     , Crypto crypto
     , Typeable crypto
     , Aeson.ToJSON ntcAddr
     )
  => (forall ev. Aeson.ToJSON ev => Tracer m (WithEventType ev))
  -> Configuration
  -> TxSubmissionMempoolReader SigId (Sig crypto) idx m
  -> TxSubmissionMempoolWriter SigId (Sig crypto) idx m
  -> Word16
  -> Codecs crypto m
  -> Apps ntcAddr m ()
ntcApps tracer
        Configuration { dmqcLocalMsgSubmissionServerTracer   = I localMsgSubmissionServerTracer,
                        dmqcLocalMsgNotificationServerTracer = I localMsgNotificationServerTracer
                      }
        mempoolReader
        mempoolWriter
        maxMsgs
        Codecs { msgSubmissionCodec, msgNotificationCodec } =
  Apps {
    aLocalMsgSubmission
  , aLocalMsgNotification
  }
  where
    aLocalMsgSubmission _version ResponderContext { rcConnectionId = connId } channel = do
      labelThisThread "LocalMsgSubmissionServer"
      runAnnotatedPeer
        (if localMsgSubmissionServerTracer
           then WithEventType "LocalMsgSubmissionServer" . Mx.WithBearer connId >$< tracer
           else nullTracer)
        msgSubmissionCodec
        channel
        (localMsgSubmissionServerPeer $
          localMsgSubmissionServer
            sigId
            -- TODO: use a separate option for this tracer rather than reusing
            -- `dmqLocalMsgSubmissionServerTracer`.
            (if localMsgSubmissionServerTracer
               then WithEventType "LocalMsgSubmissionServer" . Mx.WithBearer connId >$< tracer
               else nullTracer)
            mempoolWriter)

    aLocalMsgNotification _version ResponderContext { rcConnectionId = connId } channel = do
      labelThisThread "LocalMsgNotificationServer"
      runAnnotatedPeer
        (if localMsgNotificationServerTracer
           then WithEventType "LocalMsgNotificationServer" . Mx.WithBearer connId >$< tracer
           else nullTracer)
        msgNotificationCodec
        channel
        (localMsgNotificationServerPeer $
          localMsgNotificationServer
            nullTracer
            (pure ()) maxMsgs mempoolReader)


data Protocols appType ntcAddr bytes m a b =
  Protocols {
    msgSubmissionProtocol   :: !(RunMiniProtocolWithMinimalCtx appType ntcAddr bytes m a b)
  , msgNotificationProtocol :: !(RunMiniProtocolWithMinimalCtx appType ntcAddr bytes m a b)
  }

responders
  :: Apps ntcAddr m a
  -> NodeToClientVersion
  -> NodeToClientVersionData
  -> OuroborosApplicationWithMinimalCtx Mx.ResponderMode ntcAddr ByteString m Void a
responders Apps {
             aLocalMsgSubmission
           , aLocalMsgNotification
           }
           version =
  nodeToClientProtocols
    Protocols {
      msgSubmissionProtocol =
        ResponderProtocolOnly $
           MiniProtocolCb $ aLocalMsgSubmission version
    , msgNotificationProtocol =
        ResponderProtocolOnly $
           MiniProtocolCb $ aLocalMsgNotification version
    }
    version


-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-client protocol.
--
-- This function specifies the wire format protocol numbers as well as the
-- protocols that run for each 'NodeToClientVersion'.
--
-- They are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
nodeToClientProtocols
  :: Protocols appType ntcAddr bytes m a b
  -> NodeToClientVersion
  -> NodeToClientVersionData
  -> OuroborosApplicationWithMinimalCtx appType ntcAddr bytes m a b
nodeToClientProtocols protocols _version _versionData =
  OuroborosApplication $
    case protocols of
      Protocols {
        msgSubmissionProtocol
      , msgNotificationProtocol
      } ->
        [ localMsgSubmission msgSubmissionProtocol
        , localMsgNotification msgNotificationProtocol
        ]
  where
    localMsgSubmission protocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 14,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = protocol
      }
    localMsgNotification protocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 15,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = protocol
    }
    maximumMiniProtocolLimits =
      MiniProtocolLimits {
        maximumIngressQueue = 0xffffffff
      }
