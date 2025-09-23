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

import Data.ByteString.Lazy (ByteString)
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

import DMQ.NodeToClient.LocalMsgNotification
import DMQ.NodeToClient.LocalMsgSubmission
import DMQ.NodeToClient.Version
import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type
import DMQ.Protocol.SigSubmission.Type (Sig)

import Ouroboros.Network.Context
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, noTimeLimitsHandshake)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TxSubmissionMempoolWriter)
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.Util.ShowProxy


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


data Codecs m sig =
  Codecs {
    msgSubmissionCodec
      :: !(AnnotatedCodec (LocalMsgSubmission sig)
                 CBOR.DeserialiseFailure m ByteString)
  , msgNotificationCodec
      :: !(AnnotatedCodec (LocalMsgNotification sig)
               CBOR.DeserialiseFailure m ByteString)
  }

dmqCodecs :: ( MonadST m
             , Crypto crypto
             )
          => (SigMempoolFail -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s SigMempoolFail)
          -> Codecs m (Sig crypto)
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
  :: (MonadThrow m, MonadThread m, MonadSTM m, ShowProxy SigMempoolFail, ShowProxy sig)
  => TxSubmissionMempoolReader msgid sig idx m
  -> TxSubmissionMempoolWriter msgid sig idx m
  -> Word16
  -> Codecs m sig
  -> Apps ntcAddr m ()
ntcApps mempoolReader mempoolWriter maxMsgs
        Codecs { msgSubmissionCodec, msgNotificationCodec } =
  Apps {
    aLocalMsgSubmission
  , aLocalMsgNotification
  }
  where
    aLocalMsgSubmission _version _ctx channel = do
      labelThisThread "LocalMsgSubmissionServer"
      runAnnotatedPeer
        nullTracer
        msgSubmissionCodec
        channel
        (localMsgSubmissionServerPeer $
          localMsgSubmissionServer nullTracer mempoolWriter)

    aLocalMsgNotification _version _ctx channel = do
      labelThisThread "LocalMsgNotificationServer"
      runAnnotatedPeer
        nullTracer
        msgNotificationCodec
        channel
        (localMsgNotificationServerPeer $
          localMsgNotificationServer nullTracer (pure ()) maxMsgs mempoolReader)


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
