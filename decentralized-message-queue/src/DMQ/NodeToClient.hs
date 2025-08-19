{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE RankNTypes     #-}

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

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, nullTracer)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Term qualified as CBOR

import Network.Mux qualified as Mx
import Network.TypedProtocol.Codec hiding (encode, decode)
import Network.TypedProtocol.Codec.CBOR qualified as CBOR

import DMQ.NodeToClient.LocalMsgSubmission
import DMQ.NodeToClient.LocalMsgNotification
import DMQ.NodeToClient.Version
import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.LocalMsgSubmission.Server
import DMQ.Protocol.LocalMsgSubmission.Type

import Ouroboros.Network.Context
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake (Handshake,
           HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, noTimeLimitsHandshake)
import Ouroboros.Network.Util.ShowProxy
import Ouroboros.Network.TxSubmission.Mempool.Reader
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (TxSubmissionMempoolWriter)
import Data.Word


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


data Codecs m sig reject =
  Codecs {
    msgSubmissionCodec
      :: !(Codec (LocalMsgSubmission sig reject)
                 CBOR.DeserialiseFailure m ByteString)
  , msgNotificationCodec
      :: !(Codec (LocalMsgNotification sig)
               CBOR.DeserialiseFailure m ByteString)
  }

dmqCodecs :: MonadST m
          => (sig -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s sig)
          -> (reject -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s reject)
          -> Codecs m sig reject
dmqCodecs encodeSig decodeSig encodeReject' decodeReject' =
  Codecs {
    msgSubmissionCodec = codecLocalMsgSubmission encodeSig decodeSig encodeReject' decodeReject'
  , msgNotificationCodec = codecLocalMsgNotification encodeSig decodeSig
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
  :: (MonadThrow m, MonadThread m, MonadSTM m, ShowProxy reject, ShowProxy sig)
  => reject
  -> TxSubmissionMempoolReader msgid sig idx m
  -> TxSubmissionMempoolWriter msgid sig idx m
  -> Word16
  -> Codecs m sig reject
  -> Apps ntcAddr m ()
ntcApps reject mempoolReader mempoolWriter maxMsgs
        Codecs { msgSubmissionCodec, msgNotificationCodec } =
  Apps {
    aLocalMsgSubmission
  , aLocalMsgNotification
  }
  where
    aLocalMsgSubmission _version _ctx channel = do
      labelThisThread "LocalMsgSubmissionServer"
      runPeer
        nullTracer
        msgSubmissionCodec
        channel
        (localMsgSubmissionServerPeer $
          localMsgSubmissionServer nullTracer reject mempoolWriter)

    aLocalMsgNotification _version _ctx channel = do
      labelThisThread "LocalMsgNotificationServer"
      runPeer
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
    -- TODO: verify protocol numbers
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
