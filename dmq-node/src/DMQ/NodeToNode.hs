{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module DMQ.NodeToNode
  ( RemoteAddress
  , module DMQ.NodeToNode.Version
  , ClientApp
  , ServerApp
  , Apps (..)
  , ntnApps
  , Protocols (..)
  , nodeToNodeProtocols
  , initiatorProtocols
  , initiatorAndResponderProtocols
  , dmqCodecs
  , LimitsAndTimeouts
  , dmqLimitsAndTimeouts
  , HandshakeTr
  , ntnHandshakeArguments
  , stdVersionDataNTN
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, nullTracer)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Contravariant ((>$<))
import Data.Hashable (Hashable)
import Data.Typeable
import Data.Void (Void)
import System.Random (mkStdGen)

import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (Mode (..))
import Network.Mux.Types qualified as Mx
import Network.TypedProtocol.Codec (AnnotatedCodec, Codec)

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class qualified as KES
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCertSignable)

import DMQ.Configuration (Configuration, Configuration' (..), I (..))
import DMQ.Diffusion.NodeKernel (NodeKernel (..))
import DMQ.NodeToNode.Version
import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type
import DMQ.Tracer

import Ouroboros.Network.BlockFetch.ClientRegistry (bracketKeepAliveClient)
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Context (ExpandedInitiatorContext (..),
           ResponderContext (..))
import Ouroboros.Network.Driver.Limits (runAnnotatedPeerWithLimits,
           runPeerWithLimits, runPipelinedAnnotatedPeerWithLimits)
import Ouroboros.Network.Driver.Simple (TraceSendRecv)
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.KeepAlive (KeepAliveInterval (..), keepAliveClient,
           keepAliveServer)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
           MiniProtocolLimits (..), OuroborosBundle,
           OuroborosBundleWithExpandedCtx, RunMiniProtocol (..),
           StartOnDemandOrEagerly (..), TemperatureBundle (..),
           WithProtocolTemperature (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection (PeerSharing (..))
import Ouroboros.Network.PeerSharing (bracketPeerSharingClient,
           peerSharingClient, peerSharingServer)
import Ouroboros.Network.Snocket (RemoteAddress)
import Ouroboros.Network.TxSubmission.Inbound.V2 as SigSubmission
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.TxSubmission.Outbound

import Ouroboros.Network.OrphanInstances ()

import Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.KeepAlive.Client (keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Codec (byteLimitsKeepAlive,
           codecKeepAlive_v2, timeLimitsKeepAlive)
import Ouroboros.Network.Protocol.KeepAlive.Server (keepAliveServerPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits,
           ProtocolTimeLimits)
import Ouroboros.Network.Protocol.PeerSharing.Client (peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (byteLimitsPeerSharing,
           codecPeerSharing, timeLimitsPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Server (peerSharingServerPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as Protocol
import Ouroboros.Network.Protocol.TxSubmission2.Client (txSubmissionClientPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Server
           (txSubmissionServerPeerPipelined)

-- TODO: if we add `versionNumber` to `ctx` we could use `RunMiniProtocolCb`.
-- This makes sense, since `ctx` already contains `versionData`.
type ClientApp addr m a =
     NodeToNodeVersion
  -> ExpandedInitiatorContext addr m
  -> Channel m BL.ByteString
  -> m (a, Maybe BL.ByteString)

type ServerApp addr m a =
     NodeToNodeVersion
  -> ResponderContext addr
  -> Channel m BL.ByteString
  -> m (a, Maybe BL.ByteString)

data Apps addr m a b =
  Apps {
    -- | Start a sig-submission client
    aSigSubmissionClient :: ClientApp addr m a

    -- | Start a sig-submission server
  , aSigSubmissionServer :: ServerApp addr m b

    -- | Start a keep-alive client.
  , aKeepAliveClient     :: ClientApp addr m a

    -- | Start a keep-alive server.
  , aKeepAliveServer     :: ServerApp addr m b

    -- | Start a peer-sharing client.
  , aPeerSharingClient   :: ClientApp addr m a

    -- | Start a peer-sharing server.
  , aPeerSharingServer   :: ServerApp addr m b
  }

ntnApps
  :: forall crypto m addr .
    ( Crypto crypto
    , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
    , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
    , KES.ContextKES (KES crypto) ~ ()
    , KES.Signable (KES crypto) BS.ByteString
    , Typeable crypto
    , Alternative (STM m)
    , MonadAsync m
    , MonadDelay m
    , MonadFork m
    , MonadMask m
    , MonadMVar m
    , MonadThrow (STM m)
    , MonadTimer m
    , Ord addr
    , Show addr
    , Hashable addr
    , Aeson.ToJSON addr
    )
 => (forall ev. Aeson.ToJSON ev => Tracer m (WithEventType ev))
 -> Configuration
 -> NodeKernel crypto addr m
 -> Codecs crypto addr m
 -> LimitsAndTimeouts crypto addr
 -> TxDecisionPolicy
 -> Apps addr m () ()
ntnApps
    tracer
    Configuration {
      dmqcSigSubmissionClientTracer = I sigSubmissionClientTracer
    , dmqcSigSubmissionServerTracer = I sigSubmissionServerTracer
    , dmqcKeepAliveClientTracer     = I keepAliveClientTracer
    , dmqcKeepAliveServerTracer     = I keepAliveServerTracer
    , dmqcPeerSharingClientTracer   = I peerSharingClientTracer
    , dmqcPeerSharingServerTracer   = I peerSharingServerTracer
    }
    NodeKernel {
      fetchClientRegistry
    , peerSharingRegistry
    , peerSharingAPI
    , mempool
    , sigChannelVar
    , sigMempoolSem
    , sigSharedTxStateVar
    }
    Codecs {
      sigSubmissionCodec
    , keepAliveCodec
    , peerSharingCodec
    }
    LimitsAndTimeouts {
      sigSubmissionSizeLimits
    , sigSubmissionTimeLimits
    , keepAliveSizeLimits
    , keepAliveTimeLimits
    , peerSharingTimeLimits
    , peerSharingSizeLimits
    }
    sigDecisionPolicy
    =
    Apps {
      aSigSubmissionClient
    , aSigSubmissionServer
    , aKeepAliveClient
    , aKeepAliveServer
    , aPeerSharingClient
    , aPeerSharingServer
    }
  where
    sigSize :: Sig crypto -> SizeInBytes
    sigSize _ = 0 -- TODO

    mempoolReader = Mempool.getReader sigId sigSize mempool
    -- TODO: invalid signatures are just omitted from the mempool. For DMQ
    -- we need to validate signatures when we received them, and shutdown
    -- connection if we receive one, rather than validate them in the
    -- mempool.
    mempoolWriter = Mempool.getWriter sigId
                                      (pure ()) -- TODO not needed
                                      (\_ -> validateSig)
                                      (\_ -> True)
                                      mempool

    aSigSubmissionClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aSigSubmissionClient version
                         ExpandedInitiatorContext {
                           eicConnectionId   = connId,
                           eicControlMessage = controlMessage
                         } channel =
      runAnnotatedPeerWithLimits
        (if sigSubmissionClientTracer
          then WithEventType "SigSubmissionClient" . Mx.WithBearer connId >$< tracer
          else nullTracer)
        sigSubmissionCodec
        sigSubmissionSizeLimits
        sigSubmissionTimeLimits
        channel
        $ txSubmissionClientPeer
        $ txSubmissionOutbound
            nullTracer
            _MAX_SIGS_TO_ACK
            mempoolReader
            version
            controlMessage


    aSigSubmissionServer
      :: NodeToNodeVersion
      -> ResponderContext addr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aSigSubmissionServer _version ResponderContext { rcConnectionId = connId } channel =
        SigSubmission.withPeer
          nullTracer
          sigChannelVar
          sigMempoolSem
          sigDecisionPolicy
          sigSharedTxStateVar
          mempoolReader
          mempoolWriter
          sigSize
          (remoteAddress connId)
          $ \(peerSigAPI :: PeerTxAPI m SigId (Sig crypto)) ->
            runPipelinedAnnotatedPeerWithLimits
              (if sigSubmissionServerTracer
                 then WithEventType "SigSubmissionServer" . Mx.WithBearer connId >$< tracer
                 else nullTracer)
              sigSubmissionCodec
              sigSubmissionSizeLimits
              sigSubmissionTimeLimits
              channel
              $ txSubmissionServerPeerPipelined
              $ txSubmissionInboundV2
                  nullTracer -- TODO
                  _SIG_SUBMISSION_INIT_DELAY
                  mempoolWriter
                  peerSigAPI


    aKeepAliveClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aKeepAliveClient _version
                     ExpandedInitiatorContext {
                       eicConnectionId   = connId
                     , eicControlMessage = controlMessageSTM
                     }
                     channel = do
      labelThisThread "KeepAliveClient"
      let kacApp dqCtx =
            runPeerWithLimits
              (if keepAliveClientTracer
                 then WithEventType "KeepAliveClient" . Mx.WithBearer connId >$< tracer
                 else nullTracer)
              keepAliveCodec
              keepAliveSizeLimits
              keepAliveTimeLimits
              channel
              $ keepAliveClientPeer
              $ keepAliveClient nullTracer
                                (mkStdGen 0)
                                controlMessageSTM
                                connId
                                dqCtx
                                (KeepAliveInterval 10)

      ((), trailing) <- bracketKeepAliveClient fetchClientRegistry connId kacApp
      return ((), trailing)

    aKeepAliveServer
      :: NodeToNodeVersion
      -> ResponderContext addr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aKeepAliveServer _version
                     ResponderContext {
                       rcConnectionId = connId
                     }
                     channel = do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        (if keepAliveServerTracer
           then WithEventType "KeepAliveServer" . Mx.WithBearer connId >$< tracer
           else nullTracer)
        keepAliveCodec
        keepAliveSizeLimits
        keepAliveTimeLimits
        channel
        $ keepAliveServerPeer
          keepAliveServer

    aPeerSharingClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aPeerSharingClient _version
                       ExpandedInitiatorContext {
                         eicConnectionId   = connId
                       , eicControlMessage = controlMessageSTM
                       }
                       channel = do
      labelThisThread "PeerSharingClient"
      bracketPeerSharingClient peerSharingRegistry (remoteAddress connId)
        $ \controller -> do
          psClient <- peerSharingClient controlMessageSTM controller
          ((), trailing) <- runPeerWithLimits
            (if peerSharingClientTracer
               then WithEventType "PeerSharingClient" . Mx.WithBearer connId >$< tracer
               else nullTracer)
            peerSharingCodec
            peerSharingSizeLimits
            peerSharingTimeLimits
            channel
            (peerSharingClientPeer psClient)
          return ((), trailing)

    aPeerSharingServer
      :: NodeToNodeVersion
      -> ResponderContext addr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aPeerSharingServer _version
                       ResponderContext {
                         rcConnectionId = connId
                       }
                       channel = do
      labelThisThread "PeerSharingServer"
      runPeerWithLimits
        (if peerSharingServerTracer
           then WithEventType "PeerSharingServer" . Mx.WithBearer connId >$< tracer
           else nullTracer)
        peerSharingCodec
        peerSharingSizeLimits
        peerSharingTimeLimits
        channel
        $ peerSharingServerPeer
        $ peerSharingServer peerSharingAPI


data Protocols appType initiatorCtx responderCtx bytes m a b =
  Protocols {
    sigSubmissionProtocol :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b

    -- | keep-alive mini-protocol
    --
  , keepAliveProtocol    :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b

    -- | peer sharing mini-protocol
    --
  , peerSharingProtocol  :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b
  }

sigSubmissionMiniProtocolNum :: Mx.MiniProtocolNum
sigSubmissionMiniProtocolNum = Mx.MiniProtocolNum 11

keepAliveMiniProtocolNum :: Mx.MiniProtocolNum
keepAliveMiniProtocolNum = Mx.MiniProtocolNum 12

peerSharingMiniProtocolNum :: Mx.MiniProtocolNum
peerSharingMiniProtocolNum = Mx.MiniProtocolNum 13

nodeToNodeProtocols
  :: LimitsAndTimeouts crypto addr
  -> Protocols appType initiatorCtx responderCtx bytes m a b
  -> NodeToNodeVersion
  -- ^ negotiated version number
  -> NodeToNodeVersionData
  -- ^ negotiated version data
  -> OuroborosBundle appType initiatorCtx responderCtx bytes m a b
nodeToNodeProtocols LimitsAndTimeouts {
                      sigSubmissionLimits
                    , keepAliveLimits
                    , peerSharingLimits
                    }
                    Protocols {
                      sigSubmissionProtocol
                    , keepAliveProtocol
                    , peerSharingProtocol
                    }
                    _version
                    NodeToNodeVersionData {
                      peerSharing
                    }
                    =
    TemperatureBundle
      -- Hot protocols
      (WithHot [
        MiniProtocol {
          miniProtocolNum    = sigSubmissionMiniProtocolNum
        , miniProtocolStart  = StartOnDemandAny
        , miniProtocolLimits = sigSubmissionLimits
        , miniProtocolRun    = sigSubmissionProtocol
        }
      ])

      -- Warm protocols
      (WithWarm [])

      -- Established protocols: 'keep-alive', 'peer-sharing'.
      (WithEstablished $
        MiniProtocol {
          miniProtocolNum    = keepAliveMiniProtocolNum
        , miniProtocolStart  = StartOnDemandAny
        , miniProtocolLimits = keepAliveLimits
        , miniProtocolRun    = keepAliveProtocol
        }
        : case peerSharing of
            PeerSharingEnabled ->
              [ MiniProtocol {
                  miniProtocolNum    = peerSharingMiniProtocolNum
                , miniProtocolStart  = StartOnDemand
                , miniProtocolLimits = peerSharingLimits
                , miniProtocolRun    = peerSharingProtocol
                }
              ]
            PeerSharingDisabled ->
              []
      )

initiatorProtocols
  :: LimitsAndTimeouts crypto addr
  -> Apps addr m a b
  -> NodeToNodeVersion
  -> NodeToNodeVersionData
  -> OuroborosBundleWithExpandedCtx 'InitiatorMode addr BL.ByteString m a Void
initiatorProtocols limitsAndTimeouts
                   Apps {
                     aSigSubmissionClient
                   , aKeepAliveClient
                   , aPeerSharingClient
                   }
                   version =
  nodeToNodeProtocols
    limitsAndTimeouts
    (Protocols {
      sigSubmissionProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aSigSubmissionClient version))
    , keepAliveProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aKeepAliveClient version))
    , peerSharingProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aPeerSharingClient version))
    })
    version

initiatorAndResponderProtocols
  :: LimitsAndTimeouts crypto addr
  -> Apps addr m a b
  -> NodeToNodeVersion
  -> NodeToNodeVersionData
  -> OuroborosBundleWithExpandedCtx 'InitiatorResponderMode addr BL.ByteString m a b
initiatorAndResponderProtocols limitsAndTimeouts
                               Apps {
                                 aSigSubmissionClient
                               , aSigSubmissionServer
                               , aKeepAliveClient
                               , aKeepAliveServer
                               , aPeerSharingClient
                               , aPeerSharingServer
                               }
                               version =
  nodeToNodeProtocols
    limitsAndTimeouts
    (Protocols {
      sigSubmissionProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aSigSubmissionClient version))
           (MiniProtocolCb (aSigSubmissionServer version))
    , keepAliveProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aKeepAliveClient version))
           (MiniProtocolCb (aKeepAliveServer version))
    , peerSharingProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aPeerSharingClient version))
           (MiniProtocolCb (aPeerSharingServer version))
    })
    version

data Codecs crypto addr m =
  Codecs {
    sigSubmissionCodec :: AnnotatedCodec (SigSubmission crypto)
                            CBOR.DeserialiseFailure m BL.ByteString
  , keepAliveCodec     :: Codec KeepAlive
                            CBOR.DeserialiseFailure m BL.ByteString
  , peerSharingCodec   :: Codec (Protocol.PeerSharing addr)
                            CBOR.DeserialiseFailure m BL.ByteString
  }

dmqCodecs :: ( Crypto crypto
             , MonadST m
             )
          => (addr -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s addr)
          -> Codecs crypto addr m
dmqCodecs encodeAddr decodeAddr =
  Codecs {
    sigSubmissionCodec = codecSigSubmission
  , keepAliveCodec     = codecKeepAlive_v2
  , peerSharingCodec   = codecPeerSharing encodeAddr decodeAddr
  }

data LimitsAndTimeouts crypto addr =
  LimitsAndTimeouts {
    -- sig-submission
    sigSubmissionLimits
      :: MiniProtocolLimits
  , sigSubmissionSizeLimits
      :: ProtocolSizeLimits (SigSubmission crypto) BL.ByteString
  , sigSubmissionTimeLimits
      :: ProtocolTimeLimits (SigSubmission crypto)

    -- keep-alive
  , keepAliveLimits
      :: MiniProtocolLimits
  , keepAliveSizeLimits
      :: ProtocolSizeLimits KeepAlive BL.ByteString
  , keepAliveTimeLimits
      :: ProtocolTimeLimits KeepAlive

    -- peer sharing
  , peerSharingLimits
      :: MiniProtocolLimits
  , peerSharingTimeLimits
      :: ProtocolTimeLimits (Protocol.PeerSharing addr)
  , peerSharingSizeLimits
      :: ProtocolSizeLimits (Protocol.PeerSharing addr) BL.ByteString
  }

dmqLimitsAndTimeouts :: LimitsAndTimeouts crypto addr
dmqLimitsAndTimeouts =
    LimitsAndTimeouts {
      sigSubmissionLimits =
        MiniProtocolLimits {
          -- TODO
          maximumIngressQueue = maxBound
        }
    , sigSubmissionTimeLimits = timeLimitsSigSubmission
    , sigSubmissionSizeLimits = byteLimitsSigSubmission size

    , keepAliveLimits     =
        MiniProtocolLimits {
          -- One small outstanding message.
          maximumIngressQueue = addSafetyMargin 1280
        }

    , keepAliveTimeLimits = timeLimitsKeepAlive
    , keepAliveSizeLimits = byteLimitsKeepAlive size

    , peerSharingLimits   =
        MiniProtocolLimits {
          -- This protocol does not need to be pipelined and a peer can only ask
          -- for a maximum of 255 peers each time. Hence a reply can have up to
          -- 255 IP (IPv4 or IPv6) addresses so 255 * 16 = 4080. TCP has an initial
          -- window size of 4 and a TCP segment is 1440, which gives us 4 * 1440 =
          -- 5760 bytes to fit into a single RTT. So setting the maximum ingress
          -- queue to be a single RTT should be enough to cover for CBOR overhead.
          maximumIngressQueue = 4 * 1440
        }
    , peerSharingTimeLimits = timeLimitsPeerSharing
    , peerSharingSizeLimits = byteLimitsPeerSharing size
    }
  where
    size :: BL.ByteString -> Word
    size = fromIntegral . BL.length


type HandshakeTr ntnAddr = Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term))

ntnHandshakeArguments
  :: MonadST m
  => Tracer m (HandshakeTr ntnAddr)
  -> HandshakeArguments
      (ConnectionId ntnAddr)
      NodeToNodeVersion
      NodeToNodeVersionData
      m
ntnHandshakeArguments tracer =
  HandshakeArguments {
    haHandshakeTracer  = tracer
  , haBearerTracer     = nullTracer -- TODO
  , haHandshakeCodec   = codecHandshake nodeToNodeVersionCodec
  , haVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
  , haAcceptVersion    = acceptableVersion
  , haQueryVersion     = queryVersion
  , haTimeLimits       = timeLimitsHandshake
  }

stdVersionDataNTN :: NetworkMagic
                  -> DiffusionMode
                  -> PeerSharing
                  -> NodeToNodeVersionData
stdVersionDataNTN networkMagic diffusionMode peerSharing =
  NodeToNodeVersionData
    { networkMagic
    , diffusionMode
    , peerSharing
    , query = False
    }

-- TODO: choose wisely, is a protocol parameter.
_MAX_SIGS_TO_ACK :: NumTxIdsToAck
_MAX_SIGS_TO_ACK = 20

_SIG_SUBMISSION_INIT_DELAY :: TxSubmissionInitDelay
_SIG_SUBMISSION_INIT_DELAY = NoTxSubmissionInitDelay


-- TODO: this is duplicated code, similar function is in
-- `Cardano.Network.NodeToNode` module.
addSafetyMargin :: Int -> Int
addSafetyMargin x = x + x `div` 10
