{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- for 'debugTracer'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | This module contains experiments which can be executed either in `IO` or
-- in `IOSim`.
--
module Test.Ouroboros.Network.ConnectionManager.Experiments
  ( ClientAndServerData (..)
  , unidirectionalExperiment
  , bidirectionalExperiment
  , ConnectionManagerMonad
  , withInitiatorOnlyConnectionManager
  , withBidirectionalConnectionManager
  , runInitiatorProtocols
  , oneshotNextRequests
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (AssertionFailed)
import Control.Monad (replicateM, (>=>))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)
import Control.Tracer (Tracer (..), contramap, nullTracer)

import Codec.Serialise.Class (Serialise)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor (($>), (<&>))
import Data.Hashable
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void)

import System.Random (StdGen, split)

import Test.QuickCheck

import Codec.CBOR.Term (Term)

import Network.Mux qualified as Mx
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client

import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Codec.CBOR
import Network.TypedProtocol.ReqResp.Examples
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Type as ReqResp

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.ControlMessage
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.InboundGovernor qualified as InboundGovernor
import Ouroboros.Network.Mux
import Ouroboros.Network.MuxMode
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           noTimeLimitsHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..),
           Queryable (..))
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server (RemoteTransitionTrace)
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket (Snocket)
import Ouroboros.Network.Snocket qualified as Snocket

import Test.Ouroboros.Network.ConnectionManager.Timeouts
import Test.Ouroboros.Network.Orphans ()
import Test.Ouroboros.Network.Utils (WithName (..))

import Ouroboros.Network.ConnectionManager.InformationChannel
           (newInformationChannel)


--
-- Server tests
--

-- | The protocol will run three instances of  `ReqResp` protocol; one for each
-- state: warm, hot and established.
--
data ClientAndServerData req = ClientAndServerData {
    accumulatorInit              :: req,
    -- ^ Initial value. In for each request the server sends back a list received requests (in
    --   reverse order) terminating with the accumulatorInit.
    hotInitiatorRequests         :: [[req]],
    -- ^ list of requests run by the hot initiator in each round; Running
    -- multiple rounds allows us to test restarting of responders.
    warmInitiatorRequests        :: [[req]],
    -- ^ list of requests run by the warm initiator in each round
    establishedInitiatorRequests :: [[req]]
    -- ^ list of requests run by the established initiator in each round
  }
  deriving Show


-- Number of rounds to exhaust all the requests.
--
numberOfRounds :: ClientAndServerData req ->  Int
numberOfRounds ClientAndServerData {
                  hotInitiatorRequests,
                  warmInitiatorRequests,
                  establishedInitiatorRequests
                } =
    length hotInitiatorRequests
    `max`
    length warmInitiatorRequests
    `max`
    length establishedInitiatorRequests


-- | We use it to generate a list of messages for a list of rounds.  In each
-- round we connect to the same server, and run 'ReqResp' protocol.
--
arbitraryList :: Arbitrary a =>  Gen [[a]]
arbitraryList =
    resize 3 (listOf (resize 3 (listOf (resize 100 arbitrary))))

instance Arbitrary req => Arbitrary (ClientAndServerData req) where
    arbitrary =
      ClientAndServerData <$> arbitrary
                          <*> arbitraryList
                          <*> arbitraryList
                          <*> arbitraryList

    shrink (ClientAndServerData ini hot warm est) = concat
      [ shrink ini  <&> \ ini'  -> ClientAndServerData ini' hot  warm  est
      , shrink hot  <&> \ hot'  -> ClientAndServerData ini  hot' warm  est
      , shrink warm <&> \ warm' -> ClientAndServerData ini  hot  warm' est
      , shrink est  <&> \ est'  -> ClientAndServerData ini  hot  warm  est'
      ]

expectedResult :: ClientAndServerData req
               -> ClientAndServerData req
               -> [TemperatureBundle [[req]]]
expectedResult client@ClientAndServerData
                                   { hotInitiatorRequests
                                   , warmInitiatorRequests
                                   , establishedInitiatorRequests
                                   }
               ClientAndServerData { accumulatorInit
                                   } =
    go
      (take rounds $ hotInitiatorRequests         ++ repeat [])
      (take rounds $ warmInitiatorRequests        ++ repeat [])
      (take rounds $ establishedInitiatorRequests ++ repeat [])
  where
    rounds = numberOfRounds client
    fn acc x = (x : acc, x : acc)
    go (a : as) (b : bs) (c : cs) =
      TemperatureBundle
        (WithHot         (snd $ mapAccumL fn [accumulatorInit] a))
        (WithWarm        (snd $ mapAccumL fn [accumulatorInit] b))
        (WithEstablished (snd $ mapAccumL fn [accumulatorInit] c))
      : go as bs cs
    go [] [] [] = []
    go _  _  _  = error "expectedResult: impossible happened"

noNextRequests :: forall stm req peerAddr. Applicative stm => TemperatureBundle (ConnectionId peerAddr -> stm [req])
noNextRequests = pure $ \_ -> pure []

-- | Next requests bundle for bidirectional and unidirectional experiments.
oneshotNextRequests
  :: forall req peerAddr m. MonadSTM m
  => ClientAndServerData req
  -> m (TemperatureBundle (ConnectionId peerAddr -> STM m [req]))
oneshotNextRequests ClientAndServerData {
                      hotInitiatorRequests,
                      warmInitiatorRequests,
                      establishedInitiatorRequests
                    } = do
    -- we pass a `StrictTVar` with all the requests to each initiator.  This way
    -- the each round (which runs a single instance of `ReqResp` protocol) will
    -- use its own request list.
    hotRequestsVar         <- newTVarIO hotInitiatorRequests
    warmRequestsVar        <- newTVarIO warmInitiatorRequests
    establishedRequestsVar <- newTVarIO establishedInitiatorRequests
    return $ TemperatureBundle
               (WithHot hotRequestsVar)
               (WithWarm warmRequestsVar)
               (WithEstablished establishedRequestsVar)
              <&> \ reqVar _ -> popRequests reqVar
  where
    popRequests requestsVar = do
      requests <- readTVar requestsVar
      case requests of
        reqs : rest -> writeTVar requestsVar rest $> reqs
        []          -> pure []


--
-- Various ConnectionManagers
--

type ConnectionManagerMonad m =
       ( Alternative (STM m), MonadAsync m, MonadCatch m, MonadEvaluate m,
         MonadFork m, MonadMask  m, MonadST m, MonadTime m, MonadTimer m,
         MonadThrow m, MonadThrow (STM m)
       )


withInitiatorOnlyConnectionManager
    :: forall name peerAddr socket req resp m a.
       ( ConnectionManagerMonad m

       , resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req
       , MonadAsync m
       , MonadDelay m
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m, Show req
       , Show name
       )
    => name
    -- ^ identifier (for logging)
    -> Timeouts
    -> Tracer m (WithName name (AbstractTransitionTrace CM.ConnStateId))
    -> Tracer m (WithName name
                          (CM.Trace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    -> StdGen
    -> Snocket m socket peerAddr
    -> Mx.MakeBearer m socket
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> CM.ConnStateIdSupply m
    -> Maybe peerAddr
    -> TemperatureBundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
    -- ^ Handshake time limits
    -> AcceptedConnectionsLimit
    -> (ConnectionManagerWithExpandedCtx
          Mx.InitiatorMode socket peerAddr
          DataFlowProtocolData UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager name timeouts trTracer tracer stdGen snocket makeBearer connStateIdSupply
                                   localAddr nextRequests handshakeTimeLimits acceptedConnLimit k = do
    mainThreadId <- myThreadId
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer
    CM.with
      CM.Arguments {
          -- ConnectionManagerTrace
          CM.tracer    = WithName name
                        `contramap` tracer,
          CM.trTracer  = (WithName name . fmap CM.abstractState)
                        `contramap` trTracer,
         -- MuxTracer
          CM.muxTracer = muxTracer,
          CM.ipv4Address = localAddr,
          CM.ipv6Address = Nothing,
          CM.addressType = \_ -> Just IPv4Address,
          CM.snocket = snocket,
          CM.makeBearer = makeBearer,
          CM.configureSocket = \_ _ -> return (),
          CM.connectionDataFlow = \(DataFlowProtocolData df _) -> df,
          CM.prunePolicy = simplePrunePolicy,
          CM.stdGen,
          CM.connectionsLimits = acceptedConnLimit,
          CM.timeWaitTimeout = tTimeWaitTimeout timeouts,
          CM.outboundIdleTimeout = tOutboundIdleTimeout timeouts,
          CM.updateVersionData = \a _ -> a,
          CM.connStateIdSupply
        }
      (makeConnectionHandler
        muxTracer
        SingInitiatorMode
        noBindForkPolicy
        HandshakeArguments {
            -- TraceSendRecv
            haHandshakeTracer = (name,) `contramap` nullTracer,
            haHandshakeCodec = unversionedHandshakeCodec,
            haVersionDataCodec = cborTermVersionDataCodec dataFlowProtocolDataCodec,
            haAcceptVersion = acceptableVersion,
            haQueryVersion = queryVersion,
            haTimeLimits = handshakeTimeLimits
          }
        (dataFlowProtocol Unidirectional clientApplication)
        (mainThreadId, debugMuxErrorRethrowPolicy
                    <> debugMuxRuntimeErrorRethrowPolicy
                    <> debugIOErrorRethrowPolicy
                    <> assertRethrowPolicy))
      (\_ -> HandshakeFailure)
      NotInResponderMode
      (\cm ->
        k cm `catch` \(e :: SomeException) -> throwIO e)
  where
    clientApplication :: TemperatureBundle
                           [MiniProtocol Mx.InitiatorMode
                                         (ExpandedInitiatorContext peerAddr m)
                                         (ResponderContext peerAddr)
                                         ByteString m [resp] Void]
    clientApplication = mkProto <$> (Mx.MiniProtocolNum <$> nums)
                                <*> nextRequests

      where nums = TemperatureBundle (WithHot 1) (WithWarm 2) (WithEstablished 3)
            mkProto miniProtocolNum nextRequest =
              [MiniProtocol {
                  miniProtocolNum,
                  miniProtocolStart  = StartOnDemand,
                  miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                  miniProtocolRun = reqRespInitiator miniProtocolNum
                                                     nextRequest
                }]

    reqRespInitiator :: Mx.MiniProtocolNum
                     -> (ConnectionId peerAddr -> STM m [req])
                     -> RunMiniProtocol Mx.InitiatorMode
                                        (ExpandedInitiatorContext peerAddr m)
                                        (ResponderContext peerAddr)
                                        ByteString m [resp] Void
    reqRespInitiator protocolNum nextRequest =
      InitiatorProtocolOnly
        (MiniProtocolCb $ \ExpandedInitiatorContext { eicConnectionId = connId } channel ->
           runPeerWithLimits
             (WithName (name,"Initiator",protocolNum) `contramap` nullTracer)
             -- TraceSendRecv
             codecReqResp
             reqRespSizeLimits
             reqRespTimeLimits
             channel
             (Effect $ do
               reqs <- atomically (nextRequest connId)
               pure $ reqRespClientPeer (reqRespClientMap reqs)))


--
-- Rethrow policies
--

debugMuxErrorRethrowPolicy :: RethrowPolicy
debugMuxErrorRethrowPolicy =
    mkRethrowPolicy $
    \_ e ->
        case e of
          Mx.IOException {}  -> ShutdownPeer
          Mx.BearerClosed {} -> ShutdownPeer
          Mx.SDUReadTimeout  -> ShutdownPeer
          Mx.SDUWriteTimeout -> ShutdownPeer
          _                  -> ShutdownNode

debugMuxRuntimeErrorRethrowPolicy :: RethrowPolicy
debugMuxRuntimeErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: Mx.RuntimeError) -> ShutdownPeer

debugIOErrorRethrowPolicy :: RethrowPolicy
debugIOErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: IOError) -> ShutdownPeer


assertRethrowPolicy :: RethrowPolicy
assertRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: AssertionFailed) -> ShutdownNode


-- | Runs an example server which runs a single 'ReqResp' protocol for any hot
-- \/ warm \/ established peers and also gives access to bidirectional
-- 'ConnectionManager'.  This gives a way to connect to other peers.
-- Slightly unfortunate design decision does not give us a way to create
-- a client per connection.  This means that this connection manager takes list
-- of 'req' type which it will make to the other side (they will be multiplexed
-- across warm \/ how \/ established) protocols.
--
withBidirectionalConnectionManager
    :: forall name peerAddr socket acc req resp m a.
       ( ConnectionManagerMonad m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req

       -- debugging
       , MonadAsync m
       , MonadDelay m
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m, Show req
       , Show name
       )
    => name
    -> Timeouts
    -- ^ identifier (for logging)
    -> Tracer m (WithName name (RemoteTransitionTrace peerAddr))
    -> Tracer m (WithName name (AbstractTransitionTrace CM.ConnStateId))
    -> Tracer m (WithName name
                          (CM.Trace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    -> Tracer m (WithName name (InboundGovernor.Trace peerAddr))
    -> Tracer m (WithName name (InboundGovernor.Debug peerAddr DataFlowProtocolData))
    -> StdGen
    -> Snocket m socket peerAddr
    -> Mx.MakeBearer m socket
    -> CM.ConnStateIdSupply m
    -> (socket -> m ()) -- ^ configure socket
    -> socket
    -- ^ listening socket
    -> Maybe peerAddr
    -> acc
    -- ^ Initial state for the server
    -> TemperatureBundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
    -- ^ Handshake time limits
    -> AcceptedConnectionsLimit
    -> (ConnectionManagerWithExpandedCtx
          Mx.InitiatorResponderMode socket peerAddr
          DataFlowProtocolData UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> Async m Void
       -> m a)
    -> m a
withBidirectionalConnectionManager name timeouts
                                   inboundTrTracer trTracer
                                   tracer inboundTracer debugTracer
                                   stdGen
                                   snocket makeBearer connStateIdSupply
                                   confSock socket
                                   localAddress
                                   accumulatorInit nextRequests
                                   handshakeTimeLimits
                                   acceptedConnLimit k = do
    mainThreadId <- myThreadId
    inbgovInfoChannel <- newInformationChannel
    let muxTracer = WithName name `contramap` nullTracer -- mux tracer

    CM.with
      CM.Arguments {
          -- ConnectionManagerTrace
          CM.tracer    = WithName name
                        `contramap` tracer,
          CM.trTracer  = (WithName name . fmap CM.abstractState)
                        `contramap` trTracer,
          -- MuxTracer
          CM.muxTracer    = muxTracer,
          CM.ipv4Address  = localAddress,
          CM.ipv6Address  = Nothing,
          CM.addressType  = \_ -> Just IPv4Address,
          CM.snocket      = snocket,
          CM.makeBearer   = makeBearer,
          CM.configureSocket = \sock _ -> confSock sock,
          CM.timeWaitTimeout = tTimeWaitTimeout timeouts,
          CM.outboundIdleTimeout = tOutboundIdleTimeout timeouts,
          CM.connectionDataFlow = \(DataFlowProtocolData df _) -> df,
          CM.prunePolicy = simplePrunePolicy,
          CM.stdGen,
          CM.connectionsLimits = acceptedConnLimit,
          CM.updateVersionData = \versionData diffusionMode ->
                                  versionData { getProtocolDataFlow =
                                                  case diffusionMode of
                                                    InitiatorOnlyDiffusionMode         -> Unidirectional
                                                    InitiatorAndResponderDiffusionMode -> Duplex
                                              },
          CM.connStateIdSupply
        }
        (makeConnectionHandler
          muxTracer
          SingInitiatorResponderMode
          noBindForkPolicy
          HandshakeArguments {
              -- TraceSendRecv
              haHandshakeTracer = WithName name `contramap` nullTracer,
              haHandshakeCodec = unversionedHandshakeCodec,
              haVersionDataCodec = cborTermVersionDataCodec dataFlowProtocolDataCodec,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = handshakeTimeLimits
            }
          (dataFlowProtocol Duplex serverApplication)
          (mainThreadId,   debugMuxErrorRethrowPolicy
                        <> debugMuxRuntimeErrorRethrowPolicy
                        <> debugIOErrorRethrowPolicy
                        <> assertRethrowPolicy))
        (\_ -> HandshakeFailure)
        (InResponderMode inbgovInfoChannel)
      $ \connectionManager ->
          do
            serverAddr <- Snocket.getLocalAddr snocket socket
            Server.with
              Server.Arguments {
                  Server.sockets = socket :| [],
                  Server.snocket = snocket,
                  Server.trTracer =
                    WithName name `contramap` inboundTrTracer,
                  Server.tracer =
                    WithName name `contramap` nullTracer, -- ServerTrace
                  Server.debugInboundGovernor =
                    WithName name `contramap` debugTracer,
                  Server.inboundGovernorTracer =
                    WithName name `contramap` inboundTracer, -- InboundGovernorTrace
                  Server.connectionLimits = acceptedConnLimit,
                  Server.connectionManager = connectionManager,
                  Server.connectionDataFlow = \(DataFlowProtocolData df _) -> df,
                  Server.inboundIdleTimeout = Just (tProtocolIdleTimeout timeouts),
                  Server.inboundInfoChannel = inbgovInfoChannel,
                  Server.readNetworkState = return ()
                }
              (\inboundGovernorAsync _ -> k connectionManager serverAddr inboundGovernorAsync)
          `catch` \(e :: SomeException) -> do
            throwIO e
  where
    serverApplication :: TemperatureBundle
                          [MiniProtocol Mx.InitiatorResponderMode
                                        (ExpandedInitiatorContext peerAddr m)
                                        (ResponderContext peerAddr)
                                        ByteString m [resp] acc]
    serverApplication = mkProto <$> (Mx.MiniProtocolNum <$> nums) <*> nextRequests
      where nums = TemperatureBundle (WithHot 1) (WithWarm 2) (WithEstablished 3)
            mkProto miniProtocolNum nextRequest =
              [MiniProtocol {
                  miniProtocolNum,
                  miniProtocolStart  = Mx.StartOnDemand,
                  miniProtocolLimits = Mx.MiniProtocolLimits maxBound,
                  miniProtocolRun = reqRespInitiatorAndResponder
                                        miniProtocolNum
                                        accumulatorInit
                                        nextRequest
              }]

    reqRespInitiatorAndResponder
      :: Mx.MiniProtocolNum
      -> acc
      -> (ConnectionId peerAddr -> STM m [req])
      -> RunMiniProtocol Mx.InitiatorResponderMode
                         (ExpandedInitiatorContext peerAddr m)
                         (ResponderContext peerAddr)
                         ByteString m [resp] acc
    reqRespInitiatorAndResponder protocolNum accInit nextRequest =
      InitiatorAndResponderProtocol
        (MiniProtocolCb $ \ExpandedInitiatorContext { eicConnectionId = connId } channel ->
           runPeerWithLimits
             (WithName (name,"Initiator",protocolNum) `contramap` nullTracer)
             -- TraceSendRecv
             codecReqResp
             reqRespSizeLimits
             reqRespTimeLimits
             channel
             (Effect $ do
               reqs <- atomically (nextRequest connId)
               pure $ reqRespClientPeer (reqRespClientMap reqs)))
        (MiniProtocolCb $ \_ctx channel ->
           runPeerWithLimits
             (WithName (name,"Responder",protocolNum) `contramap` nullTracer)
             -- TraceSendRecv
             codecReqResp
             reqRespSizeLimits
             reqRespTimeLimits
             channel
             (reqRespServerPeer $ reqRespServerMapAccumL' accInit))

    reqRespServerMapAccumL' :: acc -> ReqRespServer req resp m acc
    reqRespServerMapAccumL' = go
      where
        fn acc x = (x : acc, x : acc)
        go acc =
          ReqRespServer {
              recvMsgReq = \req ->
                  let (acc', resp) = fn acc req
                  in return (resp, go acc'),
              recvMsgDone = return acc
            }


reqRespSizeLimits :: forall req resp. ProtocolSizeLimits (ReqResp req resp)
                                                         ByteString
reqRespSizeLimits = ProtocolSizeLimits
    { sizeLimitForState
    , dataSize = fromIntegral . LBS.length
    }
  where
    sizeLimitForState :: forall (st :: ReqResp req resp).
                         StateToken st -> Word
    sizeLimitForState _ = maxBound

reqRespTimeLimits :: forall req resp. ProtocolTimeLimits (ReqResp req resp)
reqRespTimeLimits = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: forall (st :: ReqResp req resp).
                         ActiveState st
                      => StateToken st -> Maybe DiffTime
    timeLimitForState SingIdle           = Nothing
    timeLimitForState SingBusy           = Just 60
    timeLimitForState a@ReqResp.SingDone = notActiveState a



-- | Run all initiator mini-protocols and collect results. Throw exception if
-- any of the thread returned an exception.
--
-- This function assumes that there's one established, one warm and one hot
-- mini-protocol, which is compatible with both
--
-- * 'withInitiatorOnlyConnectionManager', and
-- * 'withBidirectionalConnectionManager'.
--
runInitiatorProtocols
    :: forall muxMode addr m a b.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadCatch       m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadSTM         m
       , MonadThrow  (STM m)
       , HasInitiator muxMode ~ True
       , MonadSay         m
       )
    => SingMuxMode muxMode
    -> Mx.Mux muxMode m
    -> OuroborosBundle muxMode (ExpandedInitiatorContext addr m)
                               (ResponderContext addr)
                               ByteString m a b
    -> TemperatureBundle (StrictTVar m ControlMessage)
    -> ConnectionId addr
    -> m (TemperatureBundle a)
runInitiatorProtocols singMuxMode mux bundle controlBundle connId = do
    -- start all mini-protocols
    bundle' <- traverse (uncurry runInitiator) ((,) <$> (head <$> bundle)
                                                    <*> (readTVar <$> controlBundle))
    -- await for their termination
    traverse (atomically >=> either throwIO return)
             bundle'
  where
    runInitiator :: MiniProtocolWithExpandedCtx muxMode addr ByteString m a b
                 -> ControlMessageSTM m
                 -> m (STM m (Either SomeException a))
    runInitiator ptcl controlMessage =
        Mx.runMiniProtocol
          mux
          (miniProtocolNum ptcl)
          (case singMuxMode of
            SingInitiatorMode          -> Mx.InitiatorDirectionOnly
            SingInitiatorResponderMode -> Mx.InitiatorDirection)
          Mx.StartEagerly
          (runMiniProtocolCb
            (case miniProtocolRun ptcl of
              InitiatorProtocolOnly initiator           -> initiator
              InitiatorAndResponderProtocol initiator _ -> initiator)
            initiatorCtx)
      where
        initiatorCtx = ExpandedInitiatorContext {
            eicConnectionId    = connId,
            eicControlMessage  = controlMessage,
            eicIsBigLedgerPeer = IsNotBigLedgerPeer
          }

--
-- Experiments \/ Demos & Properties
--

-- | Max bound AcceptedConnectionsLimit
maxAcceptedConnectionsLimit :: AcceptedConnectionsLimit
maxAcceptedConnectionsLimit = AcceptedConnectionsLimit maxBound maxBound 0


-- | This test runs an initiator only connection manager (client side) and bidirectional
-- connection manager (which runs a server).  The client connect to the
-- server and runs protocols to completion.
--
-- There is a good reason why we don't run two bidirectional connection managers;
-- If we would do that, when the either side terminates the connection the
-- client side server would through an exception as it is listening.
--
unidirectionalExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadDelay m
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Hashable peerAddr
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => StdGen
    -> Timeouts
    -> Snocket m socket peerAddr
    -> Mx.MakeBearer m socket
    -> (socket -> m ())
    -> socket
    -> ClientAndServerData req
    -> m Property
unidirectionalExperiment stdGen timeouts snocket makeBearer confSock socket clientAndServerData = do
    let (stdGen', stdGen'') = split stdGen
    nextReqs <- oneshotNextRequests clientAndServerData
    connStateIdSupply <- atomically $ CM.newConnStateIdSupply (Proxy @m)
    withInitiatorOnlyConnectionManager
      "client" timeouts nullTracer nullTracer stdGen' snocket makeBearer connStateIdSupply Nothing nextReqs
      timeLimitsHandshake maxAcceptedConnectionsLimit
      $ \connectionManager ->
        withBidirectionalConnectionManager "server" timeouts
                                           nullTracer nullTracer nullTracer
                                           nullTracer nullTracer
                                           stdGen''
                                           snocket makeBearer connStateIdSupply
                                           confSock socket Nothing
                                           [accumulatorInit clientAndServerData]
                                           noNextRequests
                                           timeLimitsHandshake
                                           maxAcceptedConnectionsLimit
          $ \_ serverAddr _serverAsync -> do
            -- client → server: connect
            (rs :: [Either SomeException (TemperatureBundle [resp])]) <-
                replicateM
                  (numberOfRounds clientAndServerData)
                  (bracket
                     (acquireOutboundConnection connectionManager InitiatorOnlyDiffusionMode serverAddr)
                     (\case
                        Connected connId _ _ -> releaseOutboundConnection connectionManager connId
                        Disconnected {} -> error "unidirectionalExperiment: impossible happened")
                     (\connHandle -> do
                      case connHandle of
                        Connected connId _ (Handle mux muxBundle controlBundle _
                                        :: HandleWithExpandedCtx Mx.InitiatorMode peerAddr
                                              DataFlowProtocolData ByteString m [resp] Void) ->
                          try @_ @SomeException $
                            (runInitiatorProtocols
                              SingInitiatorMode mux muxBundle controlBundle connId
                              :: m (TemperatureBundle [resp])
                            )
                        Disconnected _ err ->
                          throwIO (userError $ "unidirectionalExperiment: " ++ show err))
                  )
            pure $
              foldr
                (\ (r, expected) acc ->
                  case r of
                    Left err -> counterexample (show err) False
                    Right a  -> a === expected .&&. acc)
                (property True)
                $ zip rs (expectedResult clientAndServerData clientAndServerData)


-- | Bidirectional send and receive.
--
bidirectionalExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadDelay m
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Hashable peerAddr

       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       , Show acc
       )
    => Bool
    -> StdGen
    -> Timeouts
    -> Snocket m socket peerAddr
    -> Mx.MakeBearer m socket
    -> (socket -> m ()) -- ^ configure socket
    -> socket
    -> socket
    -> peerAddr
    -> peerAddr
    -> ClientAndServerData req
    -> ClientAndServerData req
    -> m Property
bidirectionalExperiment
    useLock stdGen timeouts snocket makeBearer confSock socket0 socket1 localAddr0 localAddr1
    clientAndServerData0 clientAndServerData1 = do
      let (stdGen', stdGen'') = split stdGen
      lock <- newTMVarIO ()
      connStateIdSupply <- atomically $ CM.newConnStateIdSupply (Proxy @m)
      nextRequests0 <- oneshotNextRequests clientAndServerData0
      nextRequests1 <- oneshotNextRequests clientAndServerData1
      withBidirectionalConnectionManager "node-0" timeouts
                                         nullTracer nullTracer nullTracer nullTracer
                                         nullTracer stdGen' snocket makeBearer
                                         connStateIdSupply confSock
                                         socket0 (Just localAddr0)
                                         [accumulatorInit clientAndServerData0]
                                         nextRequests0
                                         noTimeLimitsHandshake
                                         maxAcceptedConnectionsLimit
        (\connectionManager0 _serverAddr0 _serverAsync0 -> do
          withBidirectionalConnectionManager "node-1" timeouts
                                             nullTracer nullTracer nullTracer nullTracer
                                             nullTracer stdGen'' snocket makeBearer
                                             connStateIdSupply confSock
                                             socket1 (Just localAddr1)
                                             [accumulatorInit clientAndServerData1]
                                             nextRequests1
                                             noTimeLimitsHandshake
                                             maxAcceptedConnectionsLimit
            (\connectionManager1 _serverAddr1 serverAsync1 -> do
              link serverAsync1
              -- runInitiatorProtocols returns a list of results per each
              -- protocol in each bucket (warm \/ hot \/ established); but
              -- we run only one mini-protocol. We can use `concat` to
              -- flatten the results.
              ( rs0 :: [Either SomeException (TemperatureBundle [resp])]
                , rs1 :: [Either SomeException (TemperatureBundle [resp])]
                ) <-
                -- Run initiator twice; this tests if the responders on
                -- the other end are restarted.
                replicateM
                  (numberOfRounds clientAndServerData0)
                  (bracket
                    (withLock useLock lock
                      (acquireOutboundConnection
                        connectionManager0
                        InitiatorAndResponderDiffusionMode
                        localAddr1))
                    (\case
                      Connected connId _ _ ->
                        releaseOutboundConnection
                          connectionManager0
                          connId
                      Disconnected {} ->
                        error "bidirectionalExperiment: impossible happened")
                    (\connHandle ->
                      case connHandle of
                        Connected connId _ (Handle mux muxBundle controlBundle _) -> do
                          try @_ @SomeException $
                            runInitiatorProtocols
                              SingInitiatorResponderMode
                              mux muxBundle controlBundle connId
                        Disconnected _ err ->
                          throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                  ))
                `concurrently`
                replicateM
                  (numberOfRounds clientAndServerData1)
                  (bracket
                    (withLock useLock lock
                      (acquireOutboundConnection
                        connectionManager1
                        InitiatorAndResponderDiffusionMode
                        localAddr0))
                    (\case
                      Connected connId _ _ ->
                        releaseOutboundConnection
                          connectionManager1
                          connId
                      Disconnected {} ->
                        error "ibidirectionalExperiment: impossible happened")
                    (\connHandle ->
                      case connHandle of
                        Connected connId _ (Handle mux muxBundle controlBundle _) -> do
                          try @_ @SomeException $
                            runInitiatorProtocols
                              SingInitiatorResponderMode
                              mux muxBundle controlBundle connId
                        Disconnected _ err ->
                          throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                  ))

              pure $
                foldr
                  (\ (r, expected) acc ->
                    case r of
                      Left err -> counterexample (show err) False
                      Right a  -> a === expected .&&. acc)
                  (property True)
                  (zip rs0 (expectedResult clientAndServerData0 clientAndServerData1))
                .&&.
                foldr
                  (\ (r, expected) acc ->
                    case r of
                      Left err -> counterexample (show err) False
                      Right a  -> a === expected .&&. acc)
                  (property True)
                  (zip rs1 (expectedResult clientAndServerData1 clientAndServerData0))
                ))


--
-- Utils
--


-- | Redefine this tracer to get valuable tracing information from various
-- components:
--
-- * connection-manager
-- * inbound governor
-- * server
--
-- debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
-- debugTracer = Tracer (\msg -> (,msg) <$> getCurrentTime >>= say . show)
           -- <> Tracer Debug.traceShowM


withLock :: ( MonadSTM   m
            , MonadThrow m
            )
         => Bool
         -> StrictTMVar m ()
         -> m a
         -> m a
withLock False _v m = m
withLock True   v m =
    bracket (atomically $ takeTMVar v)
            (atomically . putTMVar v)
            (const m)
