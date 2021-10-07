{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- for 'debugTracer'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Ouroboros.Network.Server2
  ( tests
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (AssertionFailed)
import           Control.Monad (replicateM, when, (>=>))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST    (MonadST)
import           Control.Monad.Class.MonadSTM.Strict
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, nullTracer)

import           Codec.Serialise.Class (Serialise)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.ByteString.Lazy (ByteString)
import           Data.Dynamic (fromDynamic)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void, ($>), (<&>))
import           Data.List (dropWhileEnd, find, foldl', mapAccumL, intercalate, (\\), delete)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List.Octopus (Octopus (..))
import qualified Data.List.Octopus as Octopus
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, fromJust, isJust)
import           Data.Monoid (Sum (..))
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Text.Printf

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import           Control.Concurrent.JobPool

import           Codec.CBOR.Term (Term)

import qualified Network.Mux as Mux
import           Network.Mux.Types (MuxRuntimeError (..))
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Core

import           Network.TypedProtocol.ReqResp.Type
import           Network.TypedProtocol.ReqResp.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Examples

import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                   RemoteSt (..))
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as Server
import           Ouroboros.Network.Mux
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec ( cborTermVersionDataCodec
                                                            , noTimeLimitsHandshake
                                                            , timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..))
import           Ouroboros.Network.RethrowPolicy
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Server2 (ServerArguments (..), RemoteTransition, RemoteTransitionTrace)
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (Snocket, TestAddress (..), socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision)
import           Simulation.Network.Snocket

import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance
import           Test.Simulation.Network.Snocket hiding (tests)
import           Test.Ouroboros.Network.ConnectionManager (verifyAbstractTransition)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Server2"
  [ testProperty "unidirectional_IO"  prop_unidirectional_IO
  , testProperty "unidirectional_Sim" prop_unidirectional_Sim
  , testProperty "bidirectional_IO"   prop_bidirectional_IO
  , testProperty "bidirectional_Sim"  prop_bidirectional_Sim
  , testProperty "multinode_Sim"      prop_multinode_Sim
  , testProperty "unit_connection_terminated_when_negotiating"
                 unit_connection_terminated_when_negotiating
  , testGroup "generators"
    [ testProperty "MultiNodeScript"  prop_generator_MultiNodeScript
    ]
  ]

--
-- Server tests
--

-- | The protocol will run three instances of  `ReqResp` protocol; one for each
-- state: warm, hot and established.
--
data ClientAndServerData req = ClientAndServerData {
    accumulatorInit              :: req,
    -- ^ Initial value. For each request the server sends back a list received
    -- requests (in reverse order) terminating with the accumulatorInit.
    hotInitiatorRequests         :: [[req]],
    -- ^ list of requests run by the hot intiator in each round; Running
    -- multiple rounds allows us to test restarting of responders.
    warmInitiatorRequests        :: [[req]],
    -- ^ list of requests run by the warm intiator in each round
    establishedInitiatorRequests :: [[req]]
    -- ^ lsit of requests run by the established intiator in each round
  }
  deriving Show


-- Number of rounds to exhoust all the requests.
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
               -> [Bundle [[req]]]
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
      Bundle
        (WithHot         (snd $ mapAccumL fn [accumulatorInit] a))
        (WithWarm        (snd $ mapAccumL fn [accumulatorInit] b))
        (WithEstablished (snd $ mapAccumL fn [accumulatorInit] c))
      : go as bs cs
    go [] [] [] = []
    go _  _  _  = error "expectedResult: impossible happened"

noNextRequests :: forall stm req peerAddr. Applicative stm => Bundle (ConnectionId peerAddr -> stm [req])
noNextRequests = pure $ \_ -> pure []

-- | Next requests bundle for bidirectional and unidirectional experiments.
oneshotNextRequests
  :: forall req peerAddr m. MonadSTM m
  => ClientAndServerData req
  -> m (Bundle (ConnectionId peerAddr -> STM m [req]))
oneshotNextRequests ClientAndServerData {
                      hotInitiatorRequests,
                      warmInitiatorRequests,
                      establishedInitiatorRequests
                    } = do
    -- we pass a `StricTVar` with all the reuqests to each initiator.  This way
    -- the each round (which runs a single instance of `ReqResp` protocol) will
    -- use its own request list.
    hotRequestsVar         <- newTVarIO hotInitiatorRequests
    warmRequestsVar        <- newTVarIO warmInitiatorRequests
    establishedRequestsVar <- newTVarIO establishedInitiatorRequests
    return $ Bundle (WithHot hotRequestsVar)
                    (WithWarm warmRequestsVar)
                    (WithEstablished establishedRequestsVar)
              <&> \ reqVar _ -> popRequests reqVar
  where
    popRequests requestsVar = do
      requests <- readTVar requestsVar
      case requests of
        reqs : rest -> writeTVar requestsVar rest $> reqs
        []          -> pure []


-- | Configurable timeouts.  We use different timeouts for 'IO' and 'IOSim' property tests.
--
data Timeouts = Timeouts {
    tProtocolIdleTimeout :: DiffTime,
    tOutboundIdleTimeout :: DiffTime,
    tTimeWaitTimeout     :: DiffTime
  }

-- | Timeouts for 'IO' tests.
--
ioTimeouts :: Timeouts
ioTimeouts = Timeouts {
    tProtocolIdleTimeout = 0.1,
    tOutboundIdleTimeout = 0.1,
    tTimeWaitTimeout     = 0.1
  }

-- | Timeouts for 'IOSim' tests.
--
simTimeouts :: Timeouts
simTimeouts = Timeouts {
    tProtocolIdleTimeout = 5,
    tOutboundIdleTimeout = 5,
    tTimeWaitTimeout     = 30
  }

--
-- Various ConnectionManagers
--

type ConnectionManagerMonad m =
       ( MonadAsync m, MonadCatch m, MonadEvaluate m, MonadFork m, MonadMask  m
       , MonadST m, MonadTime m, MonadTimer m, MonadThrow m, MonadThrow (STM m)
       )

type ConnectionState_ muxMode peerAddr m a b =
       ConnectionState peerAddr
                       (Handle muxMode peerAddr ByteString m a b)
                       (HandleError InitiatorResponderMode UnversionedProtocol)
                       (UnversionedProtocol, UnversionedProtocolData)
                       m

withInitiatorOnlyConnectionManager
    :: forall name peerAddr socket req resp m a.
       ( ConnectionManagerMonad m

       , resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req
       , Eq (LazySTM.TVar m (ConnectionState
                                peerAddr
                                (Handle 'InitiatorMode peerAddr ByteString m [resp] Void)
                                (HandleError 'InitiatorMode UnversionedProtocol)
                                (UnversionedProtocol, UnversionedProtocolData)
                                m))

       -- , Eq (LazySTM.TVar m (ConnectionState_ InitiatorMode peerAddr m [resp] Void))
       -- , Eq (TVar_ (STM m) (ConnectionState_ InitiatorMode peerAddr m [resp] Void))
       -- debugging
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req
       , Show name
       )
    => name
    -- ^ identifier (for logging)
    -> Timeouts
    -> Tracer m (WithName name (AbstractTransitionTrace peerAddr))
    -> Snocket m socket peerAddr
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> Maybe peerAddr
    -> Bundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
    -- ^ Handshake time limits
    -> (MuxConnectionManager
          InitiatorMode socket peerAddr
          UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager name timeouts cmTrTracer snocket localAddr
                                   nextRequests handshakeTimeLimits k = do
    mainThreadId <- myThreadId
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer
    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = WithName name
                        `contramap` debugTracer,
          cmTrTracer  = (WithName name . fmap abstractState)
                        `contramap` (cmTrTracer <> debugTracer),
         -- MuxTracer
          cmMuxTracer = muxTracer,
          cmIPv4Address = localAddr,
          cmIPv6Address = Nothing,
          cmAddressType = \_ -> Just IPv4Address,
          cmSnocket = snocket,
          connectionDataFlow = getProtocolDataFlow . snd,
          cmPrunePolicy = simplePrunePolicy,
          cmConnectionsLimits = AcceptedConnectionsLimit {
              acceptedConnectionsHardLimit = maxBound,
              acceptedConnectionsSoftLimit = maxBound,
              acceptedConnectionsDelay     = 0
            },
          cmTimeWaitTimeout = tTimeWaitTimeout timeouts,
          cmOutboundIdleTimeout = tOutboundIdleTimeout timeouts
        }
      (makeConnectionHandler
        muxTracer
        SingInitiatorMode
        clientMiniProtocolBundle
        HandshakeArguments {
            -- TraceSendRecv
            haHandshakeTracer = (name,) `contramap` nullTracer,
            haHandshakeCodec = unversionedHandshakeCodec,
            haVersionDataCodec = cborTermVersionDataCodec dataFlowProtocolDataCodec,
            haAcceptVersion = acceptableVersion,
            haTimeLimits = handshakeTimeLimits
          }
        (unversionedProtocol clientApplication)
        (mainThreadId, debugMuxErrorRethrowPolicy
                    <> debugMuxRuntimeErrorRethrowPolicy
                    <> debugIOErrorRethrowPolicy
                    <> assertRethrowPolicy))
      (\_ -> HandshakeFailure)
      NotInResponderMode
      k
  where
    clientMiniProtocolBundle :: Mux.MiniProtocolBundle InitiatorMode
    clientMiniProtocolBundle = Mux.MiniProtocolBundle
        [ Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 1,
            Mux.miniProtocolDir = Mux.InitiatorDirectionOnly,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 2,
            Mux.miniProtocolDir = Mux.InitiatorDirectionOnly,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 3,
            Mux.miniProtocolDir = Mux.InitiatorDirectionOnly,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        ]

    clientApplication :: Bundle
                          (ConnectionId peerAddr
                            -> ControlMessageSTM m
                            -> [MiniProtocol InitiatorMode ByteString m [resp] Void])
    clientApplication = mkProto <$> (Mux.MiniProtocolNum <$> nums)
                                <*> nextRequests

      where nums = Bundle (WithHot 1) (WithWarm 2) (WithEstablished 3)
            mkProto miniProtocolNum nextRequest connId _ =
              [MiniProtocol {
                  miniProtocolNum,
                  miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                  miniProtocolRun = reqRespInitiator miniProtocolNum
                                                     (nextRequest connId)
                }]

    reqRespInitiator :: Mux.MiniProtocolNum
                     -> STM m [req]
                     -> RunMiniProtocol InitiatorMode ByteString m [resp] Void
    reqRespInitiator protocolNum nextRequest =
      InitiatorProtocolOnly
        (MuxPeerRaw $ \channel ->
          runPeerWithLimits
            (WithName (name,"Initiator",protocolNum) `contramap` nullTracer)
            -- TraceSendRecv
            codecReqResp
            reqRespSizeLimits
            reqRespTimeLimits
            channel
            (Effect $ do
              reqs <- atomically nextRequest
              pure $ reqRespClientPeer (reqRespClientMap reqs)))


--
-- Rethrow policies
--

debugMuxErrorRethrowPolicy :: RethrowPolicy
debugMuxErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ MuxError { errorType } ->
        case errorType of
          MuxIOException _   -> ShutdownPeer
          MuxBearerClosed    -> ShutdownPeer
          MuxSDUReadTimeout  -> ShutdownPeer
          MuxSDUWriteTimeout -> ShutdownPeer
          _                  -> ShutdownNode

debugMuxRuntimeErrorRethrowPolicy :: RethrowPolicy
debugMuxRuntimeErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: MuxRuntimeError) -> ShutdownPeer

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
       , MonadLabelledSTM m
       , MonadSay m, Show req
       , Show name
       )
    => name
    -> Timeouts
    -- ^ identifier (for logging)
    -> Tracer m (WithName name (RemoteTransitionTrace peerAddr))
    -> Tracer m (WithName name (AbstractTransitionTrace peerAddr))
    -> Tracer m (WithName name (InboundGovernorTrace peerAddr))
    -> Snocket m socket peerAddr
    -> socket
    -- ^ listening socket
    -> Maybe peerAddr
    -> acc
    -- ^ Initial state for the server
    -> Bundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
    -- ^ Handshake time limits
    -> (MuxConnectionManager
          InitiatorResponderMode socket peerAddr
          UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> Async m Void
       -> m a)
    -> m a
withBidirectionalConnectionManager name timeouts
                                   inboundTrTracer cmTrTracer inboundTracer
                                   snocket socket localAddress
                                   accumulatorInit nextRequests
                                   handshakeTimeLimits k = do
    mainThreadId <- myThreadId
    inbgovControlChannel      <- Server.newControlChannel
    -- we are not using the randomness
    observableStateVar        <- Server.newObservableStateVarFromSeed 0
    let muxTracer = WithName name `contramap` nullTracer -- mux tracer

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = WithName name
                        `contramap` debugTracer,
          cmTrTracer  = (WithName name . fmap abstractState)
                        `contramap` (cmTrTracer <> debugTracer),
          -- MuxTracer
          cmMuxTracer    = muxTracer,
          cmIPv4Address  = localAddress,
          cmIPv6Address  = Nothing,
          cmAddressType  = \_ -> Just IPv4Address,
          cmSnocket      = snocket,
          cmTimeWaitTimeout = tTimeWaitTimeout timeouts,
          cmOutboundIdleTimeout = tOutboundIdleTimeout timeouts,
          connectionDataFlow = getProtocolDataFlow . snd,
          cmPrunePolicy = simplePrunePolicy,
          cmConnectionsLimits = AcceptedConnectionsLimit {
              acceptedConnectionsHardLimit = maxBound,
              acceptedConnectionsSoftLimit = maxBound,
              acceptedConnectionsDelay     = 0
            }
        }
        (makeConnectionHandler
          muxTracer
          SingInitiatorResponderMode
          serverMiniProtocolBundle
          HandshakeArguments {
              -- TraceSendRecv
              haHandshakeTracer = WithName name `contramap` nullTracer,
              haHandshakeCodec = unversionedHandshakeCodec,
              haVersionDataCodec = cborTermVersionDataCodec dataFlowProtocolDataCodec,
              haAcceptVersion = acceptableVersion,
              haTimeLimits = handshakeTimeLimits
            }
          (unversionedProtocol serverApplication)
          (mainThreadId,   debugMuxErrorRethrowPolicy
                        <> debugMuxRuntimeErrorRethrowPolicy
                        <> debugIOErrorRethrowPolicy
                        <> assertRethrowPolicy))
          (\_ -> HandshakeFailure)
          (InResponderMode inbgovControlChannel)
      $ \connectionManager ->
          do
            serverAddr <- Snocket.getLocalAddr snocket socket
            withAsync
              (Server.run
                ServerArguments {
                    serverSockets = socket :| [],
                    serverSnocket = snocket,
                    serverTrTracer =
                      WithName name `contramap` inboundTrTracer,
                    serverTracer =
                      WithName name `contramap` debugTracer, -- ServerTrace
                    serverInboundGovernorTracer =
                      WithName name `contramap` inboundTracer, -- InboundGovernorTrace
                    serverConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0,
                    serverConnectionManager = connectionManager,
                    serverInboundIdleTimeout = tProtocolIdleTimeout timeouts,
                    serverControlChannel = inbgovControlChannel,
                    serverObservableStateVar = observableStateVar
                  }
              )
              (\serverAsync -> link serverAsync
                            >> k connectionManager serverAddr serverAsync)
          `catch` \(e :: SomeException) -> do
            throwIO e
  where
    -- for a bidirectional mux we need to define 'Mu.xMiniProtocolInfo' for each
    -- protocol for each direction.
    serverMiniProtocolBundle :: Mux.MiniProtocolBundle InitiatorResponderMode
    serverMiniProtocolBundle = Mux.MiniProtocolBundle
        [ Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 1,
            Mux.miniProtocolDir = Mux.ResponderDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 1,
            Mux.miniProtocolDir = Mux.InitiatorDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 2,
            Mux.miniProtocolDir = Mux.ResponderDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 2,
            Mux.miniProtocolDir = Mux.InitiatorDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 3,
            Mux.miniProtocolDir = Mux.ResponderDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        , Mux.MiniProtocolInfo {
            Mux.miniProtocolNum = Mux.MiniProtocolNum 3,
            Mux.miniProtocolDir = Mux.InitiatorDirection,
            Mux.miniProtocolLimits = Mux.MiniProtocolLimits maxBound
          }
        ]

    serverApplication :: Bundle
                          (ConnectionId peerAddr
                            -> ControlMessageSTM m
                            -> [MiniProtocol InitiatorResponderMode ByteString m [resp] acc])
    serverApplication = mkProto <$> (Mux.MiniProtocolNum <$> nums) <*> nextRequests
      where nums = Bundle (WithHot 1) (WithWarm 2) (WithEstablished 3)
            mkProto miniProtocolNum nextRequest connId _ =
              [MiniProtocol {
                  miniProtocolNum,
                  miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                  miniProtocolRun = reqRespInitiatorAndResponder
                                        miniProtocolNum
                                        accumulatorInit
                                        (nextRequest connId)
              }]

    reqRespInitiatorAndResponder
      :: Mux.MiniProtocolNum
      -> acc
      -> STM m [req]
      -> RunMiniProtocol InitiatorResponderMode ByteString m [resp] acc
    reqRespInitiatorAndResponder protocolNum accInit nextRequest =
      InitiatorAndResponderProtocol
        (MuxPeerRaw $ \channel ->
          runPeerWithLimits
            (WithName (name,"Initiator",protocolNum) `contramap` nullTracer)
            -- TraceSendRecv
            codecReqResp
            reqRespSizeLimits
            reqRespTimeLimits
            channel
            (Effect $ do
              reqs <- atomically nextRequest
              pure $ reqRespClientPeer (reqRespClientMap reqs)))
        (MuxPeerRaw $ \channel ->
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
    sizeLimitForState :: forall (pr :: PeerRole) (st :: ReqResp req resp).
                         PeerHasAgency pr st -> Word
    sizeLimitForState _ = maxBound

reqRespTimeLimits :: forall req resp. ProtocolTimeLimits (ReqResp req resp)
reqRespTimeLimits = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: forall (pr :: PeerRole) (st :: ReqResp req resp).
                         PeerHasAgency pr st -> Maybe DiffTime
    timeLimitForState (ClientAgency TokIdle) = Nothing
    timeLimitForState (ServerAgency TokBusy) = Just 60



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
    :: forall muxMode m a b.
       ( MonadAsync      m
       , MonadCatch      m
       , MonadSTM        m
       , MonadThrow (STM m)
       , HasInitiator muxMode ~ True
       , MonadSay        m
       )
    => SingMuxMode muxMode
    -> Mux.Mux muxMode m
    -> MuxBundle muxMode ByteString m a b
    -> m (Bundle a)
runInitiatorProtocols
    singMuxMode mux
    bundle
     = do -- start all mini-protocols
          bundle' <- traverse runInitiator (head <$> bundle)
          -- await for their termination
          traverse (atomically >=> either throwIO return)
                   bundle'
  where
    runInitiator :: MiniProtocol muxMode ByteString m a b
                 -> m (STM m (Either SomeException a))
    runInitiator ptcl =
      Mux.runMiniProtocol
        mux
        (miniProtocolNum ptcl)
        (case singMuxMode of
          SingInitiatorMode -> Mux.InitiatorDirectionOnly
          SingInitiatorResponderMode -> Mux.InitiatorDirection)
        Mux.StartEagerly
        (runMuxPeer
          (case miniProtocolRun ptcl of
            InitiatorProtocolOnly initiator -> initiator
            InitiatorAndResponderProtocol initiator _ -> initiator)
          . fromChannel)

--
-- Experiments \/ Demos & Properties
--


-- | This test runs an intiator only connection manager (client side) and bidrectional
-- connection manager (which runs a server).   The the client connect to the
-- server and runs protocols to completion.
--
-- There is a good reason why we don't run two bidrectional connection managers;
-- If we would do that, when the either side terminates the connection the
-- client side server would through an exception as it is listening.
--
unidirectionalExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Eq (LazySTM.TVar m (ConnectionState
                                peerAddr
                                (Handle 'InitiatorMode peerAddr ByteString m [resp] Void)
                                (HandleError 'InitiatorMode UnversionedProtocol)
                                (UnversionedProtocol, UnversionedProtocolData)
                                m))
       -- , Eq (LazySTM.TVar m (ConnectionState_ InitiatorMode          peerAddr m [resp] Void))
       , Eq (LazySTM.TVar m (ConnectionState_ InitiatorResponderMode peerAddr m [resp] acc))
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => Timeouts
    -> Snocket m socket peerAddr
    -> socket
    -> ClientAndServerData req
    -> m Property
unidirectionalExperiment timeouts snocket socket clientAndServerData = do
    nextReqs <- oneshotNextRequests clientAndServerData
    withInitiatorOnlyConnectionManager
      "client" timeouts nullTracer snocket Nothing nextReqs timeLimitsHandshake
      $ \connectionManager ->
        withBidirectionalConnectionManager "server" timeouts
                                           nullTracer nullTracer nullTracer
                                           snocket socket Nothing
                                           [accumulatorInit clientAndServerData]
                                           noNextRequests
                                           timeLimitsHandshake
          $ \_ serverAddr _serverAsync -> do
            -- client → server: connect
            (rs :: [Either SomeException (Bundle [resp])]) <-
                replicateM
                  (numberOfRounds clientAndServerData)
                  (bracket
                     (requestOutboundConnection connectionManager serverAddr)
                     (\_ -> unregisterOutboundConnection connectionManager serverAddr)
                     (\connHandle -> do
                      case connHandle of
                        Connected _ _ (Handle mux muxBundle _
                                        :: Handle InitiatorMode peerAddr ByteString m [resp] Void) ->
                          try @_ @SomeException $
                            (runInitiatorProtocols
                              SingInitiatorMode mux muxBundle
                              :: m (Bundle [resp])
                            )
                        Disconnected _ err ->
                          throwIO (userError $ "unidirectionalExperiment: " ++ show err))
                  )
            pure $
              foldr
                (\ (r, expected) acc ->
                  case r of
                    Left err -> counterexample (show err) False
                    Right a -> a === expected .&&. acc)
                (property True)
                $ zip rs (expectedResult clientAndServerData clientAndServerData)

prop_unidirectional_Sim :: AbsBearerInfo -> ClientAndServerData Int -> Property
prop_unidirectional_Sim absBi clientAndServerData =
  simulatedPropertyWithTimeout 7200 $
    withSnocket nullTracer
                (Script (toBearerInfo absBi :| [noAttenuation])) $ \snock ->
      bracket (Snocket.open snock Snocket.TestFamily)
              (Snocket.close snock) $ \fd -> do
        Snocket.bind   snock fd serverAddr
        Snocket.listen snock fd
        unidirectionalExperiment simTimeouts snock fd clientAndServerData
  where
    serverAddr = Snocket.TestAddress (0 :: Int)

prop_unidirectional_IO
  :: ClientAndServerData Int
  -> Property
prop_unidirectional_IO clientAndServerData =
    ioProperty $ do
      withIOManager $ \iomgr ->
        bracket
          (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
          Socket.close
          $ \socket -> do
              associateWithIOManager iomgr (Right socket)
              addr <- head <$> Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
              Socket.bind socket (Socket.addrAddress addr)
              Socket.listen socket maxBound
              unidirectionalExperiment
                ioTimeouts
                (socketSnocket iomgr)
                socket
                clientAndServerData


-- | Bidirectional send and receive.
--
bidirectionalExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Eq (LazySTM.TVar m (ConnectionState_ 'InitiatorResponderMode peerAddr m [resp] acc))

       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       , Show acc
       )
    => Bool
    -> Timeouts
    -> Snocket m socket peerAddr
    -> socket
    -> socket
    -> peerAddr
    -> peerAddr
    -> ClientAndServerData req
    -> ClientAndServerData req
    -> m Property
bidirectionalExperiment
    useLock timeouts snocket socket0 socket1 localAddr0 localAddr1
    clientAndServerData0 clientAndServerData1 = do
      lock <- newTMVarIO ()
      nextRequests0 <- oneshotNextRequests clientAndServerData0
      nextRequests1 <- oneshotNextRequests clientAndServerData1
      withBidirectionalConnectionManager "node-0" timeouts
                                         nullTracer nullTracer nullTracer
                                         snocket socket0
                                         (Just localAddr0)
                                         [accumulatorInit clientAndServerData0]
                                         nextRequests0
                                         noTimeLimitsHandshake
        (\connectionManager0 _serverAddr0 _serverAsync0 ->
          withBidirectionalConnectionManager "node-1" timeouts
                                             nullTracer nullTracer nullTracer
                                             snocket socket1
                                             (Just localAddr1)
                                             [accumulatorInit clientAndServerData1]
                                             nextRequests1
                                             noTimeLimitsHandshake
            (\connectionManager1 _serverAddr1 _serverAsync1 -> do
              -- runInitiatorProtocols returns a list of results per each
              -- protocol in each bucket (warm \/ hot \/ established); but
              -- we run only one mini-protocol. We can use `concat` to
              -- flatten the results.
              ( rs0 :: [Either SomeException (Bundle [resp])]
                , rs1 :: [Either SomeException (Bundle [resp])]
                ) <-
                -- Run initiator twice; this tests if the responders on
                -- the other end are restarted.
                (replicateM
                  (numberOfRounds clientAndServerData0)
                  (bracket
                    (withLock useLock lock
                      (requestOutboundConnection
                        connectionManager0
                        localAddr1))
                    (\_ ->
                      unregisterOutboundConnection
                        connectionManager0
                        localAddr1)
                    (\connHandle ->
                      case connHandle of
                        Connected _ _ (Handle mux muxBundle _) -> do
                          try @_ @SomeException $
                            runInitiatorProtocols
                              SingInitiatorResponderMode
                              mux muxBundle
                        Disconnected _ err ->
                          throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                  )))
                `concurrently`
                (replicateM
                  (numberOfRounds clientAndServerData1)
                  (bracket
                    (withLock useLock lock
                      (requestOutboundConnection
                        connectionManager1
                        localAddr0))
                    (\_ ->
                      unregisterOutboundConnection
                        connectionManager1
                        localAddr0)
                    (\connHandle ->
                      case connHandle of
                        Connected _ _ (Handle mux muxBundle _) -> do
                          try @_ @SomeException $
                            runInitiatorProtocols
                              SingInitiatorResponderMode
                              mux muxBundle
                        Disconnected _ err ->
                          throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                  )))

              pure $
                foldr
                  (\ (r, expected) acc ->
                    case r of
                      Left err -> counterexample (show err) False
                      Right a -> a === expected .&&. acc)
                  (property True)
                  (zip rs0 (expectedResult clientAndServerData0 clientAndServerData1))
                .&&.
                foldr
                  (\ (r, expected) acc ->
                    case r of
                      Left err -> counterexample (show err) False
                      Right a -> a === expected .&&. acc)
                  (property True)
                  (zip rs1 (expectedResult clientAndServerData1 clientAndServerData0))
                ))


prop_bidirectional_Sim :: NonFailingBearerInfoScript -> ClientAndServerData Int -> ClientAndServerData Int -> Property
prop_bidirectional_Sim (NonFailingBearerInfoScript script) data0 data1 =
  simulatedPropertyWithTimeout 7200 $
    withSnocket sayTracer
                script'
                $ \snock ->
      bracket ((,) <$> Snocket.open snock Snocket.TestFamily
                   <*> Snocket.open snock Snocket.TestFamily)
              (\ (socket0, socket1) -> Snocket.close snock socket0 >>
                                       Snocket.close snock socket1)
        $ \ (socket0, socket1) -> do
          let addr0, addr1 :: SimAddr
              addr0 = Snocket.TestAddress 0
              addr1 = Snocket.TestAddress 1
          Snocket.bind   snock socket0 addr0
          Snocket.bind   snock socket1 addr1
          Snocket.listen snock socket0
          Snocket.listen snock socket1
          bidirectionalExperiment False simTimeouts snock
                                        socket0 socket1
                                        addr0 addr1
                                        data0 data1
  where
    script' = toBearerInfo <$> script

prop_bidirectional_IO
    :: ClientAndServerData Int
    -> ClientAndServerData Int
    -> Property
prop_bidirectional_IO data0 data1 =
    ioProperty $ do
      withIOManager $ \iomgr ->
        bracket
          ((,)
            <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
            <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
          (\(socket0,socket1) -> Socket.close socket0
                              >> Socket.close socket1)
          $ \(socket0, socket1) -> do
            associateWithIOManager iomgr (Right socket0)
            associateWithIOManager iomgr (Right socket1)
            Socket.setSocketOption socket0 Socket.ReuseAddr 1
            Socket.setSocketOption socket1 Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
            Socket.setSocketOption socket0 Socket.ReusePort 1
            Socket.setSocketOption socket1 Socket.ReusePort 1
#endif
            -- TODO: use ephemeral ports
            let hints = Socket.defaultHints { Socket.addrFlags = [Socket.AI_ADDRCONFIG, Socket.AI_PASSIVE] }
            addr0 : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
            addr1 : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
            Socket.bind socket0 (Socket.addrAddress addr0)
            Socket.bind socket1 (Socket.addrAddress addr1)
            addr0' <- Socket.getSocketName socket0
            addr1' <- Socket.getSocketName socket1
            Socket.listen socket0 10
            Socket.listen socket1 10

            bidirectionalExperiment
              True
              ioTimeouts
              (socketSnocket iomgr)
              socket0
              socket1
              addr0'
              addr1'
              data0
              data1


--- Multi-node experiment

-- | A test case for the multi-node property contains a sequence of connection events. The
--   `DiffTime` in each constructor is relative to the previous event in the sequence.
data ConnectionEvent req peerAddr
  = StartClient DiffTime peerAddr
    -- ^ Start a new client at the given address
  | StartServer DiffTime peerAddr req
    -- ^ Start a new server at the given address
  | InboundConnection DiffTime peerAddr
    -- ^ Create a connection from client or server with the given address to the central server.
  | OutboundConnection DiffTime peerAddr
    -- ^ Create a connection from the central server to another server.
  | InboundMiniprotocols DiffTime peerAddr (Bundle [req])
    -- ^ Run a bundle of mini protocols on the inbound connection from the given address.
  | OutboundMiniprotocols DiffTime peerAddr (Bundle [req])
    -- ^ Run a bundle of mini protocols on the outbound connection to the given address.
  | CloseInboundConnection DiffTime peerAddr
    -- ^ Close an inbound connection.
  | CloseOutboundConnection DiffTime peerAddr
    -- ^ Close an outbound connection.
  | ShutdownClientServer DiffTime peerAddr
    -- ^ Shuts down a client/server (simulates power loss)
  deriving (Show, Functor)

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim`.
newtype MultiNodeScript req peerAddr = MultiNodeScript [ConnectionEvent req peerAddr]
  deriving (Show, Functor)

-- | To generate well-formed scripts we need to keep track of what nodes are started and what
--   connections they've made.
--
--   Note: this does not track failures, e.g. `requestOutboundConnection` when there's
--   already a `Unidirectional` inbound connection (i.e. a `ForbiddenOperation`).
--
data ScriptState peerAddr = ScriptState { startedClients      :: [peerAddr]
                                        , startedServers      :: [peerAddr]
                                        , clientConnections   :: [peerAddr]
                                        , inboundConnections  :: [peerAddr]
                                        , outboundConnections :: [peerAddr] }

-- | Update the state after a connection event.
nextState :: Eq peerAddr => ConnectionEvent req peerAddr -> ScriptState peerAddr -> ScriptState peerAddr
nextState e s@ScriptState{..} =
  case e of
    StartClient             _ a   -> s{ startedClients      = a : startedClients }
    StartServer             _ a _ -> s{ startedServers      = a : startedServers }
    InboundConnection       _ a   -> s{ inboundConnections  = a : inboundConnections }
    OutboundConnection      _ a   -> s{ outboundConnections = a : outboundConnections }
    CloseInboundConnection  _ a   -> s{ inboundConnections  = delete a inboundConnections }
    CloseOutboundConnection _ a   -> s{ outboundConnections = delete a outboundConnections }
    InboundMiniprotocols{}        -> s
    OutboundMiniprotocols{}       -> s
    ShutdownClientServer    _ a   -> s{ startedClients      = delete a startedClients
                                     , startedServers       = delete a startedServers }

-- | Check if an event makes sense in a given state.
isValidEvent :: Eq peerAddr => ConnectionEvent req peerAddr -> ScriptState peerAddr -> Bool
isValidEvent e ScriptState{..} =
  case e of
    StartClient             _ a   -> notElem a (startedClients ++ startedServers)
    StartServer             _ a _ -> notElem a (startedClients ++ startedServers)
    InboundConnection       _ a   -> elem a (startedServers ++ startedClients) && notElem a inboundConnections
    OutboundConnection      _ a   -> elem a startedServers && notElem a outboundConnections
    CloseInboundConnection  _ a   -> elem a inboundConnections
    CloseOutboundConnection _ a   -> elem a outboundConnections
    InboundMiniprotocols    _ a _ -> elem a inboundConnections
    OutboundMiniprotocols   _ a _ -> elem a outboundConnections
    ShutdownClientServer    _ a   -> elem a (startedClients ++ startedServers)

-- This could be an Arbitrary instance, but it would be an orphan.
genBundle :: Arbitrary a => Gen (Bundle a)
genBundle = traverse id $ pure arbitrary

shrinkBundle :: Arbitrary a => Bundle a -> [Bundle a]
shrinkBundle (Bundle (WithHot hot) (WithWarm warm) (WithEstablished est)) =
  (shrink hot  <&> \ hot'  -> Bundle (WithHot hot') (WithWarm warm)  (WithEstablished est)) ++
  (shrink warm <&> \ warm' -> Bundle (WithHot hot)  (WithWarm warm') (WithEstablished est)) ++
  (shrink est  <&> \ est'  -> Bundle (WithHot hot)  (WithWarm warm)  (WithEstablished est'))

instance (Arbitrary peerAddr, Arbitrary req, Eq peerAddr) =>
         Arbitrary (MultiNodeScript req peerAddr) where
  arbitrary = do
      Positive len <- scale ((* 2) . (`div` 3)) arbitrary
      MultiNodeScript <$> go (ScriptState [] [] [] [] []) (len :: Integer)
    where     -- Divide delays by 100 to avoid running in to protocol and SDU timeouts if waiting
              -- too long between connections and mini protocols.
      delay = frequency [(1, pure 0), (3, (/ 100) <$> genDelayWithPrecision 2)]
      go _ 0 = pure []
      go s@ScriptState{..} n = do
        event <- frequency $
                    [ (4, StartClient             <$> delay <*> newClient)
                    , (4, StartServer             <$> delay <*> newServer <*> arbitrary) ] ++
                    [ (4, InboundConnection       <$> delay <*> elements possibleInboundConnections)        | not $ null possibleInboundConnections] ++
                    [ (4, OutboundConnection      <$> delay <*> elements possibleOutboundConnections)       | not $ null possibleOutboundConnections] ++
                    [ (4, CloseInboundConnection  <$> delay <*> elements inboundConnections)                | not $ null $ inboundConnections ] ++
                    [ (4, CloseOutboundConnection <$> delay <*> elements outboundConnections)               | not $ null $ outboundConnections ] ++
                    [ (16, InboundMiniprotocols   <$> delay <*> elements inboundConnections  <*> genBundle) | not $ null inboundConnections ] ++
                    [ (16, OutboundMiniprotocols  <$> delay <*> elements outboundConnections <*> genBundle) | not $ null outboundConnections ] ++
                    [ (8, ShutdownClientServer    <$> delay <*> elements possibleStoppable)                 | not $ null possibleStoppable ]
        (event :) <$> go (nextState event s) (n - 1)
        where
          possibleStoppable  = (startedClients ++ startedServers)
          possibleInboundConnections  = (startedClients ++ startedServers) \\ inboundConnections
          possibleOutboundConnections = startedServers \\ outboundConnections
          newClient = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))
          newServer = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))

  shrink (MultiNodeScript events) = MultiNodeScript . makeValid <$> shrinkList shrinkEvent events
    where
      makeValid = go (ScriptState [] [] [] [] [])
        where
          go _ [] = []
          go s (e : es)
            | isValidEvent e s = e : go (nextState e s) es
            | otherwise        = go s es

      shrinkDelay = map fromRational . shrink . toRational

      shrinkEvent (StartServer d a p) =
        (shrink p      <&> \ p' -> StartServer d  a p') ++
        (shrinkDelay d <&> \ d' -> StartServer d' a p)
      shrinkEvent (StartClient             d a) = shrinkDelay d <&> \ d' -> StartClient d' a
      shrinkEvent (InboundConnection       d a) = shrinkDelay d <&> \ d' -> InboundConnection  d' a
      shrinkEvent (OutboundConnection      d a) = shrinkDelay d <&> \ d' -> OutboundConnection d' a
      shrinkEvent (CloseInboundConnection  d a) = shrinkDelay d <&> \ d' -> CloseInboundConnection  d' a
      shrinkEvent (CloseOutboundConnection d a) = shrinkDelay d <&> \ d' -> CloseOutboundConnection d' a
      shrinkEvent (InboundMiniprotocols    d a r) =
        (shrinkBundle r <&> \ r' -> InboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> InboundMiniprotocols d' a r)
      shrinkEvent (OutboundMiniprotocols d a r) =
        (shrinkBundle r <&> \ r' -> OutboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> OutboundMiniprotocols d' a r)
      shrinkEvent (ShutdownClientServer d a) = shrinkDelay d <&> \ d' -> ShutdownClientServer d' a


prop_generator_MultiNodeScript :: MultiNodeScript Int TestAddr -> Property
prop_generator_MultiNodeScript (MultiNodeScript script) =
    label ("Number of events: " ++ within_ 10 (length script))
  $ label ( "Number of servers: "
          ++ ( within_ 2
             . length
             . filter (\ ev -> case ev of
                         StartServer {} -> True
                         _              -> False
                      )
                      $ script
             ))
  $ label ("Number of clients: "
          ++ ( within_ 2
             . length
             . filter (\ ev -> case ev of
                         StartClient {} -> True
                         _              -> False
                      )
             $ script
             ))
  $ label ("Active connections: "
          ++ ( within_ 5
             . length
             . filter (\ ev -> case ev of
                         InboundMiniprotocols {}  -> True
                         OutboundMiniprotocols {} -> True
                         _                        -> False)
             $ script
             ))
  $ label ("Closed connections: "
          ++ ( within_ 5
             . length
             . filter (\ ev -> case ev of
                         CloseInboundConnection {}  -> True
                         CloseOutboundConnection {} -> True
                         _                          -> False)
             $ script
             ))
  $ label ("Number of shutdown connections: "
          ++ ( within_ 2
             . length
             . filter (\ ev -> case ev of
                         ShutdownClientServer {} -> True
                         _                       -> False
                      )
             $ script
             ))
  $ True




-- | The concrete address type used by simulations.
--
type SimAddr = Snocket.TestAddress Int

-- | We use a wrapper for test addresses since the Arbitrary instance for Snocket.TestAddress only
--   generates addresses between 1 and 4.
newtype TestAddr = TestAddr { unTestAddr :: SimAddr }
  deriving (Show, Eq, Ord)

instance Arbitrary TestAddr where
  arbitrary = TestAddr . Snocket.TestAddress <$> choose (1, 100)

-- | Each node in the multi-node experiment is controlled by a thread responding to these messages.
data ConnectionHandlerMessage peerAddr req
  = NewConnection peerAddr
    -- ^ Connect to the server at the given address.
  | Disconnect peerAddr
    -- ^ Disconnect from the server at the given address.
  | RunMiniProtocols peerAddr (Bundle [req])
    -- ^ Run a bundle of mini protocols against the server at the given address (requires an active
    --   connection).
  | Shutdown
    -- ^ Shutdowns a server at the given address


data Name addr = Client addr
               | Node addr
               | MainServer
  deriving Eq

instance Show addr => Show (Name addr) where
    show (Client addr) = "client-" ++ show addr
    show (Node   addr) = "node-"   ++ show addr
    show  MainServer   = "main-server"


data ExperimentError addr =
      NodeNotRunningException addr
    | NoActiveConnection addr addr
    | SimulationTimeout
  deriving (Typeable, Show)

instance ( Show addr, Typeable addr ) => Exception (ExperimentError addr)

-- | Run a central server that talks to any number of clients and other nodes.
multinodeExperiment
    :: forall peerAddr socket acc req resp m.
       ( ConnectionManagerMonad m
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m
       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Eq (LazySTM.TVar m (ConnectionState
                                peerAddr
                                (Handle 'InitiatorMode peerAddr ByteString m [resp] Void)
                                (HandleError 'InitiatorMode UnversionedProtocol)
                                (UnversionedProtocol, UnversionedProtocolData)
                                m))
       , Eq (LazySTM.TVar m (ConnectionState_ InitiatorResponderMode peerAddr m [resp] acc))
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => Tracer m (WithName (Name peerAddr)
                          (RemoteTransitionTrace peerAddr))
    -> Tracer m (WithName (Name peerAddr)
                          (AbstractTransitionTrace peerAddr))
    -> Tracer m (WithName (Name peerAddr)
                          (InboundGovernorTrace peerAddr))
    -> Snocket m socket peerAddr
    -> Snocket.AddressFamily peerAddr
    -- ^ either run the main node in 'Duplex' or 'Unidirectional' mode.
    -> peerAddr
    -> req
    -> DataFlow
    -> MultiNodeScript req peerAddr
    -> m ()
multinodeExperiment inboundTrTracer cmTrTracer inboundTracer
                    snocket addrFamily serverAddr accInit
                    dataFlow0 (MultiNodeScript script) =
  withJobPool $ \jobpool -> do
  cc <- startServerConnectionHandler MainServer dataFlow0 [accInit] serverAddr jobpool
  loop (Map.singleton serverAddr [accInit]) (Map.singleton serverAddr cc) script jobpool
  where

    loop :: Map.Map peerAddr acc
         -> Map.Map peerAddr (TQueue m (ConnectionHandlerMessage peerAddr req))
         -> [ConnectionEvent req peerAddr]
         -> JobPool () m (Maybe SomeException)
         -> m ()
    loop _ _ [] _ = threadDelay 3600
    loop nodeAccs servers (event : events) jobpool =
      case event of

        StartClient delay localAddr -> do
          threadDelay delay
          cc <- startClientConnectionHandler (Client localAddr) localAddr jobpool
          loop nodeAccs (Map.insert localAddr cc servers) events jobpool

        StartServer delay localAddr nodeAcc -> do
          threadDelay delay
          cc <- startServerConnectionHandler (Node localAddr) Duplex [nodeAcc] localAddr jobpool
          loop (Map.insert localAddr [nodeAcc] nodeAccs) (Map.insert localAddr cc servers) events jobpool

        InboundConnection delay nodeAddr -> do
          threadDelay delay
          sendMsg nodeAddr $ NewConnection serverAddr
          loop nodeAccs servers events jobpool

        OutboundConnection delay nodeAddr -> do
          threadDelay delay
          sendMsg serverAddr $ NewConnection nodeAddr
          loop nodeAccs servers events jobpool

        CloseInboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg remoteAddr $ Disconnect serverAddr
          loop nodeAccs servers events jobpool

        CloseOutboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg serverAddr $ Disconnect remoteAddr
          loop nodeAccs servers events jobpool

        InboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg nodeAddr $ RunMiniProtocols serverAddr reqs
          loop nodeAccs servers events jobpool

        OutboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg serverAddr $ RunMiniProtocols nodeAddr reqs
          loop nodeAccs servers events jobpool

        ShutdownClientServer delay nodeAddr -> do
          threadDelay delay
          sendMsg nodeAddr $ Shutdown
          loop nodeAccs servers events jobpool
      where
        sendMsg :: peerAddr -> ConnectionHandlerMessage peerAddr req -> m ()
        sendMsg addr msg = atomically $
          case Map.lookup addr servers of
            Nothing -> throwIO (NodeNotRunningException addr)
            Just cc -> writeTQueue cc msg

    mkNextRequests :: StrictTVar m (Map.Map (ConnectionId peerAddr) (Bundle (TQueue m [req]))) ->
                      Bundle (ConnectionId peerAddr -> STM m [req])
    mkNextRequests connVar = makeBundle next
      where
        next :: forall pt. TokProtocolTemperature pt -> ConnectionId peerAddr -> STM m [req]
        next tok connId = do
          connMap <- readTVar connVar
          case Map.lookup connId connMap of
            Nothing -> retry
            Just qs -> readTQueue (projectBundle tok qs)

    startClientConnectionHandler :: Name peerAddr
                                 -> peerAddr
                                 -> JobPool () m (Maybe SomeException)
                                 -> m (TQueue m (ConnectionHandlerMessage peerAddr req))
    startClientConnectionHandler name localAddr jobpool  = do
        cc      <- atomically $ newTQueue
        labelTQueueIO cc $ "cc/" ++ show name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ show name
        threadId <- myThreadId
        forkJob jobpool
          $ Job
              ( withInitiatorOnlyConnectionManager
                    name simTimeouts nullTracer snocket (Just localAddr) (mkNextRequests connVar)
                    timeLimitsHandshake
                  ( \ connectionManager -> do
                    connectionLoop SingInitiatorMode localAddr cc connectionManager Map.empty connVar
                    return Nothing
                  )
                `catch` (\(e :: SomeException) ->
                        case fromException e :: Maybe MuxRuntimeError of
                          Nothing -> throwIO e
                          Just {} -> throwTo threadId e
                                  >> throwIO e)
              )
              (return . Just)
              ()
              (show name)
        return cc

    startServerConnectionHandler :: Name peerAddr
                                 -> DataFlow
                                 -> acc
                                 -> peerAddr
                                 -> JobPool () m (Maybe SomeException)
                                 -> m (TQueue m (ConnectionHandlerMessage peerAddr req))
    startServerConnectionHandler name dataFlow serverAcc localAddr jobpool = do
        fd <- Snocket.open snocket addrFamily
        Snocket.bind   snocket fd localAddr
        Snocket.listen snocket fd
        cc      <- atomically $ newTQueue
        labelTQueueIO cc $ "cc/" ++ show name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ show name
        threadId <- myThreadId
        let job =
              case dataFlow of
                Duplex ->
                  Job ( withBidirectionalConnectionManager
                          name simTimeouts
                          inboundTrTracer cmTrTracer inboundTracer
                          snocket fd (Just localAddr) serverAcc
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          ( \ connectionManager _ _serverAsync -> do
                            connectionLoop SingInitiatorResponderMode localAddr cc connectionManager Map.empty connVar
                            return Nothing
                          )
                        `catch` (\(e :: SomeException) ->
                                case fromException e :: Maybe MuxRuntimeError of
                                  Nothing -> throwIO e
                                  Just {} -> throwTo threadId e
                                          >> throwIO e)
                        `finally` Snocket.close snocket fd
                      )
                      (return . Just)
                      ()
                      (show name)
                Unidirectional ->
                  Job ( withInitiatorOnlyConnectionManager
                          name simTimeouts cmTrTracer snocket (Just localAddr)
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          ( \ connectionManager -> do
                            connectionLoop SingInitiatorMode localAddr cc connectionManager Map.empty connVar
                            return Nothing
                          )
                        `catch` (\(e :: SomeException) ->
                                case fromException e :: Maybe MuxRuntimeError of
                                  Nothing -> throwIO e
                                  Just {} -> throwTo threadId e
                                          >> throwIO e)
                        `finally` Snocket.close snocket fd
                      )
                      (return . Just)
                      ()
                      (show name)
        forkJob jobpool job
        return cc
      where

    connectionLoop
         :: forall muxMode a.
            (HasInitiator muxMode ~ True)
         => SingMuxMode muxMode
         -> peerAddr
         -> TQueue m (ConnectionHandlerMessage peerAddr req)                          -- control channel
         -> MuxConnectionManager muxMode socket peerAddr UnversionedProtocol ByteString m [resp] a
         -> Map.Map peerAddr (Handle muxMode peerAddr ByteString m [resp] a)          -- active connections
         -> StrictTVar m (Map.Map (ConnectionId peerAddr) (Bundle (TQueue m [req])))  -- mini protocol queues
         -> m ()
    connectionLoop muxMode localAddr cc cm connMap0 connVar = go True connMap0
      where
        go :: Bool -- if false do not run 'unregisterOutboundConnection'
           -> Map.Map peerAddr (Handle muxMode peerAddr ByteString m [resp] a) -- active connections
           -> m ()
        go !unregister !connMap = atomically (readTQueue cc) >>= \ case
          NewConnection remoteAddr -> do
            let mkQueue :: forall pt. TokProtocolTemperature pt
                        -> STM m (TQueue m [req])
                mkQueue tok = do
                  q <- newTQueue
                  let temp = case tok of
                        TokHot         -> "hot"
                        TokWarm        -> "warm"
                        TokEstablished -> "cold"
                  q <$ labelTQueue q ("protoVar." ++ temp ++ "@" ++ show localAddr)
            connHandle <- try @_ @SomeException
                          $ requestOutboundConnection cm remoteAddr
            case connHandle of
              Left _ ->
                go False connMap
              Right (Connected _ _ h) -> do
                qs <- atomically $ traverse id $ makeBundle mkQueue
                atomically $ modifyTVar connVar
                           $ Map.insert (connId remoteAddr) qs
                go True (Map.insert remoteAddr h connMap)
              Right Disconnected {} -> return ()
          Disconnect remoteAddr -> do
            atomically $ modifyTVar connVar $ Map.delete (connId remoteAddr)
            when unregister $
              void (unregisterOutboundConnection cm remoteAddr)
            go False (Map.delete remoteAddr connMap)
          RunMiniProtocols remoteAddr reqs -> do
            atomically $ do
              mqs <- Map.lookup (connId remoteAddr) <$> readTVar connVar
              case mqs of
                Nothing ->
                  -- We want to throw because the generator invariant should never put us in
                  -- this case
                  throwIO (NoActiveConnection localAddr remoteAddr)
                Just qs -> do
                  sequence_ $ writeTQueue <$> qs <*> reqs
            case Map.lookup remoteAddr connMap of
              -- We want to throw because the generator invariant should never put us in
              -- this case
              Nothing -> throwIO (NoActiveConnection localAddr remoteAddr)
              Just (Handle mux muxBundle _) ->
                -- TODO:
                -- At times this throws 'ProtocolAlreadyRunning'.
                void $ try @_ @SomeException
                     $ runInitiatorProtocols muxMode mux muxBundle
            go unregister connMap
          Shutdown -> return ()
          where
            connId remoteAddr = ConnectionId { localAddress  = localAddr
                                             , remoteAddress = remoteAddr }


-- | Test property together with classifiction.
data TestProperty = TestProperty {
    tpProperty             :: !Property,
    -- ^ 'True' if property is true

    tpNumberOfTransitions :: !(Sum Int),
    -- ^ number of all transitions

    tpNumberOfConnections :: !(Sum Int),
    -- ^ number of all connections

    --
    -- classifcation of connections
    --
    tpNegotiatedDataFlows :: ![NegotiatedDataFlow],
    tpEffectiveDataFlows  :: ![EffectiveDataFlow],
    tpTerminationTypes    :: ![TerminationType],
    tpActivityTypes       :: ![ActivityType],

    tpTransitions         :: ![AbstractTransition]

  }

instance Show TestProperty where
    show tp =
      concat [ "TestProperty "
             , "{ tpNumberOfTransitions = " ++ show (tpNumberOfTransitions tp)
             , ", tpNumberOfConnections = " ++ show (tpNumberOfConnections tp)
             , ", tpNegotiatedDataFlows = " ++ show (tpNegotiatedDataFlows tp)
             , ", tpTerminationTypes = "    ++ show (tpTerminationTypes tp)
             , ", tpActivityTypes = "       ++ show (tpActivityTypes tp)
             , ", tpTransitions = "         ++ show (tpTransitions tp)
             , "}"
             ]

instance Semigroup TestProperty where
  (<>) (TestProperty a0 a1 a2 a3 a4 a5 a6 a7)
       (TestProperty b0 b1 b2 b3 b4 b5 b6 b7) =
      TestProperty (a0 .&&. b0)
                   (a1 <> b1)
                   (a2 <> b2)
                   (a3 <> b3)
                   (a4 <> b4)
                   (a5 <> b5)
                   (a6 <> b6)
                   (a7 <> b7)

instance Monoid TestProperty where
    mempty = TestProperty (property True)
                          mempty mempty mempty
                          mempty mempty mempty mempty

mkProperty :: TestProperty -> Property
mkProperty TestProperty { tpProperty
                        , tpNumberOfTransitions = Sum numberOfTransitions_
                        , tpNumberOfConnections = Sum numberOfConnections_
                        , tpNegotiatedDataFlows
                        , tpEffectiveDataFlows
                        , tpTerminationTypes
                        , tpActivityTypes
                        , tpTransitions
                        } =
     label (concat [ "Number of transitions: "
                   , within_ 10 numberOfTransitions_
                   ]
           )
   . label (concat [ "Number of connections: "
                   , show numberOfConnections_
                   ]
           )
   . tabulate "Negotiated DataFlow" (map show tpNegotiatedDataFlows)
   . tabulate "Effective DataFLow"  (map show tpEffectiveDataFlows)
   . tabulate "Termination"         (map show tpTerminationTypes)
   . tabulate "Activity Type"       (map show tpActivityTypes)
   . tabulate "Transitions"         (map ppTransition tpTransitions)
   $ tpProperty

newtype AllProperty = AllProperty { getAllProperty :: Property }

instance Semigroup AllProperty where
    AllProperty a <> AllProperty b = AllProperty (a .&&. b)

instance Monoid AllProperty where
    mempty = AllProperty (property True)

newtype ArbDataFlow = ArbDataFlow DataFlow
  deriving Show

instance Arbitrary ArbDataFlow where
    arbitrary = ArbDataFlow <$> frequency [ (3, pure Duplex)
                                          , (1, pure Unidirectional)
                                          ]
    shrink (ArbDataFlow Duplex)         = [ArbDataFlow Unidirectional]
    shrink (ArbDataFlow Unidirectional) = []

data ActivityType
    = IdleConn

    -- | Active connections are onces that reach any of the state:
    --
    -- - 'InboundSt'
    -- - 'OutobundUniSt'
    -- - 'OutboundDupSt'
    -- - 'DuplexSt'
    --
    | ActiveConn
    deriving (Eq, Show)

data TerminationType
    = ErroredTermination
    | CleanTermination
    deriving (Eq, Show)

data NegotiatedDataFlow
    = NotNegotiated

    -- | Negotiated value of 'DataFlow'
    | NegotiatedDataFlow DataFlow
    deriving (Eq, Show)

data EffectiveDataFlow
    -- | Unlike the negotiated 'DataFlow' this indicates if the connection has
    -- ever been in 'DuplexSt'
    --
    = EffectiveDataFlow DataFlow
    deriving (Eq, Show)


-- | Pattern synonym which matches either 'RemoteHotEst' or 'RemoteWarmSt'.
--
pattern RemoteEstSt :: RemoteSt
pattern RemoteEstSt <- (( \ case
                            RemoteHotSt  -> True
                            RemoteWarmSt -> True
                            _            -> False
                         ) -> True
                        )

{-# COMPLETE RemoteEstSt, RemoteIdleSt, RemoteColdSt #-}


-- | Specification of the transition table of the inbound governor.
--
verifyRemoteTransition :: RemoteTransition -> Bool
verifyRemoteTransition Transition {fromState, toState} =
    case (fromState, toState) of
      -- The initial state must be 'RemoteIdleSt'.
      (Nothing,           Just RemoteIdleSt) -> True

      --
      -- Promotions
      --

      (Just RemoteIdleSt, Just RemoteEstSt)  -> True
      (Just RemoteColdSt, Just RemoteEstSt)  -> True
      (Just RemoteWarmSt, Just RemoteHotSt)  -> True

      --
      -- Demotions
      --

      (Just RemoteHotSt,  Just RemoteWarmSt) -> True
      -- demotion to idle state can happen from any established state
      (Just RemoteEstSt,  Just RemoteIdleSt) -> True
      -- demotion to cold can only be done from idle state; We explicitly rule
      -- out demotions to cold from warm or hot states.
      (Just RemoteEstSt,  Just RemoteColdSt) -> False
      (Just RemoteIdleSt, Just RemoteColdSt) -> True
      -- normal termination (if outbound side is not using that connection)
      (Just RemoteIdleSt, Nothing)           -> True
      -- This transition corresponds to connection manager's:
      -- @
      --   Commit^{Duplex}_{Local} : OutboundIdleState Duplex
      --                           → TerminatingState
      -- @
      (Just RemoteColdSt, Nothing)           -> True
      -- any of the mini-protocols errored
      (Just RemoteEstSt, Nothing)            -> True

      --
      -- We are conservative to name all the identity transitions.
      --

      -- This might happen if starting any of the responders errored.
      (Nothing,           Nothing)           -> True
      -- @RemoteWarmSt → RemoteWarmSt@ trnasition is observed if a hot or warm
      -- protocol terminates (which triggers @RemoteEstSt -> RemoteWarmSt@)
      (Just RemoteWarmSt, Just RemoteWarmSt) -> True

      (_,                 _)                 -> False



data Three a b c
    = First  a
    | Second b
    | Third  c
  deriving Show



-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates both connection manager and inbound governor state
-- changes.  'octoSplit' breaks streaming nature, this like causes performance
-- regression of this test.  This suggest we should:
--
-- TODO: split this test into two.
prop_multinode_Sim :: Int -> ArbDataFlow -> AbsBearerInfo -> MultiNodeScript Int TestAddr -> Property
prop_multinode_Sim serverAcc (ArbDataFlow dataFlow) absBi script =
  let evs :: Octopus (Value ())
                     (Three (RemoteTransitionTrace SimAddr)
                            (AbstractTransitionTrace SimAddr)
                            (InboundGovernorTrace SimAddr))
      evs = fmap wnEvent
          . Octopus.filter ((MainServer ==) . wnName)
          . octoSelectTraceEvents
              (\ev ->
                case ev of
                  EventLog dyn ->
                        fmap First
                        <$> fromDynamic
                              @(WithName (Name SimAddr) (RemoteTransitionTrace   SimAddr))
                              dyn
                    <|> fmap Second
                        <$> fromDynamic
                              @(WithName (Name SimAddr) (AbstractTransitionTrace SimAddr))
                              dyn
                    <|> fmap Third
                        <$> fromDynamic
                              @(WithName (Name SimAddr) (InboundGovernorTrace SimAddr))
                              dyn
                  _ ->
                       Nothing
              )
          $ runSimTrace sim
  in counterexample (ppScript script)
    . counterexample (ppOctopus show show evs)
    . (\ ( tr1 :: Octopus (Value ()) (RemoteTransitionTrace   SimAddr)
         , tr2 :: Octopus (Value ()) (AbstractTransitionTrace SimAddr)
         , tr3 :: Octopus (Value ()) (InboundGovernorTrace    SimAddr)
         )
       ->
        ( getAllProperty
        . bifoldMap
            ( \ _ -> AllProperty (property True))
            ( \ tr -> case tr of
                -- verify that 'unregisterInboundConnection' does not return
                -- 'UnsupportedState'.
                TrDemotedToColdRemote _ res ->
                  case res of
                    UnsupportedState {}
                      -> AllProperty (counterexample (show tr) False)
                    _ -> AllProperty (property True)

                -- verify that 'demotedToColdRemote' does not return
                -- 'UnsupportedState'
                TrWaitIdleRemote _ res ->
                  case res of
                    UnsupportedState {}
                      -> AllProperty (counterexample (show tr) False)
                    _ -> AllProperty (property True)

                _     -> AllProperty (property True)
            )

        $ tr3
        )
        .&&.
        ( mkProperty
        . bifoldMap
           ( \ case
               MainReturn {} -> mempty
               v             -> mempty { tpProperty = counterexample (show v) False }
           )
           ( \ trs
            -> TestProperty {
                 tpProperty =
                     (counterexample $!
                       (  "\nconnection:\n"
                       ++ intercalate "\n" (map ppTransition trs))
                       )
                   . getAllProperty
                   . foldMap ( \ tr
                              -> AllProperty
                               . (counterexample $!
                                   (  "\nUnexpected transition: "
                                   ++ show tr)
                                   )
                               . verifyAbstractTransition
                               $ tr
                             )
                   $ trs,
                 tpNumberOfTransitions = Sum (length trs),
                 tpNumberOfConnections = Sum 1,
                 tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
                 tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
                 tpTerminationTypes    = [classifyTermination        trs],
                 tpActivityTypes       = [classifyActivityType       trs],
                 tpTransitions         = trs
              }
           )
         . splitConns
         $ tr2
         )
         .&&.
         ( getAllProperty
         . bifoldMap
            ( \ _ -> AllProperty (property True) )
            ( \ TransitionTrace {ttPeerAddr = peerAddr, ttTransition = tr} ->
                  AllProperty
                . counterexample (concat [ "Unexpected transition: "
                                         , show peerAddr
                                         , " "
                                         , show tr
                                         ])
                . verifyRemoteTransition
                $ tr
            )
         $ tr1
         )

     )
   . octoSplit
   $ evs
  where
    sim :: IOSim s ()
    sim = do
      mb <- timeout 7200
              ( withSnocket
                  debugTracer
                  -- We do this instead of generating a list of
                  -- 'BearerInfo' where the last element is
                  -- 'noAttenuation' because we need the last element
                  -- to run to be 'noAttenuation' and not the last element
                  -- of the list. The test is designed in this way so we
                  -- can not do much about it. This is okay because the
                  -- diffusion simulation will not need to relay on such an
                  -- invariant; the outbound governor is the component which
                  -- makes sure that a progress is made.
                  (Script (toBearerInfo absBi :| [noAttenuation]))
              $ \snocket ->
                 multinodeExperiment (Tracer traceM)
                                     (Tracer traceM)
                                     (Tracer traceM)
                                     snocket
                                     Snocket.TestFamily
                                     (Snocket.TestAddress 0)
                                     serverAcc
                                     dataFlow
                                     (unTestAddr <$> script)
              )
      case mb of
        Nothing -> throwIO (SimulationTimeout :: ExperimentError SimAddr)
        Just a  -> return a

    -- classify negotiated data flow
    classifyNegotiatedDataFlow :: [AbstractTransition] -> NegotiatedDataFlow
    classifyNegotiatedDataFlow as =
      case find ( \ tr
                 -> case toState tr of
                      OutboundUniSt    -> True
                      OutboundDupSt {} -> True
                      InboundIdleSt {} -> True
                      _                -> False
                ) as of
         Nothing -> NotNegotiated
         Just tr ->
           case toState tr of
             OutboundUniSt      -> NegotiatedDataFlow Unidirectional
             OutboundDupSt {}   -> NegotiatedDataFlow Duplex
             (InboundIdleSt df) -> NegotiatedDataFlow df
             _                  -> error "impossible happened!"

    -- classify effective data flow
    classifyEffectiveDataFlow :: [AbstractTransition] -> EffectiveDataFlow
    classifyEffectiveDataFlow as =
      case find ((== DuplexSt) . toState) as of
        Nothing -> EffectiveDataFlow Unidirectional
        Just _  -> EffectiveDataFlow Duplex

    -- classify termination
    classifyTermination :: [AbstractTransition] -> TerminationType
    classifyTermination as =
      case last $ dropWhileEnd
                    (== (Transition TerminatedSt TerminatedSt))
                $ dropWhileEnd
                    (== (Transition TerminatedSt UnknownConnectionSt))
                $ as of
        Transition { fromState = TerminatingSt
                   , toState   = TerminatedSt
                   } -> CleanTermination
        _            -> ErroredTermination

    -- classify if a connection is active or not
    classifyActivityType :: [AbstractTransition] -> ActivityType
    classifyActivityType as =
      case find ( \ tr
                 -> case toState tr of
                      InboundSt     {} -> True
                      OutboundUniSt    -> True
                      OutboundDupSt {} -> True
                      DuplexSt      {} -> True
                      _                -> False
                ) as of
        Nothing -> IdleConn
        Just {} -> ActiveConn


-- Right fold of the 'Octopus' which splits its results.
--
octoSplit :: Octopus a (Three b c d) -> (Octopus a b, Octopus a c, Octopus a d)
octoSplit = go [] [] []
  where
    -- this version seems the fastest one (faster than a strict version, DList
    -- style or a coinductive definition).
    go :: [b] -> [c] -> [d]
       -> Octopus a (Three b c d)
       -> (Octopus a b, Octopus a c, Octopus a d)
    go bs cs ds (Nil a) = ( foldl' (flip Cons) (Nil a) bs
                          , foldl' (flip Cons) (Nil a) cs
                          , foldl' (flip Cons) (Nil a) ds
                          )
    go bs cs ds (Cons (First  b) o) = go (b : bs)      cs       ds o
    go bs cs ds (Cons (Second c) o) = go      bs  (c : cs)      ds o
    go bs cs ds (Cons (Third d) o) = go       bs       cs  (d : ds) o

-- | Connection terminated while negotiating it.
--
unit_connection_terminated_when_negotiating :: Property
unit_connection_terminated_when_negotiating =
    prop_multinode_Sim
        0 (ArbDataFlow Unidirectional)
        AbsBearerInfo
          { abiConnectionDelay = SmallDelay
          , abiInboundAttenuation = NoAttenuation FastSpeed
          , abiOutboundAttenuation = NoAttenuation FastSpeed
          , abiInboundWriteFailure = Nothing
          , abiOutboundWriteFailure = Just 3
          , abiSDUSize = LargeSDU
          }
        (MultiNodeScript
          [ StartServer 0           (TestAddr {unTestAddr = TestAddress 24}) 0
          , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 24})
          , StartServer 0           (TestAddr {unTestAddr = TestAddress 40}) 0
          , OutboundMiniprotocols 0 (TestAddr {unTestAddr = TestAddress 24})
                                    (Bundle { withHot         = WithHot [0]
                                            , withWarm        = WithWarm []
                                            , withEstablished = WithEstablished []
                                            })
          , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 40})
          ])


splitConns :: Octopus (Value ()) (AbstractTransitionTrace SimAddr)
           -> Octopus (Value ()) [AbstractTransition]
splitConns =
    bimap id fromJust
  . Octopus.filter isJust
  -- there might be some connections in the state, push them onto the 'Octopus'
  . (\(s, o) -> foldr (\a as -> Octopus.Cons (Just a) as) o (Map.elems s))
  . bimapAccumL
      ( \ s a -> ( s, a))
      ( \ s TransitionTrace { ttPeerAddr, ttTransition } ->
          case ttTransition of
            Transition _ UnknownConnectionSt ->
              case ttPeerAddr `Map.lookup` s of
                Nothing  -> ( Map.insert ttPeerAddr [ttTransition] s
                            , Nothing
                            )
                Just trs -> ( Map.delete ttPeerAddr s
                            , Just (reverse $ ttTransition : trs)
                            )
            _ ->            ( Map.alter ( \ case
                                              Nothing -> Just [ttTransition]
                                              Just as -> Just (ttTransition : as)
                                        ) ttPeerAddr s
                            , Nothing
                            )
      )
      Map.empty

ppTransition :: AbstractTransition -> String
ppTransition Transition {fromState, toState} =
    printf "%-30s → %s" (show fromState) (show toState)

ppScript :: (Show peerAddr, Show req) => MultiNodeScript peerAddr req -> String
ppScript (MultiNodeScript script) = intercalate "\n" $ go 0 script
  where
    delay (StartServer             d _ _) = d
    delay (StartClient             d _)   = d
    delay (InboundConnection       d _)   = d
    delay (OutboundConnection      d _)   = d
    delay (InboundMiniprotocols    d _ _) = d
    delay (OutboundMiniprotocols   d _ _) = d
    delay (CloseInboundConnection  d _)   = d
    delay (CloseOutboundConnection d _)   = d
    delay (ShutdownClientServer    d _)   = d

    ppEvent (StartServer             _ a i) = "Start server " ++ show a ++ " with accInit=" ++ show i
    ppEvent (StartClient             _ a)   = "Start client " ++ show a
    ppEvent (InboundConnection       _ a)   = "Connection from " ++ show a
    ppEvent (OutboundConnection      _ a)   = "Connecting to " ++ show a
    ppEvent (InboundMiniprotocols    _ a p) = "Miniprotocols from " ++ show a ++ ": " ++ ppData p
    ppEvent (OutboundMiniprotocols   _ a p) = "Miniprotocols to " ++ show a ++ ": " ++ ppData p
    ppEvent (CloseInboundConnection  _ a)   = "Close connection from " ++ show a
    ppEvent (CloseOutboundConnection _ a)   = "Close connection to " ++ show a
    ppEvent (ShutdownClientServer    _ a)   = "Shutdown client/server " ++ show a

    ppData (Bundle hot warm est) =
      concat [ "hot:", show (withoutProtocolTemperature hot)
             , " warm:", show (withoutProtocolTemperature warm)
             , " est:", show (withoutProtocolTemperature est)]

    go _ [] = []
    go t (e : es) = printf "%5s: %s" (show t') (ppEvent e) : go t' es
      where t' = t + delay e

--
-- Utils
--

data WithName name event = WithName {
    wnName  :: name,
    wnEvent :: event
  }
  deriving (Show, Functor)

type AbstractTransitionTrace addr = TransitionTrace' addr AbstractState

sayTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
sayTracer = Tracer $
  \msg -> (,msg) <$> getCurrentTime >>= say . show


-- | Redefine this tracer to get valuable tracing information from various
-- components:
--
-- * connection-manager
-- * inbound governor
-- * server
--
debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
debugTracer = Tracer (\msg -> (,msg) <$> getCurrentTime >>= say . show)
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


-- | Convenience function to create a Bundle. Could move to Ouroboros.Network.Mux.
makeBundle :: (forall pt. TokProtocolTemperature pt -> a) -> Bundle a
makeBundle f = Bundle (WithHot         $ f TokHot)
                      (WithWarm        $ f TokWarm)
                      (WithEstablished $ f TokEstablished)



-- TODO: we should use @traceResult True@; the `prop_unidirectional_Sim` and
-- `prop_bidirectional_Sim` test are failing with `<<io-sim sloppy shutdown>>`
-- exception.
--
simulatedPropertyWithTimeout :: DiffTime -> (forall s. IOSim s Property) -> Property
simulatedPropertyWithTimeout t test =
  counterexample ("\nTrace:\n" ++ ppTrace_ tr) $
  case traceResult False tr of
    Left failure ->
      counterexample ("Failure:\n" ++ displayException failure) False
    Right prop -> fromMaybe (counterexample "timeout" $ property False) prop
  where
    tr = runSimTrace $ timeout t test

ppTrace_ :: Trace a -> String
ppTrace_ tr = concat
    [ "====== Trace ======\n"
    , ppTrace' tr
    , "\n\n====== Say Events ======\n"
    , intercalate "\n" $ selectTraceEventsSay' tr
    , "\n"
    ]

within_ :: Int -> Int -> String
within_ _ 0 = "0"
within_ a b = let x = b `div` a in
              concat [ if b < a
                         then "1"
                         else show $ x * a
                     , " - "
                     , show $ x * a + a - 1
                     ]
