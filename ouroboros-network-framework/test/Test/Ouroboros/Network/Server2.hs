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
-- should be reverted once, `prop_multinode_pruning_Sim` is fixed.
{-# OPTIONS_GHC -Wno-unused-top-binds      #-}

module Test.Ouroboros.Network.Server2
  ( tests
  ) where

import           Control.Exception (AssertionFailed, SomeAsyncException (..))
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
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void, ($>), (<&>))
import           Data.List (dropWhileEnd, find, mapAccumL, intercalate, (\\), delete, foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Trace as Trace
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, fromJust, isJust)
import           Data.Monoid (Sum (..))
import           Data.Monoid.Synchronisation (FirstToFinish (..))
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Foreign.C.Error
import qualified GHC.IO.Exception as IO

import           Text.Printf

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import           Control.Concurrent.JobPool

import           Codec.CBOR.Term (Term)

import qualified Network.Mux as Mux
import           Network.Mux.Types (MuxRuntimeError)
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
import qualified Ouroboros.Network.ConnectionManager.Types as CM
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                   RemoteSt (..))
import qualified Ouroboros.Network.InboundGovernor as IG
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

import           Simulation.Network.Snocket

import           Ouroboros.Network.Testing.Data.Script (Script (..), singletonScript)
import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision)
import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance
import           Test.Simulation.Network.Snocket hiding (tests)
import           Test.Ouroboros.Network.ConnectionManager (verifyAbstractTransition)
import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                   (NonFailingBearerInfoScript(..), AbsBearerInfo (..),
                    AbsDelay (..), AbsAttenuation (..), AbsSpeed (..),
                    AbsSDUSize (..))

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Server2"
  [ testProperty "unidirectional_IO"  prop_unidirectional_IO
  , testProperty "unidirectional_Sim" prop_unidirectional_Sim
  , testProperty "bidirectional_IO"   prop_bidirectional_IO
  , testProperty "bidirectional_Sim"  prop_bidirectional_Sim
  , testProperty "connection_manager_pruning"
                 prop_connection_manager_pruning
  , testProperty "inbound_governor_pruning"
                 prop_inbound_governor_pruning
  , testProperty "never_above_hardlimit"
                 prop_never_above_hardlimit
  , testProperty "connection_manager_valid_transitions"
                 prop_connection_manager_valid_transitions
  , testProperty "connection_manager_no_invalid_traces"
                 prop_connection_manager_no_invalid_traces
  , testProperty "inbound_governor_no_invalid_traces"
                 prop_inbound_governor_no_invalid_traces
  , testProperty "inbound_governor_valid_transitions"
              prop_inbound_governor_valid_transitions
  , testProperty "inbound_governor_no_unsupported_state"
                 prop_inbound_governor_no_unsupported_state
  , testProperty "connection_manager_valid_transition_order"
                 prop_connection_manager_valid_transition_order
  , testProperty "inbound_governor_valid_transition_order"
                 prop_inbound_governor_valid_transition_order
  , testGroup    "unit_server_accept_error"
    [ testProperty "throw ConnectionAborted"
                  (unit_server_accept_error IOErrConnectionAborted IOErrThrow)
    , testProperty "throw ResourceExhausted"
                  (unit_server_accept_error IOErrResourceExhausted IOErrThrow)
    , testProperty "return ConnectionAborted"
                  (unit_server_accept_error IOErrConnectionAborted IOErrReturn)
    , testProperty "return ResourceExhausted"
                  (unit_server_accept_error IOErrResourceExhausted IOErrReturn)
    ]
  , testProperty "unit_connection_terminated_when_negotiating"
                 unit_connection_terminated_when_negotiating
  , testGroup "generators"
    [ testProperty "MultiNodeScript"
                   prop_generator_MultiNodeScript
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
    -- we pass a `StricTVar` with all the requests to each initiator.  This way
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


withInitiatorOnlyConnectionManager
    :: forall name peerAddr socket req resp m a.
       ( ConnectionManagerMonad m

       , resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req
       , Show name
       )
    => name
    -- ^ identifier (for logging)
    -> Timeouts
    -> Tracer m (WithName name (AbstractTransitionTrace peerAddr))
    -> Tracer m (WithName name
                          (ConnectionManagerTrace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    -> Snocket m socket peerAddr
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> Maybe peerAddr
    -> Bundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
    -- ^ Handshake time limits
    -> AcceptedConnectionsLimit
    -> (MuxConnectionManager
          InitiatorMode socket peerAddr
          UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager name timeouts trTracer cmTracer snocket localAddr
                                   nextRequests handshakeTimeLimits acceptedConnLimit k = do
    mainThreadId <- myThreadId
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer
    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = WithName name
                        `contramap` cmTracer,
          cmTrTracer  = (WithName name . fmap abstractState)
                        `contramap` trTracer,
         -- MuxTracer
          cmMuxTracer = muxTracer,
          cmIPv4Address = localAddr,
          cmIPv6Address = Nothing,
          cmAddressType = \_ -> Just IPv4Address,
          cmSnocket = snocket,
          connectionDataFlow = getProtocolDataFlow . snd,
          cmPrunePolicy = simplePrunePolicy,
          cmConnectionsLimits = acceptedConnLimit,
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
    -> Tracer m (WithName name
                          (ConnectionManagerTrace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
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
    -> AcceptedConnectionsLimit
    -> (MuxConnectionManager
          InitiatorResponderMode socket peerAddr
          UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> Async m Void
       -> m a)
    -> m a
withBidirectionalConnectionManager name timeouts
                                   inboundTrTracer trTracer
                                   cmTracer inboundTracer
                                   snocket socket localAddress
                                   accumulatorInit nextRequests
                                   handshakeTimeLimits
                                   acceptedConnLimit k = do
    mainThreadId <- myThreadId
    inbgovControlChannel      <- Server.newControlChannel
    -- we are not using the randomness
    observableStateVar        <- Server.newObservableStateVarFromSeed 0
    let muxTracer = WithName name `contramap` nullTracer -- mux tracer

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = WithName name
                        `contramap` cmTracer,
          cmTrTracer  = (WithName name . fmap abstractState)
                        `contramap` trTracer,
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
          cmConnectionsLimits = acceptedConnLimit
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
          (dataFlowProtocol Duplex serverApplication)
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
                      WithName name `contramap` nullTracer, -- ServerTrace
                    serverInboundGovernorTracer =
                      WithName name `contramap` inboundTracer, -- InboundGovernorTrace
                    serverConnectionLimits = acceptedConnLimit,
                    serverConnectionManager = connectionManager,
                    serverInboundIdleTimeout = tProtocolIdleTimeout timeouts,
                    serverControlChannel = inbgovControlChannel,
                    serverObservableStateVar = observableStateVar
                  }
              )
              (\serverAsync -> k connectionManager serverAddr serverAsync)
          `catch` \(e :: SomeException) -> do
            throwIO e
  where
    -- for a bidirectional mux we need to define 'Mux.MiniProtocolInfo' for each
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
       , MonadLabelledSTM m
       , MonadSay m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
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
      "client" timeouts nullTracer nullTracer snocket Nothing nextReqs
      timeLimitsHandshake maxAcceptedConnectionsLimit
      $ \connectionManager ->
        withBidirectionalConnectionManager "server" timeouts
                                           nullTracer nullTracer nullTracer
                                           nullTracer snocket socket Nothing
                                           [accumulatorInit clientAndServerData]
                                           noNextRequests
                                           timeLimitsHandshake
                                           maxAcceptedConnectionsLimit
          $ \_ serverAddr serverAsync -> do
            link serverAsync
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

prop_unidirectional_Sim :: NonFailingBearerInfoScript
                        -> ClientAndServerData Int
                        -> Property
prop_unidirectional_Sim (NonFailingBearerInfoScript script) clientAndServerData =
  simulatedPropertyWithTimeout 7200 $
    withSnocket nullTracer
                (toBearerInfo <$> script) $ \snock _ ->
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
                                         nullTracer snocket socket0
                                         (Just localAddr0)
                                         [accumulatorInit clientAndServerData0]
                                         nextRequests0
                                         noTimeLimitsHandshake
                                         maxAcceptedConnectionsLimit
        (\connectionManager0 _serverAddr0 serverAsync0 -> do
          link serverAsync0
          withBidirectionalConnectionManager "node-1" timeouts
                                             nullTracer nullTracer nullTracer
                                             nullTracer snocket socket1
                                             (Just localAddr1)
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
              ( rs0 :: [Either SomeException (Bundle [resp])]
                , rs1 :: [Either SomeException (Bundle [resp])]
                ) <-
                -- Run initiator twice; this tests if the responders on
                -- the other end are restarted.
                replicateM
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
                  ))
                `concurrently`
                replicateM
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
                  ))

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
                (toBearerInfo <$> script)
                $ \snock _ ->
      bracket ((,) <$> Snocket.open snock Snocket.TestFamily
                   <*> Snocket.open snock Snocket.TestFamily)
              (\ (socket0, socket1) -> Snocket.close snock socket0 >>
                                       Snocket.close snock socket1)
        $ \ (socket0, socket1) -> do
          let addr0 = Snocket.TestAddress (0 :: Int)
              addr1 = Snocket.TestAddress 1
          Snocket.bind   snock socket0 addr0
          Snocket.bind   snock socket1 addr1
          Snocket.listen snock socket0
          Snocket.listen snock socket1
          bidirectionalExperiment False simTimeouts snock
                                        socket0 socket1
                                        addr0 addr1
                                        data0 data1

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

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim_Pruning`.
-- This test optimizes for triggering prunings.
data MultiNodePruningScript req =
  MultiNodePruningScript AcceptedConnectionsLimit
                         -- ^ Should yield small values to trigger pruning
                         -- more often
                         [ConnectionEvent req TestAddr]
  deriving (Show)

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
                    [ (4, CloseInboundConnection  <$> delay <*> elements inboundConnections)                | not $ null inboundConnections ] ++
                    [ (4, CloseOutboundConnection <$> delay <*> elements outboundConnections)               | not $ null outboundConnections ] ++
                    [ (16, InboundMiniprotocols   <$> delay <*> elements inboundConnections  <*> genBundle) | not $ null inboundConnections ] ++
                    [ (16, OutboundMiniprotocols  <$> delay <*> elements outboundConnections <*> genBundle) | not $ null outboundConnections ] ++
                    [ (2, ShutdownClientServer    <$> delay <*> elements possibleStoppable)                 | not $ null possibleStoppable ]
        (event :) <$> go (nextState event s) (n - 1)
        where
          possibleStoppable  = startedClients ++ startedServers
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

-- | Max bound AcceptedConnectionsLimit
maxAcceptedConnectionsLimit :: AcceptedConnectionsLimit
maxAcceptedConnectionsLimit = AcceptedConnectionsLimit maxBound maxBound 0

-- | This Script has a percentage of events more favorable to trigger pruning
--   transitions. And forces a bidirectional connection between each server.
--   It also starts inbound protocols in order to trigger the:
--
--   'Connected',
--   'NegotiatedDuplexOutbound',
--   'PromotedToWarmDuplexRemote',
--   'DemotedToColdDuplexLocal'
--
--   transitions.
--
instance Arbitrary req =>
         Arbitrary (MultiNodePruningScript req) where
  arbitrary = do
    Positive len <- scale ((* 2) . (`div` 3)) arbitrary
    -- NOTE: Although we still do not enforce the configured hardlimit to be
    -- strictly positive. We assume that the hard limit is always bigger than
    -- 0.
    Small hardLimit <- (`div` 10) <$> arbitrary
    softLimit <- chooseBoundedIntegral (hardLimit `div` 2, hardLimit)
    MultiNodePruningScript (AcceptedConnectionsLimit hardLimit softLimit 0)
                           <$> go (ScriptState [] [] [] [] []) (len :: Integer)
   where
     -- Divide delays by 100 to avoid running in to protocol and SDU timeouts
     -- if waiting too long between connections and mini protocols.
     delay = frequency [ (1,  pure 0)
                       , (16, (/ 10) <$> genDelayWithPrecision 2)
                       , (32, (/ 100) <$> genDelayWithPrecision 2)
                       , (16, (/ 1000) <$> genDelayWithPrecision 2)
                       ]
     go _ 0 = pure []
     go s@ScriptState{..} n = do
       event <-
         frequency $
           [ (1, StartClient <$> delay <*> newServer)
           , (16, StartServer <$> delay <*> newServer <*> arbitrary) ] ++
           [ (4, InboundConnection
                  <$> delay <*> elements possibleInboundConnections)
           | not $ null possibleInboundConnections ] ++
           [ (4, OutboundConnection
                  <$> delay <*> elements possibleOutboundConnections)
           | not $ null possibleOutboundConnections] ++
           [ (4, CloseInboundConnection
                  <$> delay <*> elements inboundConnections)
           | not $ null inboundConnections ] ++
           [ (20, CloseOutboundConnection
                  <$> delay <*> elements outboundConnections)
           | not $ null outboundConnections ] ++
           [ (16, InboundMiniprotocols
                  <$> delay <*> elements inboundConnections <*> genBundle)
           | not $ null inboundConnections ] ++
           [ (4, OutboundMiniprotocols
                  <$> delay <*> elements outboundConnections <*> genBundle)
           | not $ null outboundConnections ] ++
           [ (1, ShutdownClientServer
                  <$> delay <*> elements possibleStoppable)
           | not $ null possibleStoppable ]
       case event of
         StartServer _ c _ -> do
           inboundConnection <- InboundConnection <$> delay <*> pure c
           outboundConnection <- OutboundConnection <$> delay <*> pure c
           inboundMiniprotocols <- InboundMiniprotocols <$> delay
                                                       <*> pure c
                                                       <*> genBundle
           let events = [ event, inboundConnection
                        , outboundConnection, inboundMiniprotocols]
           (events ++) <$> go (foldl' (flip nextState) s events) (n - 1)

         _ -> (event :) <$> go (nextState event s) (n - 1)
       where
         possibleStoppable = startedClients ++ startedServers
         possibleInboundConnections  = (startedClients ++ startedServers)
                                       \\ inboundConnections
         possibleOutboundConnections = startedServers \\ outboundConnections
         newServer = arbitrary `suchThat` (`notElem` possibleStoppable)

  shrink (MultiNodePruningScript
            (AcceptedConnectionsLimit hardLimit softLimit delay) events) =
    MultiNodePruningScript
        <$> (AcceptedConnectionsLimit
              <$> shrink hardLimit
              <*> shrink softLimit
              <*> pure delay)
        <*> (makeValid
            <$> shrinkList shrinkEvent events)
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
      shrinkEvent (StartClient             d a) =
        shrinkDelay d <&> \ d' -> StartClient d' a
      shrinkEvent (InboundConnection       d a) =
        shrinkDelay d <&> \ d' -> InboundConnection  d' a
      shrinkEvent (OutboundConnection      d a) =
        shrinkDelay d <&> \ d' -> OutboundConnection d' a
      shrinkEvent (CloseInboundConnection  d a) =
        shrinkDelay d <&> \ d' -> CloseInboundConnection  d' a
      shrinkEvent (CloseOutboundConnection d a) =
        shrinkDelay d <&> \ d' -> CloseOutboundConnection d' a
      shrinkEvent (InboundMiniprotocols    d a r) =
        (shrinkBundle r <&> \ r' -> InboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> InboundMiniprotocols d' a r)
      shrinkEvent (OutboundMiniprotocols d a r) =
        (shrinkBundle r <&> \ r' -> OutboundMiniprotocols d  a r') ++
        (shrinkDelay  d <&> \ d' -> OutboundMiniprotocols d' a r)
      shrinkEvent (ShutdownClientServer d a) =
        shrinkDelay d <&> \ d' -> ShutdownClientServer d' a


-- | The concrete address type used by simulations.
--
type SimAddr  = Snocket.TestAddress SimAddr_
type SimAddr_ = Int

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
    -> Tracer m (WithName (Name peerAddr)
                          (ConnectionManagerTrace
                            peerAddr
                            (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)))
    -> Snocket m socket peerAddr
    -> Snocket.AddressFamily peerAddr
    -- ^ either run the main node in 'Duplex' or 'Unidirectional' mode.
    -> peerAddr
    -> req
    -> DataFlow
    -> AcceptedConnectionsLimit
    -> MultiNodeScript req peerAddr
    -> m ()
multinodeExperiment inboundTrTracer trTracer cmTracer inboundTracer
                    snocket addrFamily serverAddr accInit
                    dataFlow0 acceptedConnLimit (MultiNodeScript script) =
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
          sendMsg nodeAddr Shutdown
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
                    name simTimeouts nullTracer nullTracer snocket (Just localAddr) (mkNextRequests connVar)
                    timeLimitsHandshake acceptedConnLimit
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
                          inboundTrTracer trTracer inboundTracer cmTracer
                          snocket fd (Just localAddr) serverAcc
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          acceptedConnLimit
                          ( \ connectionManager _ serverAsync -> do
                            link serverAsync
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
                          name simTimeouts trTracer inboundTracer snocket (Just localAddr)
                          (mkNextRequests connVar)
                          timeLimitsHandshake
                          acceptedConnLimit
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
            connHandle <- tryJust (\(e :: SomeException) ->
                                       case fromException e of
                                         Just SomeAsyncException {} -> Nothing
                                         _                          -> Just e)
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
              Just (Handle mux muxBundle _) -> do
                -- TODO:
                -- At times this throws 'ProtocolAlreadyRunning'.
                r <- tryJust (\(e :: SomeException) ->
                                  case fromException e of
                                    Just SomeAsyncException {} -> Nothing -- rethrown
                                    _                          -> Just e)
                     $ runInitiatorProtocols muxMode mux muxBundle
                case r of
                  -- Lost connection to peer
                  Left  {} -> do
                    atomically
                      $ modifyTVar connVar (Map.delete (connId remoteAddr))
                    go unregister (Map.delete remoteAddr connMap)
                  Right {} -> go unregister connMap
          Shutdown -> return ()
          where
            connId remoteAddr = ConnectionId { localAddress  = localAddr
                                             , remoteAddress = remoteAddr }


-- | Test property together with classification.
--
data TestProperty = TestProperty {
    tpProperty            :: !Property,
    -- ^ 'True' if property is true

    tpNumberOfTransitions :: !(Sum Int),
    -- ^ number of all transitions

    tpNumberOfConnections :: !(Sum Int),
    -- ^ number of all connections

    tpNumberOfPrunings    :: !(Sum Int),
    -- ^ number of all connections

    --
    -- classification of connections
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
             , ", tpNumberOfPrunings = "    ++ show (tpNumberOfPrunings tp)
             , ", tpNegotiatedDataFlows = " ++ show (tpNegotiatedDataFlows tp)
             , ", tpTerminationTypes = "    ++ show (tpTerminationTypes tp)
             , ", tpActivityTypes = "       ++ show (tpActivityTypes tp)
             , ", tpTransitions = "         ++ show (tpTransitions tp)
             , "}"
             ]

instance Semigroup TestProperty where
  (<>) (TestProperty a0 a1 a2 a3 a4 a5 a6 a7 a8)
       (TestProperty b0 b1 b2 b3 b4 b5 b6 b7 b8) =
      TestProperty (a0 .&&. b0)
                   (a1 <> b1)
                   (a2 <> b2)
                   (a3 <> b3)
                   (a4 <> b4)
                   (a5 <> b5)
                   (a6 <> b6)
                   (a7 <> b7)
                   (a8 <> b8)

instance Monoid TestProperty where
    mempty = TestProperty (property True)
                          mempty mempty mempty mempty
                          mempty mempty mempty mempty

mkProperty :: TestProperty -> Property
mkProperty TestProperty { tpProperty
                        , tpNumberOfTransitions = Sum numberOfTransitions_
                        , tpNumberOfConnections = Sum numberOfConnections_
                        , tpNumberOfPrunings = Sum numberOfPrunings_
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
   . tabulate "Pruning"             [show numberOfPrunings_]
   . tabulate "Negotiated DataFlow" (map show tpNegotiatedDataFlows)
   . tabulate "Effective DataFLow"  (map show tpEffectiveDataFlows)
   . tabulate "Termination"         (map show tpTerminationTypes)
   . tabulate "Activity Type"       (map show tpActivityTypes)
   . tabulate "Transitions"         (map ppTransition tpTransitions)
   $ tpProperty

mkPropertyPruning :: TestProperty -> Property
mkPropertyPruning tp@TestProperty { tpNumberOfPrunings = Sum numberOfPrunings_ } =
     cover 35 (numberOfPrunings_ > 0) "Prunings"
   . mkProperty
   $ tp

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
      -- @RemoteWarmSt → RemoteWarmSt@, @RemoteIdleSt → RemoteIdleSt@ and
      -- @RemoteColdSt → RemoteColdSt@ transition are observed if a hot or
      -- warm protocol terminates (which triggers @RemoteEstSt -> RemoteWarmSt@)
      (Just RemoteWarmSt, Just RemoteWarmSt) -> True
      (Just RemoteIdleSt, Just RemoteIdleSt) -> True
      (Just RemoteColdSt, Just RemoteColdSt) -> True

      (_,                 _)                 -> False



data Three a b c
    = First  a
    | Second b
    | Third  c
  deriving Show


-- Assuming all transitions in the transition list are valid, we only need to
-- look at the 'toState' of the current transition and the 'fromState' of the
-- next transition.
verifyAbstractTransitionOrder :: [AbstractTransition]
                              -> AllProperty
verifyAbstractTransitionOrder [] = mempty
verifyAbstractTransitionOrder (h:t) = go t h
  where
    go :: [AbstractTransition] -> AbstractTransition -> AllProperty
    -- All transitions must end in the 'UnknownConnectionSt', and since we
    -- assume that all transitions are valid we do not have to check the
    -- 'fromState'.
    go [] (Transition _ UnknownConnectionSt) = mempty
    go [] tr@(Transition _ _)          =
      AllProperty
        $ counterexample
            ("\nUnexpected last transition: " ++ show tr)
            (property False)
    -- All transitions have to be in a correct order, which means that the
    -- current state we are looking at (current toState) needs to be equal to
    -- the next 'fromState', in order for the transition chain to be correct.
    go (next@(Transition nextFromState _) : ts)
        curr@(Transition _ currToState) =
         AllProperty
           (counterexample
              ("\nUnexpected transition order!\nWent from: "
              ++ show curr ++ "\nto: " ++ show next)
              (property (currToState == nextFromState)))
         <> go ts next

-- Assuming all transitions in the transition list are valid, we only need to
-- look at the 'toState' of the current transition and the 'fromState' of the
-- next transition.
verifyRemoteTransitionOrder :: [RemoteTransition]
                            -> AllProperty
verifyRemoteTransitionOrder [] = mempty
verifyRemoteTransitionOrder (h:t) = go t h
  where
    go :: [RemoteTransition] -> RemoteTransition -> AllProperty
    -- All transitions must end in the 'Nothing' (final) state, and since
    -- we assume all transitions are valid we do not have to check the
    -- 'fromState' .
    go [] (Transition _ Nothing) = mempty
    go [] tr@(Transition _ _)          =
      AllProperty
        $ counterexample
            ("\nUnexpected last transition: " ++ show tr)
            (property False)
    -- All transitions have to be in a correct order, which means that the
    -- current state we are looking at (current toState) needs to be equal to
    -- the next 'fromState', in order for the transition chain to be correct.
    go (next@(Transition nextFromState _) : ts)
        curr@(Transition _ currToState) =
         AllProperty
           (counterexample
              ("\nUnexpected transition order!\nWent from: "
              ++ show curr ++ "\nto: " ++ show next)
              (property (currToState == nextFromState)))
         <> go ts next

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates connection manager state changes.
--
prop_connection_manager_valid_transitions :: Int
                                          -> ArbDataFlow
                                          -> AbsBearerInfo
                                          -> MultiNodeScript Int TestAddr
                                          -> Property
prop_connection_manager_valid_transitions serverAcc (ArbDataFlow dataFlow)
                                          absBi script@(MultiNodeScript l) =
  let trace = runSimTrace sim

      evsATT :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      evsATT = traceWithNameTraceEvents trace

      evsCMT :: [ConnectionManagerTrace
                  SimAddr
                  (ConnectionHandlerTrace
                    UnversionedProtocol
                    DataFlowProtocolData)]
      evsCMT = withNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript script)
    . counterexample (Trace.ppTrace show show evsATT)
    . mkProperty
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
             tpNumberOfPrunings    = classifyPrunings evsCMT,
             tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
             tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
             tpTerminationTypes    = [classifyTermination        trs],
             tpActivityTypes       = [classifyActivityType       trs],
             tpTransitions         = trs
          }
       )
    . splitConns
    $ evsATT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates that we do not get undesired traces, such as
-- TrUnexpectedlyMissingConnectionState in connection manager.
--
prop_connection_manager_no_invalid_traces :: Int
                                          -> ArbDataFlow
                                          -> AbsBearerInfo
                                          -> MultiNodeScript Int TestAddr
                                          -> Property
prop_connection_manager_no_invalid_traces serverAcc (ArbDataFlow dataFlow)
                                          absBi (MultiNodeScript l) =
  let trace = runSimTrace sim

      evsCMT :: Trace (SimResult ())
                      (ConnectionManagerTrace
                        SimAddr
                        (ConnectionHandlerTrace
                          UnversionedProtocol
                          DataFlowProtocolData))
      evsCMT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript (MultiNodeScript l)
                     , "========== ConnectionManager Events =========="
                     , Trace.ppTrace show show evsCMT
                     -- , "========== Simulation Trace =========="
                     -- , ppTrace trace
                     ])
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> AllProperty (counterexample (show v) False)
       )
       ( \ tr
        -> case tr of
          CM.TrUnexpectedlyFalseAssertion _ ->
            AllProperty (counterexample (show tr) False)
          _                                       ->
            mempty
       )
    $ evsCMT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates the order of connection manager state changes.
--
prop_connection_manager_valid_transition_order :: Int
                                               -> ArbDataFlow
                                               -> AbsBearerInfo
                                               -> MultiNodeScript Int TestAddr
                                               -> Property
prop_connection_manager_valid_transition_order serverAcc (ArbDataFlow dataFlow)
                                               absBi script@(MultiNodeScript l) =
  let trace = runSimTrace sim

      evsATT :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      evsATT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript script)
    . counterexample (Trace.ppTrace show show evsATT)
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> AllProperty (property False)
       )
       verifyAbstractTransitionOrder
    . splitConns
    $ evsATT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates inbound governor state changes.
--
prop_inbound_governor_valid_transitions :: Int
                                        -> ArbDataFlow
                                        -> AbsBearerInfo
                                        -> MultiNodeScript Int TestAddr
                                        -> Property
prop_inbound_governor_valid_transitions serverAcc (ArbDataFlow dataFlow)
                                        absBi script@(MultiNodeScript l) =
  let trace = runSimTrace sim

      evsRTT :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      evsRTT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript script)
    . counterexample (Trace.ppTrace show show evsRTT)
    -- Verify that all Inbound Governor remote transitions are valid
    . getAllProperty
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
    $ evsRTT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates inbound governor state changes.
--
prop_inbound_governor_no_unsupported_state :: Int
                                           -> ArbDataFlow
                                           -> AbsBearerInfo
                                           -> MultiNodeScript Int TestAddr
                                           -> Property
prop_inbound_governor_no_unsupported_state serverAcc (ArbDataFlow dataFlow)
                                           absBi script@(MultiNodeScript l) =
  let trace = runSimTrace sim

      evsIGT :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      evsIGT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript script)
    . counterexample (Trace.ppTrace show show evsIGT)
    -- Verify we do not return unsupported states in any of the
    -- RemoteTransitionTrace
    . getAllProperty
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
    $ evsIGT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates that we do not get undesired traces, such as
-- TrUnexpectedlyMissingConnectionState in inbound governor.
--
prop_inbound_governor_no_invalid_traces :: Int
                                        -> ArbDataFlow
                                        -> AbsBearerInfo
                                        -> MultiNodeScript Int TestAddr
                                        -> Property
prop_inbound_governor_no_invalid_traces serverAcc (ArbDataFlow dataFlow)
                                          absBi (MultiNodeScript l) =
  let trace = runSimTrace sim

      evsIGT :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      evsIGT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript (MultiNodeScript l)
                     , "========== Inbound Governor Events =========="
                     , Trace.ppTrace show show evsIGT
                     -- , "========== Simulation Trace =========="
                     -- , ppTrace trace
                     ])
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> AllProperty (counterexample (show v) False)
       )
       ( \ tr
        -> case tr of
          IG.TrUnexpectedlyFalseAssertion _ ->
            AllProperty (counterexample (show tr) False)
          _                                       ->
            mempty
       )
    $ evsIGT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates the order of inbound governor state changes.
--
prop_inbound_governor_valid_transition_order :: Int
                                             -> ArbDataFlow
                                             -> AbsBearerInfo
                                             -> MultiNodeScript Int TestAddr
                                             -> Property
prop_inbound_governor_valid_transition_order serverAcc (ArbDataFlow dataFlow)
                                             absBi script@(MultiNodeScript l) =
  let trace = runSimTrace sim

      evsRTT :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      evsRTT = traceWithNameTraceEvents trace

      evsIGT :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      evsIGT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (Trace.ppTrace show show evsIGT)
    . counterexample (ppScript script)
    . counterexample (Trace.ppTrace show show evsRTT)
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> AllProperty (property False)
       )
       verifyRemoteTransitionOrder
    . splitRemoteConns
    $ evsRTT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       (Script (toBearerInfo absBi :| [noAttenuation]))
                       maxAcceptedConnectionsLimit l


-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in Connection
-- Manager.
--
prop_connection_manager_pruning :: Int -> MultiNodePruningScript Int -> Property
prop_connection_manager_pruning serverAcc
                                (MultiNodePruningScript acceptedConnLimit l) =
  let trace = runSimTrace sim

      evsATT :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      evsATT = traceWithNameTraceEvents trace

      evsCMT :: [ConnectionManagerTrace
                  SimAddr
                  (ConnectionHandlerTrace
                    UnversionedProtocol
                    DataFlowProtocolData)]
      evsCMT = withNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript (MultiNodeScript l))
    . counterexample (Trace.ppTrace show show evsATT)
    . mkPropertyPruning
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
             tpNumberOfPrunings    = classifyPrunings evsCMT,
             tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
             tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
             tpTerminationTypes    = [classifyTermination        trs],
             tpActivityTypes       = [classifyActivityType       trs],
             tpTransitions         = trs
          }
       )
    . splitConns
    $ evsATT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex (singletonScript noAttenuation)
                       acceptedConnLimit l

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in the
-- Inbound Governor.
--
prop_inbound_governor_pruning :: Int -> MultiNodePruningScript Int -> Property
prop_inbound_governor_pruning serverAcc
                              (MultiNodePruningScript acceptedConnLimit l) =
  let trace = runSimTrace sim

      evsRTT :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      evsRTT = traceWithNameTraceEvents trace

      evsIGT :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      evsIGT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    . counterexample (ppScript (MultiNodeScript l))
    . counterexample (Trace.ppTrace show show evsRTT)
    . counterexample (Trace.ppTrace show show evsIGT)
    . (\ ( tr1
         , tr2
         )
       ->
        -- Verify we do not return unsupported states in any of the
        -- RemoteTransitionTrace
        ( getAllProperty
        . bifoldMap
            ( \ _ -> AllProperty (property True))
            ( \ tr -> case tr of
                -- verify that 'unregisterInboundConnection' does not return
                -- 'UnsupportedState'.
                TrDemotedToColdRemote _ res ->
                  case res of
                    UnsupportedState {}
                      -> AllProperty
                          $ counterexample
                              ("Unexpected UnsupportedState "
                              ++ "in unregisterInboundConnection "
                              ++ show tr)
                              False
                    _ -> AllProperty (property True)

                -- verify that 'demotedToColdRemote' does not return
                -- 'UnsupportedState'
                TrWaitIdleRemote _ res ->
                  case res of
                    UnsupportedState {}
                      -> AllProperty
                          $ counterexample
                              ("Unexpected UnsupportedState "
                              ++ "in demotedToColdRemote "
                              ++ show tr)
                              False
                    _ -> AllProperty (property True)

                _     -> AllProperty (property True)
            )

        $ tr2
        )
        .&&.
        -- Verify that all Inbound Governor remote transitions are valid
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
   $ (evsRTT, evsIGT)
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex (singletonScript noAttenuation)
                       acceptedConnLimit l

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- We test that:
--
-- * we never go above hard limit of incoming connections;
-- * the pruning set is at least as big as expected, and that
--   the picked peers belong to the choice set.
--
prop_never_above_hardlimit :: Int -> MultiNodePruningScript Int -> Property
prop_never_above_hardlimit serverAcc
                           (MultiNodePruningScript
                             acceptedConnLimit@AcceptedConnectionsLimit
                               { acceptedConnectionsHardLimit = hardlimit }
                               l
                           ) =
  let trace = runSimTrace sim

      evsCMT :: Trace (SimResult ())
                        (ConnectionManagerTrace
                          SimAddr
                          (ConnectionHandlerTrace
                            UnversionedProtocol
                            DataFlowProtocolData))
      evsCMT = traceWithNameTraceEvents trace

      evsATT :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      evsATT = traceWithNameTraceEvents trace

      evsIGT :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      evsIGT = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showCEvs l)
    -- . counterexample (ppTrace_ trace)
    . counterexample (ppScript (MultiNodeScript l))
    . counterexample (Trace.ppTrace show show evsCMT)
    . counterexample (Trace.ppTrace show show evsATT)
    . counterexample (Trace.ppTrace show show evsIGT)
    . getAllProperty
    . bifoldMap
        ( \ case
            MainReturn {} -> mempty
            _             -> AllProperty (property False)
        )
        ( \ trs ->
            case trs of
              x -> case x of
                (TrConnectionManagerCounters cmc) ->
                    AllProperty
                    . counterexample ("HardLimit: " ++ show hardlimit ++
                                      ", but got: " ++ show (incomingConns cmc) ++
                                      " incoming connections!\n" ++
                                      show cmc
                                     )
                    . property
                    $ incomingConns cmc <= fromIntegral hardlimit
                (TrPruneConnections prunnedSet numberToPrune choiceSet) ->
                  ( AllProperty
                  . counterexample (concat
                                   [ "prunned set too small: "
                                   , show numberToPrune
                                   , " ≰ "
                                   , show $ length prunnedSet
                                   ])
                  $ numberToPrune <= length prunnedSet )
                  <>
                  ( AllProperty
                  . counterexample ""
                  $ prunnedSet `Set.isSubsetOf` choiceSet )
                _ -> mempty
        )
   $ evsCMT
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex (singletonScript noAttenuation)
                       acceptedConnLimit l


-- | Checks that the server re-throws exceptions returned by an 'accept' call.
--
unit_server_accept_error :: IOErrType -> IOErrThrowOrReturn -> Property
unit_server_accept_error ioErrType ioErrThrowOrReturn =
    runSimOrThrow sim
  where
    -- The following attenuation make sure that the `accept` call will throw.
    --
    bearerAttenuation :: BearerInfo
    bearerAttenuation =
      noAttenuation { biAcceptFailures = Just (0, ioErrType, ioErrThrowOrReturn) }

    sim :: IOSim s Property
    sim = handle (\e -> return $ case fromException e of
                          Just (ExceptionInLinkedThread _ err) ->
                            case fromException err of
                              Just (_ :: IOError) -> property True
                              Nothing             -> property False
                          Nothing                 -> property False
                 )
        $ withSnocket nullTracer
                      (singletonScript bearerAttenuation )
        $ \snock _ ->
           bracket ((,) <$> Snocket.open snock Snocket.TestFamily
                        <*> Snocket.open snock Snocket.TestFamily)
                   (\ (socket0, socket1) -> Snocket.close snock socket0 >>
                                            Snocket.close snock socket1)
             $ \ (socket0, socket1) -> do

               let addr :: SimAddr
                   addr = Snocket.TestAddress (0 :: Int)
                   pdata :: ClientAndServerData Int
                   pdata = ClientAndServerData 0 [] [] []
               Snocket.bind snock socket0 addr
               Snocket.listen snock socket0
               nextRequests <- oneshotNextRequests pdata
               withBidirectionalConnectionManager "node-0" simTimeouts
                                                  nullTracer nullTracer
                                                  nullTracer nullTracer
                                                  snock socket0
                                                  (Just addr)
                                                  [accumulatorInit pdata]
                                                  nextRequests
                                                  noTimeLimitsHandshake
                                                  maxAcceptedConnectionsLimit
                 (\_connectionManager _serverAddr serverAsync -> do
                   -- connect to the server
                   Snocket.connect snock socket1 addr
                     --  connect will fail, and it will trigger accept error
                     `catch` \(_ :: SomeException) -> return ()
                   -- verify that server's `accept` error is rethrown by the
                   -- server
                   v <- registerDelay 1
                   r <- atomically $ runFirstToFinish $
                         (FirstToFinish $
                           Just <$> waitCatchSTM serverAsync)
                         <>
                         (FirstToFinish $
                           LazySTM.readTVar v >>= \a -> check a $> Nothing)
                   return $ case r of
                     Nothing        -> counterexample "server did not throw"
                                         (ioErrType == IOErrConnectionAborted)
                     Just (Right _) -> counterexample "unexpected value" False
                     Just (Left e)  | IOErrReturn <- ioErrThrowOrReturn
                                    , Just (err :: IOError) <- fromException e
                                    -- any IO exception which is not
                                    -- 'ECONNABORTED' is ok
                                    -- TODO: use isECONNABORTED function
                                    -> case IO.ioe_errno err of
                                          Just errno ->
                                            case eCONNABORTED of
                                              Errno errno' ->
                                                counterexample
                                                  ("unexpected error " ++ show e) $
                                                   errno /= errno'
                                          Nothing ->
                                            property True

                                    -- If we throw exceptions, any IO exception
                                    -- can go through; the check for
                                    -- 'ECONNABORTED' relies on that.  It is
                                    -- a bug in a snocket implementation if it
                                    -- throws instead of returns io errors.
                                    | Just (_ :: IOError) <- fromException e
                                    -> property True

                                    |  otherwise
                                    -> counterexample ("unexpected error " ++ show e)
                                                      False
                 )




multiNodeSim :: (Serialise req, Show req, Eq req, Typeable req)
             => req
             -> DataFlow
             -> Script BearerInfo
             -> AcceptedConnectionsLimit
             -> [ConnectionEvent req TestAddr]
             -> IOSim s ()
multiNodeSim serverAcc dataFlow script acceptedConnLimit l = do
      mb <- timeout 7200
                    ( withSnocket nullTracer
                                  script
              $ \snocket _ ->
                 multinodeExperiment (Tracer traceM)
                                     (Tracer traceM)
                                     (Tracer traceM)
                                     (Tracer traceM)
                                     snocket
                                     Snocket.TestFamily
                                     (Snocket.TestAddress 0)
                                     serverAcc
                                     dataFlow
                                     acceptedConnLimit
                                     (unTestAddr <$> MultiNodeScript l)
              )
      case mb of
        Nothing -> throwIO (SimulationTimeout :: ExperimentError SimAddr)
        Just a  -> return a

-- | Connection terminated while negotiating it.
--
unit_connection_terminated_when_negotiating :: Property
unit_connection_terminated_when_negotiating =
  let arbDataFlow = ArbDataFlow Unidirectional
      absBearerInfo =
        AbsBearerInfo
          { abiConnectionDelay = SmallDelay
          , abiInboundAttenuation = NoAttenuation FastSpeed
          , abiOutboundAttenuation = NoAttenuation FastSpeed
          , abiInboundWriteFailure = Nothing
          , abiOutboundWriteFailure = Just 3
          , abiSDUSize = LargeSDU
          }
      multiNodeScript =
        MultiNodeScript
         [ StartServer 0           (TestAddr {unTestAddr = TestAddress 24}) 0
         , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 24})
         , StartServer 0           (TestAddr {unTestAddr = TestAddress 40}) 0
         , OutboundMiniprotocols 0 (TestAddr {unTestAddr = TestAddress 24})
                                   (Bundle { withHot         = WithHot [0]
                                           , withWarm        = WithWarm []
                                           , withEstablished = WithEstablished []
                                           })
         , OutboundConnection 0    (TestAddr {unTestAddr = TestAddress 40})
         ]
   in
    prop_connection_manager_valid_transitions
        0 arbDataFlow absBearerInfo
        multiNodeScript
    .&&.
    prop_inbound_governor_valid_transitions
        0 arbDataFlow absBearerInfo
        multiNodeScript
    .&&.
    prop_inbound_governor_no_unsupported_state
        0 arbDataFlow absBearerInfo
        multiNodeScript


splitConns :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
           -> Trace (SimResult ()) [AbstractTransition]
splitConns =
    bimap id fromJust
  . Trace.filter isJust
  -- there might be some connections in the state, push them onto the 'Trace'
  . (\(s, o) -> foldr (\a as -> Trace.Cons (Just a) as) o (Map.elems s))
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

splitRemoteConns :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
                 -> Trace (SimResult ()) [RemoteTransition]
splitRemoteConns =
    bimap id fromJust
  . Trace.filter isJust
  -- there might be some connections in the state, push them onto the 'Trace'
  . (\(s, o) -> foldr (\a as -> Trace.Cons (Just a) as) o (Map.elems s))
  . bimapAccumL
      ( \ s a -> ( s, a))
      ( \ s TransitionTrace { ttPeerAddr, ttTransition } ->
          case ttTransition of
            Transition _ Nothing ->
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

traceWithNameTraceEvents :: forall b. Typeable b
                    => SimTrace () -> Trace (SimResult ()) b
traceWithNameTraceEvents = fmap wnEvent
          . Trace.filter ((MainServer ==) . wnName)
          . traceSelectTraceEventsDynamic
              @()
              @(WithName (Name SimAddr) b)

withNameTraceEvents :: forall b. Typeable b => SimTrace () -> [b]
withNameTraceEvents = fmap wnEvent
          . filter ((MainServer ==) . wnName)
          . selectTraceEventsDynamic
              @()
              @(WithName (Name SimAddr) b)

sayTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
sayTracer = Tracer $
  \msg -> (,msg) <$> getCurrentTime >>= say . show


showCEvs :: ConnectionEvent req peerAddr -> String
showCEvs (StartClient{})             = "StartClient"
showCEvs (StartServer{})             = "StartServer"
showCEvs (InboundConnection{})       = "InboundConnection"
showCEvs (OutboundConnection{})      = "OutboundConnection"
showCEvs (InboundMiniprotocols{})    = "InboundMiniprotocols"
showCEvs (OutboundMiniprotocols{})   = "OutboundMiniprotocols"
showCEvs (CloseInboundConnection{})  = "CloseInboundConnection"
showCEvs (CloseOutboundConnection{}) = "CloseOutboundConnection"
showCEvs (ShutdownClientServer{})    = "ShutdownClientServer"


-- classify negotiated data flow
classifyPrunings :: [ConnectionManagerTrace SimAddr (ConnectionHandlerTrace UnversionedProtocol DataFlowProtocolData)] -> Sum Int
classifyPrunings =
  Sum
  . length
  . filter ( \ tr
             -> case tr of
                  x -> case x of
                    TrPruneConnections _ _ _ -> True
                    _                        -> False
           )

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
  counterexample ("\nTrace:\n" ++ prettyPrintTrace tr) $
  case traceResult False tr of
    Left failure ->
      counterexample ("Failure:\n" ++ displayException failure) False
    Right prop -> fromMaybe (counterexample "timeout" $ property False) prop
  where
    tr = runSimTrace $ timeout t test


prettyPrintTrace :: SimTrace a -> String
prettyPrintTrace tr = concat
    [ "====== Trace ======\n"
    , ppTrace_ tr
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
