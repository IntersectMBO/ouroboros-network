{-# LANGUAGE BangPatterns        #-}
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

module Test.Ouroboros.Network.Server2 (tests) where

import           Control.Exception (AssertionFailed, SomeAsyncException (..))
import           Control.Monad (replicateM, when, (>=>))
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST (MonadST)
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, contramapM, nullTracer)

import           Codec.Serialise.Class (Serialise)
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Bool (bool)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (foldMap')
import           Data.Functor (void, ($>), (<&>))
import           Data.List (delete, dropWhileEnd, find, foldl', intercalate,
                     mapAccumL, nub, (\\))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.Trace as Trace
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import           Data.Monoid (Sum (..))
import           Data.Monoid.Synchronisation (FirstToFinish (..))
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Foreign.C.Error
import qualified GHC.IO.Exception as IO

import           Text.Printf

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Control.Concurrent.JobPool

import           Codec.CBOR.Term (Term)

import qualified Network.Mux as Mux
import           Network.Mux.Types (MuxRuntimeError)
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Core

import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Examples
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Type

import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import qualified Ouroboros.Network.ConnectionManager.Types as CM
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                     RemoteSt (..))
import qualified Ouroboros.Network.InboundGovernor as IG
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as Server
import           Ouroboros.Network.InboundGovernor.State
                     (InboundGovernorCounters (..))
import           Ouroboros.Network.Mux
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (cborTermVersionDataCodec, noTimeLimitsHandshake,
                     timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type
                     (ClientHasAgency (TokPropose), Handshake,
                     ServerHasAgency (TokConfirm))
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..))
import           Ouroboros.Network.RethrowPolicy
import           Ouroboros.Network.Server.RateLimiting
                     (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Server2 (RemoteTransition,
                     RemoteTransitionTrace, ServerArguments (..))
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (Snocket, TestAddress (..),
                     socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Simulation.Network.Snocket

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                     (AbsAttenuation (..), AbsBearerInfo (..),
                     AbsBearerInfoScript (..), AbsDelay (..), AbsSDUSize (..),
                     AbsSpeed (..), NonFailingAbsBearerInfoScript (..),
                     absNoAttenuation, toNonFailingAbsBearerInfoScript)
import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision, nightlyTest)

import           Test.Ouroboros.Network.ConnectionManager
                     (allValidTransitionsNames, validTransitionMap,
                     verifyAbstractTransition)
import           Test.Ouroboros.Network.Orphans ()
import           Test.Simulation.Network.Snocket hiding (tests)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network"
  [ testGroup "ConnectionManager"
    [ testProperty "valid transitions"      prop_connection_manager_valid_transitions
    , nightlyTest $ testProperty "valid transitions (racy)"
                  $ prop_connection_manager_valid_transitions_racy
    , testProperty "valid transition order" prop_connection_manager_valid_transition_order
    , nightlyTest $ testProperty "valid transition order (racy)"
                  $ prop_connection_manager_valid_transition_order_racy
    , testProperty "transitions coverage"   prop_connection_manager_transitions_coverage
    , testProperty "no invalid traces"      prop_connection_manager_no_invalid_traces
    , testProperty "counters"               prop_connection_manager_counters
    , testProperty "pruning"                prop_connection_manager_pruning
    ]
  , testGroup "InboundGovernor"
    [ testProperty "valid transitions"      prop_inbound_governor_valid_transitions
    , testProperty "valid transition order" prop_inbound_governor_valid_transition_order
    , testProperty "transitions coverage"   prop_inbound_governor_transitions_coverage
    , testProperty "no invalid traces"      prop_inbound_governor_no_invalid_traces
    , testProperty "no unsupported state"   prop_inbound_governor_no_unsupported_state
    , testProperty "pruning"                prop_inbound_governor_pruning
    , testProperty "counters"               prop_inbound_governor_counters
    , testProperty "timeouts enforced"      prop_timeouts_enforced
    ]
  , testGroup "Server2"
    [ testProperty "unidirectional IO"      prop_unidirectional_IO
    , testProperty "unidirectional Sim"     prop_unidirectional_Sim
    , testProperty "bidirectional IO"       prop_bidirectional_IO
    , testProperty "bidirectional Sim"      prop_bidirectional_Sim
    , testProperty "never above hardlimit"  prop_never_above_hardlimit
    , testGroup      "accept errors"
      [ testProperty "throw ConnectionAborted"
                    (unit_server_accept_error IOErrConnectionAborted IOErrThrow)
      , testProperty "throw ResourceExhausted"
                    (unit_server_accept_error IOErrResourceExhausted IOErrThrow)
      , testProperty "return ConnectionAborted"
                    (unit_server_accept_error IOErrConnectionAborted IOErrReturn)
      , testProperty "return ResourceExhausted"
                    (unit_server_accept_error IOErrResourceExhausted IOErrReturn)
      ]
    ]
  , testProperty "connection terminated when negotiating"
                 unit_connection_terminated_when_negotiating
  , testGroup "generators"
    [ testProperty "MultiNodeScript" prop_generator_MultiNodeScript
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
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
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
          SingInitiatorMode          -> Mux.InitiatorDirectionOnly
          SingInitiatorResponderMode -> Mux.InitiatorDirection)
        Mux.StartEagerly
        (runMuxPeer
          (case miniProtocolRun ptcl of
            InitiatorProtocolOnly initiator           -> initiator
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
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
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
                    Right a  -> a === expected .&&. acc)
                (property True)
                $ zip rs (expectedResult clientAndServerData clientAndServerData)

prop_unidirectional_Sim :: ClientAndServerData Int
                        -> Property
prop_unidirectional_Sim clientAndServerData =
  simulatedPropertyWithTimeout 7200 $
    withSnocket nullTracer
                noAttenuation
                Map.empty
      $ \snock _ ->
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
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
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


prop_bidirectional_Sim :: ClientAndServerData Int
                       -> ClientAndServerData Int
                       -> Property
prop_bidirectional_Sim data0 data1 =
  simulatedPropertyWithTimeout 7200 $
    withSnocket sayTracer
                noAttenuation
                Map.empty
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
data MultiNodeScript req peerAddr = MultiNodeScript
  { mnsEvents         :: [ConnectionEvent req peerAddr]
  , mnsAttenuationMap :: Map peerAddr
                             (Script AbsBearerInfo)
  }
  deriving (Show)

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim_Pruning`.
-- This test optimizes for triggering prunings.
data MultiNodePruningScript req = MultiNodePruningScript
  { mnpsAcceptedConnLimit :: AcceptedConnectionsLimit
    -- ^ Should yield small values to trigger pruning
    -- more often
  , mnpsEvents            :: [ConnectionEvent req TestAddr]
  , mnpsAttenuationMap    :: Map TestAddr
                                 (Script AbsBearerInfo)
  }
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

genAttenuationMap :: Ord peerAddr
                  => [ConnectionEvent req peerAddr]
                  -> Gen (Map peerAddr (Script AbsBearerInfo))
genAttenuationMap events = do
  let nodes = map
                (\ev -> case ev of
                  StartClient _ addr   -> pure addr
                  StartServer _ addr _ -> pure addr
                  _                    -> error "Impossible happened"
                )
            . filter
                (\ev -> case ev of
                  StartClient _ _   -> True
                  StartServer _ _ _ -> True
                  _                 -> False
                )
            $ events

  size <- chooseInt (0, length nodes)
  nodeSample <- nub <$> replicateM size (oneof nodes)

  attenuationMap <- mapM (\addr -> do
                            script <- arbitrary
                            return (addr, script)
                        )
                        nodeSample

  return (Map.fromList attenuationMap)


instance (Arbitrary peerAddr, Arbitrary req, Ord peerAddr) =>
         Arbitrary (MultiNodeScript req peerAddr) where
  arbitrary = do
      Positive len <- scale ((* 2) . (`div` 3)) arbitrary
      events <- go (ScriptState [] [] [] [] []) (len :: Integer)
      attenuationMap <- genAttenuationMap events
      return (MultiNodeScript events attenuationMap)
    where     -- Divide delays by 100 to avoid running in to protocol and SDU timeouts if waiting
              -- too long between connections and mini protocols.
      delay = frequency [(1, pure 0), (3, (/ 100) <$> genDelayWithPrecision 2)]

      go _ 0 = pure []
      go s@ScriptState{..} n = do
        event <- frequency $
                    [ (6, StartClient             <$> delay <*> newClient)
                    , (6, StartServer             <$> delay <*> newServer <*> arbitrary) ] ++
                    [ (4, InboundConnection       <$> delay <*> elements possibleInboundConnections)        | not $ null possibleInboundConnections] ++
                    [ (4, OutboundConnection      <$> delay <*> elements possibleOutboundConnections)       | not $ null possibleOutboundConnections] ++
                    [ (6, CloseInboundConnection  <$> delay <*> elements inboundConnections)                | not $ null inboundConnections ] ++
                    [ (4, CloseOutboundConnection <$> delay <*> elements outboundConnections)               | not $ null outboundConnections ] ++
                    [ (10, InboundMiniprotocols   <$> delay <*> elements inboundConnections  <*> genBundle) | not $ null inboundConnections ] ++
                    [ (8, OutboundMiniprotocols  <$> delay <*> elements outboundConnections <*> genBundle) | not $ null outboundConnections ] ++
                    [ (4, ShutdownClientServer    <$> delay <*> elements possibleStoppable)                 | not $ null possibleStoppable ]
        (event :) <$> go (nextState event s) (n - 1)
        where
          possibleStoppable  = startedClients ++ startedServers
          possibleInboundConnections  = (startedClients ++ startedServers) \\ inboundConnections
          possibleOutboundConnections = startedServers \\ outboundConnections
          newClient = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))
          newServer = arbitrary `suchThat` (`notElem` (startedClients ++ startedServers))

  shrink (MultiNodeScript events attenuationMap) = do
    events' <- makeValid <$> shrinkList shrinkEvent events
    attenuationMap' <- shrink attenuationMap
    return (MultiNodeScript events' attenuationMap')
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
prop_generator_MultiNodeScript (MultiNodeScript script _) =
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
    events <- go (ScriptState [] [] [] [] []) (len :: Integer)
    attenuationMap <- genAttenuationMap events
    return
      $ MultiNodePruningScript (AcceptedConnectionsLimit hardLimit softLimit 0)
                               events
                               attenuationMap
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

  -- TODO: The shrinking here is not optimal. It works better if we shrink one
  -- value at a time rather than all of them at once. If we shrink to quickly,
  -- we could miss which change actually introduces the failure, and be lift
  -- with a larger counter example.
  shrink (MultiNodePruningScript
            (AcceptedConnectionsLimit hardLimit softLimit delay)
            events
            attenuationMap) =
    MultiNodePruningScript
        <$> (AcceptedConnectionsLimit
              <$> shrink hardLimit
              <*> shrink softLimit
              <*> pure delay)
        <*> (makeValid
            <$> shrinkList shrinkEvent events)
        <*> shrink attenuationMap
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
       , MonadFix m
       , MonadLabelledSTM m
       , MonadTraceSTM m
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
                    dataFlow0 acceptedConnLimit
                    (MultiNodeScript script _) =
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
                            linkOnly (const True) serverAsync
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



-- | Maps each valid remote transition into one number. Collapses all invalid
-- transition into a single number.
--
-- NOTE: Should be in sync with 'verifyRemoteTransition'
--
validRemoteTransitionMap :: RemoteTransition -> (Int, String)
validRemoteTransitionMap t@Transition { fromState, toState } =
    case (fromState, toState) of
      (Nothing          , Just RemoteIdleSt) -> (00, show t)
      (Just RemoteIdleSt, Just RemoteEstSt)  -> (01, show t)
      (Just RemoteColdSt, Just RemoteEstSt)  -> (02, show t)
      (Just RemoteWarmSt, Just RemoteHotSt)  -> (03, show t)
      (Just RemoteHotSt , Just RemoteWarmSt) -> (04, show t)
      (Just RemoteEstSt , Just RemoteIdleSt) -> (05, show t)
      (Just RemoteIdleSt, Just RemoteColdSt) -> (06, show t)
      (Just RemoteIdleSt, Nothing)           -> (07, show t)
      (Just RemoteColdSt, Nothing)           -> (08, show t)
      (Just RemoteEstSt , Nothing)           -> (09, show t)
      (Nothing          , Nothing)           -> (10, show t)
      (Just RemoteWarmSt, Just RemoteWarmSt) -> (11, show t)
      (Just RemoteIdleSt, Just RemoteIdleSt) -> (12, show t)
      (Just RemoteColdSt, Just RemoteColdSt) -> (13, show t)
      (_                , _)                 -> (99, show t)

-- | List of all valid transition's names.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition'.
--
allValidRemoteTransitionsNames :: [String]
allValidRemoteTransitionsNames =
  map show
  [ Transition Nothing             (Just RemoteIdleSt)
  , Transition (Just RemoteIdleSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteIdleSt) (Just RemoteHotSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteHotSt)
  , Transition (Just RemoteWarmSt) (Just RemoteHotSt)
  , Transition (Just RemoteHotSt ) (Just RemoteWarmSt)
  , Transition (Just RemoteWarmSt) (Just RemoteIdleSt)
  -- , Transition (Just RemoteHotSt)  (Just RemoteIdleSt)
  , Transition (Just RemoteIdleSt) (Just RemoteColdSt)
  , Transition (Just RemoteIdleSt) Nothing
  , Transition (Just RemoteColdSt) Nothing
  , Transition (Just RemoteWarmSt) Nothing
  , Transition (Just RemoteHotSt)  Nothing
  , Transition Nothing             Nothing
  -- , Transition (Just RemoteWarmSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteIdleSt) (Just RemoteIdleSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteColdSt)
  ]

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


validate_transitions :: MultiNodeScript Int TestAddr
                     -> SimTrace ()
                     -> Property
validate_transitions mns@(MultiNodeScript events _) trace =
      tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show abstractTransitionEvents)
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
             tpNumberOfPrunings    = classifyPrunings connectionManagerEvents,
             tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
             tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
             tpTerminationTypes    = [classifyTermination        trs],
             tpActivityTypes       = [classifyActivityType       trs],
             tpTransitions         = trs
          }
       )
    . fmap (map ttTransition)
    . splitConns id
    $ abstractTransitionEvents
  where
    abstractTransitionEvents :: Trace (SimResult ())
                                      (AbstractTransitionTrace SimAddr)
    abstractTransitionEvents = traceWithNameTraceEvents trace

    connectionManagerEvents :: [ConnectionManagerTrace
                                  SimAddr
                                  (ConnectionHandlerTrace
                                    UnversionedProtocol
                                    DataFlowProtocolData)]
    connectionManagerEvents = withNameTraceEvents trace



-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test validates connection manager state changes.
--
prop_connection_manager_valid_transitions
  :: Int
  -> ArbDataFlow
  -> AbsBearerInfo
  -> MultiNodeScript Int TestAddr
  -> Property
prop_connection_manager_valid_transitions
  serverAcc (ArbDataFlow dataFlow) defaultBearerInfo
  mns@(MultiNodeScript events attenuationMap) =
    validate_transitions mns trace
  where
    trace = runSimTrace sim
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


prop_connection_manager_valid_transitions_racy
  :: Int
  -> ArbDataFlow
  -> AbsBearerInfo
  -> MultiNodeScript Int TestAddr
  -> Property
prop_connection_manager_valid_transitions_racy
  serverAcc (ArbDataFlow dataFlow)
  defaultBearerInfo mns@(MultiNodeScript events attenuationMap) =
    exploreSimTrace id sim $ \_ trace ->
                             validate_transitions mns trace
  where
    sim :: IOSim s ()
    sim = exploreRaces
       >> multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test coverage of connection manager state transitions.
-- TODO: Fix transitions that are not covered. #3516
prop_connection_manager_transitions_coverage :: Int
                                             -> ArbDataFlow
                                             -> AbsBearerInfo
                                             -> MultiNodeScript Int TestAddr
                                             -> Property
prop_connection_manager_transitions_coverage serverAcc
    (ArbDataFlow dataFlow)
    defaultBearerInfo
    (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      abstractTransitionEvents :: [AbstractTransitionTrace SimAddr]
      abstractTransitionEvents = withNameTraceEvents trace

      transitionsSeen = nub [ tran | TransitionTrace _ tran <- abstractTransitionEvents]
      transitionsSeenNames = map (snd . validTransitionMap) transitionsSeen

   in coverTable "valid transitions" [ (n, 0.01) | n <- allValidTransitionsNames ] $
      tabulate   "valid transitions" transitionsSeenNames
      True
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

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
                                          defaultBearerInfo
                                          (MultiNodeScript events
                                                           attenuationMap) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (ConnectionManagerTrace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript (MultiNodeScript events attenuationMap)
                     , "========== ConnectionManager Events =========="
                     , Trace.ppTrace show show connectionManagerEvents
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
    $ connectionManagerEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

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
                                               defaultBearerInfo
                                               mns@(MultiNodeScript
                                                        events
                                                        attenuationMap) =
  let trace = runSimTrace sim

      abstractTransitionEvents :: Trace (SimResult ())
                                        (AbstractTransitionTrace SimAddr)
      abstractTransitionEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show abstractTransitionEvents)
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> AllProperty (property False)
       )
       verifyAbstractTransitionOrder
    . fmap (map ttTransition)
    . splitConns id
    $ abstractTransitionEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


prop_connection_manager_valid_transition_order_racy :: Int
                                                    -> ArbDataFlow
                                                    -> AbsBearerInfo
                                                    -> MultiNodeScript Int TestAddr
                                                    -> Property
prop_connection_manager_valid_transition_order_racy serverAcc (ArbDataFlow dataFlow)
                                                    defaultBearerInfo
                                                    mns@(MultiNodeScript
                                                             events
                                                             attenuationMap) =
    exploreSimTrace
          (\a -> a { explorationReplay = Just ControlDefault })
          sim $ \_ trace ->
      let abstractTransitionEvents :: Trace (SimResult ())
                                            (AbstractTransitionTrace SimAddr)
          abstractTransitionEvents = traceWithNameTraceEvents trace

      in  -- ppDebug trace
          tabulate "ConnectionEvents" (map showConnectionEvents events)
        . counterexample (ppScript mns)
        . counterexample (Trace.ppTrace show show abstractTransitionEvents)
        . getAllProperty
        . bifoldMap
           ( \ case
               MainReturn {} -> mempty
               _             -> AllProperty (property False)
           )
           verifyAbstractTransitionOrder
        . fmap (map ttTransition)
        . splitConns id
        $ abstractTransitionEvents
  where
    sim :: IOSim s ()
    sim = exploreRaces
       >> multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap


-- | Check connection manager counters in `multinodeExperiment`.
--
-- Note: this test validates connection manager counters using an upper bound
-- approach since there's no reliable way to reconstruct the value that the
-- counters should have at a given point in time. It's not quite possible to
-- have the god's view of the system consistent with the information that's
-- traced because there can be timing issues where a connection is in
-- TerminatingSt (hence counted as 0) but in the God's view the connection is
-- still being deleted (hence counted as 1). This is all due to the not having
-- a better way of injecting god's view traces in a way that the timing issues
-- aren't an issue.
--
prop_connection_manager_counters :: Int
                                 -> ArbDataFlow
                                 -> MultiNodeScript Int TestAddr
                                 -> Property
prop_connection_manager_counters serverAcc (ArbDataFlow dataFlow)
                                 (MultiNodeScript events
                                                  attenuationMap) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (ConnectionManagerTrace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

      -- Needed for calculating a more accurate upper bound
      networkEvents :: [ObservableNetworkState SimAddr]
      networkEvents = selectTraceEventsDynamic trace

      upperBound =
        multiNodeScriptToCounters dataFlow events networkEvents

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (concat
        [ "\n\n====== Say Events ======\n"
        , intercalate "\n" $ selectTraceEventsSay' trace
        , "\n"
        ])
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> AllProperty
                            $ counterexample (show v) (property False)
       )
       ( \ trs
        -> case trs of
          TrConnectionManagerCounters cmc ->
            AllProperty
              $ counterexample
                  ("Upper bound is: " ++ show upperBound
                  ++ "\n But got: " ++ show cmc)
                  (property $ collapseCounters False cmc
                           <= collapseCounters True upperBound)
          _                               ->
            mempty
       )
    $ connectionManagerEvents
  where
    serverAddress :: SimAddr
    serverAddress = TestAddress 0

    -- We count all connections as prunable because we do not have a better way
    -- to know what transitions will a given connection go through. We also
    -- count every client connection as unidirectional because they do not
    -- negotiate duplex data flow.
    --
    -- We do not count Inbound/Outbound/Closing of connections since the
    -- 'ObservableNetworkState' is a much more reliable source of information.
    --
    -- Note that this is only valid in the case of no attenuation.
    --
    multiNodeScriptToCounters :: DataFlow
                              -> [ConnectionEvent Int TestAddr]
                              -> [ObservableNetworkState SimAddr]
                              -> ConnectionManagerCounters
    multiNodeScriptToCounters df ces uss =
      let ifDuplex = bool 0 1 (df == Duplex)
          ifUni = bool 0 1 (df == Unidirectional)
          -- Every StartClient and StartServer will be associated to an Inbound
          -- or Outbound connection request to/from the main node. Since we do
          -- not know if it will perform both (making a duplex connection),
          -- connectionManagerCounters look at the StartClient and StartServer
          -- events and count the number of unidirectional/duplex/prunable
          -- connections assuming it reaches the last state of the state
          -- machine (DuplexSt). Then the ObservableNetworkState is used to make
          -- a more accurate guess of the actual upper bound of inbound/outbound
          -- connections.
          --
          -- TODO: we are computing upper bound of contribution of each
          -- address seprately.  This avoids tracking timing information of
          -- events, which is less acurate but it might be less fragile.  We
          -- should investiage if it's possible to make accurate and robust
          -- time series of counter changes.
          connectionManagerCounters =
              foldMap' id
            . foldl'
               (\ st ce -> case ce of
                 StartClient _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 0 1 0 0 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 OutboundConnection _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 ifDuplex ifUni 0 1 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 InboundConnection _ ta ->
                   Map.alter (let c = ConnectionManagerCounters 0 ifDuplex ifUni 1 0 in
                              maybe (Just c) (Just . maxCounters c))
                             ta st

                 _ -> st
               )
               Map.empty
            $ ces

          -- This calculation is right for the main node, because the simulation
          -- never attempts to make other connections that go to or from the
          -- main node.
          networkStateCounters = foldl'
                        (\cmc (ObservableNetworkState conns) ->
                          maxCounters cmc $
                          Map.foldl'
                           (\cmc' provenance ->
                             cmc' <>
                             if provenance == serverAddress
                                then ConnectionManagerCounters 0 0 0 0 1
                                else ConnectionManagerCounters 0 0 0 1 0
                           )
                           mempty
                           conns
                        )
                       mempty
                       uss
       in maxCounters connectionManagerCounters networkStateCounters

    maxCounters :: ConnectionManagerCounters
                -> ConnectionManagerCounters
                -> ConnectionManagerCounters
    maxCounters (ConnectionManagerCounters a b c d e)
                (ConnectionManagerCounters a' b' c' d' e') =
      ConnectionManagerCounters
        (max a a')
        (max b b')
        (max c c')
        (max d d')
        (max e e')

    -- It is possible for the ObservableNetworkState to have discrepancies between the
    -- counters traced by TrConnectionManagerCounters. This leads to different
    -- observations where an inbound connection can go into a state that is
    -- counted as outbound but the provenance in the Snocket's observable
    -- network state does not change so we can not know for sure what's
    -- happening inside the ConnectionManager's state machine.
    --
    -- Given this we collapse the count of incoming and outgoing connections
    -- since this value should always be the same in both views. Note that for
    -- values besides the upper bound we have to correct the sum by removing the
    -- duplicates.
    --
    -- TODO: Try idea in: ouroboros-network/pull/3429#discussion_r746406157
    --       See issue: #3509
    --
    -- TODO: test the number of full duplex connections.
    --
    collapseCounters :: Bool -- ^ Should we remove Duplex duplicate counters out
                             -- of the total sum.
                     -> ConnectionManagerCounters
                     -> (Int, Int, Int)
    collapseCounters t (ConnectionManagerCounters _ a b c d) =
      if t
         then (a, b, c + d)
         else (a, b, c + d - a)

    networkStateTracer getState =
      sayTracer
      <> (Tracer $ \_ -> do
      state <- getState
      traceM state)

    sim :: IOSim s ()
    sim = do
      mb <- timeout 7200
                    ( withSnocket nullTracer
                                  noAttenuation
                                  Map.empty
              $ \snocket getState ->
                multinodeExperiment (sayTracer <> Tracer traceM)
                                    (sayTracer <> Tracer traceM)
                                    (sayTracer <> Tracer traceM)
                                    (   sayTracer
                                     <> Tracer traceM
                                     <> networkStateTracer getState)
                                    snocket
                                    Snocket.TestFamily
                                    serverAddress
                                    serverAcc
                                    dataFlow
                                    maxAcceptedConnectionsLimit
                                    (MultiNodeScript
                                      (fmap unTestAddr <$> events)
                                      (Map.mapKeys unTestAddr attenuationMap)
                                    )
              )
      case mb of
        Nothing -> throwIO (SimulationTimeout :: ExperimentError SimAddr)
        Just a  -> return a


-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test verifies for for all \tau state we do not stay longer than
-- the designated timeout.
--
-- This test tests simultaneously the ConnectionManager and InboundGovernor's
-- timeouts.
--
prop_timeouts_enforced :: Int
                       -> ArbDataFlow
                       -> MultiNodeScript Int TestAddr
                       -> Property
prop_timeouts_enforced serverAcc (ArbDataFlow dataFlow)
                       (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      transitionSignal :: Trace (SimResult ()) [(Time, AbstractTransitionTrace SimAddr)]
      transitionSignal = fmap (map ((,) <$> wtTime <*> wtEvent))
                       . splitConns wtEvent
                       . withTimeNameTraceEvents
                       $ trace

  in counterexample (ppTrace trace)
   $ getAllProperty
   $ verifyAllTimeouts transitionSignal
  where
    verifyAllTimeouts :: Trace (SimResult ()) [(Time, AbstractTransitionTrace SimAddr)]
                      -> AllProperty
    verifyAllTimeouts =
      bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> AllProperty
                         $ counterexample (show v) (property False)
       )
       (\ tr ->
         AllProperty
         $ counterexample ("\nConnection transition trace:\n"
                         ++ intercalate "\n" (map show tr)
                         )
         $ verifyTimeouts Nothing tr)

    -- verifyTimeouts checks that in all \tau transition states the timeout is
    -- respected. It does so by checking the stream of abstract transitions
    -- paired with the time they happened, for a given connection; and checking
    -- that transitions from \tau states to any other happens withing the correct
    -- timeout bounds. One note is that for the example
    -- InboundIdleState^\tau -> OutboundState^\tau -> OutboundState sequence
    -- The first transition would be fine, but for the second we need the time
    -- when we transitioned into InboundIdleState and not OutboundState.
    --
    verifyTimeouts :: Maybe (AbstractState, Time)
                   -- ^ Map of first occurence of a given \tau state
                   -> [(Time , AbstractTransitionTrace SimAddr)]
                   -- ^ Stream of abstract transitions for a given connection
                   -- paired with the time it ocurred
                   -> Property
    verifyTimeouts state [] =
      counterexample
        ("This state didn't timeout:\n"
        ++ show state
        )
      $ isNothing state
    -- If we already seen a \tau transition state
    verifyTimeouts st@(Just (state, t')) ((t, TransitionTrace _ tt@(Transition _ to)):xs) =
        let newState  = Just (to, t)
            idleTimeout  =
                tProtocolIdleTimeout simTimeouts
              + (0.1 * tProtocolIdleTimeout simTimeouts)
            outboundTimeout =
                tOutboundIdleTimeout simTimeouts
              + (0.1 * tOutboundIdleTimeout simTimeouts)
            timeWaitTimeout =
                tTimeWaitTimeout simTimeouts
              + (0.1 * tTimeWaitTimeout simTimeouts)
            handshakeTimeout = case timeLimitsHandshake of
              (ProtocolTimeLimits stLimit) ->
                -- Should be the same but we bias to the shorter one
                let time = min (fromMaybe 0 (stLimit (ClientAgency TokPropose)))
                               (fromMaybe 0 (stLimit (ServerAgency TokConfirm)))
                 in time + (0.1 * time)

         in case state of
           UnnegotiatedSt _ -> case to of
             -- Timeout terminating states
             OutboundUniSt ->
               counterexample (errorMsg tt t' t handshakeTimeout)
               $ diffTime t t' <= handshakeTimeout
               .&&. verifyTimeouts Nothing xs
             InboundIdleSt Unidirectional ->
               counterexample (errorMsg tt t' t handshakeTimeout)
               $ diffTime t t' <= handshakeTimeout
               .&&. verifyTimeouts Nothing xs
             TerminatedSt ->
               counterexample (errorMsg tt t' t handshakeTimeout)
               $ diffTime t t' <= handshakeTimeout
               .&&. verifyTimeouts Nothing xs

             -- These states terminate the current timeout
             -- and starts a new one
             OutboundDupSt Ticking ->
               counterexample (errorMsg tt t' t handshakeTimeout)
               $ diffTime t t' <= handshakeTimeout
               .&&. verifyTimeouts newState xs
             InboundIdleSt Duplex ->
               counterexample (errorMsg tt t' t handshakeTimeout)
               $ diffTime t t' <= handshakeTimeout
               .&&. verifyTimeouts newState xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           InboundIdleSt Duplex         -> case to of
             -- Should preserve the timeout
             OutboundDupSt Ticking -> verifyTimeouts st xs
             InboundIdleSt Duplex -> verifyTimeouts st xs

             -- Timeout terminating states
             OutboundDupSt Expired ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs
             InboundSt Duplex ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs
             DuplexSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs
             TerminatedSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs

             -- This state terminates the current timeout
             -- and starts a new one
             TerminatingSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts newState xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           InboundIdleSt Unidirectional -> case to of
             -- Timeout terminating states
             InboundSt Unidirectional ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs
             TerminatedSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts Nothing xs

             -- This state terminates the current timeout
             -- and starts a new one
             TerminatingSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= idleTimeout
               .&&. verifyTimeouts newState xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           OutboundDupSt Ticking        -> case to of
             -- Should preserve the timeout
             InboundIdleSt Duplex -> verifyTimeouts st xs
             OutboundDupSt Ticking -> verifyTimeouts st xs

             -- Timeout terminating states
             OutboundDupSt Expired ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs
             DuplexSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs
             InboundSt Duplex ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs
             TerminatedSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs

             -- This state terminates the current timeout
             -- and starts a new one
             TerminatingSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts newState xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           OutboundIdleSt _             -> case to of
             -- Timeout terminating states
             InboundSt Duplex ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs
             TerminatedSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts Nothing xs

             -- This state terminates the current timeout
             -- and starts a new one
             TerminatingSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= outboundTimeout
               .&&. verifyTimeouts newState xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           TerminatingSt                -> case to of
             -- Timeout terminating states
             UnnegotiatedSt Inbound ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= timeWaitTimeout
               .&&. verifyTimeouts Nothing xs

             TerminatedSt ->
               counterexample (errorMsg tt t' t idleTimeout)
               $ diffTime t t' <= timeWaitTimeout
               .&&. verifyTimeouts Nothing xs

             _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

           _ -> error ("Should be a \tau state: " ++ show st)
      where
        errorMsg trans time' time maxDiffTime =
          "\nAt transition: " ++ show trans ++ "\n"
          ++ "First happened at: " ++ show time' ++ "\n"
          ++ "Second happened at: " ++ show time ++ "\n"
          ++ "Should only take: "
          ++ show maxDiffTime
          ++ ", but took:" ++ show (diffTime time time')
    -- If we haven't seen a \tau transition state
    verifyTimeouts Nothing ((t, TransitionTrace _ (Transition _ to)):xs) =
        let newState = Just (to, t)
         in case to of
           InboundIdleSt _       -> verifyTimeouts newState xs
           OutboundDupSt Ticking -> verifyTimeouts newState xs
           OutboundIdleSt _      -> verifyTimeouts newState xs
           TerminatingSt         -> verifyTimeouts newState xs
           _                     -> verifyTimeouts Nothing xs

    tracerWithTime :: MonadMonotonicTime m => Tracer m (WithTime a) -> Tracer m a
    tracerWithTime = contramapM $ \a -> flip WithTime a <$> getMonotonicTime

    sim :: IOSim s ()
    sim = multiNodeSimTracer serverAcc dataFlow
                             absNoAttenuation
                             maxAcceptedConnectionsLimit
                             events
                             attenuationMap
                             (Tracer traceM <> sayTracer)
                             ( tracerWithTime (Tracer traceM)
                             <> Tracer traceM
                             <> sayTracer
                             )
                             (Tracer traceM <> sayTracer)
                             (Tracer traceM <> sayTracer)

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
                                        defaultBearerInfo
                                        mns@(MultiNodeScript
                                                  events
                                                  attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: Trace (SimResult ())
                                           (RemoteTransitionTrace SimAddr)
      remoteTransitionTraceEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show remoteTransitionTraceEvents)
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
    $ remoteTransitionTraceEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

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
                                           defaultBearerInfo
                                           mns@(MultiNodeScript
                                                    events
                                                    attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ())
                                     (InboundGovernorTrace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
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
    $ inboundGovernorEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

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
                                        defaultBearerInfo
                                        mns@(MultiNodeScript events
                                                             attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (intercalate "\n"
                     [ "========== Script =========="
                     , ppScript mns
                     , "========== Inbound Governor Events =========="
                     , Trace.ppTrace show show inboundGovernorEvents
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
    $ inboundGovernorEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Property wrapping `multinodeExperiment`.
--
-- Note: this test coverage of inbound governor state transitions.
-- TODO: Fix transitions that are not covered. #3516
prop_inbound_governor_transitions_coverage :: Int
                                           -> ArbDataFlow
                                           -> AbsBearerInfo
                                           -> MultiNodeScript Int TestAddr
                                           -> Property
prop_inbound_governor_transitions_coverage serverAcc
  (ArbDataFlow dataFlow)
  defaultBearerInfo
  (MultiNodeScript events attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: [RemoteTransitionTrace SimAddr]
      remoteTransitionTraceEvents = withNameTraceEvents trace

      transitionsSeen = nub [ tran
                            | TransitionTrace _ tran
                                <- remoteTransitionTraceEvents]
      transitionsSeenNames = map (snd . validRemoteTransitionMap)
                                 transitionsSeen

   in coverTable "valid transitions"
                  [ (n, 0.01) | n <- allValidRemoteTransitionsNames ] $
      tabulate   "valid transitions" transitionsSeenNames
      True
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

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
                                             defaultBearerInfo
                                             mns@(MultiNodeScript
                                                      events
                                                      attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      remoteTransitionTraceEvents = traceWithNameTraceEvents trace

      inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show remoteTransitionTraceEvents)
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           _             -> AllProperty (property False)
       )
       verifyRemoteTransitionOrder
    . splitRemoteConns
    $ remoteTransitionTraceEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       defaultBearerInfo
                       maxAcceptedConnectionsLimit
                       events
                       attenuationMap

-- | Check inbound governor counters in `multinodeExperiment`.
--
-- Note: this test validates warm and hot inbound governor counters only.
--
prop_inbound_governor_counters :: Int
                               -> ArbDataFlow
                               -> MultiNodeScript Int TestAddr
                               -> Property
prop_inbound_governor_counters serverAcc (ArbDataFlow dataFlow)
                               mns@(MultiNodeScript
                                        events
                                        attenuationMap) =
  let trace = runSimTrace sim

      inboundGovernorEvents :: Trace (SimResult ())
                                     (InboundGovernorTrace
                                       SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

      upperBound = multiNodeScriptToCounters events

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript mns)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . getAllProperty
    . bifoldMap
       ( \ case
           MainReturn {} -> mempty
           v             -> AllProperty
                         $ counterexample (show v) (property False)
       )
       ( \ trs
        -> case trs of
          TrInboundGovernorCounters igc ->
            AllProperty
              $ counterexample
                  ("Upper bound is: " ++ show upperBound
                  ++ "\n But got: " ++ show igc)
                  (    warmPeersRemote igc <= warmPeersRemote upperBound
                  .&&. hotPeersRemote  igc <= hotPeersRemote  upperBound
                  )
          _                               ->
            mempty
       )
    $ inboundGovernorEvents
  where
    -- Note that this is only valid in the case of no attenuation.
    bundleToCounters :: Bundle [Int] -> InboundGovernorCounters
    bundleToCounters (Bundle hot warm _) =
      let warmRemote = bool 1 0 (null warm)
          hotRemote  = bool 1 0 (null hot)
       in InboundGovernorCounters 0 0 warmRemote hotRemote

    -- We check for starting of miniprotocols that can potentially lead to
    -- inbound governor states of remote warm or remote hot connections. An
    -- upper bound is established because it is not possible to predict whether
    -- some failure will occur.
    multiNodeScriptToCounters :: [ConnectionEvent Int TestAddr]
                              -> InboundGovernorCounters
    multiNodeScriptToCounters =
      let taServerAcc = TestAddr (TestAddress 0)
       in
        (\x ->
          let serverAccEntry = x Map.! taServerAcc
              mapWithoutServerAcc = Map.delete taServerAcc x
           in Map.foldlWithKey'
                (\igt ta entry ->
                  case Map.lookup ta serverAccEntry of
                    Nothing  -> igt <> foldMap' bundleToCounters entry
                    Just bun -> igt <> foldMap' bundleToCounters entry
                                    <> bundleToCounters bun
                )
                mempty
                mapWithoutServerAcc

        )
        . foldl'
          (\ st ce -> case ce of
            StartClient _ ta ->
              Map.insertWith (<>) ta Map.empty st
            StartServer _ ta _ ->
              Map.insertWith (<>) ta Map.empty st
            InboundConnection _ ta ->
              Map.update (Just . Map.insertWith (<>) taServerAcc mempty) ta st
            OutboundConnection _ ta ->
              Map.update (Just . Map.insertWith (<>) ta mempty) taServerAcc st
            InboundMiniprotocols _ ta bun ->
              Map.update (Just . Map.update (Just . (<> bun)) taServerAcc) ta st
            OutboundMiniprotocols _ ta bun ->
              Map.update (Just . Map.update (Just . (<> bun)) ta) taServerAcc st
            _ -> st
          )
          (Map.singleton taServerAcc Map.empty)

    sim :: IOSim s ()
    sim = multiNodeSim serverAcc dataFlow
                       absNoAttenuation
                       maxAcceptedConnectionsLimit
                       events
                       (toNonFailing <$> attenuationMap)

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in Connection
-- Manager.
--
prop_connection_manager_pruning :: Int
                                -> MultiNodePruningScript Int
                                -> Property
prop_connection_manager_pruning serverAcc
                                (MultiNodePruningScript
                                  acceptedConnLimit
                                  events
                                  attenuationMap) =
  let trace = runSimTrace sim

      abstractTransitionEvents :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      abstractTransitionEvents = traceWithNameTraceEvents trace

      connectionManagerEvents :: [ConnectionManagerTrace
                                    SimAddr
                                    (ConnectionHandlerTrace
                                      UnversionedProtocol
                                      DataFlowProtocolData)]
      connectionManagerEvents = withNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (ppScript (MultiNodeScript events attenuationMap))
    . counterexample (Trace.ppTrace show show abstractTransitionEvents)
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
             tpNumberOfPrunings    = classifyPrunings connectionManagerEvents,
             tpNegotiatedDataFlows = [classifyNegotiatedDataFlow trs],
             tpEffectiveDataFlows  = [classifyEffectiveDataFlow  trs],
             tpTerminationTypes    = [classifyTermination        trs],
             tpActivityTypes       = [classifyActivityType       trs],
             tpTransitions         = trs
          }
       )
    . fmap (map ttTransition)
    . splitConns id
    $ abstractTransitionEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- This test tests if with a higher chance of pruning happening and a smaller number of
-- connections hard limit we do not end up triggering any illegal transition in the
-- Inbound Governor.
--
prop_inbound_governor_pruning :: Int
                              -> MultiNodePruningScript Int
                              -> Property
prop_inbound_governor_pruning serverAcc
                              (MultiNodePruningScript
                                acceptedConnLimit
                                events
                                attenuationMap) =
  let trace = runSimTrace sim

      remoteTransitionTraceEvents :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
      remoteTransitionTraceEvents = traceWithNameTraceEvents trace

      inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (Trace.ppTrace show show remoteTransitionTraceEvents)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
    . counterexample (ppTrace trace)
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
   $ (remoteTransitionTraceEvents, inboundGovernorEvents)
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)

-- | Property wrapping `multinodeExperiment` that has a generator optimized for triggering
-- pruning, and random generated number of connections hard limit.
--
-- We test that:
--
-- * we never go above hard limit of incoming connections;
-- * the pruning set is at least as big as expected, and that
--   the picked peers belong to the choice set.
--
prop_never_above_hardlimit :: Int
                           -> MultiNodePruningScript Int
                           -> Property
prop_never_above_hardlimit serverAcc
                           (MultiNodePruningScript
                             acceptedConnLimit@AcceptedConnectionsLimit
                               { acceptedConnectionsHardLimit = hardlimit }
                             events
                             attenuationMap
                           ) =
  let trace = runSimTrace sim

      connectionManagerEvents :: Trace (SimResult ())
                                       (ConnectionManagerTrace
                                         SimAddr
                                         (ConnectionHandlerTrace
                                           UnversionedProtocol
                                           DataFlowProtocolData))
      connectionManagerEvents = traceWithNameTraceEvents trace

      abstractTransitionEvents :: Trace (SimResult ()) (AbstractTransitionTrace SimAddr)
      abstractTransitionEvents = traceWithNameTraceEvents trace

      inboundGovernorEvents :: Trace (SimResult ()) (InboundGovernorTrace SimAddr)
      inboundGovernorEvents = traceWithNameTraceEvents trace

  in tabulate "ConnectionEvents" (map showConnectionEvents events)
    . counterexample (Trace.ppTrace show show connectionManagerEvents)
    . counterexample (Trace.ppTrace show show abstractTransitionEvents)
    . counterexample (Trace.ppTrace show show inboundGovernorEvents)
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
                                      ", but got: " ++ show (inboundConns cmc) ++
                                      " inbound connections!\n" ++
                                      show cmc
                                     )
                    . property
                    $ inboundConns cmc <= fromIntegral hardlimit
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
   $ connectionManagerEvents
  where
    sim :: IOSim s ()
    sim = multiNodeSim serverAcc Duplex
                       absNoAttenuation
                       acceptedConnLimit
                       events
                       (toNonFailing <$> attenuationMap)


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
                      bearerAttenuation
                      Map.empty
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




multiNodeSimTracer :: ( Monad m, MonadFix m, MonadTimer m, MonadLabelledSTM m
                      , MonadTraceSTM m, MonadMask m, MonadTime m
                      , MonadThrow (STM m), MonadSay m, MonadAsync m
                      , MonadEvaluate m, MonadFork m, MonadST m
                      , Serialise req, Show req, Eq req, Typeable req
                      )
                   => req
                   -> DataFlow
                   -> AbsBearerInfo
                   -> AcceptedConnectionsLimit
                   -> [ConnectionEvent req TestAddr]
                   -> Map TestAddr (Script AbsBearerInfo)
                   -> Tracer m
                      (WithName (Name SimAddr) (RemoteTransitionTrace SimAddr))
                   -> Tracer m
                      (WithName (Name SimAddr) (AbstractTransitionTrace SimAddr))
                   -> Tracer m
                      (WithName (Name SimAddr) (InboundGovernorTrace SimAddr))
                   -> Tracer m
                      (WithName
                       (Name SimAddr)
                        (ConnectionManagerTrace
                         SimAddr
                          (ConnectionHandlerTrace
                            UnversionedProtocol DataFlowProtocolData)))
                   -> m ()
multiNodeSimTracer serverAcc dataFlow defaultBearerInfo
                   acceptedConnLimit events attenuationMap
                   remoteTrTracer abstractTrTracer
                   inboundGovTracer connMgrTracer = do

      let attenuationMap' = (fmap toBearerInfo <$>)
                          . Map.mapKeys ( normaliseId
                                        . ConnectionId mainServerAddr
                                        . unTestAddr)
                          $ attenuationMap

      mb <- timeout 7200
                    ( withSnocket (Tracer (say . show))
                                  (toBearerInfo defaultBearerInfo)
                                  attenuationMap'
              $ \snocket _ ->
                 multinodeExperiment remoteTrTracer
                                     abstractTrTracer
                                     inboundGovTracer
                                     connMgrTracer
                                     snocket
                                     Snocket.TestFamily
                                     mainServerAddr
                                     serverAcc
                                     dataFlow
                                     acceptedConnLimit
                                     (MultiNodeScript
                                       ((unTestAddr <$>) <$> events)
                                       (Map.mapKeys unTestAddr attenuationMap)
                                     )
              )
      case mb of
        Nothing -> throwIO (SimulationTimeout :: ExperimentError SimAddr)
        Just a  -> return a
  where
    mainServerAddr :: SimAddr
    mainServerAddr = Snocket.TestAddress 0


multiNodeSim :: (Serialise req, Show req, Eq req, Typeable req)
             => req
             -> DataFlow
             -> AbsBearerInfo
             -> AcceptedConnectionsLimit
             -> [ConnectionEvent req TestAddr]
             -> Map TestAddr (Script AbsBearerInfo)
             -> IOSim s ()
multiNodeSim serverAcc dataFlow defaultBearerInfo
                   acceptedConnLimit events attenuationMap = do
  let dynamicTracer :: (Typeable a, Show a) => Tracer (IOSim s) a
      dynamicTracer = Tracer traceM <> sayTracer

  multiNodeSimTracer serverAcc dataFlow defaultBearerInfo acceptedConnLimit
                     events attenuationMap dynamicTracer dynamicTracer
                     dynamicTracer dynamicTracer


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
         Map.empty
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


-- | Split 'AbstractTransitionTrace' into seprate connections.  This relies on
-- the property that every connection is terminated with 'UnknownConnectionSt'.
-- This property is verified by 'verifyAbstractTransitionOrder'.
--
splitConns :: (a -> AbstractTransitionTrace SimAddr)
           -> Trace (SimResult ()) a
           -> Trace (SimResult ()) [a]
splitConns getTransition =
    fmap fromJust
  . Trace.filter isJust
  -- there might be some connections in the state, push them onto the 'Trace'
  . (\(s, o) -> foldr (\a as -> Trace.Cons (Just a) as) o (Map.elems s))
  . bimapAccumL
      ( \ s a -> (s, a))
      ( \ s a ->
          let TransitionTrace { ttPeerAddr, ttTransition } = getTransition a
           in case ttTransition of
             Transition _ UnknownConnectionSt ->
               case ttPeerAddr `Map.lookup` s of
                 Nothing  -> ( Map.insert ttPeerAddr [a] s
                             , Nothing
                             )
                 Just trs -> ( Map.delete ttPeerAddr s
                             , Just (reverse $ a : trs)
                             )
             _ ->            ( Map.alter ( \ case
                                               Nothing -> Just [a]
                                               Just as -> Just (a : as)
                                         ) ttPeerAddr s
                             , Nothing
                             )
      )
      Map.empty

splitRemoteConns :: Trace (SimResult ()) (RemoteTransitionTrace SimAddr)
                 -> Trace (SimResult ()) [RemoteTransition]
splitRemoteConns =
    fmap fromJust
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
ppScript (MultiNodeScript script _) = intercalate "\n" $ go 0 script
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


toNonFailing :: Script AbsBearerInfo -> Script AbsBearerInfo
toNonFailing = unNFBIScript
             . toNonFailingAbsBearerInfoScript
             . AbsBearerInfoScript

data WithName name event = WithName {
    wnName  :: name,
    wnEvent :: event
  }
  deriving (Show, Functor)

data WithTime event = WithTime {
  wtTime  :: Time,
  wtEvent :: event
  }
  deriving (Show, Functor)

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

withTimeNameTraceEvents :: forall b. Typeable b => SimTrace ()
                        -> Trace (SimResult ()) (WithTime b)
withTimeNameTraceEvents = fmap (\(WithTime t (WithName _ e)) -> WithTime t e)
          . Trace.filter ((MainServer ==) . wnName . wtEvent)
          . traceSelectTraceEventsDynamic
              @()
              @(WithTime (WithName (Name SimAddr) b))

sayTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
sayTracer = Tracer $
  \msg -> (,msg) <$> getCurrentTime >>= say . show


showConnectionEvents :: ConnectionEvent req peerAddr -> String
showConnectionEvents (StartClient{})             = "StartClient"
showConnectionEvents (StartServer{})             = "StartServer"
showConnectionEvents (InboundConnection{})       = "InboundConnection"
showConnectionEvents (OutboundConnection{})      = "OutboundConnection"
showConnectionEvents (InboundMiniprotocols{})    = "InboundMiniprotocols"
showConnectionEvents (OutboundMiniprotocols{})   = "OutboundMiniprotocols"
showConnectionEvents (CloseInboundConnection{})  = "CloseInboundConnection"
showConnectionEvents (CloseOutboundConnection{}) = "CloseOutboundConnection"
showConnectionEvents (ShutdownClientServer{})    = "ShutdownClientServer"


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
