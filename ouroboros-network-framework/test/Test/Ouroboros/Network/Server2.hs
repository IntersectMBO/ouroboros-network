{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- just to use 'debugTracer'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Ouroboros.Network.Server2
  ( tests
  ) where

import           Control.Exception (AssertionFailed)
import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST    (MonadST)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, nullTracer)

import           Codec.Serialise.Class (Serialise)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>), (<&>))
import           Data.List (mapAccumL, intercalate, (\\), tails, delete)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Text.Printf

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import           Control.Concurrent.JobPool

import qualified Network.Mux as Mux
import           Network.Mux.Types (MuxRuntimeError)
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Core

import           Network.TypedProtocol.ReqResp.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Examples

import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.IOManager
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as Server
import           Ouroboros.Network.Mux
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec ( cborTermVersionDataCodec
                                                            , noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..))
import           Ouroboros.Network.RethrowPolicy
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Server2 (ServerArguments (..))
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Snocket (Snocket, TestAddress (..), socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Simulation.Network.Snocket

import           Ouroboros.Network.Testing.Utils (genDelayWithPrecision)
import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance
import           Test.Simulation.Network.Snocket (NonFailingBearerInfoScript(..), toBearerInfo)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Server2"
  [ testProperty "unidirectional_IO"  prop_unidirectional_IO
  , testProperty "unidirectional_Sim" prop_unidirectional_Sim
  , testProperty "bidirectional_IO"   prop_bidirectional_IO
  , testProperty "bidirectional_Sim"  prop_bidirectional_Sim
  -- This test fails now with:
  -- > NotReleasedListeningSockets [TestAddress 0] Nothing
  -- which is likely due to rebasing.  This is fixed a few commits later. 
  --, testProperty "multinode_Sim"      prop_multinode_Sim
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
    :: forall peerAddr socket req resp m a.
       ( ConnectionManagerMonad m

       , resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req
       )
    => String
    -- ^ identifier (for logging)
    -> Timeouts
    -> Snocket m socket peerAddr
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> Maybe peerAddr
    -> Bundle (ConnectionId peerAddr -> STM m [req])
    -- ^ Functions to get the next requests for a given connection
    -> (MuxConnectionManager
          InitiatorMode socket peerAddr
          UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager name timeouts snocket localAddr nextRequests k = do
    mainThreadId <- myThreadId
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer
    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = (name,)
                        `contramap` nullTracer,
          cmTrTracer  = ((name,) . fmap abstractState)
                        `contramap` nullTracer,
         -- MuxTracer
          cmMuxTracer = muxTracer,
          cmIPv4Address = localAddr,
          cmIPv6Address = Nothing,
          cmAddressType = \_ -> Just IPv4Address,
          cmSnocket = snocket,
          connectionDataFlow = const Duplex,
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
            haVersionDataCodec = cborTermVersionDataCodec unversionedProtocolDataCodec,
            haAcceptVersion = acceptableVersion,
            haTimeLimits = noTimeLimitsHandshake
          }
        (unversionedProtocol clientApplication)
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
        (MuxPeer
          ((name,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
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
          MuxIOException _ -> ShutdownPeer
          MuxBearerClosed  -> ShutdownPeer
          _                -> ShutdownNode

debugMuxRuntimeErrorRethrowPolicy :: RethrowPolicy
debugMuxRuntimeErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: MuxRuntimeError) -> ShutdownNode

debugIOErrorRethrowPolicy :: RethrowPolicy
debugIOErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: IOError) -> ShutdownNode


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
    :: forall peerAddr socket acc req resp m a.
       ( ConnectionManagerMonad m

       , acc ~ [req], resp ~ [req]
       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Typeable req

       -- debugging
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req
       )
    => String
    -> Timeouts
    -- ^ identifier (for logging)
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
    -> (MuxConnectionManager
          InitiatorResponderMode socket peerAddr
          UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> Async m Void
       -> m a)
    -> m a
withBidirectionalConnectionManager name timeouts snocket socket localAddress
                                   accumulatorInit nextRequests k = do
    mainThreadId <- myThreadId
    inbgovControlChannel      <- Server.newControlChannel
    -- we are not using the randomness
    observableStateVar        <- Server.newObservableStateVarFromSeed 0
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer    = (name,)
                        `contramap` nullTracer,
          cmTrTracer  = ((name,) . fmap abstractState)
                        `contramap` nullTracer,
          -- MuxTracer
          cmMuxTracer    = muxTracer,
          cmIPv4Address  = localAddress,
          cmIPv6Address  = Nothing,
          cmAddressType  = \_ -> Just IPv4Address,
          cmSnocket      = snocket,
          cmTimeWaitTimeout = tTimeWaitTimeout timeouts,
          cmOutboundIdleTimeout = tOutboundIdleTimeout timeouts,
          connectionDataFlow = const Duplex,
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
              haHandshakeTracer = (name,) `contramap` nullTracer,
              haHandshakeCodec = unversionedHandshakeCodec,
              haVersionDataCodec = cborTermVersionDataCodec unversionedProtocolDataCodec,
              haAcceptVersion = acceptableVersion,
              haTimeLimits = noTimeLimitsHandshake
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
                    serverTracer = (name,) `contramap` nullTracer, -- ServerTrace
                    serverInboundGovernorTracer = (name,) `contramap` nullTracer, -- InboundGovernorTrace
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
            say (show e)
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
        (MuxPeer
          ((name,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
          (Effect $ do
            reqs <- atomically nextRequest
            pure $ reqRespClientPeer (reqRespClientMap reqs)))
        (MuxPeer
          ((name,"Responder",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
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
    (Bundle (WithHot  [hotPtcl])
            (WithWarm [warmPtcl])
            (WithEstablished [establishedPtcl])) = do
      -- start all protocols
      hotSTM <- runInitiator hotPtcl
      warmSTM <- runInitiator warmPtcl
      establishedSTM <- runInitiator establishedPtcl

      -- await for their termination
      hotRes <- atomically hotSTM
      warmRes <- atomically warmSTM
      establishedRes <- atomically establishedSTM
      case (hotRes, warmRes, establishedRes) of
        (Left err, _, _) -> throwIO err
        (_, Left err, _) -> throwIO err
        (_, _, Left err) -> throwIO err
        (Right hot, Right warm, Right established) ->
          pure $ Bundle (WithHot hot) (WithWarm warm) (WithEstablished established)
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

runInitiatorProtocols _singMuxMode _mux (Bundle {}) =
    error "runInitiatorProtocols: unsupported"


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
      "client" timeouts snocket Nothing nextReqs
      $ \connectionManager ->
        withBidirectionalConnectionManager "server" timeouts
                                           snocket socket Nothing
                                           [accumulatorInit clientAndServerData]
                                           noNextRequests
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

prop_unidirectional_Sim :: ClientAndServerData Int -> Property
prop_unidirectional_Sim clientAndServerData =
  simulatedPropertyWithTimeout 7200 $
    withSnocket nullTracer
                (singletonScript noAttenuation)
                (TestAddress 10) $ \snock ->
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
                                         snocket socket0
                                         (Just localAddr0)
                                         [accumulatorInit clientAndServerData0]
                                         nextRequests0
        (\connectionManager0 _serverAddr0 _serverAsync0 ->
          withBidirectionalConnectionManager "node-1" timeouts
                                             snocket socket1
                                             (Just localAddr1)
                                             [accumulatorInit clientAndServerData1]
                                             nextRequests1
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
    withSnocket debugTracer
                script'
                (TestAddress 10) $ \snock ->
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
  deriving (Show, Functor)

-- | A sequence of connection events that make up a test scenario for `prop_multinode_Sim`.
newtype MultiNodeScript req peerAddr = MultiNodeScript [ConnectionEvent req peerAddr]
  deriving (Show, Functor)

-- | To generate well-formed scripts we need to keep track of what nodes are started and what
--   connections they've made.
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
      NonNegative len <- scale (`div` 2) arbitrary
      MultiNodeScript <$> go (ScriptState [] [] [] [] []) (len :: Integer)
    where     -- Divide delays by 100 to avoid running in to protocol and SDU timeouts if waiting
              -- too long between connections and mini protocols.
      delay = frequency [(1, pure 0), (3, (/ 100) <$> genDelayWithPrecision 2)]
      go _ 0 = pure []
      go s@ScriptState{..} n = do
        event <- frequency $
                    [ (1, StartClient             <$> delay <*> newClient)
                    , (1, StartServer             <$> delay <*> newServer <*> arbitrary) ] ++
                    [ (4, InboundConnection       <$> delay <*> elements possibleInboundConnections)  | not $ null possibleInboundConnections] ++
                    [ (4, OutboundConnection      <$> delay <*> elements possibleOutboundConnections) | not $ null possibleOutboundConnections] ++
                    [ (4, CloseInboundConnection  <$> delay <*> elements inboundConnections)  | not $ null $ inboundConnections ] ++
                    [ (4, CloseOutboundConnection <$> delay <*> elements outboundConnections) | not $ null $ outboundConnections ] ++
                    [ (16, InboundMiniprotocols   <$> delay <*> elements inboundConnections  <*> genBundle) | not $ null inboundConnections ] ++
                    [ (16, OutboundMiniprotocols  <$> delay <*> elements outboundConnections <*> genBundle) | not $ null outboundConnections ]
        (event :) <$> go (nextState event s) (n - 1)
        where
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

-- | We use a wrapper for test addresses since the Arbitrary instance for Snocket.TestAddress only
--   generates addresses between 1 and 4.
newtype TestAddr = TestAddr { unTestAddr :: Snocket.TestAddress Int }
  deriving (Show, Eq, Ord)

instance Arbitrary TestAddr where
  arbitrary = TestAddr . Snocket.TestAddress <$> choose (1, 100)

-- | Each node in the multi-node experiment is controlled by a thread responding to these messages.
data ConnectionHandlerMessage peerAddr req
  = NewConnection peerAddr [req]
    -- ^ Connect to the server at the given address. Needs to know the `accumulatorInit` of the
    --   server in order to validate the responses.
  | Disconnect peerAddr
    -- ^ Disconnect from the server at the given address.
  | RunMiniProtocols peerAddr (Bundle [req])
    -- ^ Run a bundle of mini protocols against the server at the given address (requires an active
    --   connection).

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
    => Snocket m socket peerAddr
    -> Snocket.AddressFamily peerAddr
    -> peerAddr
    -> req
    -> MultiNodeScript req peerAddr
    -> m Property
multinodeExperiment snocket addrFamily serverAddr accInit (MultiNodeScript script) =
  withJobPool $ \jobpool -> do
  -- Avoid parallel connections. This can cause one side to think that the existing connection
  -- should be used and the other side thinking that there should be two separate connections,
  -- causing the latter to wait on messages that never come.
  lock <- newTMVarIO ()
  labelTMVarIO lock "lock"
  -- TVar keeping the resulting property. Connection handler threads update this after each
  -- mini-protocol run.
  propVar <- newTVarIO (property True)
  labelTVarIO propVar "propVar"
  cc <- startServerConnectionHandler "main-server" [accInit] serverAddr lock propVar jobpool
  loop lock (Map.singleton serverAddr [accInit]) (Map.singleton serverAddr cc) propVar script jobpool
  where

    loop :: StrictTMVar m ()
         -> Map.Map peerAddr acc
         -> Map.Map peerAddr (TQueue m (ConnectionHandlerMessage peerAddr req))
         -> StrictTVar m Property
         -> [ConnectionEvent req peerAddr]
         -> JobPool () m (Maybe SomeException)
         -> m Property
    loop _ _ _ propVar [] _ = do
      threadDelay 3600
      atomically $ readTVar propVar
    loop lock nodeAccs servers propVar (event : events) jobpool =
      case event of

        StartClient delay localAddr -> do
          threadDelay delay
          cc <- startClientConnectionHandler ("client-" ++ show localAddr) localAddr lock propVar jobpool
          loop lock nodeAccs (Map.insert localAddr cc servers) propVar events jobpool

        StartServer delay localAddr nodeAcc -> do
          threadDelay delay
          cc <- startServerConnectionHandler ("node-" ++ show localAddr) [nodeAcc] localAddr lock propVar jobpool
          loop lock (Map.insert localAddr [nodeAcc] nodeAccs) (Map.insert localAddr cc servers) propVar events jobpool

        InboundConnection delay nodeAddr -> do
          threadDelay delay
          acc <- getAcc serverAddr
          sendMsg nodeAddr $ NewConnection serverAddr acc
          loop lock nodeAccs servers propVar events jobpool

        OutboundConnection delay nodeAddr -> do
          threadDelay delay
          acc <- getAcc nodeAddr
          sendMsg serverAddr $ NewConnection nodeAddr acc
          loop lock nodeAccs servers propVar events jobpool

        CloseInboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg remoteAddr $ Disconnect serverAddr
          loop lock nodeAccs servers propVar events jobpool

        CloseOutboundConnection delay remoteAddr -> do
          threadDelay delay
          sendMsg serverAddr $ Disconnect remoteAddr
          loop lock nodeAccs servers propVar events jobpool

        InboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg nodeAddr $ RunMiniProtocols serverAddr reqs
          loop lock nodeAccs servers propVar events jobpool

        OutboundMiniprotocols delay nodeAddr reqs -> do
          threadDelay delay
          sendMsg serverAddr $ RunMiniProtocols nodeAddr reqs
          loop lock nodeAccs servers propVar events jobpool
      where
        sendMsg :: peerAddr -> ConnectionHandlerMessage peerAddr req -> m ()
        sendMsg addr msg = atomically $
          case Map.lookup addr servers of
            Nothing -> assertProperty propVar $ counterexample (show addr ++ " is not a started node") False
            Just cc -> writeTQueue cc msg

        getAcc :: peerAddr -> m acc
        getAcc addr =
          case Map.lookup addr nodeAccs of
            Nothing  -> do
              assertPropertyIO propVar $ counterexample (show addr ++ " is not a started server node") False
              return []
            Just acc -> return acc

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

    assertPropertyIO :: StrictTVar m Property -> Property -> m ()
    assertPropertyIO propVar p = atomically $ assertProperty propVar p

    assertProperty :: StrictTVar m Property -> Property -> STM m ()
    assertProperty propVar p = modifyTVar propVar (.&&. p)

    startClientConnectionHandler :: String -> peerAddr
                                 -> StrictTMVar m ()
                                 -> StrictTVar m Property
                                 -> JobPool () m (Maybe SomeException)
                                 -> m (TQueue m (ConnectionHandlerMessage peerAddr req))
    startClientConnectionHandler name localAddr lock propVar jobpool = do
        cc      <- atomically $ newTQueue
        labelTQueueIO cc $ "cc/" ++ name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ name
        threadId <- myThreadId
        forkJob jobpool
          $ Job
              ( withInitiatorOnlyConnectionManager
                    name simTimeouts snocket (Just localAddr) (mkNextRequests connVar)
                  ( \ connectionManager -> do
                    connectionLoop SingInitiatorMode localAddr lock propVar cc connectionManager Map.empty connVar
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
              name
        return cc

    startServerConnectionHandler :: String -> acc -> peerAddr
                                 -> StrictTMVar m ()
                                 -> StrictTVar m Property
                                 -> JobPool () m (Maybe SomeException)
                                 -> m (TQueue m (ConnectionHandlerMessage peerAddr req))
    startServerConnectionHandler name serverAcc localAddr lock propVar jobpool = do
        fd <- Snocket.open snocket addrFamily
        Snocket.bind   snocket fd localAddr
        Snocket.listen snocket fd
        cc      <- atomically $ newTQueue
        labelTQueueIO cc $ "cc/" ++ name
        connVar <- newTVarIO Map.empty
        labelTVarIO connVar $ "connVar/" ++ name
        threadId <- myThreadId
        forkJob jobpool
              $ Job
                  (  withBidirectionalConnectionManager
                          name simTimeouts snocket fd (Just localAddr) serverAcc
                          (mkNextRequests connVar)
                          (\ connectionManager _ _serverAsync -> do
                             connectionLoop SingInitiatorResponderMode localAddr lock propVar cc connectionManager Map.empty connVar
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
                  name
        return cc

    connectionLoop
         :: (HasInitiator muxMode ~ True)
         => SingMuxMode muxMode
         -> peerAddr
         -> StrictTMVar m ()
         -> StrictTVar m Property
         -> TQueue m (ConnectionHandlerMessage peerAddr req)                          -- control channel
         -> MuxConnectionManager muxMode socket peerAddr UnversionedProtocol ByteString m [resp] a
         -> Map.Map peerAddr (Handle muxMode peerAddr ByteString m [resp] a, acc)     -- active connections
         -> StrictTVar m (Map.Map (ConnectionId peerAddr) (Bundle (TQueue m [req])))  -- mini protocol queues
         -> m ()
    connectionLoop muxMode localAddr lock propVar cc cm connMap connVar = atomically (readTQueue cc) >>= \ case
      NewConnection remoteAddr remoteAcc -> do
        let mkQueue :: forall pt. TokProtocolTemperature pt -> STM m (TQueue m [req])
            mkQueue tok = do
              q <- newTQueue
              let temp = case tok of
                    TokHot         -> "hot"
                    TokWarm        -> "warm"
                    TokEstablished -> "cold"
              q <$ labelTQueue q ("protoVar." ++ temp ++ "@" ++ show localAddr)
        qs <- atomically $ traverse id $ makeBundle mkQueue
        atomically $ modifyTVar connVar $ Map.insert (connId remoteAddr) qs
        connHandle <- withLock False lock $ requestOutboundConnection cm remoteAddr
        case connHandle of
          Connected _ _ h -> do
            connectionLoop muxMode localAddr lock propVar cc cm (Map.insert remoteAddr (h, remoteAcc) connMap) connVar
          Disconnected _ err ->
            failureIO $ "connection failure: " ++ show err
      Disconnect remoteAddr -> do
        atomically $ modifyTVar connVar $ Map.delete (connId remoteAddr)
        _ <- unregisterOutboundConnection cm remoteAddr
        connectionLoop muxMode localAddr lock propVar cc cm (Map.delete remoteAddr connMap) connVar
      RunMiniProtocols remoteAddr reqs -> do
        atomically $ do
          mqs <- (Map.lookup $ connId remoteAddr) <$> readTVar connVar
          case mqs of
            Nothing -> failure $ "No active connection " ++ show localAddr ++ " => " ++ show remoteAddr
            Just qs -> do
              sequence_ $ writeTQueue <$> qs <*> reqs
        case Map.lookup remoteAddr connMap of
          Nothing -> failureIO $ "no connection " ++ show localAddr ++ " => " ++ show remoteAddr
          Just (Handle mux muxBundle _, acc)  -> do
            rs <- try @_ @SomeException $ runInitiatorProtocols muxMode mux muxBundle
            case rs of
              Left err -> failureIO $ "protocol error: " ++ show err
              Right r  -> assertPropertyIO propVar $ r === fmap (drop 2 . reverse .  tails . (++ acc) . reverse) reqs
        connectionLoop muxMode localAddr lock propVar cc cm connMap connVar
      where
        connId remoteAddr = ConnectionId{ localAddress = localAddr, remoteAddress = remoteAddr }

        failureIO :: String -> m ()
        failureIO = atomically . failure

        failure :: String -> STM m ()
        failure err = assertProperty propVar $ counterexample err False

-- | Property wrapping `multinodeExperiment`.
prop_multinode_Sim :: Int -> MultiNodeScript Int TestAddr -> Property
prop_multinode_Sim serverAcc script' =
  simulatedPropertyWithTimeout 7200 $
    withSnocket debugTracer (singletonScript noAttenuation) (TestAddress 10) $ \snocket ->
    let script  = unTestAddr <$> script' in
    counterexample (ppScript script) <$>
      multinodeExperiment snocket Snocket.TestFamily (Snocket.TestAddress 0) serverAcc script

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

    ppEvent (StartServer             _ a i) = "Start server " ++ show a ++ " with accInit=" ++ show i
    ppEvent (StartClient             _ a)   = "Start client " ++ show a
    ppEvent (InboundConnection       _ a)   = "Connection from " ++ show a
    ppEvent (OutboundConnection      _ a)   = "Connecting to " ++ show a
    ppEvent (InboundMiniprotocols    _ a p) = "Miniprotocols from " ++ show a ++ ": " ++ ppData p
    ppEvent (OutboundMiniprotocols   _ a p) = "Miniprotocols to " ++ show a ++ ": " ++ ppData p
    ppEvent (CloseInboundConnection  _ a)   = "Close connection from " ++ show a
    ppEvent (CloseOutboundConnection _ a)   = "Close connection to " ++ show a

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

debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
debugTracer = Tracer $
  \msg -> (,msg) <$> getCurrentTime >>= say . show

-- | Convenience function to create a Bundle. Could move to Ouroboros.Network.Mux.
makeBundle :: (forall pt. TokProtocolTemperature pt -> a) -> Bundle a
makeBundle f = Bundle (WithHot         $ f TokHot)
                      (WithWarm        $ f TokWarm)
                      (WithEstablished $ f TokEstablished)



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
