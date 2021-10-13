{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
import           Control.Tracer (contramap, nullTracer)

import           Codec.Serialise.Class (Serialise)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.List (mapAccumL)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

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
import           Ouroboros.Network.Snocket (Snocket, socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Test.Ouroboros.Network.Orphans ()  -- ShowProxy ReqResp instance

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Server2"
  [ testProperty "unidirectional_IO" prop_unidirectional_IO
  , testProperty "bidirectional_IO"  prop_bidirectional_IO
  ]


--
-- Server tests (IO only)
--

-- | The protocol will run three instances of  `ReqResp` protocol; one for each
-- state: warm, hot and established.
--
data ClientAndServerData req resp acc = ClientAndServerData {
    responderAccumulatorFn       :: Fun (acc, req) (acc, resp),
    -- ^ folding function as required by `mapAccumL`, `acc -> req -> (acc, res)`
    -- written using QuickCheck's 'Fun' type; all three responders (hot \/ warm
    -- and established) are using the same
    -- accumulation function, but different initial values.
    accumulatorInit              :: acc,
    -- ^ initial value of accumulator
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
numberOfRounds :: ClientAndServerData req resp acc ->  Int
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

instance ( Arbitrary req
         , Arbitrary resp
         , Arbitrary acc
         , Function acc
         , CoArbitrary acc
         , Function req
         , CoArbitrary req
         ) => Arbitrary (ClientAndServerData req resp acc) where
    arbitrary =
      ClientAndServerData <$> arbitrary
                          <*> arbitrary
                          <*> arbitraryList
                          <*> arbitraryList
                          <*> arbitraryList


expectedResult :: ClientAndServerData req resp acc
               -> ClientAndServerData req resp acc
               -> [Bundle [resp]]
expectedResult client@ClientAndServerData
                                   { hotInitiatorRequests
                                   , warmInitiatorRequests
                                   , establishedInitiatorRequests
                                   }
               ClientAndServerData { responderAccumulatorFn
                                   , accumulatorInit
                                   } =
    go
      (take rounds $ hotInitiatorRequests         ++ repeat [])
      (take rounds $ warmInitiatorRequests        ++ repeat [])
      (take rounds $ establishedInitiatorRequests ++ repeat [])
  where
    rounds = numberOfRounds client
    go (a : as) (b : bs) (c : cs) =
      Bundle
        (WithHot
          (snd $ mapAccumL
            (applyFun2 responderAccumulatorFn)
            accumulatorInit
            a))
        (WithWarm
          (snd $ mapAccumL
            (applyFun2 responderAccumulatorFn)
            accumulatorInit
            b))
        (WithEstablished
          (snd $ mapAccumL
            (applyFun2 responderAccumulatorFn)
            accumulatorInit
            c))
      : go as bs cs
    go [] [] [] = []
    go _  _  _  = error "expectedResult: impossible happened"


--
-- Various ConnectionManagers
--

type ConnectionManagerMonad m =
       ( MonadAsync m, MonadCatch m, MonadEvaluate m, MonadFork m, MonadMask  m
       , MonadST m, MonadTime m, MonadTimer m, MonadThrow m, MonadThrow (STM m)
       )


-- | Initiator only connection manager.
--
withInitiatorOnlyConnectionManager
    :: forall peerAddr socket req resp m acc a.
       ( ConnectionManagerMonad m

       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Typeable req, Typeable resp
       , Serialise req, Serialise resp
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req, Show resp
       )
    => String
    -- ^ identifier (for logging)
    -> Snocket m socket peerAddr
    -> ClientAndServerData req resp acc
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> (MuxConnectionManager
          InitiatorMode socket peerAddr
          UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager
    name snocket
    ClientAndServerData {
        hotInitiatorRequests,
        warmInitiatorRequests,
        establishedInitiatorRequests
      }
    k = do
    mainThreadId <- myThreadId
    -- we pass a `StricTVar` with all the reuqests to each initiator.  This way
    -- the each round (which runs a single instance of `ReqResp` protocol) will
    -- use its own request list.
    hotRequestsVar         <- newTVarIO hotInitiatorRequests
    warmRequestsVar        <- newTVarIO warmInitiatorRequests
    establishedRequestsVar <- newTVarIO establishedInitiatorRequests
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
          cmIPv4Address = Nothing,
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
          cmTimeWaitTimeout = timeWaitTimeout
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
        (unversionedProtocol
          (clientApplication hotRequestsVar
                             warmRequestsVar
                             establishedRequestsVar))
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

    clientApplication :: StrictTVar m [[req]]
                      -> StrictTVar m [[req]]
                      -> StrictTVar m [[req]]
                      -> Bundle
                          (ConnectionId peerAddr
                            -> ControlMessageSTM m
                            -> [MiniProtocol InitiatorMode ByteString m [resp] Void])
    clientApplication hotRequestsVar
                      warmRequestsVar
                      establishedRequestsVar = Bundle {
        withHot = WithHot $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 1
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiator
                    miniProtocolNum
                    hotRequestsVar
               }
          ],
        withWarm = WithWarm $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 2
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiator
                    miniProtocolNum
                    warmRequestsVar
              }
          ],
        withEstablished = WithEstablished $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 3
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiator
                    miniProtocolNum
                    establishedRequestsVar
              }
          ]
      }

    reqRespInitiator :: Mux.MiniProtocolNum
                     -> StrictTVar m [[req]]
                     -> RunMiniProtocol InitiatorMode ByteString m [resp] Void
    reqRespInitiator protocolNum requestsVar =
      InitiatorProtocolOnly
        (MuxPeer
          ((name,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
          (Effect $ do
            reqs <-
              atomically $ do
                requests <- readTVar requestsVar
                case requests of
                  (reqs : rest) -> do
                    writeTVar requestsVar rest $> reqs
                  [] -> pure []
            pure $ 
              reqRespClientPeer (reqRespClientMap reqs)))


--
-- Constants
--

protocolIdleTimeout :: DiffTime
protocolIdleTimeout = 0.1

timeWaitTimeout :: DiffTime
timeWaitTimeout = 0.1


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

       , Ord peerAddr, Show peerAddr, Typeable peerAddr
       , Serialise req, Serialise resp
       , Typeable req, Typeable resp

       -- debugging
       , MonadAsync m
       , MonadLabelledSTM m
       , MonadSay m, Show req, Show resp
       )
    => String
    -- ^ identifier (for logging)
    -> Snocket m socket peerAddr
    -> socket
    -- ^ listening socket
    -> Maybe peerAddr
    -> ClientAndServerData req resp acc
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> (MuxConnectionManager
          InitiatorResponderMode socket peerAddr
          UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> Async m Void
       -> m a)
    -> m a
withBidirectionalConnectionManager name snocket socket localAddress
                                   ClientAndServerData {
                                       responderAccumulatorFn,
                                       accumulatorInit,
                                       hotInitiatorRequests,
                                       warmInitiatorRequests,
                                       establishedInitiatorRequests
                                     }
                                   k = do
    mainThreadId <- myThreadId
    inbgovControlChannel      <- Server.newControlChannel
    -- as in the 'withInitiatorOnlyConnectionManager' we use a `StrictTVar` to
    -- pass list of requests, but since we are also interested in the results we
    -- need to have multable cells to pass the accumulators around.
    hotRequestsVar            <- newTVarIO hotInitiatorRequests
    warmRequestsVar           <- newTVarIO warmInitiatorRequests
    establishedRequestsVar    <- newTVarIO establishedInitiatorRequests
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
          cmTimeWaitTimeout = timeWaitTimeout,
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
          (unversionedProtocol
            (serverApplication hotRequestsVar
                               warmRequestsVar
                                establishedRequestsVar))
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
                    serverInboundIdleTimeout = protocolIdleTimeout,
                    serverControlChannel = inbgovControlChannel,
                    serverObservableStateVar = observableStateVar
                  }
              )
              (\serverAsync -> k connectionManager serverAddr serverAsync)
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

    serverApplication :: StrictTVar m [[req]]
                      -> StrictTVar m [[req]]
                      -> StrictTVar m [[req]]
                      -> Bundle
                          (ConnectionId peerAddr
                      -> ControlMessageSTM m
                      -> [MiniProtocol InitiatorResponderMode ByteString m [resp] acc])
    serverApplication hotRequestsVar
                      warmRequestsVar
                      establishedRequestsVar
                      = Bundle {
        withHot = WithHot $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 1
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    miniProtocolNum
                    responderAccumulatorFn
                    accumulatorInit
                    hotRequestsVar
               }
          ],
        withWarm = WithWarm $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 2
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    miniProtocolNum
                    responderAccumulatorFn
                    accumulatorInit
                    warmRequestsVar
              }
          ],
        withEstablished = WithEstablished $ \_ _ ->
          [ let miniProtocolNum = Mux.MiniProtocolNum 3
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    (Mux.MiniProtocolNum 3)
                    responderAccumulatorFn
                    accumulatorInit
                    establishedRequestsVar
              }
          ]
      }

    reqRespInitiatorAndResponder
      :: Mux.MiniProtocolNum
      -> Fun (acc, req) (acc, resp)
      -> acc
      -> StrictTVar m [[req]]
      -> RunMiniProtocol InitiatorResponderMode ByteString m [resp] acc
    reqRespInitiatorAndResponder protocolNum fn accInit requestsVar =
      InitiatorAndResponderProtocol
        (MuxPeer
          ((name,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
          (Effect $ do
            reqs <-
              atomically $ do
                requests <- readTVar requestsVar
                case requests of
                  (reqs : rest) -> do
                    writeTVar requestsVar rest $> reqs
                  [] -> pure []
            pure $ 
              reqRespClientPeer
              (reqRespClientMap reqs)))
        (MuxPeer
          ((name,"Responder",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
          (reqRespServerPeer $ reqRespServerMapAccumL' (applyFun2 fn) accInit))

    reqRespServerMapAccumL' :: (acc -> req -> (acc, resp))
                            -> acc
                            -> ReqRespServer req resp m acc
    reqRespServerMapAccumL' fn = go
      where
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

       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => Snocket m socket peerAddr
    -> socket
    -> ClientAndServerData req resp acc
    -> m Property
unidirectionalExperiment snocket socket clientAndServerData = do
    withInitiatorOnlyConnectionManager
      "client" snocket clientAndServerData
      $ \connectionManager ->
        withBidirectionalConnectionManager
          "server" snocket socket Nothing clientAndServerData
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
                (\(r, expected) acc ->
                  case r of
                    Left _ -> acc
                    Right a -> a === expected .&&. acc)
                (property True)
                $ zip rs (expectedResult clientAndServerData clientAndServerData)

prop_unidirectional_IO
  :: ClientAndServerData Int Int Int
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

       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr

       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       , Show acc
       )
    => Snocket m socket peerAddr
    -> socket
    -> socket
    -> peerAddr
    -> peerAddr
    -> ClientAndServerData req resp acc
    -> ClientAndServerData req resp acc
    -> m Property
bidirectionalExperiment
    snocket socket0 socket1 localAddr0 localAddr1
    clientAndServerData0 clientAndServerData1 = do
      lock <- newTMVarIO ()
      -- connect lock: only one side can run 'requestOutboundConnection' in
      -- turn.  Otherwise when both sides call 'requestOutboundConnection' they
      -- both will run 'connect' and one of the calls will fail.  Using a lock
      -- forces to block until negotiation is done, which is not ideal.
      withBidirectionalConnectionManager
        "node-0" snocket socket0 (Just localAddr0) clientAndServerData0
        (\connectionManager0 _serverAddr0 serverAsync0 ->
          withBidirectionalConnectionManager
            "node-1" snocket socket1 (Just localAddr1) clientAndServerData1
            (\connectionManager1 _serverAddr1 serverAsync1 -> do
              link serverAsync0
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
                (replicateM
                  (numberOfRounds clientAndServerData0)
                  (bracket
                    (withLock lock
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
                    (withLock lock
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
                  (\(r, expected) acc ->
                    case r of
                      Left _ -> acc
                      Right a -> a === expected .&&. acc)
                  (property True)
                  (zip rs0 (expectedResult clientAndServerData0 clientAndServerData1))
                .&&.
                foldr
                  (\(r, expected) acc ->
                    case r of
                      Left _ -> acc
                      Right a -> a === expected .&&. acc)
                  (property True)
                  (zip rs1 (expectedResult clientAndServerData1 clientAndServerData0))
                ))


prop_bidirectional_IO
    :: ClientAndServerData Int Int Int
    -> ClientAndServerData Int Int Int
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
              (socketSnocket iomgr)
              socket0
              socket1
              addr0'
              addr1'
              data0
              data1


--
-- Utils
--

{-
debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
debugTracer = Tracer $
  \msg -> (,msg) <$> getCurrentTime >>= say . show
-}

withLock :: ( MonadSTM   m
            , MonadThrow m
            )
         => StrictTMVar m ()
         -> m a
         -> m a
withLock v m = 
    bracket (atomically $ takeTMVar v)
            (atomically . putTMVar v)
            (const m)
