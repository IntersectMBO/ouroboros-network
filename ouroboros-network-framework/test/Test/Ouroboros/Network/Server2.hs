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
-- `ShowProxy (ReqResp req resp)` is an orphaned instance
{-# OPTIONS_GHC -Wno-orphans               #-}

module Test.Ouroboros.Network.Server2
  ( tests
  ) where

import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST    (MonadST)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTime  (MonadTime)
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import           Codec.Serialise.Class (Serialise)
import           Data.ByteString.Lazy (ByteString)
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import           Data.Functor (($>))
import           Data.List (mapAccumL)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Test.QuickCheck
import           Test.Tasty.QuickCheck
import           Test.Tasty (TestTree, testGroup)

import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Core

import           Network.TypedProtocol.ReqResp.Type (ReqResp)
import           Network.TypedProtocol.ReqResp.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server
import           Network.TypedProtocol.ReqResp.Examples

import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.RethrowPolicy
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..))
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Server2 (ServerArguments (..))
import qualified Ouroboros.Network.Server2 as Server
import qualified Ouroboros.Network.Server2.ControlChannel as Server
import           Ouroboros.Network.Snocket (Snocket, socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Util.ShowProxy


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Server2"
  [ testProperty "unidirectional_IO" (withMaxSuccess 10 prop_unidirectional_IO)
  , testProperty "bidirectional_IO"  prop_bidirectional_IO
  ]

instance ShowProxy (ReqResp req resp) where
    showProxy _ = "ReqResp"

--
-- Server tests (IO only)
--

-- | The protocol will run three instances of  `ReqResp` protocol; one for each
-- state: warm, hot and established.
--
data ClientAndServerData req resp acc = ClientAndServerData {
    responderAccumulatorFn          :: Fun (acc, req) (acc, resp),
    -- ^ folding function as required by `mapAccumL`, `acc -> req -> (acc, res)`
    -- written using QuickCheck's 'Fun' type; all three responders (hot \/ warm
    -- and established) are using the same
    -- accumulation function, but different initial values.
    hotResponderAccumulator         :: acc,
    -- ^ initial accumulator value for hot responder
    warmResponderAccumulator        :: acc,
    -- ^ initial accumulator value for worm responder
    establishedResponderAccumulator :: acc,
    -- ^ initial accumulator value for established responder
    hotInitiatorRequests            :: [[req]],
    -- ^ list of requests run by the hot intiator in each round; Running
    -- multiple rounds allows us to test restarting of responders.
    warmInitiatorRequests           :: [[req]],
    -- ^ list of requests run by the warm intiator in each round
    establishedInitiatorRequests    :: [[req]]
    -- ^ lsit of requests run by the established intiator in each round
  }
  deriving Show


-- Number of rounds to exhoust all the requests.
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
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitraryList
                          <*> arbitraryList
                          <*> arbitraryList


expectedResult :: ClientAndServerData req resp acc
               -> ClientAndServerData req resp acc
               -> Bundle [resp]
expectedResult ClientAndServerData { hotInitiatorRequests
                                   , warmInitiatorRequests
                                   , establishedInitiatorRequests
                                   }
               ClientAndServerData { responderAccumulatorFn
                                   , hotResponderAccumulator
                                   , warmResponderAccumulator
                                   , establishedResponderAccumulator
                                   } =
  Bundle
    (WithHot
      (snd $ mapAccumL
        (applyFun2 responderAccumulatorFn)
        hotResponderAccumulator
        (concat hotInitiatorRequests)))
    (WithWarm
      (snd $ mapAccumL
        (applyFun2 responderAccumulatorFn)
        warmResponderAccumulator
        (concat warmInitiatorRequests)))
    (WithEstablished
      (snd $ mapAccumL
        (applyFun2 responderAccumulatorFn)
        establishedResponderAccumulator
        (concat establishedInitiatorRequests)))


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
    -> Bool
    -- ^ use compat mode
    -> ClientAndServerData req resp acc
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> (MuxConnectionManager
          InitiatorMode socket peerAddr
          UnversionedProtocol ByteString m [resp] Void
       -> m a)
    -> m a
withInitiatorOnlyConnectionManager
    name snocket compatMode
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
          cmTracer    = (name,) `contramap` connectionManagerTracer,
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
          cmWaitTimeTimeout = waitTimeTimeout,
          cmProtocolIdleTimeout = protocolIdleTimeout
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
            haVersions = unversionedProtocol
              (clientApplication
                hotRequestsVar
                warmRequestsVar
                establishedRequestsVar),
            haAcceptVersion = acceptableVersion
          }
        (\_ -> compatMode)
        (mainThreadId, muxErrorRethrowPolicy <> ioErrorRethrowPolicy))
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

respondersIdleTimeout :: DiffTime
respondersIdleTimeout = 0.1

protocolIdleTimeout :: DiffTime
protocolIdleTimeout = 0.1

waitTimeTimeout :: DiffTime
waitTimeTimeout = 0.1


-- 
-- Rethrow policy
--

debugMuxErrorRethrowPolicy :: RethrowPolicy
debugMuxErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ MuxError { errorType } ->
        case errorType of
          MuxIOException _ -> ShutdownPeer
          MuxBearerClosed  -> ShutdownPeer
          _                -> ShutdownNode

debugIOErrorRethrowPolicy :: RethrowPolicy
debugIOErrorRethrowPolicy =
    mkRethrowPolicy $
      \_ (_ :: IOError) -> ShutdownNode


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
    -> Bool
    -- ^ compat mode
    -> Maybe peerAddr
    -> ClientAndServerData req resp acc
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> (MuxConnectionManager
          InitiatorResponderMode socket peerAddr
          UnversionedProtocol ByteString m [resp] acc
       -> peerAddr
       -> m a)
    -> m a
withBidirectionalConnectionManager name snocket socket compatMode localAddress
                                   ClientAndServerData {
                                       responderAccumulatorFn,
                                       hotResponderAccumulator,
                                       warmResponderAccumulator,
                                       establishedResponderAccumulator,
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
    hotAccumulatorVar         <- newTVarIO hotResponderAccumulator
    warmAccumulatorVar        <- newTVarIO warmResponderAccumulator
    establishedAccumulatorVar <- newTVarIO establishedResponderAccumulator
    -- we are not using the randomness
    observableStateVar        <- Server.newObservableStateVarFromSeed 0
    let muxTracer = (name,) `contramap` nullTracer -- mux tracer

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer       = ((name,) `contramap` connectionManagerTracer),
          -- MuxTracer
          cmMuxTracer    = muxTracer,
          cmIPv4Address  = localAddress,
          cmIPv6Address  = Nothing,
          cmAddressType  = \_ -> Just IPv4Address,
          cmSnocket      = snocket,
          cmProtocolIdleTimeout = protocolIdleTimeout,
          cmWaitTimeTimeout = waitTimeTimeout,
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
              haVersions = unversionedProtocol
                            (serverApplication 
                              hotRequestsVar
                              warmRequestsVar
                              establishedRequestsVar
                              hotAccumulatorVar
                              warmAccumulatorVar
                              establishedAccumulatorVar),
              haAcceptVersion = acceptableVersion
            }
          (\_ -> compatMode)
          (mainThreadId,   debugMuxErrorRethrowPolicy
                        <> debugIOErrorRethrowPolicy))
          (\_ -> HandshakeFailure)
          (InResponderMode inbgovControlChannel)
      $ \connectionManager -> do
            serverAddr <- Snocket.getLocalAddr snocket socket
            withAsync
              (Server.run
                ServerArguments {
                    serverHasInitiator = SingHasInitiator,
                    serverSockets = socket :| [],
                    serverSnocket = snocket,
                    serverTracer = (name,) `contramap` nullTracer, -- ServerTrace
                    serverConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0,
                    serverConnectionManager = connectionManager,
                    serverRespondersIdleTimeout = respondersIdleTimeout,
                    serverControlChannel = inbgovControlChannel,
                    serverObservableStateVar = observableStateVar
                  }
              )
              (\thread -> link thread
                       >> k connectionManager serverAddr)
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
                      -> StrictTVar m acc
                      -> StrictTVar m acc
                      -> StrictTVar m acc
                      -> Bundle
                          (ConnectionId peerAddr
                      -> ControlMessageSTM m
                      -> [MiniProtocol InitiatorResponderMode ByteString m [resp] acc])
    serverApplication hotRequestsVar
                      warmRequestsVar
                      establishedRequestsVar
                      hotAccumulatorVar
                      warmAccumulatorVar
                      establishedAccumulatorVar
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
                    hotAccumulatorVar
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
                    warmAccumulatorVar
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
                    establishedAccumulatorVar
                    establishedRequestsVar
              }
          ]
      }

    reqRespInitiatorAndResponder
      :: Mux.MiniProtocolNum
      -> Fun (acc, req) (acc, resp)
      -> StrictTVar m acc
      -> StrictTVar m [[req]]
      -> RunMiniProtocol InitiatorResponderMode ByteString m [resp] acc
    reqRespInitiatorAndResponder protocolNum fn accumulatorVar requestsVar =
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
          (Effect $ reqRespServerPeer <$> reqRespServerMapAccumL' accumulatorVar (applyFun2 fn)))

    reqRespServerMapAccumL' :: StrictTVar m acc
                            -> (acc -> req -> (acc, resp))
                            -> m (ReqRespServer req resp m acc)
    reqRespServerMapAccumL' accumulatorVar fn = pure go
      where
        go =
          ReqRespServer {
              recvMsgReq = \req -> do
                atomically $ do
                  acc <- readTVar accumulatorVar
                  let (acc', resp) = fn acc req
                  writeTVar accumulatorVar acc'
                  return (resp, go),
              recvMsgDone =
                atomically (readTVar accumulatorVar)
            }




-- | Run all initiator mini-protocols and collect results. Throw exception if
-- any of the thread returned an exception.
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
    -> m (Bundle [a])
runInitiatorProtocols
    singMuxMode mux
    (Bundle (WithHot hotPtcls)
            (WithWarm warmPtcls)
            (WithEstablished establishedPtcls)) = do
      -- start all protocols
      hotSTMs <- traverse runInitiator hotPtcls
      warmSTMs <- traverse runInitiator warmPtcls
      establishedSTMs <- traverse runInitiator establishedPtcls

      -- await for their termination
      hotRes <- traverse atomically hotSTMs
      warmRes <- traverse atomically warmSTMs
      establishedRes <- traverse atomically establishedSTMs
      case (partitionEithers hotRes, partitionEithers warmRes, partitionEithers establishedRes) of
        ((err : _, _), _, _) -> throwIO err
        (_, (err : _, _), _) -> throwIO err
        (_, _, (err : _, _)) -> throwIO err
        (([], hot), ([], warm), ([], established)) ->
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

       , Ord peerAddr, Show peerAddr, Typeable peerAddr, Eq peerAddr
       , Serialise req, Show req
       , Serialise resp, Show resp, Eq resp
       , Typeable req, Typeable resp
       )
    => Snocket m socket peerAddr
    -> socket
    -> Bool -- ^ compat mode
    -> ClientAndServerData req resp acc
    -> m Property
unidirectionalExperiment snocket socket compatMode clientAndServerData = do
    withInitiatorOnlyConnectionManager
      "client" snocket compatMode clientAndServerData
      $ \connectionManager ->
        withBidirectionalConnectionManager
          "server" snocket socket compatMode Nothing clientAndServerData
          $ \_ serverAddr -> do
            -- client → server: connect
            ( resp0 :: Bundle [[resp]]) <-
              fold <$>
                replicateM
                  (numberOfRounds clientAndServerData)
                  (do connHandle
                        <- requestOutboundConnection
                             connectionManager serverAddr
                      case connHandle of
                        Connected _ _ (Handle mux muxBundle _ _) -> do
                          res <-
                            runInitiatorProtocols
                              SingInitiatorMode mux muxBundle
                          _ <-
                            unregisterOutboundConnection
                              connectionManager serverAddr
                          return res
                        Disconnected _ err ->
                          throwIO (userError $ "unidirectionalExperiment: " ++ show err)
                  )
            pure $
              (concat <$> resp0) === expectedResult
                                      clientAndServerData
                                      clientAndServerData


prop_unidirectional_IO
  :: ClientAndServerData Int Int Int
  -> Bool
  -- ^ compat mode
  -> Property
prop_unidirectional_IO clientAndServerData compatMode =
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
                compatMode
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
        "node-0" snocket socket0 False (Just localAddr0) clientAndServerData0
        (\connectionManager0 _serverAddr0 ->
          withBidirectionalConnectionManager
            "node-1" snocket socket1 False (Just localAddr1) clientAndServerData1
            (\connectionManager1 _serverAddr1 -> do
              -- runInitiatorProtcols returns a list of results per each
              -- protocol in each bucket (warm \/ hot \/ established); but
              -- we run only one mini-protocol. We can use `concat` to
              -- flatten the results.
              ( resp0 :: Bundle [[resp]]
                , resp1 :: Bundle [[resp]]
                ) <-
                -- Run initiator twice; this tests if the responders on
                -- the other end are restarted.
                (fold <$> replicateM
                            (numberOfRounds clientAndServerData0)
                            (do connHandle <-
                                  withLock lock $
                                    requestOutboundConnection connectionManager0 localAddr1
                                case connHandle of
                                  Connected _ _ (Handle mux muxBundle _ _) -> do
                                    res <-
                                      runInitiatorProtocols
                                        SingInitiatorResponderMode
                                        mux muxBundle
                                    res' <-
                                      unregisterOutboundConnection
                                        connectionManager0 localAddr1
                                    case res' of
                                      UnsupportedState inState ->
                                        throwIO
                                          (userError
                                            . concat
                                            $ [ "bidirectionalExperiment: unregigisterOutboundConnection "
                                              , show inState
                                              ])
                                      OperationSuccess _ -> return ()
                                    return res
                                  Disconnected _ err ->
                                    throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                            ))
                `concurrently`
                (fold <$> replicateM
                             (numberOfRounds clientAndServerData1)
                             (do connHandle <-
                                   withLock lock $
                                     requestOutboundConnection connectionManager1 localAddr0
                                 case connHandle of
                                   Connected _ _ (Handle mux muxBundle _ _) -> do
                                     res <-
                                       runInitiatorProtocols
                                         SingInitiatorResponderMode
                                         mux muxBundle
                                     res' <-
                                       unregisterOutboundConnection
                                         connectionManager1 localAddr0
                                     case res' of
                                       UnsupportedState inState ->
                                         throwIO
                                           (userError
                                             . concat
                                             $ [ "bidirectionalExperiment: unregigisterInboundConnection "
                                               , show inState
                                               ])
                                       OperationSuccess _ -> return ()
                                     return res
                                   Disconnected _ err ->
                                     throwIO (userError $ "bidirectionalExperiment: " ++ show err)
                             ))
              pure $
                counterexample "0"
                  ((concat <$> resp0) === expectedResult clientAndServerData0
                                                         clientAndServerData1)
                .&&.
                counterexample "1"
                  ((concat <$> resp1) === expectedResult clientAndServerData1
                                                         clientAndServerData0)
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
            addr0 : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "6000")
            addr1 : _ <- Socket.getAddrInfo (Just hints) (Just "172.16.0.1") (Just "6001")
            Socket.bind socket0 (Socket.addrAddress addr0)
            Socket.bind socket1 (Socket.addrAddress addr1)
            Socket.listen socket0 10
            Socket.listen socket1 10
            localAddr0 <- Socket.getSocketName socket0
            localAddr1 <- Socket.getSocketName socket1
            print (Socket.addrAddress addr0, Socket.addrAddress addr1)
            print (localAddr0, localAddr1)
            print (socket0, socket1)

            bidirectionalExperiment
              (socketSnocket iomgr)
              socket0
              socket1
              (Socket.addrAddress addr0)
              (Socket.addrAddress addr1)
              data0
              data1


--
-- Utils
--

debugTracer :: (MonadSay m, Show a) => Tracer m a
debugTracer = Tracer (say . show)


connectionManagerTracer :: (MonadSay m, Show peerAddr, Show a)
         => Tracer m (String, ConnectionManagerTrace peerAddr a)
connectionManagerTracer =
    Tracer
      $ \msg ->
        case msg of
          (_, TrConnectError{})
            -> -- this way 'debugTracer' does not trigger a warning :)
              traceWith debugTracer msg
          (_, _) ->
              pure ()


withLock :: ( MonadSTM   m
            , MonadThrow m
            )
         => StrictTMVar m ()
         -> m a
         -> m a
withLock _ m = m
{-
withLock v m = do
    atomically $ takeTMVar v
    m `finally` atomically (putTMVar v ())
-}
