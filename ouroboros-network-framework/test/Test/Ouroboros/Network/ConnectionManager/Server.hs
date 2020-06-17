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

module Test.Ouroboros.Network.ConnectionManager.Server
  ( tests
  ) where

import           Control.Applicative
import           Control.Monad (MonadPlus, join, replicateM)
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
import           Data.Foldable (fold, toList)
import           Data.Functor (($>))
import           Data.List (mapAccumL)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

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
import           Ouroboros.Network.ConnectionManager.RethrowPolicy
import           Ouroboros.Network.ConnectionManager.Server (ServerArguments (..))
import qualified Ouroboros.Network.ConnectionManager.Server as Server
import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.ConnectionHandler
                  ( MuxPromise (..)
                  , MuxConnectionManager
                  , makeConnectionHandler)
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.Snocket (Snocket, socketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Util.ShowProxy


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager.Server"
  [ testGroup "peekAlt"
    [ testProperty "foldr    (List)"  (prop_peekAlt_foldr    @[]    @Int)
    , testProperty "foldr    (Maybe)" (prop_peekAlt_foldr    @Maybe @Int)
    , testProperty "sequence (Maybe)" (prop_peekAlt_sequence @Maybe @Int)
    , testProperty "cons     (Maybe)" (prop_peekAlt_cons     @Maybe @Int)
    ]
  , testProperty "unidirectional" prop_unidirectional_IO
  , testProperty "bidirectional"  prop_bidirectional_IO
  ]


instance ShowProxy (ReqResp req resp) where
    showProxy _ = "ReqResp"

--
-- peekAlt properties
--

-- We are ulitmately interested in this properties for `STM` functor, but we
-- only test them for 'Maybe' monad.  This is enough since there is an
-- isomrphism (it preserves 'Alternative' operations) in `Kleisli IO`:
--
-- > toSTM :: Maybe a -> IO (STM m a)
-- > toSTM Nothing  = pure retry
-- > toSTM (Just a) = pure (pure a)
--
-- with an inverse:
--
-- > fromSTM :: STM m a -> IO (Maybe a)
-- > fromSTM ma = atomically (ma `orElse` (pure Nothing))


prop_peekAlt_foldr
    :: forall m a.
       ( Eq (m a)
       , Show (m a)
       , Alternative m )
    => [m a] -> Property
prop_peekAlt_foldr as =
    (fst <$> Server.peekAlt (Seq.fromList as))
    ===
    (foldr (<|>) empty as)


-- | Recursively calling 'peekAlt' is like filtering non 'empty' elements and
-- 'sequence'.
--
prop_peekAlt_sequence
    :: forall m a.
       ( Eq (m a)
       , Eq (m [a])
       , Eq (m (a, StrictSeq (m a)))
       , Show (m [a])
       , MonadPlus m )
    => [m a] -> Property
prop_peekAlt_sequence as =
      peekAll [] (Seq.fromList as)
      ===
      sequence (filter (/= empty) as)
    where
      -- recursievly 'peekAlt' and collect results
      peekAll :: [a] -> StrictSeq (m a) -> m [a]
      peekAll acc s =
       case Server.peekAlt s of
         res | res == empty -> pure (reverse acc)
             | otherwise    -> join $ (\(a, s') -> peekAll (a : acc) s') <$> res


-- | Calling `peekAlt` and then cominging the result with a cons ('<|'), should
-- put the first non 'empty' element in front.
--
prop_peekAlt_cons
    :: forall m a.
      ( Eq   (m a)
      , Eq   (m [m a])
      , Show (m [m a])
      , Alternative m )
    => [m a] -> Property
prop_peekAlt_cons as =
    let x = Server.peekAlt (Seq.fromList as)

        mhead :: m a
        mhead = fst <$> x

        mtail :: m (StrictSeq (m a))
        mtail = snd <$> x

    in ((toList . (mhead Seq.<|)) <$> mtail)
       ===
       case span (empty ==) as of
         -- if all 'as' entries where 'empty'
         (_, []) -> empty
         -- otherwise take the first element of `as'`, then list all the empty
         -- elements from start of `as`, then the rest of `as'`.
         (empties, (a : as')) -> pure (a : empties ++ as')

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


arbitraryList :: Arbitrary a =>  Gen [[a]]
arbitraryList =
    resize 5 (listOf (resize 10 (listOf (resize 100 arbitrary))))

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
    hotRequestsVar         <- newTVarM hotInitiatorRequests
    warmRequestsVar        <- newTVarM warmInitiatorRequests
    establishedRequestsVar <- newTVarM establishedInitiatorRequests
    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          connectionManagerTracer    = (name,) `contramap` cmTracer,
         -- MuxTracer
          connectionManagerMuxTracer = (name,) `contramap` nullTracer,
          connectionManagerIPv4Address = Nothing,
          connectionManagerIPv6Address = Nothing,
          connectionManagerAddressType = const Nothing,
          connectionHandler =
            makeConnectionHandler
              ((name,) `contramap` nullTracer) -- mux tracer
              SInitiatorMode
              clientMiniProtocolBundle
              HandshakeArguments {
                  -- TraceSendRecv
                  haHandshakeTracer = (name,) `contramap` nullTracer,
                  haHandshakeCodec = unversionedHandshakeCodec,
                  haVersionDataCodec = cborTermVersionDataCodec,
                  haVersions = unversionedProtocol
                    (clientApplication
                      hotRequestsVar
                      warmRequestsVar
                      establishedRequestsVar)
                }
              (\_ -> pure ())
              (mainThreadId, RethrowPolicy (\_ _ -> ShutdownNode)),
          connectionSnocket = snocket
        }
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
                    (Mux.MiniProtocolNum 3)
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
          ((localAddress,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
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
       -> m a)
    -> m a
withBidirectionalConnectionManager name snocket socket localAddress
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
    serverControlChannel      <- Server.newControlChannel
    -- as in the 'withInitiatorOnlyConnectionManager' we use a `StrictTVar` to
    -- pass list of requests, but since we are also interested in the results we
    -- need to have multable cells to pass the accumulators around.
    hotRequestsVar            <- newTVarM hotInitiatorRequests
    warmRequestsVar           <- newTVarM warmInitiatorRequests
    establishedRequestsVar    <- newTVarM establishedInitiatorRequests
    hotAccumulatorVar         <- newTVarM hotResponderAccumulator
    warmAccumulatorVar        <- newTVarM warmResponderAccumulator
    establishedAccumulatorVar <- newTVarM establishedResponderAccumulator

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          connectionManagerTracer    = (name,) `contramap` cmTracer,
          -- MuxTracer
          connectionManagerMuxTracer = (name,) `contramap` nullTracer,
          connectionManagerIPv4Address = localAddress,
          connectionManagerIPv6Address = Nothing,
          connectionManagerAddressType = const (Just IPv4Address),
          connectionHandler =
            makeConnectionHandler
              -- mux tracer
              ((name,) `contramap` nullTracer)
              SInitiatorResponderMode
              serverMiniProtocolBundle
              HandshakeArguments {
                  -- TraceSendRecv
                  haHandshakeTracer = (name,) `contramap` nullTracer,
                  haHandshakeCodec = unversionedHandshakeCodec,
                  haVersionDataCodec = cborTermVersionDataCodec,
                  haVersions = unversionedProtocol
                                (serverApplication 
                                  hotRequestsVar
                                  warmRequestsVar
                                  establishedRequestsVar
                                  hotAccumulatorVar
                                  warmAccumulatorVar
                                  establishedAccumulatorVar)
                }
              (Server.newOutboundConnection serverControlChannel)
              (mainThreadId, RethrowPolicy (\_ _ -> ShutdownNode)),
          connectionSnocket = snocket
        }
      $ \connectionManager -> do
            serverAddr <- Snocket.getLocalAddr snocket socket
            withAsync
              (Server.run
                ServerArguments {
                    serverSockets = socket :| [],
                    serverSnocket = snocket,
                    serverTracer = (name,) `contramap` nullTracer, -- ServerTrace
                    serverConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0,
                    serverConnectionManager = connectionManager,
                    serverControlChannel
                  }
              )
              (\_ -> k connectionManager serverAddr)
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
          ((localAddress,"Initiator",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
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
          ((localAddress,"Responder",protocolNum,) `contramap` nullTracer) -- TraceSendRecv
          codecReqResp
          (Effect $ reqRespServerPeer <$> reqRespServerMapAccumL' accumulatorVar (applyFun2 fn)))

    reqRespServerMapAccumL' :: StrictTVar m acc
                            -> (acc -> req -> (acc, resp))
                            -> m (ReqRespServer req resp m acc)
    reqRespServerMapAccumL' accumulatorVar fn = do
        acc <- atomically (readTVar accumulatorVar)
        pure $ go acc
      where
        go acc =
          ReqRespServer {
              recvMsgReq = \req -> case fn acc req of
                (acc', resp) -> pure (resp, go acc'),
              recvMsgDone = do
                atomically $ writeTVar accumulatorVar acc
                pure acc
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
       )
    => SingInitiatorResponderMode muxMode
    -> Mux.Mux muxMode m
    -> MuxBundle muxMode ByteString m a b
    -> m (Bundle [a])
runInitiatorProtocols singMuxMode mux (Bundle (WithHot hotPtcls) (WithWarm warmPtcls) (WithEstablished establishedPtcls)) = do
      -- start all protocols
      hotSTMs <- traverse runInitiator hotPtcls
      warmSTMs <- traverse runInitiator warmPtcls
      establishedSTMs <- traverse runInitiator establishedPtcls

      -- await for their termination
      hotRes <- traverse atomically hotSTMs
      warmRes <- traverse atomically warmSTMs
      establishedRes <- traverse atomically establishedSTMs
      case (partitionEithers hotRes, partitionEithers warmRes, partitionEithers establishedRes) of
        ((err : _, _), _, _) -> throwM err
        (_, (err : _, _), _) -> throwM err
        (_, _, (err : _, _)) -> throwM err
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
          SInitiatorMode -> Mux.InitiatorDirectionOnly
          SInitiatorResponderMode -> Mux.InitiatorDirection)
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
          $ \_ serverAddr -> do
            -- client → server: connect
            muxPromise <- includeOutboundConnection connectionManager serverAddr >>= atomically
            case muxPromise of
                MuxRunning _ mux muxBundle _ -> do
                    ( resp0 :: Bundle [[resp]]) <-
                      fold <$> replicateM (numberOfRounds clientAndServerData)
                                          (runInitiatorProtocols SInitiatorMode mux muxBundle)
                    pure $
                      (concat <$> resp0) === expectedResult
                                              clientAndServerData
                                              clientAndServerData


                _ -> pure $ counterexample "mux failed" False


prop_unidirectional_IO
    :: ClientAndServerData Int Int Int
    -> Property
prop_unidirectional_IO clientAndServerData =
    ioProperty $ do
      -- threadDelay (0.100)
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
      withBidirectionalConnectionManager
        "node-0" snocket socket0 (Just localAddr0) clientAndServerData0
        (\connectionManager0 _serverAddr0 ->
          withBidirectionalConnectionManager
            "node-1" snocket socket1 (Just localAddr1) clientAndServerData1
            (\connectionManager1 _serverAddr1 -> do
              -- node 0 → node 1: connect
              muxPromise0 <- includeOutboundConnection connectionManager0 localAddr1 >>= atomically
              -- node 1 → node 0: reuse existing connection
              muxPromise1 <- includeOutboundConnection connectionManager1 localAddr0 >>= atomically
              case (muxPromise0, muxPromise1) of
                  ( MuxRunning connId0 mux0 muxBundle0 _
                    , MuxRunning connId1 mux1 muxBundle1 _) -> do
                      -- runInitiatorProtcols returns a list of results per each
                      -- protocol in each bucket (warm \/ hot \/ established); but
                      -- we run only one mini-protocol. We can use `concat` to
                      -- flatten the results.
                      ( resp0 :: Bundle [[resp]]
                        , resp1 :: Bundle [[resp]]
                        ) <-
                        -- Run initiator twice; this tests if the responders on
                        -- the other end are restarted.
                        (fold <$> replicateM (numberOfRounds clientAndServerData0)
                                     (runInitiatorProtocols SInitiatorResponderMode mux0 muxBundle0))
                        `concurrently`
                        (fold <$> replicateM (numberOfRounds clientAndServerData1)
                                     (runInitiatorProtocols SInitiatorResponderMode mux1 muxBundle1))
                      pure $
                        counterexample "0"
                          ((concat <$> resp0) === expectedResult clientAndServerData0
                                                                 clientAndServerData1)
                        .&&.
                        counterexample "1"
                          ((concat <$> resp1) === expectedResult clientAndServerData1
                                                                 clientAndServerData0)
                        .&&.
                        -- check weather we reused the connection
                        connId0 === flipConnectionId connId1
                  _ -> pure $ counterexample "mux failed" False
              ))
  where
    flipConnectionId :: ConnectionId peerAddr -> ConnectionId peerAddr
    flipConnectionId ConnectionId {localAddress, remoteAddress}
      = ConnectionId {
          localAddress = remoteAddress,
          remoteAddress = localAddress
        }


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
            addr <- head <$> Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
            Socket.bind socket0 (Socket.addrAddress addr)
            Socket.listen socket0 maxBound
            Socket.bind socket1 (Socket.addrAddress addr)
            Socket.listen socket1 maxBound
            localAddr0 <- Socket.getSocketName socket0
            localAddr1 <- Socket.getSocketName socket1
            -- we need to make a dance with addresses; when a connection is
            -- accepted the remote `Socket.SockAddr` will be `127.0.0.1:port`
            -- rather than `0.0.0.0:port`.  If we pass `0.0.0.0:port` the
            -- then the connection would not be found by the connection manager
            -- and creating a socket would fail as we would try to create
            -- a connection with the same quadruple as an existing connection.
            let localAddr0' = case localAddr0 of
                  Socket.SockAddrInet port _ ->
                    Socket.SockAddrInet port (Socket.tupleToHostAddress (127,0,0,1))
                  _ -> error "unexpected address"

                localAddr1' = case localAddr1 of
                  Socket.SockAddrInet port _ ->
                    Socket.SockAddrInet port (Socket.tupleToHostAddress (127,0,0,1))
                  _ -> error "unexpected address"

            bidirectionalExperiment
              (socketSnocket iomgr)
              socket0     socket1
              localAddr0' localAddr1'
              data0       data1


--
-- Utils
--

debugTracer :: (MonadSay m, Show a) => Tracer m a
debugTracer = Tracer (say . show)


cmTracer :: (MonadSay m, Show peerAddr, Show a)
         => Tracer m (String, ConnectionManagerTrace peerAddr a)
cmTracer =
    Tracer
      $ \msg ->
        case msg of
          (_, ErrorFromHandler{})
            -> -- this way 'debugTracer' does not trigger a warning :)
              traceWith debugTracer msg
          (_, RethrownErrorFromHandler{})
            -> traceWith debugTracer msg
          _ -> pure ()

