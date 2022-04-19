{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- just to use 'debugTracer'
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- `ShowProxy (ReqResp req resp)` is an orphaned instance
{-# OPTIONS_GHC -Wno-orphans               #-}

-- | 'demo-connection-manager LOCAL_ADDR LOCAL_PORT REMOTE_ADDR REMOTE_PORT'
--
module Main (main) where

import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (MonadTime (..))
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)
import Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import Data.ByteString.Lazy (ByteString)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)

import Network.Mux qualified as Mux
import Network.Mux.Bearer qualified as Mux
import Network.Socket qualified as Socket
import Network.TypedProtocol.Peer

import Options.Applicative

import System.Random (RandomGen)
import System.Random qualified as Random

import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Codec.CBOR
import Network.TypedProtocol.ReqResp.Examples
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Type (ReqResp)

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Core
import Ouroboros.Network.ConnectionManager.InformationChannel
           (newInformationChannel)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.MuxMode
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec (timeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version (Acceptable (..),
           Queryable (..))
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Server2 (ServerArguments (..))
import Ouroboros.Network.Server2 qualified as Server
import Ouroboros.Network.Snocket (Snocket, socketSnocket)
import Ouroboros.Network.Snocket qualified as Snocket
import Ouroboros.Network.Util.ShowProxy


instance ShowProxy (ReqResp req resp) where
    showProxy _ = "ReqResp"

--
-- Server tests (IO only)
--

-- | The protocol will run three instances of  `ReqResp` protocol; one for each
-- state: warm, hot and established.
--
data ClientAndServerData = ClientAndServerData {
    hotInitiatorRequests         :: [[Int]],
    -- ^ list of requests run by the hot intiator in each round; Running
    -- multiple rounds allows us to test restarting of responders.
    warmInitiatorRequests        :: [[Int]],
    -- ^ list of requests run by the warm intiator in each round
    establishedInitiatorRequests :: [[Int]]
    -- ^ lsit of requests run by the established intiator in each round
  }
  deriving Show


{-
genList :: RandomGen g => (g -> a) -> Int -> g -> [a]
genList gen = go []
  where
    go !acc len _g | len < 0 = acc
    go !acc len  g =
        go (a : acc) (pred len) g''
      where
        (g', g'') = Random.split g
        !a = gen g'
-}

genInfList :: RandomGen g => (g -> a) -> g -> [a]
genInfList gen g =
    case Random.split g of
      (g', g'') -> gen g' : genInfList gen g''


genClientAndServerData :: forall g. RandomGen g
                       => g -> Int -> ClientAndServerData
genClientAndServerData g0 len = ClientAndServerData {
        hotInitiatorRequests         = genListOfLists g1,
        warmInitiatorRequests        = genListOfLists g2,
        establishedInitiatorRequests = genListOfLists g3
      }
    where
      (g1, (g2, g3)) = Random.split <$> Random.split g0

      genListOfLists :: g -> [[Int]]
      genListOfLists = \g -> genInfList (take len . Random.randoms) g

--
-- Various ConnectionManagers
--

type ConnectionManagerMonad m =
       ( Alternative (STM m), MonadAsync m, MonadCatch m, MonadEvaluate m,
         MonadFork m, MonadMask m, MonadST m, MonadTime m, MonadTimer m,
         MonadThrow m, MonadThrow (STM m)
       )


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
    :: forall peerAddr socket m a.
       ( ConnectionManagerMonad m

       , Ord peerAddr, Show peerAddr, Typeable peerAddr

       -- debugging
       , MonadFix m
       , MonadAsync m
       , MonadDelay m
       , MonadLabelledSTM m
       , MonadTraceSTM m
       , MonadSay m
       )
    => Snocket m socket peerAddr
    -> Mux.MakeBearer m socket
    -> socket
    -- ^ listening socket
    -> DiffTime -- protocol idle timeout
    -> DiffTime -- wait time timeout
    -> Maybe peerAddr
    -> Random.StdGen
    -> ClientAndServerData
    -- ^ series of request possible to do with the bidirectional connection
    -- manager towards some peer.
    -> (ConnectionManagerWithExpandedCtx
          InitiatorResponderMode socket peerAddr UnversionedProtocolData
          UnversionedProtocol ByteString m () ()
       -> peerAddr
       -> m a)
    -> m a
withBidirectionalConnectionManager snocket makeBearer socket
                                   protocolIdleTimeout
                                   timeWaitTimeout
                                   localAddress
                                   stdGen
                                   ClientAndServerData {
                                       hotInitiatorRequests,
                                       warmInitiatorRequests,
                                       establishedInitiatorRequests
                                     }
                                   k = do
    mainThreadId <- myThreadId
    inbgovInfoChannel <- newInformationChannel
    -- as in the 'withInitiatorOnlyConnectionManager' we use a `StrictTVar` to
    -- pass list of requests, but since we are also interested in the results we
    -- need to have multable cells to pass the accumulators around.
    hotRequestsVar         <- LazySTM.newTVarIO hotInitiatorRequests
    warmRequestsVar        <- LazySTM.newTVarIO warmInitiatorRequests
    establishedRequestsVar <- LazySTM.newTVarIO establishedInitiatorRequests
    let muxTracer = ("mux",) `contramap` nullTracer -- mux tracer

    withConnectionManager
      ConnectionManagerArguments {
          -- ConnectionManagerTrace
          cmTracer       = (("cm",) `contramap` debugTracer),
          cmTrTracer     = (("cm-state",) `contramap` debugTracer),
          -- MuxTracer
          cmMuxTracer    = muxTracer,
          cmIPv4Address  = localAddress,
          cmIPv6Address  = Nothing,
          cmAddressType  = \_ -> Just IPv4Address,
          cmSnocket      = snocket,
          cmMakeBearer   = makeBearer,
          cmConfigureSocket = \_ _ -> return (),
          cmTimeWaitTimeout = timeWaitTimeout,
          cmOutboundIdleTimeout = protocolIdleTimeout,
          connectionDataFlow = \_ _ -> Duplex,
          cmPrunePolicy = simplePrunePolicy,
          cmStdGen      = stdGen,
          cmConnectionsLimits = AcceptedConnectionsLimit {
              acceptedConnectionsHardLimit = maxBound,
              acceptedConnectionsSoftLimit = maxBound,
              acceptedConnectionsDelay     = 0
            }
        }
        (makeConnectionHandler
          muxTracer
          SingInitiatorResponderMode
          HandshakeArguments {
              -- TraceSendRecv
              haHandshakeTracer = ("handshake",) `contramap` debugTracer,
              haHandshakeCodec = unversionedHandshakeCodec,
              haVersionDataCodec = unversionedProtocolDataCodec,
              haAcceptVersion = acceptableVersion,
              haQueryVersion = queryVersion,
              haTimeLimits = timeLimitsHandshake
            }
          (unversionedProtocol
             (serverApplication hotRequestsVar
                                warmRequestsVar
                                establishedRequestsVar))
          (mainThreadId,   debugMuxErrorRethrowPolicy
                        <> debugIOErrorRethrowPolicy))
          (\_ -> HandshakeFailure)
          (InResponderMode inbgovInfoChannel)
      $ \connectionManager -> do
            serverAddr <- Snocket.getLocalAddr snocket socket
            Server.with
              ServerArguments {
                  serverSockets = socket :| [],
                  serverSnocket = snocket,
                  serverTracer = ("server",) `contramap` debugTracer, -- ServerTrace
                  serverTrTracer = nullTracer,
                  serverInboundGovernorTracer = ("inbound-governor",) `contramap` debugTracer,
                  serverDebugInboundGovernor = nullTracer,
                  serverConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0,
                  serverConnectionManager = connectionManager,
                  serverInboundIdleTimeout = Just protocolIdleTimeout,
                  serverInboundInfoChannel = inbgovInfoChannel
                }
              (\_ _ -> k connectionManager serverAddr)
  where
    serverApplication :: LazySTM.TVar m [[Int]]
                      -> LazySTM.TVar m [[Int]]
                      -> LazySTM.TVar m [[Int]]
                      -> TemperatureBundle
                          ([MiniProtocolWithExpandedCtx
                              InitiatorResponderMode peerAddr ByteString m () ()])
    serverApplication hotRequestsVar
                      warmRequestsVar
                      establishedRequestsVar
                      = TemperatureBundle {
        withHot = WithHot
          [ let miniProtocolNum = Mux.MiniProtocolNum 1
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    miniProtocolNum
                    hotRequestsVar
               }
          ],
        withWarm = WithWarm
          [ let miniProtocolNum = Mux.MiniProtocolNum 2
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    miniProtocolNum
                    warmRequestsVar
              }
          ],
        withEstablished = WithEstablished
          [ let miniProtocolNum = Mux.MiniProtocolNum 3
            in MiniProtocol {
                miniProtocolNum,
                miniProtocolLimits = Mux.MiniProtocolLimits maxBound,
                miniProtocolRun =
                  reqRespInitiatorAndResponder
                    (Mux.MiniProtocolNum 3)
                    establishedRequestsVar
              }
          ]
      }

    reqRespInitiatorAndResponder
      :: Mux.MiniProtocolNum
      -> LazySTM.TVar m [[Int]]
      -> RunMiniProtocolWithExpandedCtx
           InitiatorResponderMode peerAddr ByteString m () ()
    reqRespInitiatorAndResponder protocolNum requestsVar =
      InitiatorAndResponderProtocol
        (mkMiniProtocolCbFromPeer
          (\_ctx -> ( ("Initiator",protocolNum,) `contramap` debugTracer -- TraceSendRecv
                    , codecReqResp @Int @Int
                    , Effect $ do
                        reqs <-
                          atomically $ do
                            requests <- LazySTM.readTVar requestsVar
                            case requests of
                              (reqs : rest) -> do
                                LazySTM.writeTVar requestsVar rest $> reqs
                              [] -> pure []
                        pure $ reqRespClientPeer (reqRespClient reqs)
                    )
          ))
        (mkMiniProtocolCbFromPeer
          (\_ctx -> ( ("Responder",protocolNum,) `contramap` debugTracer -- TraceSendRecv
                    , codecReqResp @Int @Int
                    , Effect $ reqRespServerPeer <$> reqRespServerId
                    )
          ))

    reqRespServerId :: m (ReqRespServer Int Int m ())
    reqRespServerId = pure go
      where
        go =
          ReqRespServer {
              recvMsgReq  = \req -> pure (req, go),
              recvMsgDone = pure ()
            }




-- | Run all initiator mini-protocols and collect results.
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
    -> Mux.Mux muxMode m
    -> (forall pt. SingProtocolTemperature pt -> ExpandedInitiatorContext addr m)
    -> OuroborosBundleWithExpandedCtx muxMode addr ByteString m a b
    -> m (Maybe (TemperatureBundle [a]))
runInitiatorProtocols
    singMuxMode mux getContext
    (TemperatureBundle (WithHot hotPtcls)
                       (WithWarm warmPtcls)
                       (WithEstablished establishedPtcls)) = do
      -- start all protocols
      hotSTMs <- traverse (runInitiator SingHot) hotPtcls
      warmSTMs <- traverse (runInitiator SingWarm) warmPtcls
      establishedSTMs <- traverse (runInitiator SingEstablished) establishedPtcls

      -- await for their termination
      hotRes <- traverse atomically hotSTMs
      warmRes <- traverse atomically warmSTMs
      establishedRes <- traverse atomically establishedSTMs
      case (partitionEithers hotRes, partitionEithers warmRes, partitionEithers establishedRes) of
        ((_ : _, _), _, _) -> return Nothing
        (_, (_ : _, _), _) -> return Nothing
        (_, _, (_ : _, _)) -> return Nothing
        (([], hot),
           ([], warm),
             ([], established)) ->
          return
            . Just
            $ TemperatureBundle
                (WithHot hot)
                (WithWarm warm)
                (WithEstablished established)
  where
    runInitiator :: SingProtocolTemperature pt
                 -> MiniProtocolWithExpandedCtx muxMode addr ByteString m a b
                 -> m (STM m (Either SomeException a))
    runInitiator sing ptcl =
        Mux.runMiniProtocol
          mux
          (miniProtocolNum ptcl)
          (case singMuxMode of
            SingInitiatorMode          -> Mux.InitiatorDirectionOnly
            SingInitiatorResponderMode -> Mux.InitiatorDirection)
          Mux.StartEagerly
          (runMiniProtocolCb
            (case miniProtocolRun ptcl of
              InitiatorProtocolOnly initiator           -> initiator
              InitiatorAndResponderProtocol initiator _ -> initiator)
            (getContext sing))


-- | Bidirectional send and receive.
--
bidirectionalExperiment
    :: forall peerAddr socket.
       ( Ord peerAddr
       , Show peerAddr
       , Typeable peerAddr
       , Eq peerAddr
       )
    => Snocket IO socket peerAddr
    -> Mux.MakeBearer IO socket
    -> socket
    -> DiffTime
    -> DiffTime
    -> peerAddr
    -> peerAddr
    -> ClientAndServerData
    -> IO ()
bidirectionalExperiment
    snocket makeBearer socket0
    protocolIdleTimeout
    timeWaitTimeout
    localAddr remoteAddr
    clientAndServerData = do
      stdGen <- Random.newStdGen
      withBidirectionalConnectionManager
        snocket makeBearer socket0
        protocolIdleTimeout timeWaitTimeout
        (Just localAddr) stdGen clientAndServerData $
        \connectionManager _serverAddr -> forever' $ do
          -- runInitiatorProtocols returns a list of results per each protocol
          -- in each bucket (warm \/ hot \/ established); but we run only one
          -- mini-protocol. We can use `concat` to flatten the results.
          connHandle <-
                connect 10 connectionManager
          case connHandle of
            Connected connId _ (Handle mux muxBundle controlMessageBundle _) -> do
              traceWith debugTracer ( "initiator-loop"
                                    , "connected"
                                    )
              _ <-
                runInitiatorProtocols
                  SingInitiatorResponderMode
                  mux
                  (\tok -> ExpandedInitiatorContext {
                             eicConnectionId    = connId,
                             eicControlMessage  = readTVar
                                                . projectBundle tok
                                                $ controlMessageBundle,
                             eicIsBigLedgerPeer = IsNotBigLedgerPeer
                           })
                  muxBundle
              res <-
                unregisterOutboundConnection
                  connectionManager remoteAddr
              case res of
                UnsupportedState inState -> do
                  traceWith debugTracer ( "initiator-loop"
                                        , "unregisterOutboundConnection in unsupported state "
                                        , inState
                                        )
                  throwIO
                    (userError
                      . concat
                      $ [ "bidirectionalExperiment: unregigisterOutboundConnection "
                        , show inState
                        ])
                TerminatedConnection inState -> do
                  traceWith debugTracer ( "initiator-loop"
                                        , "unregisterOutboundConnection in unsupported state "
                                        , inState
                                        )
                  throwIO
                    (userError
                      . concat
                      $ [ "bidirectionalExperiment: unregigisterOutboundConnection "
                        , show inState
                        ])
                OperationSuccess _ ->
                  traceWith debugTracer ( "initiator-loop"
                                        , "unregistered"
                                        )
            Disconnected _ err -> do
              traceWith debugTracer ( "initiator-loop"
                                    , "connect error"
                                    , err
                                    )
              throwIO (userError $ "bidirectionalExperiment: " ++ show err)

          x <- Random.randomRIO (1_000 :: Int, 10_000)
          traceWith debugTracer ( "initiator-loop"
                                , "delay"
                                , x
                                )
          threadDelay (realToFrac x / 1_000)
  where
    connect :: Int
            -> ConnectionManagerWithExpandedCtx
                 InitiatorResponderMode
                 socket peerAddr UnversionedProtocolData
                 UnversionedProtocol ByteString
                 IO () ()
            -> IO (Connected peerAddr
                            (HandleWithExpandedCtx
                              InitiatorResponderMode peerAddr
                              UnversionedProtocolData ByteString IO () ())
                            (HandleError
                              InitiatorResponderMode
                              UnversionedProtocol))
    connect n cm | n <= 1 =
      requestOutboundConnection cm remoteAddr
    connect n cm =
      requestOutboundConnection cm remoteAddr
        `catch` \(_ :: IOException) -> threadDelay 1
                                    >> connect (pred n) cm
        `catch` \(_ :: MuxError)    -> threadDelay 1
                                    >> connect (pred n) cm

type Addr = String
type Port = String

data Options = Options {
    localAddr           :: Addr,
    localPort           :: Port,
    remoteAddr          :: Addr,
    remotePort          :: Port,
    seed                :: Maybe Int,
    protocolIdleTimeout :: DiffTime,
    timeWaitTimeout     :: DiffTime
  }

defaultPort :: Port
defaultPort = "3000"

optionParser :: Parser Options
optionParser =
    Options
      <$> strOption
           (  long "local-addr"
           <> metavar "ADDR"
           <> help "local address"
           )
      <*> strOption
           (  long "local-port"
           <> metavar "PORT"
           <> help "local port"
           <> value defaultPort
           <> showDefault
           )
      <*> strOption
           (  long "remote-addr"
           <> metavar "ADDR"
           <> help "remote address"
           <> showDefault
           )
      <*> strOption
           (  long "remote-port"
           <> metavar "PORT"
           <> help "remote port"
           <> value defaultPort
           <> showDefault
           )
      <*> (optional $ option auto
                          (  long "seed"
                          <> short 's'
                          <> metavar "SEED"
                          <> help (concat [ "seed used to generate random data, "
                                          , "if not given system stdgen will be used"
                                          ])
                          )
           )
      <*> option (realToFrac @Float <$> auto)
            (  long "protocol-idle-timeout"
            <> short 'i'
            <> metavar "TIME"
            <> help "responder inactivity timeout in seconds"
            <> value 5
            <> showDefault
            )
      <*> option (realToFrac @Float <$> auto)
            (  long "wait-time-timeout"
            <> short 'w'
            <> metavar "TIME"
            <> help "TCP's WAIT_TIME timeout in seconds"
            <> value 60
            <> showDefault
            )


run :: (Addr, Port)
    -> (Addr, Port)
    -> DiffTime -- ^ protocol idle timeout
    -> DiffTime -- ^ wait-time timeout
    -> ClientAndServerData
    -> IO ()
run localAddr remoteAddr protocolIdleTimeout timeWaitTimeout data_ =
    withIOManager $ \iomgr ->
      bracket
        (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        Socket.close
        $ \socket -> do
          associateWithIOManager iomgr (Right socket)
          Socket.setSocketOption socket Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          Socket.setSocketOption socket Socket.ReusePort 1
#endif
          let hints = Socket.defaultHints
                        { Socket.addrFlags = [ Socket.AI_ADDRCONFIG
                                             , Socket.AI_PASSIVE
                                             ] }
          addrLocal  : _ <- Socket.getAddrInfo (Just hints)
                                               (Just $ fst localAddr)
                                               (Just $ snd localAddr)
          addrRemote : _ <- Socket.getAddrInfo (Just hints)
                                               (Just $ fst remoteAddr)
                                               (Just $ snd remoteAddr)
          Socket.bind socket (Socket.addrAddress addrLocal)
          Socket.listen socket 10

          bidirectionalExperiment
            (socketSnocket iomgr)
            Mux.makeSocketBearer
            socket
            protocolIdleTimeout
            timeWaitTimeout
            (Socket.addrAddress addrLocal)
            (Socket.addrAddress addrRemote)
            data_

main :: IO ()
main = do
    Options { localAddr,
              localPort,
              remoteAddr,
              remotePort,
              seed,
              protocolIdleTimeout,
              timeWaitTimeout
            }
      <- execParser $ info (optionParser <**> helper)
                           fullDesc
    data_ <-
      case seed of
        Just a  -> pure $ genClientAndServerData (Random.mkStdGen a) 10
        Nothing -> (\g -> genClientAndServerData g 10) <$> Random.newStdGen
    run (localAddr,  localPort)
        (remoteAddr, remotePort)
        protocolIdleTimeout
        timeWaitTimeout
        data_


--
-- Utils
--


forever' :: MonadCatch m => m a -> m b
forever' io = do
    _ <- io `catch` (\(_ :: IOException) -> forever' io)
            `catch` (\(_ :: MuxError)    -> forever' io)
    forever' io


debugTracer :: (MonadSay m, MonadTime m, Show a) => Tracer m a
debugTracer = Tracer $ \msg -> do
    t <- getCurrentTime
    say (show t ++ " " ++ show msg)
