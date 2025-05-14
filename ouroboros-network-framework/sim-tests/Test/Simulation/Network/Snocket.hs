{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- TODO: Create a 'snocket' package, in order to avoid having to have
-- ouroboros-network-testing as a dependency for this cabal library.
module Test.Simulation.Network.Snocket
  ( tests
  , toBearerInfo
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (Tracer (..), contramap, contramapM, nullTracer)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as Serialise

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Printf

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Snocket
import Simulation.Network.Snocket

import Network.Mux as Mx
import Network.Mux.Types as Mx
import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Type

import Test.Ouroboros.Network.Data.AbsBearerInfo
import Test.Ouroboros.Network.Orphans ()

import Test.QuickCheck hiding (Result (..))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Simulation.Network.Snocket"
    [ testProperty "client-server" prop_client_server
    , testProperty "connect_to_accepting_socket"
                   prop_connect_to_accepting_socket
    , testProperty "connect_and_not_close"
                   prop_connect_and_not_close
    , testProperty "connect_to_not_accepting_socket"
                   prop_connect_to_not_accepting_socket
    , testProperty "connect_to_uninitialised_socket"
                   prop_connect_to_uninitialised_socket
    , testProperty "connect_to_not_listening_socket"
                   prop_connect_to_not_listening_socket
    , testProperty "simultaneous_open"
                   prop_simultaneous_open
    , testProperty "self connect"
                   prop_self_connect
    ]

type TestAddr      = TestAddress Int
type TestFD      m = FD m TestAddr
type TestSnocket m = Snocket m (TestFD m) TestAddr

pingServer :: forall payload m. Applicative m
           => ReqRespServer payload payload m ()
pingServer = ReqRespServer {
        recvMsgReq  = \req -> pure (req, pingServer),
        recvMsgDone = pure ()
      }

pingClient :: ( Applicative m
              , Eq payload
              )
           => [payload]
           -> ReqRespClient payload payload m Bool
pingClient = go True
  where
    go !res []       = SendMsgDone (pure res)
    go !res (a : as) = SendMsgReq a $ \a' -> pure (go (a == a' && res) as)


codecReqResp :: forall req resp m.
                ( MonadST m
                , Serialise req
                , Serialise resp
                )
             => Codec (ReqResp req resp) CBOR.DeserialiseFailure m ByteString
codecReqResp = mkCodecCborLazyBS encodeMsg decodeMsg
  where
    encodeMsg :: forall st st'.
                 Message (ReqResp req resp) st st'
              -> CBOR.Encoding
    encodeMsg (MsgReq req) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> Serialise.encode req
    encodeMsg (MsgResp resp) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> Serialise.encode resp
    encodeMsg MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 3

    decodeMsg :: forall s (st :: ReqResp req resp).
                 ActiveState st
              => StateToken st
              -> CBOR.Decoder s (SomeMessage st)
    decodeMsg stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (SingIdle, 2, 1) -> do
          payload <- Serialise.decode
          return (SomeMessage $ MsgReq payload)
        (SingBusy, 2, 2) -> do
          payload <- Serialise.decode
          return (SomeMessage $ MsgResp payload)
        (SingIdle, 1, 3) ->
          return (SomeMessage MsgDone)
        (SingIdle, _, _) -> fail (printf "codecReqResp.StIdle: unexpected key (%d, %d)" key len)
        (SingBusy, _, _) -> fail (printf "codecReqResp.StBusy: unexpected key (%d, %d)" key len)
        (SingDone, _, _) -> notActiveState stok


untilSuccess :: ( MonadCatch m
                , MonadDelay m
                , MonadSay   m
                )
             => m a
             -> m a
untilSuccess go =
    go `catch` \(_ :: SomeException) -> do
      threadDelay 1
      untilSuccess go


clientServerSimulation
    :: forall m addr payload.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadDelay       m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadSay         m
       , MonadST          m
       , MonadThrow  (STM m)
       , MonadTimer       m

       , Serialise payload
       , Eq payload
       , Show payload
       , Ord (Async m ())
       )
    => [payload]
    -> m (Either (TestError addr) ())
clientServerSimulation payloads =
    withSnocket nullTracer (toBearerInfo absNoAttenuation) Map.empty
    $ \snocket _ ->
      withAsync (server snocket) $ \_serverAsync -> do
        res <- untilSuccess (client snocket)
        if res
           then return (Right ())
           else return (Left UnexpectedOutcome)
  where
    reqRespProtocolNum :: MiniProtocolNum
    reqRespProtocolNum = MiniProtocolNum 0

    serverAddr :: TestAddr
    serverAddr = TestAddress 1

    serverPeer :: Peer (ReqResp payload payload) AsServer NonPipelined StIdle m ()
    serverPeer = reqRespServerPeer pingServer

    clientPeer :: Peer (ReqResp payload payload) AsClient NonPipelined StIdle m Bool
    clientPeer = reqRespClientPeer (pingClient payloads)

    server :: TestSnocket m
           -> m ()
    server snocket = do
        labelThisThread "server"
        threadsVar <- newTVarIO Set.empty
        bracket (open snocket TestFamily)
                (close snocket)
                (\fd -> do
                  bind snocket fd serverAddr
                  listen snocket fd
                  accept snocket fd >>= acceptLoop threadsVar)
          `finally` do
            threads <- readTVarIO threadsVar
            traverse_ cancel threads
      where
        acceptLoop :: StrictTVar m (Set (Async m ()))
                   -> Accept m (TestFD m) TestAddr
                   -> m ()
        acceptLoop threadsVar accept0 = do
          (accepted, accept1) <- runAccept accept0
          case accepted of
            Accepted fd' remoteAddr -> do
              bearer <- getBearer makeFDBearer 10 nullTracer fd' Nothing
              thread <- async $ handleConnection bearer remoteAddr
                                `finally`
                               close snocket fd'
              atomically $
                modifyTVar threadsVar (Set.insert thread)
              acceptLoop threadsVar accept1
            AcceptFailure _err ->
              acceptLoop threadsVar accept1

        handleConnection :: Mx.Bearer m -> TestAddr -> m ()
        handleConnection bearer remoteAddr = do
          labelThisThread "server-handler"
          bracket
            (Mx.new [ MiniProtocolInfo {
                        miniProtocolNum        = reqRespProtocolNum,
                        miniProtocolDir        = Mx.ResponderDirectionOnly,
                        miniProtocolLimits     = Mx.MiniProtocolLimits maxBound,
                        miniProtocolCapability = Nothing
                      }
                    ])
            Mx.stop
            $ \mux -> do
              let connId = ConnectionId {
                      localAddress  = serverAddr,
                      remoteAddress = remoteAddr
                    }
                  tr = (connId,) `contramap`
                       traceTime (   Tracer (say . show)
                                  -- <> Tracer Debug.traceShowM
                                 )

              resSTM <- runMiniProtocol
                          mux reqRespProtocolNum
                          Mx.ResponderDirectionOnly
                          Mx.StartOnDemand
                          (\channel -> runPeer tr codecReqResp
                                               channel
                                               serverPeer)
              withAsync
                (do labelThisThread "server-mux"
                    Mx.run (("server", connId,)
                             `contramap`
                             traceTime (Tracer (say . show)))
                           mux bearer)
                $ \_muxThread -> do
                  res <- atomically resSTM
                  say $ "SERVER HANDLER " ++ show res


    client :: TestSnocket m
           -> m Bool
    client snocket = do
        labelThisThread "client"
        bracket (openToConnect snocket serverAddr)
                (close snocket)
                $ \fd -> do
                  connect snocket fd serverAddr
                  mux <- Mx.new [ MiniProtocolInfo {
                                    miniProtocolNum        = reqRespProtocolNum,
                                    miniProtocolDir        = Mx.InitiatorDirectionOnly,
                                    miniProtocolLimits     = Mx.MiniProtocolLimits maxBound,
                                    miniProtocolCapability = Nothing
                                  }
                                ]
                  localAddr <- getLocalAddr snocket fd
                  let connId = ConnectionId {
                          localAddress  = localAddr,
                          remoteAddress = serverAddr
                        }
                      tr = (connId,) `contramap`
                           traceTime (   Tracer (say . show)
                                      -- <> Tracer Debug.traceShowM
                                     )
                  resSTM <- runMiniProtocol
                              mux reqRespProtocolNum
                              InitiatorDirectionOnly
                              StartEagerly
                              (\channel -> runPeer tr codecReqResp
                                                   channel
                                                   clientPeer)
                  bearer <- Mx.getBearer makeFDBearer 10 nullTracer fd Nothing

                  -- kill mux as soon as the client returns
                  withAsync
                    (do labelThisThread "client-mux"
                        Mx.run (("client", connId,)
                                 `contramap`
                                 traceTime (Tracer (say . show)))
                               mux bearer)
                    $ \_ -> do
                      res <- atomically resSTM
                      Mx.stop mux
                      case res of
                        (Left err) -> throwIO err
                        (Right b)  -> return b
                        -- Right _         -> error "client: mux died"

--
-- Auxiliary
--

-- TODO: should be defined where 'AbsBearerInfo' is defined, but it also
-- requires access to 'ouroboros-network-framework' where 'BearerInfo' is
-- defined.  This can be fixed by moving `ouroboros-network-testing` to
-- `ouroboros-network-framework:testlib`.
--
toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = (\(errDelay, ioErr) -> (delay errDelay, ioErr)) <$> abiAcceptFailure abi,
        biSDUSize              = toSduSize (abiSDUSize abi)
      }

--
-- Properties
--

data TestError addr = UnexpectedOutcome
                    | UnexpectedError SomeException
                    | UnexpectedlyReleasedListeningSockets
                    | DidNotTimeout
                    | DidNotComplainAboutNoSuchListeningSockets
                    | DoNotExistInNetworkState addr addr
                    -- ^ LocalAddress, RemoteAddress
  deriving Show

verify_no_error
  :: (forall s . IOSim s (Either (TestError (TestAddress Int)) ()))
  -> Property
verify_no_error sim =
  let tr = runSimTrace sim
   in case traceResult True tr of
     Left e -> counterexample
                (unlines
                  [ "=== Say Events ==="
                  , unlines (selectTraceEventsSay' tr)
                  , "=== Trace Events ==="
                  , unlines (show `map` traceEvents tr)
                  , "=== Error ==="
                  , show e ++ "\n"
                  ])
                False
     Right (Left e) -> counterexample
                        (unlines
                          [ "=== Say Events ==="
                          , unlines (selectTraceEventsSay' tr)
                          , "=== Trace Events ==="
                          , unlines (show `map` traceEvents tr)
                          , "=== Error ==="
                          , show e ++ "\n"
                          ])
                        False
     Right (Right _) -> property True

prop_client_server :: [ByteString] -> Property
prop_client_server payloads =
  verify_no_error sim
  where
    sim :: forall s addr . IOSim s (Either (TestError addr) ())
    sim = clientServerSimulation payloads

prop_connect_to_accepting_socket :: AbsBearerInfo -> Property
prop_connect_to_accepting_socket defaultBearerInfo =
    verify_no_error sim
  where
    serverAddr :: TestAddress Int
    serverAddr = TestAddress 0

    clientAddr :: TestAddress Int
    clientAddr = TestAddress 1

    sim :: forall s . IOSim s (Either (TestError (TestAddress Int)) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      $ \snocket getState ->
        withAsync
          (runServer serverAddr snocket (close snocket)
                     acceptOne (assertNetworkState clientAddr
                                                   serverAddr
                                                   getState))
          $ \serverAsync -> do
            _ <- runClient clientAddr serverAddr
                            snocket (close snocket)
            r <- wait serverAsync
            return $
              case (r, abiAcceptFailure defaultBearerInfo) of
                (Left (UnexpectedError e), Just (_, ioe)) ->
                  case fromException e :: Maybe IOError of
                    Just ioe' | ioe' == ioe
                              -> Right ()
                    _         -> r

                (Right {}, Just {}) -> Left UnexpectedOutcome
                _                   -> r


prop_connect_and_not_close :: AbsBearerInfo -> Property
prop_connect_and_not_close defaultBearerInfo =
    verify_no_error sim
  where
    sim :: forall s . IOSim s (Either (TestError (TestAddress Int)) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      (\snocket _ ->
        withAsync
          (runServer (TestAddress (0 :: Int)) snocket (\_ -> pure ())
                     acceptOne return)
          $ \serverAsync -> do
            res <- runClient (TestAddress 1) (TestAddress 0)
                            snocket (\_ -> pure ())
            _ <- wait serverAsync
            return res
      )
      `catch` \(err :: SomeException) ->
        -- Should error with NotReleasedListeningSockets
        case fromException err :: Maybe ResourceException of
          Just _ ->
            return (Right ())
          Nothing ->
            return (Left UnexpectedlyReleasedListeningSockets)

prop_connect_to_not_accepting_socket :: AbsBearerInfo -> Property
prop_connect_to_not_accepting_socket defaultBearerInfo =
    verify_no_error sim
  where
    sim :: forall s addr . IOSim s (Either (TestError addr) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      $ \snocket _ ->
        withAsync (runServer (TestAddress (0 :: Int)) snocket
                             (close snocket) loop return)
          $ \_ -> do
            res <- runClient (TestAddress 1) (TestAddress 0)
                            snocket (close snocket)
            case res of
              -- Should timeout
              Left _  -> return (Right ())
              Right _ -> return (Left DidNotTimeout)

    loop :: MonadDelay m => a -> m b
    loop a = do
      threadDelay 10000
      loop a

prop_connect_to_uninitialised_socket :: AbsBearerInfo -> Property
prop_connect_to_uninitialised_socket defaultBearerInfo =
    verify_no_error sim
  where
    sim :: forall s addr . IOSim s (Either (TestError addr) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      $ \snocket _ -> do
        res <- runClient (TestAddress (1 :: Int)) (TestAddress 0)
                         snocket (close snocket)
        case res of
          -- Should complain about no such listening socket
          Left _  -> return (Right ())
          Right _ -> return (Left DidNotComplainAboutNoSuchListeningSockets)

prop_connect_to_not_listening_socket :: AbsBearerInfo -> Property
prop_connect_to_not_listening_socket defaultBearerInfo =
    verify_no_error sim
  where
    sim :: forall s addr . IOSim s (Either (TestError addr) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      $ \snocket _ ->
        withAsync (runServerNotListening (TestAddress (0 :: Int)) snocket
                                         (close snocket) acceptOne)
          $ \_ -> do
            res <- runClient (TestAddress (1 :: Int)) (TestAddress 0)
                             snocket (close snocket)
            case res of
              -- Should complain about no such listening socket
              Left _  -> return (Right ())
              Right _ -> return (Left DidNotComplainAboutNoSuchListeningSockets)

    runServerNotListening
      :: ( MonadThread m
         , MonadThrow m
         )
      => TestAddress addr -- ^ Local Address
      -> Snocket m fd (TestAddress addr)
      -> (fd -> m ()) -- ^ Resource cleanup function (socket close)
      -> (m (Accept m fd (TestAddress addr)) -> m (Either err fd))
      -- ^ Accepting function
      -> m (Either err ())
    runServerNotListening localAddress snocket closeF acceptF = do
      labelThisThread "server"
      bracket (open snocket TestFamily)
              closeF
              (\fd -> do
                bind snocket fd localAddress
                mbFd <- acceptF (accept snocket fd)
                traverse_ (close snocket) mbFd
                return (void mbFd)
              )


prop_simultaneous_open :: AbsBearerInfo -> Property
prop_simultaneous_open defaultBearerInfo =
    verify_no_error sim
  where
    sim :: forall s . IOSim s (Either (TestError (TestAddress Int)) ())
    sim =
      withSnocket nullTracer (toBearerInfo defaultBearerInfo) Map.empty
      $ \snocket getState ->
        withAsync
          (listenAndConnect (TestAddress (0 :: Int)) (TestAddress 1)
                            snocket getState)
          $ \clientAsync -> do
            _ <- listenAndConnect (TestAddress 1) (TestAddress 0)
                                 snocket getState
            wait clientAsync


-- | Check that when we bind both outbound and inbound socket to the same
-- address, and connect the outbound to inbound:
--
-- * accept loop never returns
-- * the outbound socket acts as a mirror
--
-- This is how socket API behaves on Linux.
--
prop_self_connect :: ByteString -> Property
prop_self_connect payload =
  BL.length payload > 0 && BL.length payload <= 0xffff ==>
    runSimOrThrow sim
  where
    addr :: TestAddress Int
    addr = TestAddress 0

    sim :: forall s. IOSim s Property
    sim =
      withSnocket nullTracer noAttenuation Map.empty
      $ \snocket _getState ->
        withAsync (runServer addr snocket
                             (close snocket) acceptOne return)
          $ \serverThread -> do
            bracket (openToConnect snocket addr)
                    (close snocket)
                  $ \fd -> do
                    bind snocket fd addr
                    connect snocket fd addr
                    bearer <- getBearer makeFDBearer 10 nullTracer fd Nothing
                    let channel = bearerAsChannel bearer (MiniProtocolNum 0) InitiatorDir
                    send channel payload
                    payload' <- recv channel
                    threadDelay 1
                    serverResult <- atomically $
                                    (Just <$> waitSTM serverThread)
                                   `orElse`
                                   pure Nothing
                    return $ Just payload === payload'
                        .&&. isNothing serverResult


--
-- Utils
--

runServer
  :: ( MonadThread m
     , MonadThrow m
     )
  => TestAddress addr -- ^ Local Address
  -> Snocket m fd (TestAddress addr)
  -> (fd -> m ()) -- ^ Resource cleanup function (socket close)
  -> (m (Accept m fd (TestAddress addr)) -> m (Either err fd))
  -- ^ Accepting function
  -> (Either err fd -> m (Either err fd))
  -- ^ Assert NetworkState
  -> m (Either err ())
runServer localAddress snocket closeF acceptF assertState = do
    labelThisThread "server"
    bracket (open snocket TestFamily)
            closeF
            (\fd -> do
              bind snocket fd localAddress
              listen snocket fd
              mbFd <- acceptF (accept snocket fd)
              mbFd' <- assertState mbFd
              traverse_ closeF mbFd'
              return (void mbFd')
            )

acceptOne :: MonadMask m
          => m (Accept m fd addr)
          -> m (Either (TestError addr) fd)
acceptOne accept = mask_ $ do
  accept0 <- accept
  (accepted, _) <- runAccept accept0
  case accepted of
    Accepted fd' _ -> do
      return (Right fd')
    AcceptFailure err ->
      return (Left (UnexpectedError err))

runClient
  :: ( MonadThread m
     , MonadCatch m
     )
  => addr -- ^ Local Address
  -> addr -- ^ Remote Address
  -> Snocket m fd addr
  -> (fd -> m ()) -- ^ Resource cleanup function (socket close)
  -> m (Either (TestError addr) ())
runClient localAddress remoteAddress snocket closeF = do
    labelThisThread "client"
    bracket (openToConnect snocket localAddress)
            closeF
            $ \fd -> do
              bind snocket fd localAddress
              Right <$> connect snocket fd remoteAddress
              `catch` (\(e :: SomeException) ->
                  return (Left (UnexpectedError e)))

listenAndConnect
  :: ( MonadThread m
     , MonadCatch m
     , Ord addr
     )
  => TestAddress addr -- ^ Local Address
  -> TestAddress addr -- ^ Remote Address
  -> Snocket m fd (TestAddress addr)
  -> m (ObservableNetworkState (TestAddress addr))
  -> m (Either (TestError (TestAddress addr)) ())
listenAndConnect localAddress remoteAddress snocket getState = do
    labelThisThread "connectingServer"
    bracket (open snocket TestFamily)
            (close snocket)
            $ \fd -> do
              bind snocket fd localAddress
              listen snocket fd
              bracket (openToConnect snocket localAddress)
                      (close snocket)
                      $ \fd' -> do
                        bind snocket fd' localAddress
                        res <- (Right <$> connect snocket fd' remoteAddress)
                              `catch` (\(e :: SomeException) ->
                                return (Left (UnexpectedError e)))
                        assertNetworkState localAddress remoteAddress
                                            getState res

-- | Asserts that the local address and remote address pair exists in the
-- NetworkState.
assertNetworkState :: (Monad m, Ord addr)
                    => addr -- ^ Local Address
                    -> addr -- ^ Remote Address
                    -> m (ObservableNetworkState addr)
                    -> Either (TestError addr) b
                    -> m (Either (TestError addr) b)
assertNetworkState localAddress remoteAddress getState res = do
  us <- onsConnections <$> getState
  -- Important to use serverAddr as first argument
  let normalisedId = normaliseId (ConnectionId localAddress remoteAddress)
  case Map.lookup normalisedId us of
    Just _ -> return res
    Nothing -> case res of
      Right _ ->
        return (Left (DoNotExistInNetworkState localAddress
                                               remoteAddress))
      Left _ -> return res

traceTime :: MonadMonotonicTime m => Tracer m (Time, a) -> Tracer m a
traceTime = contramapM (\a -> (,a) <$> getMonotonicTime)
