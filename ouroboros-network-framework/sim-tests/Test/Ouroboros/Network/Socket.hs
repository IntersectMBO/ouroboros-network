{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Ouroboros.Network.Socket (tests) where

import Data.Bifoldable (bitraverse_)
import Data.ByteString.Lazy qualified as BL
import Data.List (mapAccumL)
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Void (Void)
#ifndef mingw32_HOST_OS
import System.Directory (removeFile)
import System.IO.Error
#endif
import Network.Socket qualified as Socket
#if defined(mingw32_HOST_OS)
import System.Win32.Async.Socket.ByteString.Lazy qualified as Win32.Async
           (sendAll)
#else
import Network.Socket.ByteString.Lazy qualified as Socket (sendAll)
#endif

import Control.Concurrent (ThreadId)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork hiding (ThreadId)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Tracer

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client (Client)
import Network.TypedProtocol.ReqResp.Client qualified as ReqResp
import Network.TypedProtocol.ReqResp.Codec.CBOR qualified as ReqResp
import Network.TypedProtocol.ReqResp.Examples qualified as ReqResp
import Network.TypedProtocol.ReqResp.Server qualified as ReqResp
import Network.TypedProtocol.ReqResp.Type qualified as ReqResp

import Ouroboros.Network.Context
import Ouroboros.Network.Driver
import Ouroboros.Network.IOManager
import Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Socket
-- TODO: remove Mx prefixes
import Ouroboros.Network.Mux
import Ouroboros.Network.Server.Simple qualified as Server.Simple

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Mux.Timeout
import Network.Mux.Types (MiniProtocolDir (..), RemoteClockModel (..))
import Network.Mux.Types qualified as Mx

import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version

import Test.Ouroboros.Network.Orphans ()

import Test.QuickCheck
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf
import Text.Show.Functions ()

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Socket"
  [ testProperty "socket send receive IPv4"              prop_socket_send_recv_ipv4
#ifdef OUROBOROS_NETWORK_IPV6
  , after AllFinish "socket send receive IPv4" $
    testProperty "socket send receive IPv6"              prop_socket_send_recv_ipv6
#define LAST_IP_TEST "socket send receive IPv6"
#else
#define LAST_IP_TEST "socket send receive IPv4"
#endif
#ifndef mingw32_HOST_OS
  , testProperty "socket send receive Unix"              prop_socket_send_recv_unix
#endif
  , after AllFinish LAST_IP_TEST $
    testProperty "socket error during receive"           (withMaxSuccess 10 prop_socket_recv_error)
  , after AllFinish LAST_IP_TEST $
    testProperty "socket error during send"              (withMaxSuccess 10 prop_socket_send_error)
  , after AllFinish "socket close during receive" $
    testProperty "socket client connection failure"      prop_socket_client_connect_error
  ]
#undef LAST_IP_TEST

activeMuxTracer :: Tracer IO a
activeMuxTracer = nullTracer
--activeMuxTracer = _verboseTracer -- Dump log messages to stdout.

defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

-- |
-- Allow to run a singly req-resp protocol.
--
testProtocols2 :: RunMiniProtocolWithMinimalCtx appType () addr bytes m a b
               -> OuroborosApplicationWithMinimalCtx appType () addr bytes m a b
testProtocols2 reqResp =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 4,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = reqResp
      }
    ]


--
-- Properties
--

-- | Send and receive over IPv4
prop_socket_send_recv_ipv4
  :: (Int -> Int -> (Int, Int))
  -> [Int]
  -> Property
prop_socket_send_recv_ipv4 f xs = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    client:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    prop_socket_send_recv (Socket.addrAddress client)
                          (Socket.addrAddress server)
                          configureSocket
                          f xs


#ifdef OUROBOROS_NETWORK_IPV6
-- | Send and receive over IPv6
prop_socket_send_recv_ipv6 :: (Int ->  Int -> (Int, Int))
                           -> [Int]
                           -> Property
prop_socket_send_recv_ipv6 request response = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6061")
    client:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "0")
    prop_socket_send_recv (Socket.addrAddress client)
                          (Socket.addrAddress server)
                          configureSocket request response
#endif

#ifndef mingw32_HOST_OS
prop_socket_send_recv_unix :: (Int ->  Int -> (Int, Int))
                           -> [Int]
                           -> Property
prop_socket_send_recv_unix request response = ioProperty $ do
    let serverName = "server_socket.test"
    let clientName = "client_socket.test"
    cleanUp serverName
    cleanUp clientName
    let serverAddr = Socket.AddrInfo [] Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                         (Socket.SockAddrUnix serverName) Nothing
        clientAddr = Socket.AddrInfo [] Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                         (Socket.SockAddrUnix clientName) Nothing
    r <- prop_socket_send_recv (Socket.addrAddress clientAddr)
                               (Socket.addrAddress serverAddr)
                               mempty request response
    cleanUp serverName
    cleanUp clientName
    return r
  where
    cleanUp name = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile name)
                  (\_ -> return ())
#endif

-- | Verify that an initiator and a responder can send and receive messages from each other
-- over a TCP socket. Large DummyPayloads will be split into smaller segments and the
-- testcases will verify that they are correctly reassembled into the original message.
prop_socket_send_recv :: Socket.SockAddr
                      -> Socket.SockAddr
                      -> (Socket.Socket -> Maybe Socket.SockAddr -> IO ())
                      -- ^ configure a socket
                      -> (Int -> Int -> (Int, Int))
                      -> [Int]
                      -> IO Bool
prop_socket_send_recv initiatorAddr responderAddr configureSock f xs =
    withIOManager $ \iomgr -> do

    cv <- newEmptyTMVarIO
    sv <- newEmptyTMVarIO

    {- The siblingVar is used by the initiator and responder to wait on each other before exiting.
     - Without this wait there is a risk that one side will finish first causing the Muxbearer to
     - be torn down and the other side exiting before it has a chance to write to its result TMVar.
     -}
    siblingVar <- newTVarIO 2

    let -- Server Node; only req-resp server
        responderApp :: OuroborosApplicationWithMinimalCtx
                          Mx.ResponderMode () Socket.SockAddr BL.ByteString IO Void ()
        responderApp = testProtocols2 reqRespResponder

        reqRespResponder =
          ResponderProtocolOnly $
          MiniProtocolCb $ \_ctx channel -> do
            (r, trailing) <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r
            waitSibling siblingVar
            pure ((), trailing)

        -- Client Node; only req-resp client
        initiatorApp :: OuroborosApplicationWithMinimalCtx
                          Mx.InitiatorMode () Socket.SockAddr BL.ByteString IO () Void
        initiatorApp = testProtocols2 reqRespInitiator

        reqRespInitiator =
          InitiatorProtocolOnly $
          MiniProtocolCb $ \_ctx channel -> do
            (r, trailing) <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs))
            atomically $ putTMVar cv r
            waitSibling siblingVar
            pure ((), trailing)

    let snocket = socketSnocket iomgr
    bracket (open snocket (Snocket.addrFamily snocket responderAddr))
            (close snocket) $ \sock -> do
      bind snocket sock responderAddr
      listen snocket sock
      res <-
        Server.Simple.with
          snocket
          Mx.makeSocketBearer
          (\fd addr -> configureSock fd (Just addr))
          responderAddr
          HandshakeArguments {
            haHandshakeTracer  = nullTracer,
            haHandshakeCodec   = unversionedHandshakeCodec,
            haVersionDataCodec = unversionedProtocolDataCodec,
            haAcceptVersion    = acceptableVersion,
            haQueryVersion     = queryVersion,
            haTimeLimits       = noTimeLimitsHandshake
          }
          (unversionedProtocol (SomeResponderApplication responderApp))
          $ \_ _ -> do
            void $ connectToNode
              snocket
              Mx.makeSocketBearer
              ConnectToArgs {
                ctaHandshakeCodec      = unversionedHandshakeCodec,
                ctaHandshakeTimeLimits = noTimeLimitsHandshake,
                ctaVersionDataCodec    = unversionedProtocolDataCodec,
                ctaConnectTracers      = NetworkConnectTracers activeMuxTracer nullTracer,
                ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
              }
              (`configureSock` Nothing)
              (unversionedProtocol initiatorApp)
              (Just initiatorAddr)
              responderAddr
            atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv
      return (res == mapAccumL f 0 xs)

  where
    waitSibling :: StrictTVar IO Int -> IO ()
    waitSibling cntVar = do
        atomically $ modifyTVar cntVar (\a -> a - 1)
        atomically $ do
            cnt <- readTVar cntVar
            unless (cnt == 0) retry

data RecvErrorType = RecvSocketClosed | RecvSDUTimeout deriving (Eq, Show)

instance Arbitrary RecvErrorType where
    arbitrary = oneof [pure RecvSocketClosed, pure RecvSDUTimeout]

-- |
-- Verify that we raise the correct exception in case a socket closes or a timeout during
-- a read.
prop_socket_recv_error :: (Int -> Int -> (Int, Int))
                       -> RecvErrorType
                       -> Property
prop_socket_recv_error f rerr =
    ioProperty $
    withIOManager $ \iomgr -> do

    sv   <- newEmptyTMVarIO

    let app :: OuroborosApplicationWithMinimalCtx
                 Mx.ResponderMode () Socket.SockAddr BL.ByteString IO Void ()
        app = testProtocols2 reqRespResponder

        reqRespResponder =
          ResponderProtocolOnly $
          MiniProtocolCb $ \_ctx channel -> do
            (r, trailing) <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r
            pure ((), trailing)

    let snocket :: SocketSnocket
        snocket = socketSnocket iomgr

    bracket
      (open snocket (SocketFamily Socket.AF_INET))
      (close snocket)
      $ \sd -> do
        -- bind the socket
        muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
        Socket.setSocketOption sd Socket.ReuseAddr 1
        Socket.bind sd (Socket.addrAddress muxAddress)
        addr <- Socket.getSocketName sd
        Socket.listen sd 1
        lock <- newEmptyTMVarIO

        withAsync
          (
              -- accept a connection and start mux on it
              bracket
                (accept snocket sd >>= runAccept)
                (bitraverse_ Socket.close pure . fst)
                $ \(accepted, _acceptNext) -> case accepted of
                  AcceptFailure err -> throwIO err
                  Accepted sd' _ -> do
                    remoteAddress <- Socket.getPeerName sd'
                    let timeout = if rerr == RecvSDUTimeout then 0.10
                                                            else (-1) -- No timeout
                        connectionId = ConnectionId {
                            localAddress = Socket.addrAddress muxAddress,
                            remoteAddress
                          }
                    bearer <- Mx.getBearer Mx.makeSocketBearer timeout nullTracer sd' Nothing
                    _ <- async $ do
                      threadDelay 0.1
                      atomically $ putTMVar lock ()
                    mux <- Mx.new (toMiniProtocolInfos (\_ _ -> Nothing) app)
                    let respCtx = ResponderContext connectionId
                    resOps <- sequence
                      [ Mx.runMiniProtocol
                          mux
                          miniProtocolNum
                          miniProtocolDir
                          Mx.StartEagerly
                          (\a -> do
                            r <- action a
                            return (r, Nothing)
                          )
                      | MiniProtocol{miniProtocolNum, miniProtocolRun}
                          <- getOuroborosApplication app
                      , (miniProtocolDir, action) <-
                          case miniProtocolRun of
                            ResponderProtocolOnly responder ->
                              [(Mx.ResponderDirectionOnly, void . runMiniProtocolCb responder respCtx)]
                            ResponderProtocolOnlyWithState _ ->
                              error "prop_socket_recv_error: ResponderProtocolOnlyWithState - not supported"
                      ]

                    withAsync (Mx.run nullTracer mux bearer) $ \aid -> do
                      _ <- atomically $ runFirstToFinish $ foldMap FirstToFinish resOps
                      Mx.stop mux
                      wait aid
          )
          $ \muxAsync -> do

          -- connect to muxAddress
          sd' <- openToConnect snocket addr
          _ <- connect snocket sd' addr

#if defined(mingw32_HOST_OS)
          Win32.Async.sendAll sd' $ BL.singleton 0xa
#else
          Socket.sendAll sd' $ BL.singleton 0xa
#endif

          when (rerr == RecvSocketClosed) $ do
            atomically $ takeTMVar lock
            Socket.close sd'

          res <- waitCatch muxAsync
          result <- case res of
              Left e  ->
                  case fromException e of
                        Just me -> return $
                            case me of
                                 Mx.BearerClosed {} -> rerr === RecvSocketClosed
                                 Mx.SDUReadTimeout  -> rerr === RecvSDUTimeout
                                 _                  -> counterexample (show me) False
                        Nothing -> return $ counterexample (show e) False
              Right _ -> return $ counterexample "expected error" False

          when (rerr /= RecvSocketClosed) $ Socket.close sd'
          return result

data SendErrorType = SendSocketClosed | SendSDUTimeout deriving (Eq, Show)

instance Arbitrary SendErrorType where
    arbitrary = oneof [pure SendSocketClosed, pure SendSDUTimeout]

-- |
-- Verify that we raise the correct exception in case a socket closes or a timeout during
-- a write.
prop_socket_send_error :: SendErrorType
                       -> Property
prop_socket_send_error rerr =
    ioProperty $
    withIOManager $ \iomgr -> do

    let snocket :: SocketSnocket
        snocket = socketSnocket iomgr

    bracket
      (open snocket (SocketFamily Socket.AF_INET))
      (close snocket)
      $ \sd -> do
        -- bind the socket
        muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
        Socket.setSocketOption sd Socket.ReuseAddr 1
        Socket.bind sd (Socket.addrAddress muxAddress)
        addr <- Socket.getSocketName sd
        Socket.listen sd 1

        withAsync
          (
              -- accept a connection and start mux on it
              bracket
                (accept snocket sd >>= runAccept)
                (bitraverse_ Socket.close pure . fst)
                $ \(accepted, _acceptNext) -> case accepted of
                  AcceptFailure err -> throwIO err
                  Accepted sd' _ -> do
                    let sduTimeout = if rerr == SendSDUTimeout then 0.10
                                                               else (-1) -- No timeout
                        blob = BL.pack $ replicate 0xffff 0xa5
                    bearer <- Mx.getBearer Mx.makeSocketBearer sduTimeout nullTracer sd' Nothing
                    withTimeoutSerial $ \timeout ->
                      -- send maximum mux sdus until we've filled the window.
                      replicateM 100 $ do
                        ((), Nothing) <$ Mx.write bearer timeout (wrap blob ResponderDir (MiniProtocolNum 0))
          )
          $ \muxAsync -> do

          sd' <- openToConnect snocket addr
          -- connect to muxAddress
          _ <- connect snocket sd' addr

          when (rerr == SendSocketClosed) $ Socket.close sd'


          res <- waitCatch muxAsync
          result <- case res of
              Left e  ->
                  case fromException e of
                        Just me -> return $
                            case me of
                                 Mx.IOException {}  -> rerr === SendSocketClosed
                                 Mx.SDUWriteTimeout -> rerr === SendSDUTimeout
                                 _                  -> counterexample (show me) False
                        Nothing -> return $ counterexample (show e) False
              Right _ -> return $ property False

          when (rerr /= SendSocketClosed) $ Socket.close sd'
          return result
  where
      -- wrap a 'ByteString' as 'Mx.SDU'
      wrap :: BL.ByteString -> MiniProtocolDir -> MiniProtocolNum -> Mx.SDU
      wrap blob ptclDir ptclNum = Mx.SDU {
            -- it will be filled when the 'MuxSDU' is send by the 'bearer'
            Mx.msHeader = Mx.SDUHeader {
                Mx.mhTimestamp = RemoteClockModel 0,
                Mx.mhNum       = ptclNum,
                Mx.mhDir       = ptclDir,
                Mx.mhLength    = fromIntegral $ BL.length blob
              },
            Mx.msBlob = blob
          }

prop_socket_client_connect_error :: (Int -> Int -> (Int, Int))
                                 -> [Int]
                                 -> Property
prop_socket_client_connect_error _ xs =
    ioProperty $
    withIOManager $ \iomgr -> do
    serverAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    clientAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    cv <- newEmptyTMVarIO

    let app :: OuroborosApplicationWithMinimalCtx
                 Mx.InitiatorMode () Socket.SockAddr BL.ByteString IO () Void
        app = testProtocols2 reqRespInitiator

        reqRespInitiator =
          InitiatorProtocolOnly $
          MiniProtocolCb $ \_ctx channel -> do
            (_, trailing) <- runPeer nullTracer
                    ReqResp.codecReqResp
                    channel
                    (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs)
                            :: Client (ReqResp.ReqResp Int Int)
                                      NonPipelined ReqResp.StIdle IO [Int])
            ((), trailing)
              <$ atomically (putTMVar cv ())

    (res :: Either IOException Bool)
      <- try $ False <$ connectToNode
        (socketSnocket iomgr)
        Mx.makeSocketBearer
        ConnectToArgs {
          ctaHandshakeCodec      = unversionedHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec    = unversionedProtocolDataCodec,
          ctaConnectTracers      = NetworkConnectTracers activeMuxTracer nullTracer,
          ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
        }
        (`configureSocket` Nothing)
        (unversionedProtocol app)
        (Just $ Socket.addrAddress clientAddr)
        (Socket.addrAddress serverAddr)

    -- XXX Disregarding the exact exception type
    pure $ either (const True) id res



data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !UTCTime
    , wtatWithinThread :: !ThreadId
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

_verboseTracer :: Show a => Tracer IO a
_verboseTracer = threadAndTimeTracer $ showTracing stdoutTracer

threadAndTimeTracer :: Tracer IO (WithThreadAndTime a) -> Tracer IO a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getCurrentTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now tid s
