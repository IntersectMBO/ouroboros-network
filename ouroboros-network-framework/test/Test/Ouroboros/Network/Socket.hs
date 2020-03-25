{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Ouroboros.Network.Socket (tests) where

import           Data.Functor ((<$))
import           Data.Int (Int64)
import           Data.Void (Void)
import           Data.List (mapAccumL)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Word
#ifndef mingw32_HOST_OS
import           System.Directory (removeFile)
import           System.IO.Error
#endif
import qualified Network.Socket as Socket
#if defined(mingw32_HOST_OS)
import qualified System.Win32.Async.Socket.ByteString.Lazy as Win32.Async (sendAll)
#else
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
#endif

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadTimer (threadDelay)
import           Control.Concurrent (ThreadId)
import           Control.Exception (IOException)
import           Control.Tracer

import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.ReqResp.Type   as ReqResp
import qualified Network.TypedProtocol.ReqResp.Client as ReqResp
import qualified Network.TypedProtocol.ReqResp.Server as ReqResp
import qualified Network.TypedProtocol.ReqResp.Examples   as ReqResp
import qualified Network.TypedProtocol.ReqResp.Codec.CBOR as ReqResp

import           Ouroboros.Network.Driver
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.IOManager
-- TODO: remove Mx prefixes
import           Ouroboros.Network.Mux
import qualified Network.Mux as Mx (MuxError(..), MuxErrorType(..), muxStart)
import qualified Network.Mux.Bearer.Socket as Mx (socketAsMuxBearer)

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version

import           Test.QuickCheck
import           Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf
import           Text.Show.Functions ()

{-
 - The travis build hosts does not support IPv6 so those test cases are hidden
 - behind the OUROBOROS_NETWORK_IPV6 define for now.
 -}
-- #define OUROBOROS_NETWORK_IPV6

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
  , after AllFinish "socket close during receive" $
    testProperty "socket client connection failure"      prop_socket_client_connect_error
  ]
#undef LAST_IP_TEST

activeMuxTracer :: Show a => Tracer IO a
activeMuxTracer = nullTracer
--activeMuxTracer = _verboseTracer -- Dump log messages to stdout.

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

-- |
-- Allow to run a singly req-resp protocol.
--
testProtocols2 :: RunMiniProtocol appType bytes m a b
               -> OuroborosApplication appType bytes m a b
testProtocols2 reqResp =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 4,
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
    prop_socket_send_recv (Socket.addrAddress client) (Socket.addrAddress server) f xs


#ifdef OUROBOROS_NETWORK_IPV6

-- | Send and receive over IPv6
prop_socket_send_recv_ipv6 :: (Int ->  Int -> (Int, Int))
                           -> [Int]
                           -> Property
prop_socket_send_recv_ipv6 request response = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6061")
    client:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "0")
    prop_socket_send_recv client server request response
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
    r <- prop_socket_send_recv (Socket.addrAddress clientAddr) (Socket.addrAddress serverAddr)
                               request response
    cleanUp serverName
    cleanUp clientName
    return $ r
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
                      -> (Int -> Int -> (Int, Int))
                      -> [Int]
                      -> IO Bool
prop_socket_send_recv initiatorAddr responderAddr f xs =
    withIOManager $ \iomgr -> do

    cv <- newEmptyTMVarM
    sv <- newEmptyTMVarM
    networkState <- newNetworkMutableState

    {- The siblingVar is used by the initiator and responder to wait on each other before exiting.
     - Without this wait there is a risk that one side will finish first causing the Muxbearer to
     - be torn down and the other side exiting before it has a chance to write to its result TMVar.
     -}
    siblingVar <- newTVarM 2

    let -- Server Node; only req-resp server
        responderApp :: OuroborosApplication ResponderApp BL.ByteString IO Void ()
        responderApp = testProtocols2 reqRespResponder

        reqRespResponder =
          ResponderProtocolOnly $
          -- TODO: For the moment this needs MuxPeerRaw because it has to
          -- do something with the result after the protocol is run.
          -- This should be replaced with use of the handles.
          MuxPeerRaw $ \channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r
            waitSibling siblingVar

        -- Client Node; only req-resp client
        initiatorApp :: OuroborosApplication InitiatorApp BL.ByteString IO () Void
        initiatorApp = testProtocols2 reqRespInitiator

        reqRespInitiator =
          InitiatorProtocolOnly $
          -- TODO: For the moment this needs MuxPeerRaw because it has to
          -- do something with the result after the protocol is run.
          -- This should be replaced with use of the handles.
          MuxPeerRaw $ \channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs))
            atomically $ putTMVar cv r
            waitSibling siblingVar

    let snocket = socketSnocket iomgr
    res <-
      withServerNode
        snocket
        networkTracers
        networkState
        responderAddr
        cborTermVersionDataCodec
        (\(DictVersion _) -> acceptableVersion)
        (unversionedProtocol (\_peerid -> SomeResponderApplication responderApp))
        nullErrorPolicies
        $ \_ _ -> do
          connectToNode
            snocket
            cborTermVersionDataCodec
            (NetworkConnectTracers activeMuxTracer nullTracer)
            (unversionedProtocol (\_peerid -> initiatorApp))
            (Just initiatorAddr)
            responderAddr
          atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv

    return (res == mapAccumL f 0 xs)

  where
    networkTracers = NetworkServerTracers {
        nstMuxTracer         = activeMuxTracer,
        nstHandshakeTracer   = nullTracer,
        nstErrorPolicyTracer = showTracing stdoutTracer
      }


    waitSibling :: StrictTVar IO Int -> IO ()
    waitSibling cntVar = do
        atomically $ modifyTVar cntVar (\a -> a - 1)
        atomically $ do
            cnt <- readTVar cntVar
            unless (cnt == 0) retry

data RecvErrorType = SocketClosed | SDUTimeout | CodecTimeout deriving (Eq, Show)

instance Arbitrary RecvErrorType where
    arbitrary = oneof [pure SocketClosed, pure SDUTimeout, pure CodecTimeout]


byteLimitsReqResp :: ProtocolSizeLimits (ReqResp.ReqResp req resp) BL.ByteString
byteLimitsReqResp = ProtocolSizeLimits stateToLimit (fromIntegral . BL.length)
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: ReqResp.ReqResp req resp).
                    PeerHasAgency pr st -> Word
    stateToLimit _ = maxBound

timeLimitsReqResp :: ProtocolTimeLimits (ReqResp.ReqResp req resp)
timeLimitsReqResp = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: ReqResp.ReqResp req resp).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit _ = Nothing


-- |
-- Verify that we raise the correct exception in case a socket closes or a timeout during
-- a read.
prop_socket_recv_error :: (Int -> Int -> (Int, Int))
                       -> RecvErrorType
                       -> Property
prop_socket_recv_error f rerr =
    ioProperty $
    withIOManager $ \iomgr -> do
    sv   <- newEmptyTMVarM

    let app :: OuroborosApplication ResponderApp BL.ByteString IO Void ()
        app = testProtocols2 reqRespResponder

        reqRespResponder =
          ResponderProtocolOnly $
          -- TODO: For the moment this needs MuxPeerRaw because it has to
          -- do something with the result after the protocol is run.
          -- This should be replaced with use of the handles.
          MuxPeerRaw $ \channel -> do
            r <- runPeerWithLimits nullTracer
                         ReqResp.codecReqResp
                         byteLimitsReqResp
                         timeLimitsReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r

    let snocket :: SocketSnocket
        snocket = socketSnocket iomgr

    bracket
      (open snocket (SocketFamily Socket.AF_INET))
      (close snocket)
      $ \sd -> do
        -- bind the socket
        muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
        Socket.setSocketOption sd Socket.ReuseAddr 1
        Socket.bind sd (Socket.addrAddress muxAddress)
        Socket.listen sd 1

        withAsync
          (
              -- accept a connection and start mux on it
              bracket
                (runAccept $ accept snocket sd)
                (\(sd', _, _) -> Socket.close sd')
                $ \(sd', _, _) -> do
                  let timeout = if rerr == SDUTimeout then Just 0.10
                                                      else Nothing
                  let bearer = Mx.socketAsMuxBearer timeout nullTracer sd'
                  Mx.muxStart nullTracer (toApplication app) bearer
          )
          $ \muxAsync -> do

          -- connect to muxAddress
          sd' <- openToConnect snocket (Socket.addrAddress muxAddress)
          _ <- connect snocket sd' (Socket.addrAddress muxAddress)

          provoke rerr sd'

          res <- waitCatch muxAsync
          result <- case res of
              Left e  ->
                  case fromException e of
                        Just me -> return $
                            case Mx.errorType me of
                                 Mx.MuxBearerClosed   -> rerr === SocketClosed
                                 Mx.MuxSDUReadTimeout -> rerr === SDUTimeout
                                 _                    -> property False
                        Nothing -> return $ counterexample (show e) False
              Right _ -> return $ property False

          when (rerr /= SocketClosed) $ Socket.close sd'
          return result

  where
    provoke :: RecvErrorType -> Socket.Socket -> IO ()
    provoke CodecTimeout sd = do
        sendRaw sd
          [ 0x00, 0x01, 0x02, 0x03 -- Transmission time
          , 0x80, 0x04             -- Initiator for ReqResp
          , 0x00, 0x01             -- Lenght, 1 byte.
          , 0x82                   -- Start of a CBOR list
          ]
        threadDelay 10
        printf "Codec provoked\n"
    provoke SocketClosed sd = do
        sendRaw sd [0xa]
        Socket.close sd
    provoke SDUTimeout sd = do
        sendRaw sd [0xa]
        threadDelay 0.2

    sendRaw :: Socket.Socket -> [Word8] -> IO ()
    sendRaw sd r =
#if defined(mingw32_HOST_OS)
          Win32.Async.sendAll sd $ BL.pack r
#else
          Socket.sendAll sd $ BL.pack r
#endif

prop_socket_client_connect_error :: (Int -> Int -> (Int, Int))
                                 -> [Int]
                                 -> Property
prop_socket_client_connect_error _ xs =
    ioProperty $
    withIOManager $ \iomgr -> do
    serverAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    clientAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    cv <- newEmptyTMVarM

    let app :: OuroborosApplication InitiatorApp BL.ByteString IO () Void
        app = testProtocols2 reqRespInitiator

        reqRespInitiator =
          InitiatorProtocolOnly $
          -- TODO: For the moment this needs MuxPeerRaw because it has to
          -- do something with the result after the protocol is run.
          -- This should be replaced with use of the handles.
          MuxPeerRaw $ \channel -> do
            _ <- runPeer nullTracer
                    ReqResp.codecReqResp
                    channel
                    (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs)
                            :: Peer (ReqResp.ReqResp Int Int) AsClient ReqResp.StIdle IO [Int])
            atomically $ putTMVar cv ()

    (res :: Either IOException Bool)
      <- try $ False <$ connectToNode
        (socketSnocket iomgr)
        cborTermVersionDataCodec
        nullNetworkConnectTracers
        (unversionedProtocol (\_peerid -> app))
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
