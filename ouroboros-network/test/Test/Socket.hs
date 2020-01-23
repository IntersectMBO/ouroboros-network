{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Socket (tests) where

import           Control.Concurrent (ThreadId)
import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<$))
import           Data.Int (Int64)
import           Data.List (mapAccumL)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
#ifndef mingw32_HOST_OS
import           System.Directory (removeFile)
import           System.IO.Error
#endif

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import qualified Network.TypedProtocol.ReqResp.Client as ReqResp
import qualified Network.TypedProtocol.ReqResp.Codec.Cbor as ReqResp
import qualified Network.TypedProtocol.ReqResp.Examples as ReqResp
import qualified Network.TypedProtocol.ReqResp.Server as ReqResp
import qualified Network.TypedProtocol.ReqResp.Type as ReqResp

import           Control.Tracer

-- TODO: remove Mx prefixes
import qualified Network.Mux as Mx hiding (MiniProtocolLimits(..))
import qualified Network.Mux.Bearer.Socket as Mx
import           Ouroboros.Network.Mux as Mx

import           Ouroboros.Network.Socket

import           Ouroboros.Network.Block (Tip, encodeTip, decodeTip)
import           Ouroboros.Network.Magic
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Type (acceptEq, cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (simpleSingletonVersions)
import           Ouroboros.Network.Testing.Serialise

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))

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
    testProperty "socket close during receive"           prop_socket_recv_close
  , after AllFinish "socket close during receive" $
    testProperty "socket client connection failure"      prop_socket_client_connect_error
  , after AllFinish "socket client connection failure" $
    testProperty "socket sync demo"                      prop_socket_demo
  ]
#undef LAST_IP_TEST

activeMuxTracer :: Show a => Tracer IO a
activeMuxTracer = nullTracer
--activeMuxTracer = _verboseTracer -- Dump log messages to stdout.

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

data TestProtocols1 = ChainSyncPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ChainSyncPr = MiniProtocolNum 2

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ChainSyncPr  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSyncPr = defaultMiniProtocolLimit

-- |
-- Allow to run a singly req-resp protocol.
--
data TestProtocols2 = ReqRespPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ReqRespPr = MiniProtocolNum 4

instance Mx.MiniProtocolLimits TestProtocols2 where
  maximumMessageSize ReqRespPr  = defaultMiniProtocolLimit
  maximumIngressQueue ReqRespPr = defaultMiniProtocolLimit

--
-- Properties
--

-- | Test chainsync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates

-- | Send and receive over IPv4
prop_socket_send_recv_ipv4
  :: (Int -> Int -> (Int, Int))
  -> [Int]
  -> Property
prop_socket_send_recv_ipv4 f xs = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    client:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    prop_socket_send_recv client server f xs


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
    r <- prop_socket_send_recv clientAddr serverAddr request response
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
prop_socket_send_recv :: Socket.AddrInfo
                      -> Socket.AddrInfo
                      -> (Int -> Int -> (Int, Int))
                      -> [Int]
                      -> IO Bool
prop_socket_send_recv initiatorAddr responderAddr f xs = do

    cv <- newEmptyTMVarM
    sv <- newEmptyTMVarM
    networkState <- newNetworkMutableState

    {- The siblingVar is used by the initiator and responder to wait on each other before exiting.
     - Without this wait there is a risk that one side will finish first causing the Muxbearer to
     - be torn down and the other side exiting before it has a chance to write to its result TMVar.
     -}
    siblingVar <- newTVarM 2

    let -- Server Node; only req-resp server
        responderApp :: OuroborosApplication Mx.ResponderApp ConnectionId TestProtocols2 IO BL.ByteString Void ()
        responderApp = OuroborosResponderApplication $
          \_peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r
            waitSibling siblingVar

        -- Client Node; only req-resp client
        initiatorApp :: OuroborosApplication Mx.InitiatorApp ConnectionId TestProtocols2 IO BL.ByteString () Void
        initiatorApp = OuroborosInitiatorApplication $
          \_peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs))
            atomically $ putTMVar cv r
            waitSibling siblingVar

    res <-
      withServerNode
        networkTracers
        networkState
        responderAddr
        cborTermVersionDataCodec
        (\(DictVersion _) -> acceptEq)
        (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) responderApp)
        nullErrorPolicies
        $ \_ _ -> do
          connectToNode
            cborTermVersionDataCodec
            (NetworkConnectTracers activeMuxTracer nullTracer)
            (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) initiatorApp)
            (Just initiatorAddr)
            responderAddr
          atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv

    return (res == mapAccumL f 0 xs)

  where
    networkTracers = NetworkServerTracers {
        nstMuxTracer         = activeMuxTracer,
        nstHandshakeTracer   = nullTracer,
        nstErrorPolicyTracer = nullTracer
      }


    waitSibling :: StrictTVar IO Int -> IO ()
    waitSibling cntVar = do
        atomically $ modifyTVar cntVar (\a -> a - 1)
        atomically $ do
            cnt <- readTVar cntVar
            unless (cnt == 0) retry

-- |
-- Verify that we raise the correct exception in case a socket closes during
-- a read.
prop_socket_recv_close :: (Int -> Int -> (Int, Int))
                       -> [Int]
                       -> Property
prop_socket_recv_close f _ = ioProperty $ do

    sv   <- newEmptyTMVarM

    let app :: OuroborosApplication ResponderApp () TestProtocols2 IO BL.ByteString Void ()
        app = OuroborosResponderApplication $
          \_peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r

    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \sd -> do
        -- bind the socket
        muxAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
        Socket.setSocketOption sd Socket.ReuseAddr 1
        Socket.bind sd (Socket.addrAddress muxAddress)
        Socket.listen sd 1

        withAsync
           -- accept a connection and start mux on it
          (bracket
             (Socket.accept sd)
             (\(sd',_) -> Socket.close sd') $ \(sd',_) -> do
               let bearer = Mx.socketAsMuxBearer nullTracer sd'
               Mx.traceMuxBearerState nullTracer Mx.Connected
               Mx.muxStart nullTracer (toApplication app ()) bearer
          )
          $ \muxAsync -> do

          -- connect to muxAddress
          sd' <- Socket.socket (Socket.addrFamily muxAddress) Socket.Stream Socket.defaultProtocol
          Socket.connect sd' (Socket.addrAddress muxAddress)

          Socket.sendAll sd' $ BL.singleton 0xa
          Socket.close sd'

          res <- waitCatch muxAsync
          case res of
              Left e  ->
                  case fromException e of
                        Just me -> return $ Mx.errorType me === Mx.MuxBearerClosed
                        Nothing -> return $ counterexample (show e) False
              Right _ -> return $ property $ False


prop_socket_client_connect_error :: (Int -> Int -> (Int, Int))
                                 -> [Int]
                                 -> Property
prop_socket_client_connect_error _ xs = ioProperty $ do
    serverAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    clientAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    cv <- newEmptyTMVarM

    let app :: OuroborosApplication Mx.InitiatorApp ConnectionId TestProtocols2 IO BL.ByteString () Void
        app = OuroborosInitiatorApplication $
                \_peerid ReqRespPr channel -> do
                  _ <- runPeer nullTracer
                          ReqResp.codecReqResp
                          channel
                          (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs)
                                  :: Peer (ReqResp.ReqResp Int Int) AsClient ReqResp.StIdle IO [Int])
                  atomically $ putTMVar cv ()


    (res :: Either IOException Bool)
      <- try $ False <$ connectToNode
        cborTermVersionDataCodec
        nullNetworkConnectTracers
        (simpleSingletonVersions (0::Int) (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) app)
        (Just clientAddr)
        serverAddr

    -- XXX Disregarding the exact exception type
    pure $ either (const True) id res


demo :: forall block .
        ( Chain.HasHeader block, Serialise (Chain.HeaderHash block)
        , Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = do
    producerAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    consumerAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    producerVar <- newTVarM (CPS.initChainProducerState chain0)
    consumerVar <- newTVarM chain0
    done <- atomically newEmptyTMVar
    networkState <- newNetworkMutableState

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        initiatorApp :: OuroborosApplication Mx.InitiatorApp ConnectionId TestProtocols1 IO BL.ByteString () Void
        initiatorApp = simpleInitiatorApplication $
          \ChainSyncPr ->
              MuxPeer nullTracer
                      codecChainSync
                      (ChainSync.chainSyncClientPeer
                        (ChainSync.chainSyncClientExample consumerVar
                        (consumerClient done target consumerVar)))

        server :: ChainSync.ChainSyncServer block (Tip block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar

        responderApp :: OuroborosApplication Mx.ResponderApp ConnectionId TestProtocols1 IO BL.ByteString Void ()
        responderApp = simpleResponderApplication $
          \ChainSyncPr ->
            MuxPeer nullTracer
                    codecChainSync
                    (ChainSync.chainSyncServerPeer server)

        codecChainSync = ChainSync.codecChainSync encode (fmap const decode)
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode)

    withServerNode
      nullNetworkServerTracers
      networkState
      producerAddress
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) responderApp)
      nullErrorPolicies
      $ \_ _ -> do
      withAsync
        (connectToNode
          cborTermVersionDataCodec
          nullNetworkConnectTracers
          (simpleSingletonVersions (0::Int) (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) initiatorApp)
          (Just consumerAddress)
          producerAddress)
        $ \ _connAsync -> do
          void $ fork $ sequence_
              [ do
                  threadDelay 10e-4 -- just to provide interest
                  atomically $ do
                    p <- readTVar producerVar
                    let Just p' = CPS.applyChainUpdate update p
                    writeTVar producerVar p'
              | update <- updates
              ]

          atomically $ takeTMVar done

  where
    checkTip target consumerVar = atomically $ do
      chain <- readTVar consumerVar
      return (Chain.headPoint chain == target)

    -- A simple chain-sync client which runs until it recieves an update to
    -- a given point (either as a roll forward or as a roll backward).
    consumerClient :: StrictTMVar IO Bool
                   -> Point block
                   -> StrictTVar IO (Chain block)
                   -> ChainSync.Client block (Tip block) IO ()
    consumerClient done target chain =
      ChainSync.Client
        { ChainSync.rollforward = \_ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.rollbackward = \_ _ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.points = \_ -> pure $ consumerClient done target chain
        }

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
