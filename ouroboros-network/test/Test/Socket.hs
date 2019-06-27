{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Socket (tests) where

import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<$))
import           Data.Int (Int64)
import           Data.List (mapAccumL)
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
import qualified Network.TypedProtocol.ReqResp.Server as ReqResp
import qualified Network.TypedProtocol.ReqResp.Type as ReqResp
import qualified Network.TypedProtocol.ReqResp.Codec.Cbor as ReqResp
import qualified Network.TypedProtocol.ReqResp.Examples as ReqResp

import           Codec.SerialiseTerm
import           Control.Tracer

-- TODO: remove Mx prefixes
import           Ouroboros.Network.Mux as Mx
import qualified Network.Mux.Bearer.Socket as Mx

import           Ouroboros.Network.Socket

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Type (acceptEq)
import           Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import           Ouroboros.Network.Testing.Serialise

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
-- import           Test.Mux.ReqResp

import           Test.QuickCheck
import           Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
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

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

data TestProtocols1 = ChainSyncPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ChainSyncPr = 2

  toProtocolEnum 2 = Just ChainSyncPr
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ChainSyncPr  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSyncPr = defaultMiniProtocolLimit

-- |
-- Allow to run a singly req-resp protocol.
--
data TestProtocols2 = ReqRespPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ReqRespPr = 4

  toProtocolEnum 4 = Just ReqRespPr
  toProtocolEnum _ = Nothing

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
    tbl <- newConnectionTable

    {- The siblingVar is used by the initiator and responder to wait on each other before exiting.
     - Without this wait there is a risk that one side will finish first causing the Muxbearer to
     - be torn down and the other side exiting before it has a chance to write to its result TMVar.
     -}
    siblingVar <- newTVarM 2

    let -- Server Node; only req-resp server
        responderApp :: MuxApplication ResponderApp () TestProtocols2 IO BL.ByteString Void ()
        responderApp = Mx.MuxResponderApplication $
          \peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         peerid
                         channel
                         (ReqResp.reqRespServerPeer (ReqResp.reqRespServerMapAccumL (\a -> pure . f a) 0))
            atomically $ putTMVar sv r
            waitSibling siblingVar

        -- Client Node; only req-resp client
        initiatorApp :: MuxApplication InitiatorApp () TestProtocols2 IO BL.ByteString () Void
        initiatorApp = MuxInitiatorApplication $
          \peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         peerid
                         channel
                         (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs))
            atomically $ putTMVar cv r
            waitSibling siblingVar

    res <-
      withSimpleServerNode
        tbl
        responderAddr
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        (\_ _ -> ())
        (\(DictVersion _) -> acceptEq)
        (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) responderApp)
        $ \_ _ -> do
          connectToNode
            (\(DictVersion codec) -> encodeTerm codec)
            (\(DictVersion codec) -> decodeTerm codec)
            (\_ _ -> ())
            (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) initiatorApp)
            (Just initiatorAddr)
            responderAddr
          atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv

    return (res == mapAccumL f 0 xs)

  where
    waitSibling :: TVar IO Int -> IO ()
    waitSibling cntVar = do
        atomically $ modifyTVar' cntVar (\a -> a - 1)
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

    let app :: MuxApplication ResponderApp () TestProtocols2 IO BL.ByteString Void ()
        app = MuxResponderApplication $
          \peerid ReqRespPr channel -> do
            r <- runPeer nullTracer
                         ReqResp.codecReqResp
                         peerid
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
          (do
              -- accept a connection and start mux on it
              bracket
                (Socket.accept sd)
                (\(sd',_) -> Socket.close sd')
                $ \(sd',_) -> do
                  bearer <- Mx.socketAsMuxBearer sd'
                  Mx.muxBearerSetState bearer Mx.Connected
                  Mx.muxStart () app bearer
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

    let app :: MuxApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) TestProtocols2 IO BL.ByteString () Void
        app = MuxInitiatorApplication $
                \peerid ReqRespPr channel -> do
                  _ <- runPeer nullTracer
                          ReqResp.codecReqResp
                          peerid
                          channel
                          (ReqResp.reqRespClientPeer (ReqResp.reqRespClientMap xs)
                                  :: Peer (ReqResp.ReqResp Int Int) AsClient ReqResp.StIdle IO [Int])
                  atomically $ putTMVar cv ()


    (res :: Either IOException Bool)
      <- try $ False <$ connectToNode
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        (,)
        (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app)
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
    tbl <- newConnectionTable

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        initiatorApp :: MuxApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) TestProtocols1 IO BL.ByteString () Void
        initiatorApp = simpleMuxInitiatorApplication $
          \ChainSyncPr ->
              MuxPeer nullTracer
                      (ChainSync.codecChainSync encode decode encode decode)
                      (ChainSync.chainSyncClientPeer
                        (ChainSync.chainSyncClientExample consumerVar
                        (consumerClient done target consumerVar)))

        server :: ChainSync.ChainSyncServer block (Point block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar

        responderApp :: MuxApplication ResponderApp (Socket.SockAddr, Socket.SockAddr)TestProtocols1 IO BL.ByteString Void ()
        responderApp = simpleMuxResponderApplication $
          \ChainSyncPr ->
            MuxPeer nullTracer
                    (ChainSync.codecChainSync encode decode encode decode)
                    (ChainSync.chainSyncServerPeer server)

    withSimpleServerNode
      tbl
      producerAddress
      (\(DictVersion codec)-> encodeTerm codec)
      (\(DictVersion codec)-> decodeTerm codec)
      (,)
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) responderApp)
      $ \_ _ -> do
      withAsync
        (connectToNode
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          (,)
          (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) initiatorApp)
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
    consumerClient :: TMVar IO Bool
                   -> Point block
                   -> TVar IO (Chain block)
                   -> ChainSync.Client block IO ()
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
