{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Socket (tests) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.List (mapAccumL)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
#ifndef mingw32_HOST_OS
import           System.Directory (removeFile)
import           System.IO.Error
#endif

import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.ReqResp.Type       as ReqResp
import qualified Network.TypedProtocol.ReqResp.Client     as ReqResp
import qualified Network.TypedProtocol.ReqResp.Server     as ReqResp
import qualified Ouroboros.Network.Protocol.ReqResp.Codec as ReqResp

import           Control.Tracer

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.Socket

import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import qualified Ouroboros.Network.Protocol.ChainSync.Client   as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec    as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server   as ChainSync
import           Ouroboros.Network.Testing.Serialise

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import qualified Test.Mux as Mxt
import           Test.Mux.ReqResp

import           Test.QuickCheck
import           Text.Show.Functions ()
import           Test.Tasty (TestTree, testGroup, after, DependencyType(..))
import           Test.Tasty.QuickCheck (testProperty)

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
    client:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    server:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    prop_socket_send_recv client server f xs


#ifdef OUROBOROS_NETWORK_IPV6

-- | Send and receive over IPv6
prop_socket_send_recv_ipv6 :: (Int ->  Int -> (Int, Int))
                           -> [Int]
                           -> Property
prop_socket_send_recv_ipv6 request response = ioProperty $ do
    client:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "0")
    server:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6061")
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
    let clientAddr = Socket.AddrInfo [] Socket.AF_UNIX  Socket.Stream Socket.defaultProtocol
                         (Socket.SockAddrUnix clientName) Nothing
        serverAddr = Socket.AddrInfo [] Socket.AF_UNIX  Socket.Stream Socket.defaultProtocol
                         (Socket.SockAddrUnix serverName) Nothing
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
prop_socket_send_recv clientAddr serverAddr f xs = do

    cv <- newEmptyTMVarM
    sv <- newEmptyTMVarM

    let -- Server Node; only req-resp server
        serverApp = simpleMuxServerApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespServerPeer (reqRespServerMapAccumL sv (\a -> pure . f a) 0))
        serNet = NetworkInterface {
            nodeAddress = serverAddr,
            nodeApplication = serverApp
          }

        -- Client Node; only req-resp client
        clientApp = simpleMuxClientApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespClientPeer (reqRespClientMap cv xs))
        cliNet = NetworkInterface {
             nodeAddress = clientAddr,
             nodeApplication = clientApp
           }

    res <-
      withNetworkNode serNet $ \_ ->
        withNetworkNode cliNet $ \cliNode ->
          runWithConnection (connect cliNode) serverAddr $ \conn -> do
            runConnection conn
            atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv

    return (res == mapAccumL f 0 xs)


-- |
-- Verify that we raise the correct exception in case a socket closes during
-- a read.
prop_socket_recv_close :: (Int -> Int -> (Int, Int))
                       -> [Int]
                       -> Property
prop_socket_recv_close f _ = ioProperty $ do

    sv   <- newEmptyTMVarM

    let app :: MuxApplication Mxt.TestProtocols3 IO
        app = simpleMuxServerApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespServerPeer (reqRespServerMapAccumL sv (\a -> pure . f a) 0))

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
                  bearer <- socketAsMuxBearer sd'
                  Mx.muxBearerSetState bearer Mx.Connected
                  Mx.muxStart (miniProtocolDescription app) bearer
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
    clientAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    serverAddr:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    cv <- newEmptyTMVarM

    let app :: MuxApplication Mxt.TestProtocols3 IO
        app = simpleMuxClientApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespClientPeer (reqRespClientMap cv xs)
                      :: Peer (ReqResp.ReqResp Int Int) AsClient ReqResp.StIdle IO ()
                    )

        ni = NetworkInterface {
            nodeAddress = serverAddr,
            nodeApplication = app
          }

    (res :: Either IOException Bool)
      <- try $ withNetworkNode ni $ \nn ->
                 const False <$> withConnection (connect nn) clientAddr

    -- XXX Disregarding the exact exception type
    pure $ either (const True) id res


demo :: forall block .
        ( Chain.HasHeader block, Serialise (Chain.HeaderHash block)
        , Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do
    consumerAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    producerAddress:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    producerVar <- newTVarM (CPS.initChainProducerState chain0)
    consumerVar <- newTVarM chain0
    done <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        consumerApp :: MuxApplication Mxt.TestProtocols1 IO
        consumerApp = simpleMuxClientApplication $
          \Mxt.ChainSync1 ->
              MuxPeer nullTracer
                      (ChainSync.codecChainSync encode encode decode decode)
                      (ChainSync.chainSyncClientPeer
                        (ChainSync.chainSyncClientExample consumerVar
                        (consumerClient done target consumerVar)))

        consumerNet = NetworkInterface {
              nodeAddress = consumerAddress,
              nodeApplication = consumerApp
            }

        producerApp :: MuxApplication Mxt.TestProtocols1 IO
        producerApp = simpleMuxServerApplication $
          \Mxt.ChainSync1 ->
            MuxPeer nullTracer
                    (ChainSync.codecChainSync encode encode decode decode)
                    (ChainSync.chainSyncServerPeer (ChainSync.chainSyncServerExample () producerVar))

        producerNet = NetworkInterface {
              nodeAddress = producerAddress,
              nodeApplication = producerApp
            }

    withNetworkNode producerNet $ \_ ->
      withNetworkNode consumerNet $  \consumerNode ->
        withConnectionAsync (connect consumerNode) (nodeAddress producerNet) $ \_connAsync -> do
          void $ fork $ sequence_
              [ do
                  threadDelay 10e-3 -- just to provide interest
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
