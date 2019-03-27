{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Socket (tests) where

import           Control.Monad
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Lazy as BL
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Socket

import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainProducerState as CPS
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Protocol.ChainSync.Client
import           Ouroboros.Network.Protocol.ChainSync.Codec
import           Ouroboros.Network.Protocol.ChainSync.Examples
import           Ouroboros.Network.Protocol.ChainSync.Server
import           Ouroboros.Network.Testing.Serialise

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import qualified Test.Mux as Mxt

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
  [ testProperty "socket send receive IPv4"         prop_socket_send_recv_ipv4
#ifdef OUROBOROS_NETWORK_IPV6
  , testProperty "socket send receive IPv6"         prop_socket_send_recv_ipv6
#endif
  , testProperty "socket close during receive"      prop_socket_recv_close
  , testProperty "socket client connection failure" prop_socket_client_connect_error
  , testProperty "socket sync demo"                 prop_socket_demo
  ]


--
-- Properties
--

-- | Test chainsync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates

-- | Send and receive over IPv4
prop_socket_send_recv_ipv4 :: Mxt.DummyPayload
                           -> Mxt.DummyPayload
                           -> Property
prop_socket_send_recv_ipv4 request response = ioProperty $ do
    client:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    server:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    return $ prop_socket_send_recv client server request response

#ifdef OUROBOROS_NETWORK_IPV6
-- | Send and receive over IPv6
prop_socket_send_recv_ipv6 :: Mxt.DummyPayload
                      -> Mxt.DummyPayload
                      -> Property
prop_socket_send_recv_ipv6 request response = ioProperty $ do
    client:_ <- getAddrInfo Nothing (Just "::1") (Just "0")
    server:_ <- getAddrInfo Nothing (Just "::1") (Just "6061")
    return $ prop_socket_send_recv client server request response
#endif

-- | Verify that an initiator and a responder can send and receive messages from each other
-- over a TCP socket. Large DummyPayloads will be split into smaller segments and the
-- testcases will verify that they are correctly reassembled into the original message.
prop_socket_send_recv :: AddrInfo
                      -> AddrInfo
                      -> Mxt.DummyPayload
                      -> Mxt.DummyPayload
                      -> Property
prop_socket_send_recv clientAddr serverAddr request response = ioProperty $ do

    endMpsVar <- atomically $ newTVar 2

    (verify, client_mp, server_mp) <- Mxt.setupMiniReqRsp
                                        (Mx.AppProtocolId Mxt.ChainSync1)
                                        (return ()) endMpsVar request response

    let client_mps Mxt.ChainSync1 = client_mp
        server_mps Mxt.ChainSync1 = server_mp


    server_h <- startResponder server_mps serverAddr
    startInitiator client_mps clientAddr serverAddr

    v <- verify

    killResponder server_h
    return $ property v

-- | Verify that we raise the correct exception in case a socket closes during a read.
prop_socket_recv_close :: Mxt.DummyPayload
                       -> Mxt.DummyPayload
                       -> Property
prop_socket_recv_close request response = ioProperty $ do
    b:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    endMpsVar <- atomically $ newTVar 2
    resq <- atomically $ newTBQueue 1

    (_, _, server_mp) <- Mxt.setupMiniReqRsp (Mx.AppProtocolId Mxt.ChainSync1)
                                             (return ()) endMpsVar request response

    let server_mps Mxt.ChainSync1 = server_mp

    server_h <- startResponderT server_mps b (Just $ rescb resq)

    sd <- socket (addrFamily b) Stream defaultProtocol
    connect sd (addrAddress b)

    Socket.sendAll sd $ BL.singleton 0xa
    close sd

    res <- atomically $ readTBQueue resq

    killResponder server_h
    case res of
         Just e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxBearerClosed
                  Nothing -> return False
         Nothing -> return False

  where
    rescb resq e_m = atomically $ writeTBQueue resq e_m

prop_socket_client_connect_error :: Mxt.DummyPayload
                                 -> Mxt.DummyPayload
                                 -> Property
prop_socket_client_connect_error request response = ioProperty $ do
    clientAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    serverAddr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    endMpsVar <- atomically $ newTVar 2

    (_, client_mp, _) <- Mxt.setupMiniReqRsp (Mx.AppProtocolId Mxt.ChainSync1)
                                             (return ()) endMpsVar request response

    let client_mps Mxt.ChainSync1 = client_mp

    res_e <- try $ startInitiator client_mps clientAddr serverAddr :: IO (Either SomeException ())
    case res_e of
         Left _  -> return $ property True -- XXX Dissregarding the exact exception type
         Right _ -> return $ property False


demo :: forall block .
        (Chain.HasHeader block, Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block] -> IO Bool
demo chain0 updates = do
    a:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    b:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    producerVar <- newTVarM (CPS.initChainProducerState chain0)
    consumerVar <- newTVarM chain0
    consumerDone <- atomically newEmptyTMVar

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain
        a_mps Mxt.ChainSync1 = Mx.MiniProtocolDescription
                                   (Mx.AppProtocolId Mxt.ChainSync1)
                                   (consumerInit consumerDone target consumerVar)
                                   dummyCallback
        b_mps Mxt.ChainSync1 = Mx.MiniProtocolDescription
                                   (Mx.AppProtocolId Mxt.ChainSync1)
                                   dummyCallback
                                   (producerRsp producerVar)

    b_h <- startResponder b_mps b
    a_h <- startResponder a_mps a
    startInitiator a_mps a b

    void $ fork $ sequence_
        [ do threadDelay 10e-3 -- 10 milliseconds, just to provide interest
             atomically $ do
                 p <- readTVar producerVar
                 let Just p' = CPS.applyChainUpdate update p
                 writeTVar producerVar p'
             | update <- updates
        ]

    r <- atomically $ takeTMVar consumerDone
    killResponder b_h
    killResponder a_h

    return r
  where
    checkTip target consumerVar = atomically $ do
          chain <- readTVar consumerVar
          return (Chain.headPoint chain == target)

    consumerClient :: Point block -> TVar IO (Chain block) -> Client block IO ()
    consumerClient target consChain =
      Client
        { rollforward = \_ -> checkTip target consChain >>= \b ->
            if b then pure $ Left ()
                 else pure $ Right $ consumerClient target consChain
        , rollbackward = \_ _ -> checkTip target consChain >>= \b ->
            if b then pure $ Left ()
                 else pure $ Right $ consumerClient target consChain
        , points = \_ -> pure $ consumerClient target consChain
        }

    consumerInit :: TMVar IO Bool -> Point block -> TVar IO (Chain block)
                 -> Channel IO BL.ByteString -> IO ()
    consumerInit done target consChain channel = do
       let consumerPeer = chainSyncClientPeer (chainSyncClientExample consChain
                                               (consumerClient target consChain))

       runPeer codecChainSync channel consumerPeer
       atomically $ putTMVar done True

       return ()

    dummyCallback _ = forever $
        threadDelay 1.0

    producerRsp ::  TVar IO (CPS.ChainProducerState block)
                -> Channel IO BL.ByteString -> IO ()
    producerRsp prodChain channel = do
        let producerPeer = chainSyncServerPeer (chainSyncServerExample () prodChain)

        runPeer codecChainSync channel producerPeer

