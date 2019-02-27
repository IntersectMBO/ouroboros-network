{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Socket (tests) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (sendAll)
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Socket

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import qualified Test.Mux as Mxt

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Socket"
  [ testProperty "socket send receive" prop_socket_send_recv
  , testProperty "socket close during receive"    prop_socket_recv_close
  , testProperty "socket sync demo"    prop_socket_demo
  ]


--
-- Properties
--

-- | Test chainsync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo2 chain updates

-- | Verify that an initiator and a responder can send and receive messages from each other
-- over a TCP socket. Large DummyPayloads will be split into smaller segments and the
-- testcases will verify that they are correctly reassembled into the original message.
prop_socket_send_recv :: Mxt.DummyPayload
                      -> Mxt.DummyPayload
                      -> Property
prop_socket_send_recv request response = ioProperty $ do
    a:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    b:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")

    endMpsVar <- atomically $ newTVar 2

    (verify, client_mp, server_mp) <- Mxt.setupMiniReqRsp
                                        (Mx.AppProtocolId Mxt.ChainSync1)
                                        (return ()) endMpsVar request response

    let client_mps Mxt.ChainSync1 = client_mp
        server_mps Mxt.ChainSync1 = server_mp


    server_h <- startResponder server_mps b
    startInitiator client_mps a b

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
    resq <- atomically newTQueue

    (verify, client_mp, server_mp) <- Mxt.setupMiniReqRsp
                                        (Mx.AppProtocolId Mxt.ChainSync1)
                                        (return ()) endMpsVar request response

    let client_mps Mxt.ChainSync1 = client_mp
        server_mps Mxt.ChainSync1 = server_mp

    server_h <- startResponderT server_mps b $ Just resq

    sd <- socket (addrFamily b) Stream defaultProtocol
    connect sd (addrAddress b)

    Socket.sendAll sd $ BL.singleton 0xa
    close sd

    res <- atomically $ readTQueue resq

    killResponder server_h
    case res of
         Just e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxBearerClosed
                  Nothing -> return False
         Nothing -> return False





