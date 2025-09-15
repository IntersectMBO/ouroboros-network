{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.Server.IO (tests) where

import Control.Monad.Class.MonadThrow
import System.Random (mkStdGen)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Network.Mux.Bearer qualified as Mux
import Network.Socket qualified as Socket

import Ouroboros.Network.IOManager
import Ouroboros.Network.Snocket (socketSnocket)
import Ouroboros.Network.Socket (configureSocket)

import Test.Ouroboros.Network.ConnectionManager.Experiments
import Test.Ouroboros.Network.ConnectionManager.Timeouts
import Test.Ouroboros.Network.Orphans ()

tests :: TestTree
tests =
  testGroup "Ouroboros.Network"
  [ testGroup "Server"
    [ testProperty "unidirectional IO" prop_unidirectional_IO
    , testProperty "bidirectional IO"  prop_bidirectional_IO
    ]
  ]


--
-- Experiments \/ Demos & Properties
--

prop_unidirectional_IO
  :: Fixed Int
  -> ClientAndServerData Int
  -> Property
prop_unidirectional_IO (Fixed rnd) clientAndServerData =
    ioProperty $ do
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
                (mkStdGen rnd)
                ioTimeouts
                (socketSnocket iomgr)
                Mux.makeSocketBearer
                (`configureSocket` Nothing)
                socket
                clientAndServerData



prop_bidirectional_IO
    :: Fixed Int
    -> ClientAndServerData Int
    -> ClientAndServerData Int
    -> Property
prop_bidirectional_IO (Fixed rnd) data0 data1 =
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
            -- TODO: use ephemeral ports
            let hints = Socket.defaultHints { Socket.addrFlags = [Socket.AI_ADDRCONFIG, Socket.AI_PASSIVE] }

            addr0 : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
            configureSocket socket0 (Just $ Socket.addrAddress addr0)
            Socket.bind socket0 (Socket.addrAddress addr0)
            addr0' <- Socket.getSocketName socket0
            Socket.listen socket0 10

            addr1 : _ <- Socket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
            configureSocket socket1 (Just $ Socket.addrAddress addr1)
            Socket.bind socket1 (Socket.addrAddress addr1)
            addr1' <- Socket.getSocketName socket1
            Socket.listen socket1 10

            bidirectionalExperiment
              True
              (mkStdGen rnd)
              ioTimeouts
              (socketSnocket iomgr)
              Mux.makeSocketBearer
              (`configureSocket` Nothing)
              socket0
              socket1
              addr0'
              addr1'
              data0
              data1
