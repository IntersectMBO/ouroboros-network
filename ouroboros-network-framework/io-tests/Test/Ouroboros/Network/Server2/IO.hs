{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.Server2.IO (tests) where

import           Control.Monad.Class.MonadThrow

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import qualified Network.Mux.Bearer as Mux
import qualified Network.Socket as Socket

import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Snocket (socketSnocket)
import           Ouroboros.Network.Socket (configureSocket)

import           Ouroboros.Network.ConnectionManager.Test.Experiments
import           Ouroboros.Network.ConnectionManager.Test.Timeouts
import           Ouroboros.Network.Test.Orphans ()

tests :: TestTree
tests =
  testGroup "Ouroboros.Network"
  [ testGroup "Server2"
    [ testProperty "unidirectional IO"      prop_unidirectional_IO
    , testProperty "bidirectional IO"       prop_bidirectional_IO
    ]
  ]


--
-- Experiments \/ Demos & Properties
--

prop_unidirectional_IO
  :: ClientAndServerData Int
  -> Property
prop_unidirectional_IO clientAndServerData =
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
                ioTimeouts
                (socketSnocket iomgr)
                Mux.makeSocketBearer
                (flip configureSocket Nothing)
                socket
                clientAndServerData



prop_bidirectional_IO
    :: ClientAndServerData Int
    -> ClientAndServerData Int
    -> Property
prop_bidirectional_IO data0 data1 =
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
              ioTimeouts
              (socketSnocket iomgr)
              Mux.makeSocketBearer
              (flip configureSocket Nothing)
              socket0
              socket1
              addr0'
              addr1'
              data0
              data1
