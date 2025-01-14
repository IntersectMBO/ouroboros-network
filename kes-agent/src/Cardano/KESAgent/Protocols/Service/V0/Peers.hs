{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Service.V0.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Protocols.Service.V0.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

serviceReceiver :: forall (c :: Type) (m :: Type -> Type)
             . KESAlgorithm (KES c)
            => Monad m
            => (Bundle m c -> m RecvResult)
            -> Client (ServiceProtocol m c) NonPipelined InitialState m ()
serviceReceiver receiveBundle =
    Client.Await $ \case
      VersionMessage -> go
      AbortMessage -> Client.Done ()
      ProtocolErrorMessage -> Client.Done ()
  where
    go :: Client (ServiceProtocol m c) NonPipelined IdleState m ()
    go = Client.Await $ \case
          KeyMessage bundle ->
            Client.Effect $ do
              result <- receiveBundle bundle
              return $ Client.Yield (RecvResultMessage result) go
          ProtocolErrorMessage ->
            Client.Done ()
          ServerDisconnectMessage ->
            Client.Done ()

servicePusher :: forall (c :: Type) (m :: (Type -> Type))
           . KESAlgorithm (KES c)
          => MonadSTM m
          => MonadThrow m
          => MonadAsync m
          => MonadTimer m
          => m (Bundle m c)
          -> m (Bundle m c)
          -> (RecvResult -> m ())
          -> Server (ServiceProtocol m c) NonPipelined InitialState m ()
servicePusher currentKey nextKey handleResult =
  Server.Yield VersionMessage $
    Server.Effect $ do
      bundle <- currentKey
      return $
        Server.Yield (KeyMessage bundle) $
          Server.Await $ \(RecvResultMessage result) -> goR result
  where
    goR :: RecvResult -> Server (ServiceProtocol m c) NonPipelined IdleState m ()
    goR result = Server.Effect $ do
      handleResult result
      go

    go :: m (Server (ServiceProtocol m c) NonPipelined IdleState m ())
    go = do
      bundle <- nextKey
      return $
        Server.Yield (KeyMessage bundle) $
          Server.Await $ \(RecvResultMessage result) -> goR result
