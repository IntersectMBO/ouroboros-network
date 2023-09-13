{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Service.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer
import Network.TypedProtocol.Core

serviceReceiver :: forall (c :: *) (m :: * -> *)
             . KESAlgorithm (KES c)
            => Monad m
            => (Bundle m c -> m RecvResult)
            -> Peer (ServiceProtocol m c) AsClient InitialState m ()
serviceReceiver receiveBundle =
    Await (ServerAgency TokInitial) $ \case
      VersionMessage -> go
      AbortMessage -> Done TokEnd ()
      ProtocolErrorMessage -> Done TokEnd ()
  where
    go :: Peer (ServiceProtocol m c) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          KeyMessage bundle ->
            Effect $ do
              result <- receiveBundle bundle
              return $ Yield (ClientAgency TokWaitForConfirmation) (RecvResultMessage result) go
          ProtocolErrorMessage ->
            Done TokEnd ()
          ServerDisconnectMessage ->
            Done TokEnd ()

servicePusher :: forall (c :: *) (m :: (* -> *))
           . KESAlgorithm (KES c)
          => MonadSTM m
          => MonadThrow m
          => MonadAsync m
          => MonadTimer m
          => m (Bundle m c)
          -> m (Bundle m c)
          -> (RecvResult -> m ())
          -> Peer (ServiceProtocol m c) AsServer InitialState m ()
servicePusher currentKey nextKey handleResult =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      bundle <- currentKey
      return $
        Yield (ServerAgency TokIdle) (KeyMessage bundle) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
  where
    goR :: RecvResult -> Peer (ServiceProtocol m c) AsServer IdleState m ()
    goR result = Effect $ do
      handleResult result
      go

    go :: m (Peer (ServiceProtocol m c) AsServer IdleState m ())
    go = do
      bundle <- nextKey
      return $
        Yield (ServerAgency TokIdle) (KeyMessage bundle) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
