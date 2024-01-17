{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Service.V1.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Protocols.Service.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer
import Network.TypedProtocol.Core

serviceReceiver :: forall (m :: * -> *)
             . KESAlgorithm (KES StandardCrypto)
            => Monad m
            => (Bundle m StandardCrypto -> m RecvResult)
            -> Peer (ServiceProtocol m) AsClient InitialState m ()
serviceReceiver receiveBundle =
    Await (ServerAgency TokInitial) $ \case
      VersionMessage -> go
      AbortMessage -> Done TokEnd ()
      ProtocolErrorMessage -> Done TokEnd ()
  where
    go :: Peer (ServiceProtocol m) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          KeyMessage bundle ->
            Effect $ do
              result <- receiveBundle bundle
              return $ Yield (ClientAgency TokWaitForConfirmation) (RecvResultMessage result) go
          ProtocolErrorMessage ->
            Done TokEnd ()
          ServerDisconnectMessage ->
            Done TokEnd ()

servicePusher :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => MonadAsync m
              => MonadTimer m
              => m (Bundle m StandardCrypto)
              -> m (Bundle m StandardCrypto)
              -> (RecvResult -> m ())
              -> Peer (ServiceProtocol m) AsServer InitialState m ()
servicePusher currentKey nextKey handleResult =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      bundle <- currentKey
      return $
        Yield (ServerAgency TokIdle) (KeyMessage bundle) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
  where
    goR :: RecvResult -> Peer (ServiceProtocol m) AsServer IdleState m ()
    goR result = Effect $ do
      handleResult result
      go

    go :: m (Peer (ServiceProtocol m) AsServer IdleState m ())
    go = do
      bundle <- nextKey
      return $
        Yield (ServerAgency TokIdle) (KeyMessage bundle) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
