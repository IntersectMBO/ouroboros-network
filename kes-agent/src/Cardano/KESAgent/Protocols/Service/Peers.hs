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
            => (CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m RecvResult)
            -> Peer (ServiceProtocol m c) AsClient InitialState m ()
serviceReceiver receiveKey =
    Await (ServerAgency TokInitial) $ \VersionMessage -> go
  where
    go :: Peer (ServiceProtocol m c) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          KeyMessage sk oc ->
            Effect $ do
              result <- receiveKey sk oc
              return $ Yield (ClientAgency TokWaitForConfirmation) (RecvResultMessage result) go
          NoKeyYetMessage ->
            Yield (ClientAgency TokWaitForPong) PongMessage go
          PingMessage ->
            Yield (ClientAgency TokWaitForPong) PongMessage go
          ProtocolErrorMessage ->
            Done TokEnd ()
          EndMessage ->
            Done TokEnd ()

servicePusher :: forall (c :: *) (m :: (* -> *))
           . KESAlgorithm (KES c)
          => MonadSTM m
          => MonadThrow m
          => MonadAsync m
          => MonadTimer m
          => m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c)
          -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)), OCert c))
          -> (RecvResult -> m ())
          -> Peer (ServiceProtocol m c) AsServer InitialState m ()
servicePusher currentKey nextKey handleResult =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      keyOrNoneAvail <- race (threadDelay 1000000) currentKey
      case keyOrNoneAvail of
        Right (sk, oc) ->
          return $
            Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
              Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
        Left () ->
          return $
            Yield (ServerAgency TokIdle) NoKeyYetMessage $
              Await (ClientAgency TokWaitForPong) $ \PongMessage ->
                Effect go
  where
    goR :: RecvResult -> Peer (ServiceProtocol m c) AsServer IdleState m ()
    goR result = Effect $ do
      handleResult result
      go

    go :: m (Peer (ServiceProtocol m c) AsServer IdleState m ())
    go = do
      keyOrPing <- race (threadDelay 1000000) nextKey
      case keyOrPing of
        Left () ->
          return $
            Yield (ServerAgency TokIdle) PingMessage $
              Await (ClientAgency TokWaitForPong) $ \PongMessage ->
                Effect go
        Right Nothing ->
          return $
            Yield (ServerAgency TokIdle) EndMessage $
              Done TokEnd ()
        Right (Just (sk, oc)) ->
          return $
            Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
              Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> goR result
