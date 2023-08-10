{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Peers
  where

import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Cardano.KESAgent.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Network.TypedProtocol.Core

kesReceiver :: forall (c :: *) (m :: * -> *)
             . KESAlgorithm (KES c)
            => Monad m
            => (CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ())
            -> Peer (KESProtocol m c) AsClient InitialState m ()
kesReceiver receiveKey =
    Await (ServerAgency TokInitial) $ \VersionMessage -> go
  where
    go :: Peer (KESProtocol m c) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          KeyMessage sk oc ->
            Effect $ do
              receiveKey sk oc
              return $ Yield (ClientAgency TokWaitForConfirmation) ConfirmMessage go
          EndMessage ->
            Done TokEnd ()

kesPusher :: forall (c :: *) (m :: (* -> *))
           . KESAlgorithm (KES c)
          => MonadSTM m
          => MonadThrow m
          => m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c)
          -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)), OCert c))
          -> Peer (KESProtocol m c) AsServer InitialState m ()
kesPusher currentKey nextKey =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      (sk, oc) <- currentKey
      return (
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
          Await (ClientAgency TokWaitForConfirmation) $ \ConfirmMessage -> go
        )
  where
    go :: Peer (KESProtocol m c) AsServer IdleState m ()
    go = Effect $ do
      skOcMay <- nextKey
      case skOcMay of
        Nothing -> return $
          Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd ()
        Just (sk, oc) -> return $
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
          Await (ClientAgency TokWaitForConfirmation) $ \ConfirmMessage -> go
