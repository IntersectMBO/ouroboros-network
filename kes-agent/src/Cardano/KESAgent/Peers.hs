{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.KESAgent.Peers
where

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.OCert
import Cardano.KESAgent.RefCounting

import Cardano.Crypto.KES.Class

import Network.TypedProtocol.Core

kesReceiver :: forall (c :: *) (m :: * -> *)
             . KESAlgorithm (KES c)
            => Monad m
            => (CRef m (SignKeyWithPeriodKES (KES c)) -> OCert c -> m ())
            -> Peer (KESProtocol m c) AsClient InitialState m ()
kesReceiver receiveKey =
  Effect $ do
    return $
      Await (ServerAgency TokInitial) $ \VersionMessage ->
        Effect $ return go
  where
    go :: Peer (KESProtocol m c) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          KeyMessage sk oc ->
            Effect $ do
              receiveKey sk oc
              return go
          EndMessage ->
            Done TokEnd ()

kesPusher :: forall (c :: *) (m :: (* -> *))
           . KESAlgorithm (KES c)
          => Monad m
          => m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c)
          -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)), OCert c))
          -> Peer (KESProtocol m c) AsServer InitialState m ()
kesPusher currentKey nextKey =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      (sk, oc) <- currentKey
      return $ Yield (ServerAgency TokIdle) (KeyMessage sk oc) go
  where
    go :: Peer (KESProtocol m c) AsServer IdleState m ()
    go = Effect $ do
      skOcMay <- nextKey
      case skOcMay of
        Nothing -> return $
          Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd ()
        Just (sk, oc) -> return $
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) go
