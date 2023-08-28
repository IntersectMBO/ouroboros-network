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
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
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
          EndMessage ->
            Done TokEnd ()

servicePusher :: forall (c :: *) (m :: (* -> *))
           . KESAlgorithm (KES c)
          => MonadSTM m
          => MonadThrow m
          => m (CRef m (SignKeyWithPeriodKES (KES c)), OCert c)
          -> m (Maybe (CRef m (SignKeyWithPeriodKES (KES c)), OCert c))
          -> (RecvResult -> m ())
          -> Peer (ServiceProtocol m c) AsServer InitialState m ()
servicePusher currentKey nextKey handleResult =
  Yield (ServerAgency TokInitial) VersionMessage $
    Effect $ do
      (sk, oc) <- currentKey
      return (
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> go result
        )
  where
    go :: RecvResult -> Peer (ServiceProtocol m c) AsServer IdleState m ()
    go result = Effect $ do
      handleResult result
      skOcMay <- nextKey
      case skOcMay of
        Nothing -> return $
          Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd ()
        Just (sk, oc) -> return $
          Yield (ServerAgency TokIdle) (KeyMessage sk oc) $
          Await (ClientAgency TokWaitForConfirmation) $ \(RecvResultMessage result) -> go result
