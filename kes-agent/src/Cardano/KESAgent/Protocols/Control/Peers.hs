{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Control.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Network.TypedProtocol.Core

controlReceiver :: forall (c :: *) (m :: * -> *)
             . KESAlgorithm (KES c)
            => Monad m
            => m (Maybe (VerKeyKES (KES c)))
            -> m (Maybe (VerKeyKES (KES c)))
            -> m (Maybe (VerKeyKES (KES c)))
            -> (OCert c -> m RecvResult)
            -> Peer (ControlProtocol m c) AsClient InitialState m ()
controlReceiver genKey dropKey queryKey installKey =
    Await (ServerAgency TokInitial) $ \VersionMessage -> go
  where
    go :: Peer (ControlProtocol m c) AsClient IdleState m ()
    go = Await (ServerAgency TokIdle) $ \case
          InstallKeyMessage oc ->
            Effect $ do
              result <- installKey oc
              return $ Yield (ClientAgency TokWaitForConfirmation) (InstallResultMessage result) go
          GenStagedKeyMessage ->
            Effect $ do
              vkeyMay <- genKey
              return $ Yield (ClientAgency TokWaitForPublicKey) (PublicKeyMessage vkeyMay) go
          QueryStagedKeyMessage ->
            Effect $ do
              vkeyMay <- queryKey
              return $ Yield (ClientAgency TokWaitForPublicKey) (PublicKeyMessage vkeyMay) go
          DropStagedKeyMessage ->
            Effect $ do
              vkeyMay <- dropKey
              return $ Yield (ClientAgency TokWaitForPublicKey) (PublicKeyMessage vkeyMay) go
              
          EndMessage ->
            Done TokEnd ()

          ProtocolErrorMessage ->
            Done TokEnd ()

type ControlPeer c m a = Peer (ControlProtocol m c) AsServer InitialState m a

controlGenKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlGenKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) GenStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlQueryKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlQueryKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) QueryStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlDropKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlDropKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) DropStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlInstallKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => OCert c
              -> ControlPeer c m RecvResult
controlInstallKey oc = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) (InstallKeyMessage oc) $
      Await (ClientAgency TokWaitForConfirmation) $ \(InstallResultMessage result) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd result
