{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Control.V0.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V0.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

controlReceiver :: forall (c :: *) (m :: * -> *)
             . KESAlgorithm (KES c)
            => Monad m
            => m (Maybe (VerKeyKES (KES c)))
            -> m (Maybe (VerKeyKES (KES c)))
            -> m (Maybe (VerKeyKES (KES c)))
            -> (OCert c -> m RecvResult)
            -> m (AgentInfo c)
            -> Client (ControlProtocol m c) NonPipelined InitialState m ()
controlReceiver genKey dropKey queryKey installKey getAgentInfo =
    Client.Await $ \case
      VersionMessage -> go
      AbortMessage -> Client.Done ()
      ProtocolErrorMessage -> Client.Done ()
  where
    go :: Client (ControlProtocol m c) NonPipelined IdleState m ()
    go = Client.Await $ \case
          InstallKeyMessage oc ->
            Client.Effect $ do
              result <- installKey oc
              return $ Client.Yield (InstallResultMessage result) go
          GenStagedKeyMessage ->
            Client.Effect $ do
              vkeyMay <- genKey
              return $ Client.Yield (PublicKeyMessage vkeyMay) go
          QueryStagedKeyMessage ->
            Client.Effect $ do
              vkeyMay <- queryKey
              return $ Client.Yield (PublicKeyMessage vkeyMay) go
          DropStagedKeyMessage ->
            Client.Effect $ do
              vkeyMay <- dropKey
              return $ Client.Yield (PublicKeyMessage vkeyMay) go

          RequestInfoMessage ->
            Client.Effect $ do
              info <- getAgentInfo
              return $ Client.Yield (InfoMessage info) go

          EndMessage ->
            Client.Done ()

          ProtocolErrorMessage ->
            Client.Done ()

type ControlPeer c m a = Server (ControlProtocol m c) NonPipelined InitialState m a

controlGenKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlGenKey = do
  Server.Yield VersionMessage $
    Server.Yield GenStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlQueryKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlQueryKey = do
  Server.Yield VersionMessage $
    Server.Yield QueryStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlDropKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlDropKey = do
  Server.Yield VersionMessage $
    Server.Yield DropStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlInstallKey :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => OCert c
              -> ControlPeer c m RecvResult
controlInstallKey oc = do
  Server.Yield VersionMessage $
    Server.Yield (InstallKeyMessage oc) $
      Server.Await $ \(InstallResultMessage result) ->
        Server.Yield EndMessage $
          Server.Done result

controlGetInfo :: forall (c :: *) (m :: (* -> *))
               . KESAlgorithm (KES c)
              => MonadSTM m
              => MonadThrow m
              => ControlPeer c m (AgentInfo c)
controlGetInfo = do
  Server.Yield VersionMessage $
    Server.Yield RequestInfoMessage $
      Server.Await $ \(InfoMessage info) ->
        Server.Yield EndMessage $
          Server.Done info
