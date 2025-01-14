{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.Control.V1.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

controlReceiver :: forall (m :: Type -> Type)
             .  Monad m
            => m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> (OCert StandardCrypto -> m RecvResult)
            -> m AgentInfo
            -> Client (ControlProtocol m) NonPipelined InitialState m ()
controlReceiver genKey dropKey queryKey installKey getAgentInfo =
    Client.Await $ \case
      VersionMessage -> go
      AbortMessage -> Client.Done ()
      ProtocolErrorMessage -> Client.Done ()
  where
    go :: Client (ControlProtocol m) NonPipelined IdleState m ()
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

type ControlServer m a = Server (ControlProtocol m) NonPipelined InitialState m a

controlGenKey :: forall (m :: (Type -> Type))
               . MonadSTM m
              => MonadThrow m
              => ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlGenKey = do
  Server.Yield VersionMessage $
    Server.Yield GenStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlQueryKey :: forall (m :: (Type -> Type))
               . MonadSTM m
              => MonadThrow m
              => ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlQueryKey = do
  Server.Yield VersionMessage $
    Server.Yield QueryStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlDropKey :: forall (m :: (Type -> Type))
               . MonadSTM m
              => MonadThrow m
              => ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlDropKey = do
  Server.Yield VersionMessage $
    Server.Yield DropStagedKeyMessage $
      Server.Await $ \(PublicKeyMessage vkeyMay) ->
        Server.Yield EndMessage $
          Server.Done vkeyMay

controlInstallKey :: forall (m :: (Type -> Type))
               . MonadSTM m
              => MonadThrow m
              => OCert StandardCrypto
              -> ControlServer m RecvResult
controlInstallKey oc = do
  Server.Yield VersionMessage $
    Server.Yield (InstallKeyMessage oc) $
      Server.Await $ \(InstallResultMessage result) ->
        Server.Yield EndMessage $
          Server.Done result

controlGetInfo :: forall (m :: (Type -> Type))
               . MonadSTM m
              => MonadThrow m
              => ControlServer m AgentInfo
controlGetInfo = do
  Server.Yield VersionMessage $
    Server.Yield RequestInfoMessage $
      Server.Await $ \(InfoMessage info) ->
        Server.Yield EndMessage $
          Server.Done info
