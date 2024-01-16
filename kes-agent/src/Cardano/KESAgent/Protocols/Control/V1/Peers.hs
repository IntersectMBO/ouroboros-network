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
import Network.TypedProtocol.Core

controlReceiver :: forall (m :: * -> *)
             .  Monad m
            => m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> m (Maybe (VerKeyKES (KES StandardCrypto)))
            -> (OCert StandardCrypto -> m RecvResult)
            -> m AgentInfo
            -> Peer (ControlProtocol m) AsClient InitialState m ()
controlReceiver genKey dropKey queryKey installKey getAgentInfo =
    Await (ServerAgency TokInitial) $ \case
      VersionMessage -> go
      AbortMessage -> Done TokEnd ()
      ProtocolErrorMessage -> Done TokEnd ()
  where
    go :: Peer (ControlProtocol m) AsClient IdleState m ()
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

          RequestInfoMessage ->
            Effect $ do
              info <- getAgentInfo
              return $ Yield (ClientAgency TokWaitForInfo) (InfoMessage info) go

          EndMessage ->
            Done TokEnd ()

          ProtocolErrorMessage ->
            Done TokEnd ()

type ControlPeer m a = Peer (ControlProtocol m) AsServer InitialState m a

controlGenKey :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => ControlPeer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlGenKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) GenStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlQueryKey :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => ControlPeer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlQueryKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) QueryStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlDropKey :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => ControlPeer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlDropKey = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) DropStagedKeyMessage $
      Await (ClientAgency TokWaitForPublicKey) $ \(PublicKeyMessage vkeyMay) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd vkeyMay

controlInstallKey :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => OCert StandardCrypto
              -> ControlPeer m RecvResult
controlInstallKey oc = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) (InstallKeyMessage oc) $
      Await (ClientAgency TokWaitForConfirmation) $ \(InstallResultMessage result) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd result

controlGetInfo :: forall (m :: (* -> *))
               . MonadSTM m
              => MonadThrow m
              => ControlPeer m AgentInfo
controlGetInfo = do
  Yield (ServerAgency TokInitial) VersionMessage $
    Yield (ServerAgency TokIdle) RequestInfoMessage $
      Await (ClientAgency TokWaitForInfo) $ \(InfoMessage info) ->
        Yield (ServerAgency TokIdle) EndMessage $
          Done TokEnd info
