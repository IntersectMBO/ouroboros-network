{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocols.VersionHandshake.Peers
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

versionHandshakeClient :: Monad m
                       => [VersionIdentifier]
                       -> Client VersionHandshakeProtocol NonPipelined InitialState m (Maybe VersionIdentifier)
versionHandshakeClient acceptableVersions =
    Client.Await $ \case
      VersionOfferMessage availableVersions ->
        let commonVersions = [ v | v <- acceptableVersions, w <- availableVersions, v == w ]
        in
          case commonVersions of
            [] ->
              Client.Yield VersionRejectedMessage $
              Client.Done Nothing
            (v:_) ->
              Client.Yield (VersionAcceptMessage v) $
              Client.Done (Just v)

versionHandshakeServer :: Monad m
                       => [VersionIdentifier]
                       -> Server VersionHandshakeProtocol NonPipelined InitialState m (Maybe VersionIdentifier)
versionHandshakeServer availableVersions =
  Server.Yield (VersionOfferMessage availableVersions) $
    Server.Await $ \case
      VersionRejectedMessage ->
        Server.Done Nothing
      VersionAcceptMessage v ->
        Server.Done (Just v)
