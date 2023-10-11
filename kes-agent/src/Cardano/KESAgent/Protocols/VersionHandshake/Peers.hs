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

versionHandshakeClient :: Monad m
                       => [VersionIdentifier]
                       -> Peer VersionHandshakeProtocol AsClient InitialState m (Maybe VersionIdentifier)
versionHandshakeClient acceptableVersions =
    Await (ServerAgency TokInitial) $ \case
      VersionOfferMessage availableVersions ->
        let commonVersions = [ v | v <- acceptableVersions, w <- availableVersions, v == w ]
        in
          case commonVersions of
            [] ->
              Yield (ClientAgency TokVersionsOffered) VersionRejectedMessage $
              Done TokEnd Nothing
            (v:_) ->
              Yield (ClientAgency TokVersionsOffered) (VersionAcceptMessage v) $
              Done TokEnd (Just v)

versionHandshakeServer :: Monad m
                       => [VersionIdentifier]
                       -> Peer VersionHandshakeProtocol AsServer InitialState m (Maybe VersionIdentifier)
versionHandshakeServer availableVersions =
  Yield (ServerAgency TokInitial) (VersionOfferMessage availableVersions) $
    Await (ClientAgency TokVersionsOffered) $ \case
      VersionRejectedMessage ->
        Done TokEnd Nothing
      VersionAcceptMessage v ->
        Done TokEnd (Just v)
