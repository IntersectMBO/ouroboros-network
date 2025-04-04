{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.KESAgent.Protocols.VersionHandshake.Driver
where

import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Util.Pretty

import Control.Exception
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Tracer (Tracer, traceWith)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver

import Ouroboros.Network.RawBearer

import Data.SerDoc.Class (HasInfo (..))

-- | Logging messages that the Driver may send
data VersionHandshakeDriverTrace
  = VersionHandshakeDriverOfferingVersions ![VersionIdentifier]
  | VersionHandshakeDriverAcceptingVersion !VersionIdentifier
  | VersionHandshakeDriverRejectingVersion
  | VersionHandshakeDriverMisc !String
  deriving (Show)

instance Pretty VersionHandshakeDriverTrace where
  pretty (VersionHandshakeDriverOfferingVersions versions) =
    "OfferingVersions: [" ++ intercalate ", " (map pretty versions) ++ "]"
  pretty (VersionHandshakeDriverAcceptingVersion v) =
    "AcceptingVersion: " ++ pretty v
  pretty (VersionHandshakeDriverMisc x) = x
  pretty x = drop (strLength "VersionHandshakeDriver") (show x)

versionHandshakeDriver ::
  forall (m :: Type -> Type) pr.
  ( Monad m
  , MonadThrow m
  , MonadST m
  ) =>
  RawBearer m ->
  Tracer m VersionHandshakeDriverTrace ->
  Driver VersionHandshakeProtocol pr () m
versionHandshakeDriver s tracer =
  Driver {sendMessage, recvMessage, initialDState = ()}
  where
    sendMessage ::
      forall (st :: VersionHandshakeProtocol) (st' :: VersionHandshakeProtocol).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) WeHaveAgency (Relative pr (StateAgency st)) ->
      Message VersionHandshakeProtocol st st' ->
      m ()
    sendMessage = \_ msg -> case (stateToken @st, msg) of
      (SInitialState, VersionOfferMessage versions) -> do
        traceWith tracer $ VersionHandshakeDriverOfferingVersions versions
        sendItem s versions
        return ()
      (SVersionsOfferedState, VersionAcceptMessage version) -> do
        traceWith tracer $ VersionHandshakeDriverAcceptingVersion version
        sendItem s (Just version)
        return ()
      (SVersionsOfferedState, VersionRejectedMessage) -> do
        traceWith tracer $ VersionHandshakeDriverRejectingVersion
        sendItem s (Nothing :: Maybe VersionIdentifier)
        return ()
      (SEndState, _) -> error "This cannot happen"

    recvMessage ::
      forall (st :: VersionHandshakeProtocol).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) TheyHaveAgency (Relative pr (StateAgency st)) ->
      () ->
      m (SomeMessage st, ())
    recvMessage = \_ () -> case stateToken @st of
      SInitialState -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK versions -> return (SomeMessage (VersionOfferMessage versions), ())
          x -> throw x
      SVersionsOfferedState -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK Nothing -> return (SomeMessage VersionRejectedMessage, ())
          ReadOK (Just version) -> return (SomeMessage (VersionAcceptMessage version), ())
          x -> throw x
      SEndState -> error "This cannot happen"

instance HasInfo (DirectCodec m) (Message VersionHandshakeProtocol InitialState VersionsOfferedState) where
  info codec _ = info codec (Proxy @[VersionIdentifier])

instance HasInfo (DirectCodec m) (Message VersionHandshakeProtocol VersionsOfferedState EndState) where
  info codec _ = info codec (Proxy @(Maybe VersionIdentifier))
