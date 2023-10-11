{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.KESAgent.Protocols.VersionHandshake.Driver
  where

import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec
import Cardano.KESAgent.Util.Pretty

import Control.Exception
import Control.Tracer ( Tracer, traceWith )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Data.Coerce
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Monad.Class.MonadST

import Ouroboros.Network.RawBearer

-- | Logging messages that the Driver may send
data VersionHandshakeDriverTrace
  = VersionHandshakeDriverOfferingVersions ![VersionIdentifier]
  | VersionHandshakeDriverAcceptingVersion !VersionIdentifier
  | VersionHandshakeDriverRejectingVersion
  | VersionHandshakeDriverMisc !String
  deriving (Show)

instance Pretty VersionHandshakeDriverTrace where
  pretty (VersionHandshakeDriverMisc x) = x
  pretty x = drop (strLength "VersionHandshakeDriver") (show x)

versionHandshakeDriver :: ( Monad m
                          , MonadThrow m
                          , MonadST m
                          , (forall a b. Coercible a b => Coercible (m a) (m b))
                          )
                       => RawBearer m
                       -> Tracer m VersionHandshakeDriverTrace
                       -> Driver VersionHandshakeProtocol () m
versionHandshakeDriver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionOfferMessage versions) -> do
        sendItem s versions
        return ()

      (ClientAgency TokVersionsOffered, VersionAcceptMessage version) -> do
        sendItem s (Just version)
        return ()

      (ClientAgency TokVersionsOffered, VersionRejectedMessage) -> do
        sendItem s (Nothing :: Maybe VersionIdentifier)
        return ()

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK versions -> return (SomeMessage (VersionOfferMessage versions), ())
          x -> throw x

      (ClientAgency TokVersionsOffered) -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK Nothing -> return (SomeMessage VersionRejectedMessage, ())
          ReadOK (Just version) -> return (SomeMessage (VersionAcceptMessage version), ())
          x -> throw x

  , startDState = ()
  }

instance HasSerInfo (Message VersionHandshakeProtocol InitialState VersionsOfferedState) where
  info _ = info (Proxy @[VersionIdentifier])

instance HasSerInfo (Message VersionHandshakeProtocol VersionsOfferedState EndState) where
  info _ = info (Proxy @(Maybe VersionIdentifier))
