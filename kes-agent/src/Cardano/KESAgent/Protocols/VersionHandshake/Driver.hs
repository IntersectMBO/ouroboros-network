{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.VersionHandshake.Driver
  where

import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.Pretty

import Control.Exception
import Control.Tracer ( Tracer, traceWith )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Data.Coerce
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Monad.Class.MonadST
import Data.Proxy

import Ouroboros.Network.RawBearer

import Data.SerDoc.Info ( Description (..), aliasField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)

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

versionHandshakeDriver :: forall (m :: * -> *) pr
                        . ( Monad m
                          , MonadThrow m
                          , MonadST m
                          )
                       => RawBearer m
                       -> Tracer m VersionHandshakeDriverTrace
                       -> Driver VersionHandshakeProtocol pr () m
versionHandshakeDriver s tracer =
  Driver { sendMessage, recvMessage, initialDState = () }
  where

    sendMessage :: forall (st :: VersionHandshakeProtocol) (st' :: VersionHandshakeProtocol)
                 . ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st) WeHaveAgency (Relative pr (StateAgency st))
                -> Message VersionHandshakeProtocol st st'
                -> m ()
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

    recvMessage :: forall (st :: VersionHandshakeProtocol)
                 . ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st) TheyHaveAgency (Relative pr (StateAgency st))
                -> ()
                -> m (SomeMessage st, ())
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
