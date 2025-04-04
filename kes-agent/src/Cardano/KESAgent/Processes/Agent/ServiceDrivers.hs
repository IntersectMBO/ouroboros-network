{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.KESAgent.Processes.Agent.ServiceDrivers
where

import Control.Monad (void)
import Control.Tracer (Tracer)
import Data.Proxy (Proxy (..))
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Ouroboros.Network.RawBearer

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..))
import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.Monad
import Cardano.KESAgent.Protocols.RecvResult (RecvResult (..))
import qualified Cardano.KESAgent.Protocols.Service.V0.Driver as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Peers as SP0
import qualified Cardano.KESAgent.Protocols.Service.V0.Protocol as SP0
import qualified Cardano.KESAgent.Protocols.Service.V1.Driver as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Peers as SP1
import qualified Cardano.KESAgent.Protocols.Service.V1.Protocol as SP1
import qualified Cardano.KESAgent.Protocols.Service.V2.Driver as SP2
import qualified Cardano.KESAgent.Protocols.Service.V2.Peers as SP2
import qualified Cardano.KESAgent.Protocols.Service.V2.Protocol as SP2
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol

data ServiceDriver m c
  = ServiceDriver
  { serviceDriverVersionID :: VersionIdentifier
  , serviceDriverRun :: ServiceDriverRun m c
  }

type ServiceDriverRun m c =
  RawBearer m ->
  Tracer m ServiceDriverTrace ->
  m (TaggedBundle m c) ->
  m (TaggedBundle m c) ->
  (RecvResult -> m ()) ->
  m ()

lookupServiceDriver ::
  VersionIdentifier ->
  [ServiceDriver m c] ->
  Maybe (ServiceDriverRun m c)
lookupServiceDriver _ [] =
  Nothing
lookupServiceDriver vid (x : xs)
  | serviceDriverVersionID x == vid =
      Just (serviceDriverRun x)
  | otherwise =
      lookupServiceDriver vid xs

class ServiceCrypto c where
  availableServiceDrivers ::
    forall m.
    AgentContext m c =>
    [ServiceDriver m c]

wrapCurrentKey :: MonadAgent m => m (TaggedBundle m c) -> m (Maybe (Bundle m c))
wrapCurrentKey currentKey = do
  currentKey >>= \case
    TaggedBundle {taggedBundle = Nothing} -> return Nothing
    TaggedBundle {taggedBundle = (Just bundle)} -> return (Just bundle)

wrapNextKey :: MonadAgent m => m (TaggedBundle m c) -> m (Maybe (Bundle m c))
wrapNextKey currentKey = go
  where
    go = do
      currentKey >>= \case
        TaggedBundle {taggedBundle = Nothing} -> return Nothing
        TaggedBundle {taggedBundle = (Just bundle)} -> return (Just bundle)

mkServiceDriverSP0 ::
  forall m c.
  AgentContext m c =>
  ServiceDriver m c
mkServiceDriverSP0 =
  ServiceDriver
    (versionIdentifier (Proxy @(SP0.ServiceProtocol _ c)))
    ( \bearer tracer currentKey nextKey reportPushResult -> do
        void $
          runPeerWithDriver
            (SP0.serviceDriver bearer tracer)
            ( SP0.servicePusher
                (wrapCurrentKey currentKey)
                (wrapNextKey nextKey)
                reportPushResult
            )
    )

mkServiceDriverSP1 ::
  forall m.
  AgentContext m StandardCrypto =>
  ServiceDriver m StandardCrypto
mkServiceDriverSP1 =
  ServiceDriver
    (versionIdentifier (Proxy @(SP1.ServiceProtocol _)))
    ( \bearer tracer currentKey nextKey reportPushResult -> do
        void $
          runPeerWithDriver
            (SP1.serviceDriver bearer tracer)
            ( SP1.servicePusher
                (wrapCurrentKey currentKey)
                (wrapNextKey nextKey)
                reportPushResult
            )
    )

mkServiceDriverSP2 ::
  forall m.
  AgentContext m StandardCrypto =>
  ServiceDriver m StandardCrypto
mkServiceDriverSP2 =
  ServiceDriver
    (versionIdentifier (Proxy @(SP2.ServiceProtocol _)))
    ( \bearer tracer currentKey nextKey reportPushResult -> do
        void $
          runPeerWithDriver
            (SP2.serviceDriver bearer tracer)
            (SP2.servicePusher currentKey nextKey reportPushResult)
    )

instance ServiceCrypto StandardCrypto where
  availableServiceDrivers =
    [ mkServiceDriverSP2
    , mkServiceDriverSP1
    , mkServiceDriverSP0
    ]

instance ServiceCrypto MockCrypto where
  availableServiceDrivers =
    [mkServiceDriverSP0]

instance ServiceCrypto SingleCrypto where
  availableServiceDrivers =
    [mkServiceDriverSP0]
