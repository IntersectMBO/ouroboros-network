{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.VersionHandshake.Protocol
where

import Cardano.KESAgent.Protocols.VersionedProtocol

import Network.TypedProtocol.Core

data VersionHandshakeProtocol where
  -- | Default state after connecting.
  InitialState :: VersionHandshakeProtocol
  -- | Available versions have been sent, waiting for the other end to either
  -- accept one, or decline the connection.
  VersionsOfferedState :: VersionHandshakeProtocol
  -- | The session has ended.
  EndState :: VersionHandshakeProtocol

-- | Protocol for negotiating protocol versions.
--
-- Intended use:
--
-- When a client connects to a server, the server sends a list of supported
-- protocol versions; the client then either picks one and sends a
-- 'VersionAcceptMessage', or fails to find a common supported version and
-- closes the connection.
instance Protocol VersionHandshakeProtocol where
  data Message VersionHandshakeProtocol st st' where
    VersionOfferMessage ::
      [VersionIdentifier] ->
      Message VersionHandshakeProtocol InitialState VersionsOfferedState
    VersionAcceptMessage ::
      VersionIdentifier ->
      Message VersionHandshakeProtocol VersionsOfferedState EndState
    VersionRejectedMessage :: Message VersionHandshakeProtocol VersionsOfferedState EndState

  -- \| Server always has agency, except between sending a key and confirming it
  type StateAgency InitialState = ServerAgency

  -- \| Client only has agency between sending a key and confirming it
  type StateAgency VersionsOfferedState = ClientAgency

  -- \| Someone, i.e., the server, always has agency
  type StateAgency EndState = NobodyAgency

  type StateToken = SVersionHandshakeProtocol

data SVersionHandshakeProtocol (st :: VersionHandshakeProtocol) where
  SInitialState :: SVersionHandshakeProtocol InitialState
  SVersionsOfferedState :: SVersionHandshakeProtocol VersionsOfferedState
  SEndState :: SVersionHandshakeProtocol EndState

instance StateTokenI InitialState where stateToken = SInitialState
instance StateTokenI VersionsOfferedState where stateToken = SVersionsOfferedState
instance StateTokenI EndState where stateToken = SEndState

instance VersionedProtocol VersionHandshakeProtocol where
  versionIdentifier _ = vpVersionIdentifier

vpVersionIdentifier :: VersionIdentifier
vpVersionIdentifier = mkVersionIdentifier $ "VersionHandshake:0.1"
