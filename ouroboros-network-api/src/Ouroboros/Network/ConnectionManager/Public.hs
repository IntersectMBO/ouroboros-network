{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.ConnectionManager.Public
  ( Provenance (..)
  , DataFlow (..)
  , TimeoutExpired (..)
  , AbstractState (..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.String (fromString)
import GHC.Generics


-- | Each connection is is either initiated locally (outbound) or by a remote
-- peer (inbound).
--
data Provenance =
    -- | An inbound connection: one that was initiated by a remote peer.
    --
    Inbound

    -- | An outbound connection: one that was initiated by us.
    --
  | Outbound
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON Provenance where
  toEncoding = Aeson.string . show

-- | Each connection negotiates if it is uni- or bi-directional.  'DataFlow'
-- is a life time property of a connection, once negotiated it never changes.
--
-- NOTE: This type is isomorphic to `DiffusionMode` for `node-to-node`
-- connections (see `Ouroboros.Network.Diffusion.P2P.ntnDataFlow`), but it isn't
-- for `node-to-client` connections (see
-- `Ouroboros.Network.Diffusion.P2P.ntcDataFlow).
--
data DataFlow
    = Unidirectional
    | Duplex
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON DataFlow where
  toEncoding = Aeson.string . show


-- | Boolean like type which indicates if the timeout on 'OutboundStateDuplex'
-- has expired.
data TimeoutExpired = Expired | Ticking
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON TimeoutExpired where
  toEncoding = Aeson.string . show


-- | Useful for tracing and error messages.
--
data AbstractState =
    -- | Unknown connection.  This state indicates the connection manager
    -- removed this connection from its state.
      UnknownConnectionSt
    | ReservedOutboundSt
    | UnnegotiatedSt !Provenance
    | InboundIdleSt  !DataFlow
    | InboundSt      !DataFlow
    | OutboundUniSt
    | OutboundDupSt  !TimeoutExpired
    | OutboundIdleSt !DataFlow
    | DuplexSt
    | WaitRemoteIdleSt
    | TerminatingSt
    | TerminatedSt
    deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON AbstractState where
  toEncoding UnknownConnectionSt =
    Aeson.pairs $ fromString "type" Aeson..= "UnknownConnectionState"
  toEncoding ReservedOutboundSt =
    Aeson.pairs $ fromString "type" Aeson..= "ReservedOutboundState"
  toEncoding (UnnegotiatedSt a) =
    Aeson.pairs $ fromString "type" Aeson..= "UnnegotiatedState"
               <> fromString "provenance" Aeson..= a
  toEncoding (InboundIdleSt a) =
    Aeson.pairs $ fromString "type" Aeson..= "InboundIdleState"
               <> fromString "dataFlow" Aeson..= a
  toEncoding (InboundSt a) =
    Aeson.pairs $ fromString "type" Aeson..= "InboundState"
               <> fromString "dataFlow" Aeson..= a
  toEncoding OutboundUniSt =
    Aeson.pairs $ fromString "type" Aeson..= "OutboundUnidirectionalState"
  toEncoding (OutboundDupSt a) =
    Aeson.pairs $ fromString "type" Aeson..= "OutboundDuplexState"
               <> fromString "timeout" Aeson..= a
  toEncoding (OutboundIdleSt a) =
    Aeson.pairs $ fromString "type" Aeson..= "OutboundIdleState"
               <> fromString "dataFlow" Aeson..= a
  toEncoding DuplexSt =
    Aeson.pairs $ fromString "type" Aeson..= "DuplexState"
  toEncoding WaitRemoteIdleSt =
    Aeson.pairs $ fromString "type" Aeson..= "WaitRemoteIdleState"
  toEncoding TerminatingSt =
    Aeson.pairs $ fromString "type" Aeson..= "TerminatingState"
  toEncoding TerminatedSt =
    Aeson.pairs $ fromString "type" Aeson..= "TerminatedState"
