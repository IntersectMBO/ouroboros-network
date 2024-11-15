module Ouroboros.Network.ConnectionManager.Public
  ( Provenance (..)
  , DataFlow (..)
  , TimeoutExpired (..)
  , AbstractState (..)
  ) where


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
  deriving (Eq, Ord, Show)


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
  deriving (Eq, Ord, Show)


-- | Boolean like type which indicates if the timeout on 'OutboundStateDuplex'
-- has expired.
data TimeoutExpired = Expired | Ticking
  deriving (Eq, Ord, Show)


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
    deriving (Eq, Ord, Show)
