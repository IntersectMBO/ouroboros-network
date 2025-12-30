module Ouroboros.Network.DiffusionMode (DiffusionMode (..)) where


-- | The flag which indicates whether the node runs only initiator or both
-- initiator or responder node.
--
-- This data structure has two proposes:
--
-- * instruct the diffusion layer if it should listen on incoming connections;
--
-- * it is communicated via 'NodeToNodeVersionData' during handshake
--   negotiation. In non-p2p mode we always send 'InitiatorOnlyDiffusionMode',
--   in p2p mode we send exactly what the diffusion is given.  In non-p2p mode
--   every connection outbound port is ephemeral, the remote side cannot connect
--   to it, however in p2p mode the outbound port is actually the port on which
--   the node is listening (if it runs in 'InitiatorAndResponderDiffusionMode').
--
data DiffusionMode
    = InitiatorOnlyDiffusionMode
    | InitiatorAndResponderDiffusionMode
  deriving (Eq, Ord, Show)
