module Ouroboros.Network.PerasSupportStatus (PerasSupportStatus (..)) where

-- | The flag which indicates whether the node can support Peras protocol.
--
-- It is communicated via 'NodeToNodeVersionData' during handshake
-- negotiation. If both the local and remote nodes support Peras protocol,
-- then Peras protocol can be used.

data PerasSupportStatus = PerasUnsupported | PerasSupported
  deriving (Eq, Show)

instance Ord PerasSupportStatus where
  PerasUnsupported <= PerasUnsupported = True
  PerasSupported <= PerasUnsupported   = False
  PerasUnsupported <= PerasSupported   = True
  PerasSupported <= PerasSupported     = True
