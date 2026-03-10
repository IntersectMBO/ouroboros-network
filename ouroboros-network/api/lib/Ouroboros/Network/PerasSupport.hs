{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.PerasSupport
  ( PerasSupport (..)
  , perasSupportToBool
  , perasSupportFromBool
  ) where

import GHC.Generics (Generic)

-- | The flag which indicates whether the node can support Peras protocol.
--
-- It is communicated via 'NodeToNodeVersionData' during handshake
-- negotiation. If both the local and remote nodes support Peras protocol,
-- then Peras protocol can be used.

data PerasSupport = PerasUnsupported | PerasSupported
  deriving (Eq, Ord, Show, Bounded, Generic)


perasSupportToBool :: PerasSupport -> Bool
perasSupportToBool PerasUnsupported = False
perasSupportToBool PerasSupported   = True

perasSupportFromBool :: Bool -> PerasSupport
perasSupportFromBool False = PerasUnsupported
perasSupportFromBool True  = PerasSupported
