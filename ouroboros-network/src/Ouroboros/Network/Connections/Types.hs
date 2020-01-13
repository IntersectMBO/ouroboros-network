module Ouroboros.Network.Connections.Types
  ( Provenance (..)
  , Decision (..)
  , Connections (..)

  , Client
  , Server
  ) where

import Data.Void (Void)

data Provenance = Incoming | Outgoing
  deriving (Show)

data Decision reject accept = Rejected reject | Accepted accept
  deriving (Show)

data Connections socket reject accept m = Connections
  { include :: Provenance -> socket -> m (Decision reject accept) }

type Client addr reject conn m = addr -> m (Decision reject conn)

type Server m = m Void
