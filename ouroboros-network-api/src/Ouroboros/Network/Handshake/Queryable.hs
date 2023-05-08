module Ouroboros.Network.Handshake.Queryable (Queryable (..)) where

class Queryable v where
  -- | Whether or not there was a query for the supported version.
  queryVersion :: v -> Bool
