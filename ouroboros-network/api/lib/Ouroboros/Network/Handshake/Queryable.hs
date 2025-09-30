module Ouroboros.Network.Handshake.Queryable (Queryable (..)) where

class Queryable versionData where
  -- | Whether or not there was a query for the supported version.
  queryVersion :: versionData -> Bool
