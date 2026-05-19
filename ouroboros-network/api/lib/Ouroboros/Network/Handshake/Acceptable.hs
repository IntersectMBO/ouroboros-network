module Ouroboros.Network.Handshake.Acceptable
  ( Accept (..)
  , Acceptable (..)
  ) where


import Data.Text

-- | A @'Maybe'@ like type which better explains its purpose.
--
data Accept vData
  = Accept vData
  | Refuse !Text
  deriving (Eq, Show)


class Acceptable v where
  -- | The 'acceptableVersion' function ought to be symmetric, this guarantees
  -- that local and remote sides will agree on the same data.
  acceptableVersion :: v -> v -> Accept v
