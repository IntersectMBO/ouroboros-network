{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DeriveGeneric       #-}

module Ouroboros.Network.ConnectionId where

import           Cardano.Prelude (UseIsNormalForm (..), NoUnexpectedThunks (..))

import           Data.Hashable
import           GHC.Generics (Generic)


-- | Connection is identified by local and remote address.
--
-- TODO: the type variable which this data type fills in is called `peerid`.  We
-- should renamed to `connectionId`.
--
data ConnectionId addr = ConnectionId {
    localAddress  :: !addr,
    remoteAddress :: !addr
  }
  deriving (Eq, Ord, Show, Generic)
  deriving NoUnexpectedThunks via (UseIsNormalForm (ConnectionId addr))

instance Hashable a => Hashable (ConnectionId a)
