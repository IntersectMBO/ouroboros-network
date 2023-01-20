
module Ouroboros.Network.Handshake
  ( HandshakeCallbacks (..)
  ) where

import           Ouroboros.Network.Handshake.Acceptable (Accept)

-- | A record that holds handshake callbacks.
--
data HandshakeCallbacks vData = HandshakeCallbacks {
    acceptCb :: vData -> vData -> Accept vData
  , queryCb  :: vData -> Bool
  }
