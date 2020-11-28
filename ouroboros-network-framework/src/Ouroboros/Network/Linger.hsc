#include "HsNet.h"

module Ouroboros.Network.Linger
  ( StructLinger (..)
  , reset
  ) where

import           Control.Monad (when)

import           Foreign.C (CInt)
import           Foreign.Storable (Storable (..))

import           Network.Socket (Socket)
import qualified Network.Socket as Socket


data StructLinger = StructLinger {
    -- | Set the linger option on.
    sl_onoff  :: CInt,

    -- | Linger timeout.
    sl_linger :: CInt
  }
  deriving (Eq, Ord, Show)

instance Storable StructLinger where
    sizeOf    _ = (#const sizeof(struct linger))
    alignment _ = alignment (0 :: CInt)

    peek p = do
        onoff  <- (#peek struct linger, l_onoff) p
        linger <- (#peek struct linger, l_linger) p
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        (#poke struct linger, l_onoff)  p onoff
        (#poke struct linger, l_linger) p linger


-- | Restet a TCP connection, by setting 'RST' header on a 'TCP' @FIN@ packet
-- when closing a socket.
--
reset :: Socket -> IO ()
reset sock = do
    when (Socket.isSupportedSocketOption Socket.Linger) $
      Socket.setSockOpt sock
                        Socket.Linger
                        (StructLinger { sl_onoff  = 1,
                                        sl_linger = 0 })
    Socket.close sock
