#include "HsNet.h"

module Linger
  ( StructLinger (..)
  ) where

import           Foreign.C (CInt)
import           Foreign.Storable (Storable (..))


-- TODO: to be removed once a new version of `network` library is released.
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
