module Cardano.KESAgent.DirectReadWrite
where

import Foreign.Ptr
import Foreign.C.Types

class DirectWrite a where
  directWrite :: (Ptr CChar -> CSize -> IO ()) -> IO a

class DirectRead a where
  directRead :: (Ptr CChar -> CSize -> IO ()) -> a -> IO ()
