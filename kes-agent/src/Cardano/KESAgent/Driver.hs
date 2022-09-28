{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE FlexibleContexts #-}
module Cardano.KESAgent.Driver
where

import Foreign (Ptr)
import Foreign.C.Types (CSize)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Cardano.Crypto.KES.Class
import Cardano.KESAgent.Protocol
import System.Socket
import System.Socket.Unsafe
import Control.Monad (void)

class RawMemWrite v where
  rawMemWrite :: (Ptr a -> CSize -> IO ()) -> IO v

class RawMemRead v where
  rawMemRead :: (Ptr a -> CSize -> IO ()) -> v -> IO ()

clientDriver :: RawMemWrite (SignKeyKES k)
             => Socket f t p -- | A socket to read from
             -> (SignKeyKES k -> IO ()) -- | What to do with KES keys we receive
             -> Driver (KESProtocol k) () IO
clientDriver s handle = Driver
  { sendMessage = \_ msg -> case msg of {}
  , recvMessage = \_ () -> do
      sk <- rawMemWrite (\buf bufSize -> void (unsafeReceive s buf bufSize msgNoSignal))
      handle sk
      return (SomeMessage (Message sk), ())
  , startDState = ()
  }
