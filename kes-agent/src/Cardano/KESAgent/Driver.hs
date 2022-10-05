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
import System.Socket
import System.Socket.Unsafe
import Control.Monad (void)

import Cardano.Crypto.KES.Class

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.DirectReadWrite

driver :: DirectWrite (SignKeyKES k)
       => DirectRead (SignKeyKES k)
       => Socket f t p -- | A socket to read from
       -> (SignKeyKES k -> IO ()) -- | What to do with KES keys we receive
       -> Driver (KESProtocol k) () IO
driver s handle = Driver
  { sendMessage = \(ServerAgency TokIdle) (Message sk) -> do
      directRead (\buf bufSize -> void $ unsafeSend s buf bufSize msgNoSignal) sk
  , recvMessage = \(ServerAgency TokIdle) () -> do
      sk <- directWrite (\buf bufSize -> void (unsafeReceive s buf bufSize msgNoSignal))
      handle sk
      return (SomeMessage (Message sk), ())
  , startDState = ()
  }
