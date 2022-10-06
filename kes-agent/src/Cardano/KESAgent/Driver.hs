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
import Cardano.KESAgent.DirectSerialise

driver :: DirectDeserialise (SignKeyKES k)
       => DirectSerialise (SignKeyKES k)
       => Socket f t p -- | A socket to read from
       -> Driver (KESProtocol k) () IO
driver s = Driver
  { sendMessage = \(ServerAgency TokIdle) (Message sk) -> do
      directSerialise (\buf bufSize -> void $ unsafeSend s buf bufSize msgNoSignal) sk
  , recvMessage = \(ServerAgency TokIdle) () -> do
      sk <- directDeserialise (\buf bufSize -> void (unsafeReceive s buf bufSize msgNoSignal))
      return (SomeMessage (Message sk), ())
  , startDState = ()
  }
