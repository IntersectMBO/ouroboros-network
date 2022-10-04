{-#LANGUAGE FlexibleContexts #-}
module Cardano.KESAgent.API
where

import System.Socket
import System.Socket.Unsafe
import Control.Exception
import Control.Monad
import Cardano.Crypto.KES.Class
import Foreign.Ptr
import Foreign.C.Types
import System.IO

import Cardano.KESAgent.DirectReadWrite

runClient :: ( DirectWrite k
             , Family f
             )
          => Socket f t p
          -> SocketAddress f
          -> (k -> IO ())
          -> IO ()
runClient socket address handle = do
  bracket_
    (connect socket address)
    (close socket)
    (forever $ directWrite pull >>= handle)
  where
    pull :: Ptr CChar -> CSize -> IO ()
    pull buf bufsize =
      void $ unsafeReceive socket buf bufsize msgNoSignal

runServer :: ( DirectRead k
             , Family f
             )
          => Socket f t p
          -> SocketAddress f
          -> IO k -- ^ return the current key immediately
          -> IO k -- ^ block until a new key is available, then return it
          -> IO ()
runServer socket address currentKey nextKey = do
  bracket_
    (bind socket address >> listen socket 0)
    (close socket)
    (forever $ acceptAndHandle socket `catch` \e -> hPrint stderr (e :: SocketException))
    where
      acceptAndHandle socket = bracket
        (accept socket)
        (\(p, addr) -> do
            close p
        )
        (\(p, addr) -> do
            currentKey >>= directRead (sendKey p)
            forever $ nextKey >>= directRead (sendKey p)
        )

      sendKey :: Family f => Socket f t p -> Ptr CChar -> CSize -> IO ()
      sendKey p buf bufsize =
        void $ unsafeSend p buf bufsize msgNoSignal
