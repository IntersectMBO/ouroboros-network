module System.Win32.Async.Socket.ByteString
  ( send
  , sendAll
  ) where

import           Control.Monad (when)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import           Foreign.Ptr (castPtr)

import           Network.Socket (Socket)

import           System.Win32.Async.Socket


-- | Send a 'ByteString' over a socket, which must be in a connected state.
-- Returns number of bytes sent.
--
send :: Socket
     -> ByteString
     -> IO Int
send sock bs = BS.unsafeUseAsCStringLen bs $ \(str, size) ->
    sendBuf sock (castPtr str) size


-- | Like 'send' but makes sure that all bytes are actually sent.
--
sendAll :: Socket
        -> ByteString
        -> IO ()
sendAll _    bs | BS.null bs = return ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent >= 0)
      $ sendAll sock (BS.drop sent bs)
