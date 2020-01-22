module System.Win32.Async.Socket.ByteString
  ( send
  , sendAll
  , recv
  ) where

import           Control.Exception
import           Control.Monad (when)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (createAndTrim)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import           Foreign.Ptr (castPtr)
import           GHC.IO.Exception (IOErrorType(..))
import           System.IO.Error (mkIOError, ioeSetErrorString)

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


recv :: Socket
     -> Int
     -> IO ByteString
recv _sock size | size <= 0 =
    throwIO $
      ioeSetErrorString
        (mkIOError InvalidArgument "System.Win32.Async.Socket.ByteString.recv" Nothing Nothing)
        "non-positive length"
recv sock size = BS.createAndTrim size $ \ptr -> recvBuf sock ptr size
