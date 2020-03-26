module System.Win32.Async.Socket.ByteString
  ( send
  , sendAll
  , sendTo
  , sendAllTo
  , recv
  , recvFrom
  ) where

import           Control.Exception
import           Control.Monad (when)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (createAndTrim)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import           Foreign.Ptr (castPtr)
import           Foreign.Marshal.Alloc (allocaBytes)
import           GHC.IO.Exception (IOErrorType(..))
import           System.IO.Error (mkIOError, ioeSetErrorString)

import           Network.Socket (Socket, SockAddr)

import           System.Win32.Async.Socket


-- | Send a 'ByteString' over a socket, which must be in a connected state, and
-- must be associated with an IO completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.  Returns number
-- of bytes sent.
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


sendTo :: Socket
       -> ByteString
       -> SockAddr
       -> IO Int
sendTo sock bs sa =
    BS.unsafeUseAsCStringLen bs $ \(str, size) ->
      sendBufTo sock (castPtr str) size sa


sendAllTo :: Socket
          -> ByteString
          -> SockAddr
          -> IO ()
sendAllTo _    bs _  | BS.null bs = return ()
sendAllTo sock bs sa = do
    sent <- sendTo sock bs sa
    when (sent >= 0) $ sendAllTo sock (BS.drop sent bs) sa


-- | Recv a 'ByteString' from a socket, which must be in a connected state, and
-- must be associated with an IO completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.  It may return
-- less bytes than requested.
--
recv :: Socket
     -> Int
     -> IO ByteString
recv _sock size | size <= 0 =
    throwIO $
      ioeSetErrorString
        (mkIOError InvalidArgument "System.Win32.Async.Socket.ByteString.recv" Nothing Nothing)
        "non-positive length"
recv sock size = BS.createAndTrim size $ \ptr -> recvBuf sock ptr size


recvFrom :: Socket                     -- ^ Socket
         -> Int                        -- ^ Maximum number of bytes to receive
         -> IO (ByteString, SockAddr)  -- ^ Data received and sender address
recvFrom sock size =
    allocaBytes size $ \ptr -> do
      (len, sockAddr) <- recvBufFrom sock (castPtr ptr) size
      str <- BS.packCStringLen (ptr, len)
      return (str, sockAddr)
