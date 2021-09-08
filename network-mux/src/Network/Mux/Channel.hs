{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An extension of 'Network.TypedProtocol.Channel', with additional 'Channel'
-- implementations.
--
module Network.Mux.Channel
  ( Channel (..)
  , createBufferConnectedChannels
  , createPipeConnectedChannels
#if !defined(mingw32_HOST_OS)
  , createSocketConnectedChannels
#endif
  , withFifosAsChannel
  , socketAsChannel
  , channelEffect
  , delayChannel
  , loggingChannel
  ) where

import qualified Data.ByteString               as BS
#if !defined(mingw32_HOST_OS)
import qualified Data.ByteString.Internal      as BS (createAndTrim')
#endif
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import           Data.Foldable (traverse_)
#if !defined(mingw32_HOST_OS)
import           Data.Word (Word8)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.C.Error (eAGAIN, eWOULDBLOCK, getErrno, throwErrno)
import           Foreign.C.Types
#endif
import qualified System.Process as IO (createPipe)
import qualified System.IO      as IO ( Handle, withFile, IOMode(..), hFlush,
                                        hIsEOF )
import qualified Network.Socket            as Socket
import qualified Network.Socket.ByteString as Socket

import           Control.Concurrent.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTimer.SI


data Channel m = Channel {

    -- | Write bytes to the channel.
    --
    -- It maybe raise exceptions.
    --
    send :: LBS.ByteString -> m (),

    -- | Read some input from the channel, or @Nothing@ to indicate EOF.
    --
    -- Note that having received EOF it is still possible to send.
    -- The EOF condition is however monotonic.
    --
    -- It may raise exceptions (as appropriate for the monad and kind of
    -- channel).
    --
    recv :: m (Maybe LBS.ByteString),

    -- | Try read some input from the channel.  Return @Nothing@ if no input is
    -- available.  It must be a non-blocking IO.
    --
    tryRecv :: m (Maybe (Maybe LBS.ByteString))
  }


-- | Make a 'Channel' from a pair of IO 'Handle's, one for reading and one
-- for writing.
--
-- The Handles should be open in the appropriate read or write mode, and in
-- binary mode. Writes are flushed after each write, so it is safe to use
-- a buffering mode.
--
-- For bidirectional handles it is safe to pass the same handle for both.
--
handlesAsChannel :: IO.Handle -- ^ Read handle
                 -> IO.Handle -- ^ Write handle
                 -> Channel IO
handlesAsChannel hndRead hndWrite =
    Channel{send, recv, tryRecv}
  where
    send :: LBS.ByteString -> IO ()
    send chunk = do
      LBS.hPut hndWrite chunk
      IO.hFlush hndWrite

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      eof <- IO.hIsEOF hndRead
      if eof
        then return Nothing
        else Just . LBS.fromStrict <$> BS.hGetSome hndRead LBS.smallChunkSize

    tryRecv :: IO (Maybe (Maybe LBS.ByteString))
    tryRecv = do
      eof <- IO.hIsEOF hndRead
      if eof
        then return (Just Nothing)
        else Just . Just <$> LBS.hGetNonBlocking hndRead LBS.smallChunkSize

-- | Create a pair of 'Channel's that are connected internally.
--
-- This is intended for inter-thread communication, such as between a
-- multiplexing thread and a thread running a peer.
--
-- It uses lazy 'ByteString's but it ensures that data written to the channel
-- is /fully evaluated/ first. This ensures that any work to serialise the data
-- takes place on the /writer side and not the reader side/.
--
createBufferConnectedChannels :: forall m. MonadSTM m
                              => m (Channel m,
                                    Channel m)
createBufferConnectedChannels = do
    bufferA <- newEmptyTMVarIO
    bufferB <- newEmptyTMVarIO

    return (buffersAsChannel bufferB bufferA,
            buffersAsChannel bufferA bufferB)
  where
    buffersAsChannel bufferRead bufferWrite =
        Channel{send, recv, tryRecv}
      where
        send :: LBS.ByteString -> m ()
        send x = sequence_ [ atomically (putTMVar bufferWrite c)
                           | !c <- LBS.toChunks x ]
                           -- Evaluate the chunk c /before/ doing the STM
                           -- transaction to write it to the buffer.

        recv :: m (Maybe LBS.ByteString)
        recv   = Just . LBS.fromStrict <$> atomically (takeTMVar bufferRead)

        tryRecv :: m (Maybe (Maybe LBS.ByteString))
        tryRecv = fmap (Just . LBS.fromStrict) <$> atomically (tryTakeTMVar bufferRead)


-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'Channel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createPipeConnectedChannels :: IO (Channel IO,
                                   Channel IO)
createPipeConnectedChannels = do
    -- Create two pipes (each one is unidirectional) to make both ends of
    -- a bidirectional channel
    (hndReadA, hndWriteB) <- IO.createPipe
    (hndReadB, hndWriteA) <- IO.createPipe

    return (handlesAsChannel hndReadA hndWriteA,
            handlesAsChannel hndReadB hndWriteB)

-- | Open a pair of Unix FIFOs, and expose that as a 'Channel'.
--
-- The peer process needs to open the same files but the other way around,
-- for writing and reading.
--
-- This is primarily for the purpose of demonstrations that use communication
-- between multiple local processes. It is Unix specific.
--
withFifosAsChannel :: FilePath -- ^ FIFO for reading
                   -> FilePath -- ^ FIFO for writing
                   -> (Channel IO -> IO a) -> IO a
withFifosAsChannel fifoPathRead fifoPathWrite action =
    IO.withFile fifoPathRead  IO.ReadMode  $ \hndRead  ->
    IO.withFile fifoPathWrite IO.WriteMode $ \hndWrite ->
      let channel = handlesAsChannel hndRead hndWrite
       in action channel


-- | Make a 'Channel' from a 'Socket'. The socket must be a stream socket
--- type and status connected.
---
socketAsChannel :: Socket.Socket -> Channel IO
socketAsChannel socket =
    Channel{send, recv, tryRecv}
  where
    send :: LBS.ByteString -> IO ()
    send chunks =
     -- Use vectored writes.
     Socket.sendMany socket (LBS.toChunks chunks)
     -- TODO: limit write sizes, or break them into multiple sends.

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      -- We rely on the behaviour of stream sockets that a zero length chunk
      -- indicates EOF.
      chunk <- Socket.recv socket LBS.smallChunkSize
      if BS.null chunk
        then return Nothing
        else return (Just (LBS.fromStrict chunk))

    tryRecv :: IO (Maybe (Maybe LBS.ByteString))
#if defined(mingw32_HOST_OS)
    -- TODO
    tryRecv = return Nothing
#else
    tryRecv = do
      (bs, wouldBlock) <- BS.createAndTrim' LBS.smallChunkSize $ \ptr -> do
        r <- recvBufNoWait socket ptr LBS.smallChunkSize
        case r of
          (-1) -> return (0, 0, True)
          (-2) -> throwErrno "tryRecv"
          _    -> return (0, r, False)
      return $
        case () of
          _ | wouldBlock -> Nothing
            | BS.null bs -> Just Nothing
            | otherwise  -> Just (Just (LBS.fromStrict bs))


-- | Copied from 'Network.Socket.Buffer.recvBufNoWait'.
--
recvBufNoWait :: Socket.Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWait s ptr nbytes = Socket.withFdSocket s $ \fd -> do
    r <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    if r >= 0 then
        return $ fromIntegral r
      else do
        err <- getErrno
        if err == eAGAIN || err == eWOULDBLOCK then
            return (-1)
          else
            return (-2)

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
#endif

#if !defined(mingw32_HOST_OS)
--- | Create a local socket, with both ends in this process, and expose that as
--- a pair of 'ByteChannel's, one for each end.
---
--- This is primarily for testing purposes since it does not allow actual IPC.
---
createSocketConnectedChannels :: Socket.Family -- ^ Usually AF_UNIX or AF_INET
                              -> IO (Channel IO,
                                     Channel IO)
createSocketConnectedChannels family = do
   -- Create a socket pair to make both ends of a bidirectional channel
   (socketA, socketB) <- Socket.socketPair family Socket.Stream
                                           Socket.defaultProtocol

   return (socketAsChannel socketA,
           socketAsChannel socketB)
#endif

channelEffect :: forall m.
                 Monad m
              => (LBS.ByteString -> m ())       -- ^ Action before 'send'
              -> (Maybe LBS.ByteString -> m ()) -- ^ Action after 'recv'
              -> Channel m
              -> Channel m
channelEffect beforeSend afterRecv Channel{send, recv, tryRecv} =
    Channel{
      send = \x -> do
        beforeSend x
        send x

    , recv = do
        mx <- recv
        afterRecv mx
        return mx

    , tryRecv = do
        mx <- tryRecv
        traverse_ afterRecv mx
        return mx
    }

-- | Delay a channel on the receiver end.
--
-- This is intended for testing, as a crude approximation of network delays.
-- More accurate models along these lines are of course possible.
--
delayChannel :: MonadDelay m
             => DiffTime
             -> Channel m
             -> Channel m
delayChannel delay = channelEffect (\_ -> return ())
                                   (\_ -> threadDelay delay)

-- | Channel which logs sent and received messages.
--
loggingChannel :: ( MonadSay m
                  , Show id
                  )
               => id
               -> Channel m
               -> Channel m
loggingChannel ident Channel{send,recv,tryRecv} =
  Channel {
    send    = loggingSend,
    recv    = loggingRecv,
    tryRecv = loggingTryRecv
  }
 where
  loggingSend a = do
    say (show ident ++ ":send:" ++ show a)
    send a

  loggingRecv = do
    msg <- recv
    case msg of
      Nothing -> return ()
      Just a  -> say (show ident ++ ":recv:" ++ show a)
    return msg

  loggingTryRecv = do
    msg <- tryRecv
    case msg of
      Just (Just a) -> say (show ident ++ ":recv:" ++ show a)
      _             -> return ()
    return msg
