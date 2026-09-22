{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

module Network.Mux.Bearer
  ( Bearer (..)
  , MakeBearerCb
  , MakeBearer (..)
  , BearerTrace (..)
  , makeSocketBearer
  , makeSocketBearer'
  , makeSocketBearerWith
  , tcpNotSentLowWat
  , makePipeChannelBearer
  , makeQueueChannelBearer
#if defined(mingw32_HOST_OS)
  , makeNamedPipeBearer
#endif
  , MonadReadBuffer (..)
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI

import           Control.Exception (IOException)
import           Data.ByteString.Lazy qualified as BL
import           Network.Socket (Socket)
import           Network.Socket qualified as Socket
#if defined(mingw32_HOST_OS)
import           System.Win32 (HANDLE)
#endif
import           Foreign.Marshal.Alloc

import           Network.Mux.Bearer.Pipe
import           Network.Mux.Bearer.Queues
import           Network.Mux.Bearer.Socket
import           Network.Mux.Trace
import           Network.Mux.Types hiding (egressInterval)
#if defined(mingw32_HOST_OS)
import           Network.Mux.Bearer.NamedPipe
#endif

-- | Callback which constructs a bearer, see `MakeBearer`.
--
type MakeBearerCb m fd =
       DiffTime
    -- ^ Timeout for reading an SDU segment, if negative no timeout is
    -- applied.  The timeout is not applied to the first SDU segment received
    -- from the network, which allows a mini-protocol to have longer
    -- timeouts than the one given here (or even have no timeout).
    --
    -- NOTE: a mini-protocol timeouts (which are not responsibility of
    -- `network-mux` library) might include the time waiting for the response,
    -- receiving all bytes, and the time required to parse the message.
    -> fd
    -- ^ file descriptor
    -> Maybe (ReadBuffer m)
    -- ^ optional `ReadBuffer`
    -> m (Bearer m)


-- | Construct a bearer using a `MakeBearerCb`.
--
newtype MakeBearer m fd = MakeBearer { getBearer :: MakeBearerCb m fd }

pureBearer :: Applicative m
           => (DiffTime -> fd -> Maybe (ReadBuffer m) ->    Bearer m)
           ->  DiffTime -> fd -> Maybe (ReadBuffer m) -> m (Bearer m)
pureBearer f = \sduTimeout rb fd -> pure (f sduTimeout rb fd)


-- | `Socket` Bearer without egress interval.
--
makeSocketBearer :: MakeBearer IO Socket
makeSocketBearer = makeSocketBearer' 0

makeSocketBearer'
  :: DiffTime
  -- ^ egress interval
  -> MakeBearer IO Socket
makeSocketBearer' egressInterval = makeSocketBearerWith egressInterval Nothing

-- | @TCP_NOTSENT_LOWAT@ for this platform, if it has one.  FreeBSD and Windows
-- have none: there "writable" means space in the send buffer, and the egress
-- gate closes on a stalled peer only once that buffer is full.
tcpNotSentLowWat :: Maybe Socket.SocketOption
#if defined(linux_HOST_OS)
tcpNotSentLowWat = Just (Socket.SockOpt 6 25)      -- IPPROTO_TCP, TCP_NOTSENT_LOWAT
#elif defined(darwin_HOST_OS)
tcpNotSentLowWat = Just (Socket.SockOpt 6 0x201)   -- Darwin's value, same writability semantics
#else
tcpNotSentLowWat = Nothing
#endif

-- | Socket bearer with an egress interval and, optionally, @TCP_NOTSENT_LOWAT@
-- (bytes) set on the socket.  The low-water mark bounds the bytes the kernel
-- holds unsent beyond the congestion window, so that a writable socket is one
-- that can put bytes on the wire now rather than into a buffer.  Silently
-- skipped where the platform lacks the option or refuses it.
makeSocketBearerWith
  :: DiffTime
  -- ^ egress interval
  -> Maybe Int
  -- ^ @TCP_NOTSENT_LOWAT@ in bytes; 'Nothing' or @<= 0@ leaves the socket alone
  -> MakeBearer IO Socket
makeSocketBearerWith egressInterval notSentLowWat = MakeBearer $ \sduTimeout fd rb -> do
    case (tcpNotSentLowWat, notSentLowWat) of
      (Just opt, Just lowat) | lowat > 0 ->
        Socket.setSocketOption fd opt lowat
          `catch` \(_ :: IOException) -> return ()
      _ -> return ()
    return (socketAsBearer size batch rb sduTimeout egressInterval fd)
  where
    size = SDUSize 12_288
    batch = 131_072

class MonadReadBuffer m where
  withReadBufferIO :: (Maybe (ReadBuffer m) -> m a) -> m a

instance MonadReadBuffer IO where
  withReadBufferIO f = allocaBytesAligned size 8 $ \ptr -> do
      v <- newTVarIO BL.empty
      f $ Just $ ReadBuffer v ptr size
    where
      -- Maximum amount of data read in one call.
      -- Corresponds to the default readbuffer size on Linux.
      -- We want it larger than 64Kbyte, but not too large since
      -- it is a memory overhead per mux bearer in an application.
      size = 131_072

makePipeChannelBearer :: MakeBearer IO PipeChannel
makePipeChannelBearer = MakeBearer $ pureBearer (\_ fd _ -> pipeAsBearer size fd)
  where
    size = SDUSize 32_768

makeQueueChannelBearer :: ( MonadSTM   m
                          , MonadMonotonicTime m
                          , MonadThrow m
                          )
                       => MakeBearer m (QueueChannel m)
makeQueueChannelBearer = MakeBearer $ pureBearer (\_ q _ -> queueChannelAsBearer size q)
  where
    size = SDUSize 1_280

#if defined(mingw32_HOST_OS)
makeNamedPipeBearer :: MakeBearer IO HANDLE
makeNamedPipeBearer = MakeBearer $ pureBearer (\_ fd _ -> namedPipeAsBearer size fd)
  where
    size = SDUSize 24_576
#endif
