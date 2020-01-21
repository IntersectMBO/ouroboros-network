{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Snocket
  ( Accept (..)
  , AddressFamily (..)
  , Snocket (..)
  ) where

import           Control.Exception
import           Control.Tracer (Tracer)
import           Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket
#if defined(mingw32_HOST_OS)
import qualified System.Win32.Async      as Win32.Async
#endif

import           Network.Mux.Types (MuxBearer)
import           Network.Mux.Trace (MuxTrace)

import           Ouroboros.Network.IOManager


-- | Named pipes and Berkeley sockets have different API when accepting
-- a connection.  For named pipes the file descriptor created by 'createNamedPipe' is
-- supposed to be used for the first connected client.  Named pipe accept loop
-- looks this way:
--
-- > acceptLoop k = do
-- >   h <- createNamedPipe name
-- >   connectNamedPipe h
-- >   -- h is now in connected state
-- >   forkIO (k h)
-- >   acceptLoop k
--
-- For Berkeley sockets equivalent loop starts by creating a socket
-- which accepts connections and accept returns a new socket in connected
-- state
--
-- > acceptLoop k = do
-- >     s <- socket ...
-- >     bind s address
-- >     listen s
-- >     loop s
-- >   where
-- >     loop s = do
-- >       (s' , _addr') <- accept s
-- >       -- s' is in connected state
-- >       forkIO (k s')
-- >       loop s
--
-- To make common API for both we use a recursive type 'Accept', see
-- 'berkeleyAccept' below.  Creation of the socket / named pipe is part of
-- 'Snocket', but this means we need to have different recursion step for named
-- pipe & sockets.  For sockets its recursion step will always return 'accept'
-- syscall; for named pipes the first callback wil reuse the file descriptor
-- created by 'open' and only subsequent calls will create a new file
-- descriptor by `createNamedPipe`, see 'namedPipeSnocket'.
--
newtype Accept addr fd = Accept
  { runAccept :: IO (fd, addr, Accept addr fd)
  }

data AddressFamily addr where

    SocketFamily  :: !Socket.Family
                  -> AddressFamily Socket.SockAddr

    NamedPipeFamily :: AddressFamily FilePath


berkeleyAccept :: AssociateWithIOCP
               -> Socket
               -> Accept SockAddr Socket
berkeleyAccept iocp sock = go
    where
      go = Accept $ do
        (sock', addr') <-
#if !defined(mingw32_HOST_OS)
          Socket.accept sock
#else
          Win32.Async.accept sock
#endif
        associateWithIOCP iocp (Right sock')
          `catch` \(e :: IOException) -> do
            Socket.close sock'
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Socket.close sock'
            throwIO e
        return (sock', addr', go)


-- | Abstract communication interface that can be used by more than
-- 'Socket'.  Snockets are polymorphic over moand which is used, this feature
-- is useful for testing and/or simulations.
--
data Snocket m fd addr = Snocket {
    getLocalAddr  :: fd -> m addr
  , getRemoteAddr :: fd -> m addr

  , addrFamily :: addr -> AddressFamily addr

  -- | Open a file descriptor  (socket / namedPipe).  For named pipes this is
  -- using 'CreateNamedPipe' syscall, for Berkeley sockets 'socket' is used..
  --
  , open          :: AddressFamily addr -> m fd

    -- | A way to create 'fd' to pass to 'connect'.  For named pipes it will
    -- use 'CreateFile' syscall.  For Berkeley sockets this the same as 'open'.
    --
    -- For named pipes we need full 'addr' rather than just address family as
    -- it is for sockets.
    --
  , openToConnect :: addr ->  m fd

    -- | `connect` is only needed for Berkeley sockets, for named pipes this is
    -- no-op.
    --
  , connect       :: fd -> addr -> m ()
  , bind          :: fd -> addr -> m ()
  , listen        :: fd -> m ()

  , accept        :: fd -> Accept addr fd

  , close         :: fd -> m ()

  , toBearer      :: Tracer m MuxTrace -> fd -> (MuxBearer m)
  }
