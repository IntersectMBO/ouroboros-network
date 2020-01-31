{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Snocket
  ( -- * Snocket Interface
    Accept (..)
  , AddressFamily (..)
  , Snocket (..)
    -- ** Socket based Snocktes
  , SocketSnocket
  , socketSnocket
  , rawSocketSnocket
    -- ** Client Snockets
    -- Using unix sockets (posix) or named pipes (windows)
    --
  , ClientSnocket
  , clientSnocket
  , ClientAddress
  , clientAddressFromPath
  ) where

import           Control.Exception
import           Control.Monad (when)
import           Control.Tracer (Tracer)
import           Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket
#if defined(mingw32_HOST_OS)
import           Data.Bits
import qualified System.Win32            as Win32
import qualified System.Win32.NamedPipes as Win32
import qualified System.Win32.Async      as Win32.Async

import           Network.Mux.Bearer.NamedPipe (namedPipeAsBearer)
#endif

import           Network.Mux.Types (MuxBearer)
import           Network.Mux.Trace (MuxTrace)
import qualified Network.Mux.Bearer.Socket as Mx

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
-- 'berkeleyAccept' below.  Creation of a socket / named pipe is part of
-- 'Snocket', but this means we need to have different recursion step for named
-- pipe & sockets.  For sockets its recursion step will always return 'accept'
-- syscall; for named pipes the first callback will reuse the file descriptor
-- created by 'open' and only subsequent calls will create a new file
-- descriptor by `createNamedPipe`, see 'namedPipeSnocket'.
--
newtype Accept addr fd = Accept
  { runAccept :: IO (fd, addr, Accept addr fd)
  }


-- | BSD accept loop.
--
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


-- | We support either sockets or named pipes.
--
data AddressFamily addr where

    SocketFamily    :: !Socket.Family
                    -> AddressFamily Socket.SockAddr

    NamedPipeFamily :: AddressFamily FilePath

instance Eq (AddressFamily addr) where
    SocketFamily fam0 == SocketFamily fam1 = fam0 == fam1
    NamedPipeFamily   == NamedPipeFamily   = True

instance Show (AddressFamily addr) where
    show (SocketFamily fam) = show fam
    show NamedPipeFamily  = "NamedPipeFamily"


-- | Abstract communication interface that can be used by more than
-- 'Socket'.  Snockets are polymorphic over monad which is used, this feature
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


--
-- Socket based Snockets
--


socketAddrFamily
    :: Socket.SockAddr
    -> AddressFamily Socket.SockAddr
socketAddrFamily (Socket.SockAddrInet  _ _    ) = SocketFamily Socket.AF_INET
socketAddrFamily (Socket.SockAddrInet6 _ _ _ _) = SocketFamily Socket.AF_INET6
socketAddrFamily (Socket.SockAddrUnix _       ) = SocketFamily Socket.AF_UNIX


type SocketSnocket = Snocket IO Socket SockAddr


-- | Create a 'Snocket' for the given 'Socket.Family'. In the 'bind' method set
-- 'Socket.ReuseAddr` and 'Socket.ReusePort'.
--
socketSnocket
  :: AssociateWithIOCP
  -- ^ associate the socket with I/O CP.  We use it when we create a new socket
  -- and when we accept a connection.
  --
  -- Though it could be used in `open`, but that is going to be used in
  -- a bracket so it's better to keep it simple.
  --
  -> SocketSnocket
socketSnocket iocp = Snocket {
      getLocalAddr   = Socket.getSocketName
    , getRemoteAddr  = Socket.getPeerName
    , addrFamily     = socketAddrFamily
    , open           = openSocket
    , openToConnect  = \addr -> openSocket (socketAddrFamily addr)
    , connect        = \s a -> do
#if !defined(mingw32_HOST_OS)
        Socket.connect s a
#else
        Win32.Async.connect s a
#endif
    , bind = \sd addr -> do
        let SocketFamily fml = socketAddrFamily addr
        when (fml == Socket.AF_INET ||
              fml == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          -- not supported on Windows 10
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
        when (fml == Socket.AF_INET6)
          -- An AF_INET6 socket can be used to talk to both IPv4 and IPv6 end points, and
          -- it is enabled by default on some systems. Disabled here since we run a separate
          -- IPv4 server instance if configured to use IPv4.
          $ Socket.setSocketOption sd Socket.IPv6Only 1

        Socket.bind sd addr
    , listen   = \s -> Socket.listen s 1
    , accept   = berkeleyAccept iocp
    , close    = Socket.close
    , toBearer = Mx.socketAsMuxBearer
    }
  where
    openSocket :: AddressFamily SockAddr -> IO Socket
    openSocket (SocketFamily family_) = do
      sd <- Socket.socket family_ Socket.Stream Socket.defaultProtocol
      associateWithIOCP iocp (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associteWithIOCP' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return sd



-- | Create a snocket for the given 'Socket.Family'.  This snocket does not set
-- any options on the underlying socket.
--
rawSocketSnocket
  :: AssociateWithIOCP
  -> SocketSnocket
rawSocketSnocket iocp = Snocket {
      getLocalAddr  = Socket.getSocketName
    , getRemoteAddr = Socket.getPeerName
    , addrFamily    = socketAddrFamily
    , connect       = \s a -> do
#if !defined(mingw32_HOST_OS)
        Socket.connect s a
#else
        Win32.Async.connect s a
#endif
    , bind          = \fd addr -> Socket.bind fd addr
    , listen        = flip Socket.listen 1
    , accept        = berkeleyAccept iocp
    , open          = openSocket
    , openToConnect = \addr -> openSocket (socketAddrFamily addr)
    , close         = Socket.close
    , toBearer      = Mx.socketAsMuxBearer
    }
  where
    openSocket :: AddressFamily SockAddr -> IO Socket
    openSocket (SocketFamily family_) = do
      sd <- Socket.socket family_ Socket.Stream Socket.defaultProtocol
      associateWithIOCP iocp (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associteWithIOCP' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return sd
      

--
-- NamedPipes based Snocket
--


#if defined(mingw32_HOST_OS)
type HANDLESnocket = Snocket IO Win32.HANDLE FilePath

-- | Create a Windows Named Pipe Snocket.
--
namedPipeSnocket
  :: AssociateWithIOCP
  -> FilePath
  -> HANDLESnocket
namedPipeSnocket iocp name = Snocket {
      getLocalAddr  = \_ -> return name
    , getRemoteAddr = \_ -> return name
    , addrFamily  = \_ -> NamedPipeFamily

    , open = \_addrFamily -> do
        hpipe <- Win32.createNamedPipe
                   name
                   (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                   (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                   Win32.pIPE_UNLIMITED_INSTANCES
                   maxBound
                   maxBound
                   0
                   Nothing
        associateWithIOCP iocp (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        pure hpipe

    -- To connect, simply create a file whose name is the named pipe name.
    , openToConnect  = \pipeName -> do
        hpipe <- Win32.createFile pipeName
                   (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE )
                   Win32.fILE_SHARE_NONE
                   Nothing
                   Win32.oPEN_EXISTING
                   Win32.fILE_FLAG_OVERLAPPED
                   Nothing
        associateWithIOCP iocp (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        return hpipe
    , connect  = \_ _ -> pure ()

    -- Bind and listen are no-op.
    , bind     = \_ _ -> pure ()
    , listen   = \_ -> pure ()

    , accept   = \hpipe -> Accept $ do
          Win32.Async.connectNamedPipe hpipe
          return (hpipe, name, acceptNext)

    , close    = Win32.closeHandle

    , toBearer = namedPipeAsBearer
    }
  where
    acceptNext :: Accept FilePath Win32.HANDLE
    acceptNext = Accept $ do
      hpipe <- Win32.createNamedPipe
                 name
                 (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                 (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                 Win32.pIPE_UNLIMITED_INSTANCES
                 maxBound
                 maxBound
                 0
                 Nothing
              `catch` \(e :: IOException) -> do
                 putStrLn $ "accept: " ++ show e
                 throwIO e
      associateWithIOCP iocp (Left hpipe)
      Win32.Async.connectNamedPipe hpipe
      return (hpipe, name, acceptNext)
#endif


--
-- Windows/POSIX type aliases
--


-- | System dependent ClientSnocket type
#if defined(mingw32_HOST_OS)
type ClientSnocket = HANDLESnocket

clientSnocket :: AssociateWithIOCP -> FilePath -> ClientSnocket
clientSnocket = namedPipeSnocket
#else
type ClientSnocket = SocketSnocket

clientSnocket :: AssociateWithIOCP -> FilePath -> ClientSnocket
clientSnocket iocp _  = rawSocketSnocket iocp
#endif

#if defined(mingw32_HOST_OS)
type ClientAddress = FilePath
#else
type ClientAddress = Socket.SockAddr
#endif

clientAddressFromPath :: FilePath -> ClientAddress
#if defined(mingw32_HOST_OS)
clientAddressFromPath = id
#else
clientAddressFromPath = Socket.SockAddrUnix
#endif
