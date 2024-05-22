{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE PackageImports             #-}
#endif

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Ouroboros.Network.Snocket
  ( -- * Snocket Interface
    Accept (..)
  , Accepted (..)
  , AddressFamily (..)
  , Snocket (..)
  , makeSocketBearer
    -- ** Socket based Snockets
  , SocketSnocket
  , socketSnocket
    -- ** Local Snockets
    -- Using unix sockets (POSIX) or named pipes (Windows)
    --
  , LocalSnocket
  , localSnocket
  , makeLocalBearer
  , LocalSocket (..)
  , LocalAddress (..)
  , localAddressFromPath
  , TestAddress (..)
  , FileDescriptor
  , socketFileDescriptor
  , localSocketFileDescriptor
    -- ** for testing
  , invalidFileDescriptor
    -- * Re-exports
  , MakeBearer (..)
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Quiet (Quiet (..))
#if defined(mingw32_HOST_OS)
import Data.Bits
import Foreign.Ptr (IntPtr (..), ptrToIntPtr)
import System.Win32 qualified as Win32
import System.Win32.Async qualified as Win32.Async
import "Win32-network" System.Win32.NamedPipes qualified as Win32
#endif

import NoThunks.Class

import Network.Socket (SockAddr (..), Socket)
import Network.Socket qualified as Socket

import Network.Mux.Bearer

import Ouroboros.Network.IOManager


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
newtype Accept m fd addr = Accept
  { runAccept :: m (Accepted fd addr, Accept m fd addr)
  }
  deriving Functor

instance Functor m => Bifunctor (Accept m) where
    bimap f g (Accept ac) = Accept (h <$> ac)
      where
        h (accepted, next) = (bimap f g accepted, bimap f g next)


data Accepted fd addr where
    AcceptFailure :: !SomeException -> Accepted fd addr
    Accepted      :: !fd -> !addr -> Accepted fd addr

instance Functor (Accepted fd) where
    fmap f (Accepted fd addr)  = Accepted fd (f addr)
    fmap _ (AcceptFailure err) = AcceptFailure err

instance Bifunctor Accepted where
    bimap f g (Accepted fd addr)  = Accepted (f fd) (g addr)
    bimap _ _ (AcceptFailure err) = AcceptFailure err

instance Bifoldable Accepted where
    bifoldMap f g (Accepted fd addr) = f fd <> g addr
    bifoldMap _ _ (AcceptFailure _)  = mempty


-- | BSD accept loop.
--
berkeleyAccept :: IOManager
               -> Socket
               -> IO (Accept IO Socket SockAddr)
berkeleyAccept ioManager sock =
      go 0 <$> Socket.getSocketName sock
    where
      go !cnt !addr = Accept (acceptOne addr cnt `catch` handleException addr cnt)

      acceptOne
        :: SockAddr
        -> Word64
        -> IO ( Accepted  Socket SockAddr
              , Accept IO Socket SockAddr
              )
      acceptOne addr cnt =
        bracketOnError
#if defined(POSIX)
          (Socket.accept sock)
#else
          (Win32.Async.accept sock)
#endif
          (uninterruptibleMask_ . Socket.close . fst)
          $ \(sock', addr') -> do
            associateWithIOManager ioManager (Right sock')

            -- UNIX sockets don't provide a unique endpoint for the remote
            -- side, but the InboundGovernor/Server requires one in order to
            -- track connections.
            -- So to differentiate clients we use a simple counter as the
            -- remote end's address.
            --
            addr'' <- case addr of
                           Socket.SockAddrUnix path
                             -> return (Socket.SockAddrUnix $ path ++ "@" ++ show cnt)
                           _ -> return addr'

            return (Accepted sock' addr'', go (succ cnt) addr)

      -- Only non-async exceptions will be caught and put into the
      -- AcceptFailure variant.
      handleException
        :: SockAddr
        -> Word64
        -> SomeException
        -> IO ( Accepted  Socket SockAddr
              , Accept IO Socket SockAddr
              )
      handleException addr cnt err =
        case fromException err of
          Just (SomeAsyncException _) -> throwIO err
          Nothing                     -> pure (AcceptFailure err, go cnt addr)

-- | Local address, on Unix is associated with `Socket.AF_UNIX` family, on
--
-- Windows with `named-pipes`.
--
newtype LocalAddress = LocalAddress { getFilePath :: FilePath }
  deriving (Eq, Ord, Generic)
  deriving Show via Quiet LocalAddress

instance Hashable LocalAddress where
    hashWithSalt s (LocalAddress path) = hashWithSalt s path

newtype TestAddress addr = TestAddress { getTestAddress :: addr }
  deriving (Eq, Ord, Typeable, Generic, NFData)
  deriving NoThunks via InspectHeap (TestAddress addr)
  deriving Show via Quiet (TestAddress addr)

instance Hashable addr => Hashable (TestAddress addr)

-- | We support either sockets or named pipes.
--
-- There are three families of addresses: 'SocketFamily' used for Berkeley
-- sockets, 'LocalFamily' used for 'LocalAddress'es (either Unix sockets or
-- Windows named pipe addresses), and 'TestFamily' for testing purposes.
--
-- 'LocalFamily' requires 'LocalAddress', this is needed to provide path of the
-- opened Win32 'HANDLE'.
--
data AddressFamily addr where

    SocketFamily :: !Socket.Family
                 -> AddressFamily Socket.SockAddr

    LocalFamily  :: !LocalAddress -> AddressFamily LocalAddress

    -- | Using a newtype wrapper 'TestAddress' makes pattern matches on
    -- @AddressFamily@ complete, e.g. it makes 'AddressFamily' injective:
    -- @AddressFamily addr == AddressFamily addr'@ then @addr == addr'@. .
    --
    TestFamily   :: AddressFamily (TestAddress addr)

deriving instance Eq   addr => Eq   (AddressFamily addr)
deriving instance Show addr => Show (AddressFamily addr)


-- | Abstract communication interface that can be used by more than
-- 'Socket'.  Snockets are polymorphic over monad which is used, this feature
-- is useful for testing and/or simulations.
--
data Snocket m fd addr = Snocket {
    -- | Get local address of a file descriptor.
    --
    -- /For Berkeley sockets:/ `Socket.getSocketName` is used.
    --
    -- /For named pipes:/ a file name is returned which was used when the file
    -- descriptor was opened.
    --
    getLocalAddr  :: fd -> m addr
    -- | Get remote address of a file descriptor.
    --
    -- /For Berkeley sockets:/ `Socket.getPeerName` is used.
    --
    -- /For named pipes:/ either an empty string (`open`) or file name
    -- (`openToConnect`) which was used to open the file descriptor is returned.
    --
  , getRemoteAddr :: fd -> m addr

    -- | Get address family of an address.
    --
  , addrFamily    :: addr -> AddressFamily addr

    -- | Open a file descriptor  (socket / namedPipe).
    --
    -- /For Berkeley sockets:/ `Socket.socket` is used.
    --
    -- /For named pipes:/ 'Win32.createNamedPipe' is used.
    --
  , open          :: AddressFamily addr -> m fd

    -- | A way to create 'fd' to pass to 'connect'.
    --
    -- /For Berkeley sockets:/ this the same as 'Socket.open'.
    --
    -- /For named pipes:/ it will use 'CreateFile' syscall.
    --
    -- NOTE: For named pipes,  one must pass the `LocalAddress` of the named
    -- pipe to connect to.  This call will open this file as required by the
    -- semantics of named pipes.
    --
  , openToConnect :: addr -> m fd

    -- | `connect` is only needed for Berkeley sockets, for named pipes it is
    -- no-op.
    --
  , connect       :: fd -> addr -> m ()
    -- | `bind` is only needed for Berkeley sockets, for named pipes it is
    -- no-op.
  , bind          :: fd -> addr -> m ()
    -- | `listen` is only needed for Berkeley sockets, for named pipes it is
    -- no-op.
  , listen        :: fd -> m ()

    -- | 'Accept' loop for Berkeley sockets and named pipes.
    --
    -- /For Berkeley sockets:/ calling `runAccept` calls `Snocket.accept', while
    -- `accept` just returns `Accept` without any IO.
    --
    -- /For named pipes:/ `accept` calls `Win32.Async.connectNamedPipe` on the
    -- file descriptor passed to it; then each subsequent `runAccept` creates
    -- a new file descriptor and calls `Win32.Async.ConnectNamedPipe` on it.
    --
  , accept        :: fd -> m (Accept m fd addr)

    -- | Close a file descriptor opened with `open` or `openToConnect`.
    --
  , close         :: fd -> m ()
  }


--
-- Socket based Snockets
--


socketAddrFamily :: Socket.SockAddr -> AddressFamily Socket.SockAddr
socketAddrFamily Socket.SockAddrInet  {} = SocketFamily Socket.AF_INET
socketAddrFamily Socket.SockAddrInet6 {} = SocketFamily Socket.AF_INET6
socketAddrFamily Socket.SockAddrUnix  {} = SocketFamily Socket.AF_UNIX


type SocketSnocket = Snocket IO Socket SockAddr


-- | Create a 'Snocket' for the given 'Socket.Family'. In the 'bind' method set
-- 'Socket.ReuseAddr` and 'Socket.ReusePort'.
--
socketSnocket
  :: IOManager
  -- ^ 'IOManager' interface.  We use it when we create a new socket and when we
  -- accept a connection.
  --
  -- Though it could be used in `open`, but that is going to be used in
  -- a bracket so it's better to keep it simple.
  --
  -> SocketSnocket
socketSnocket ioManager = Snocket {
      getLocalAddr   = Socket.getSocketName
    , getRemoteAddr  = Socket.getPeerName
    , addrFamily     = socketAddrFamily
    , open           = openSocket
    , openToConnect  = openSocket . socketAddrFamily
    , connect        =
#if defined(POSIX)
                       Socket.connect
#else
                       Win32.Async.connect
#endif
    , bind           = Socket.bind
    , listen         = \s -> Socket.listen s 8
    , accept         = berkeleyAccept ioManager
      -- TODO: 'Socket.close' is interruptible by asynchronous exceptions; it
      -- should be fixed upstream, once that's done we can remove
      -- `uninterruptibleMask_'
    , close    = uninterruptibleMask_ . Socket.close
    }
  where
    openSocket :: AddressFamily SockAddr -> IO Socket
    openSocket (SocketFamily family_) = do
      sd <- Socket.socket family_ Socket.Stream Socket.defaultProtocol
      associateWithIOManager ioManager (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associateWithIOCP' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return sd


--
-- LocalSnockets either based on unix sockets or named pipes.
--

#if defined(mingw32_HOST_OS)
type LocalHandle = Win32.HANDLE
#else
type LocalHandle = Socket
#endif

-- | System dependent LocalSnocket type
--
#if defined(mingw32_HOST_OS)
data LocalSocket = LocalSocket { getLocalHandle :: !LocalHandle
                                 -- ^ underlying windows 'HANDLE'
                               , getLocalPath   :: !LocalAddress
                                 -- ^ original path, used when creating the handle
                               , getRemotePath  :: !LocalAddress
                                 -- ^ unique identifier (not a real path).  It
                                 -- makes the pair of local and remote
                                 -- addresses unique.
                               }
    deriving (Eq, Generic)
    deriving Show via Quiet LocalSocket
#else
newtype LocalSocket  = LocalSocket { getLocalHandle :: LocalHandle }
    deriving (Eq, Generic)
    deriving Show via Quiet LocalSocket
#endif

makeLocalBearer :: MakeBearer IO LocalSocket
#if defined(mingw32_HOST_OS)
makeLocalBearer = MakeBearer $ \sduTimeout tracer LocalSocket { getLocalHandle = fd } ->
  getBearer makeNamedPipeBearer sduTimeout tracer fd
#else
makeLocalBearer = MakeBearer $ \sduTimeout tracer (LocalSocket fd) ->
  getBearer makeSocketBearer sduTimeout tracer fd
#endif

-- | System dependent LocalSnocket
--
type LocalSnocket = Snocket IO LocalSocket LocalAddress


-- | Create a 'LocalSnocket'.
--
-- On /Windows/, there is no way to get path associated to a named pipe.  To go
-- around this, the address passed to 'open' via 'LocalFamily' will be
-- referenced by 'LocalSocket'.
--
localSnocket :: IOManager -> LocalSnocket
#if defined(mingw32_HOST_OS)
localSnocket ioManager = Snocket {
      getLocalAddr  = return . getLocalPath
    , getRemoteAddr = return . getRemotePath
    , addrFamily    = LocalFamily

    , open = \(LocalFamily addr) -> do
        hpipe <- Win32.createNamedPipe
                   (getFilePath addr)
                   (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                   (Win32.pIPE_TYPE_BYTE     .|. Win32.pIPE_READMODE_BYTE)
                   Win32.pIPE_UNLIMITED_INSTANCES
                   65536   -- outbound pipe size
                   16384   -- inbound pipe size
                   0       -- default timeout
                   Nothing -- default security
        associateWithIOManager ioManager (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        pure (LocalSocket hpipe addr (LocalAddress ""))

    -- To connect, simply create a file whose name is the named pipe name.
    , openToConnect  = \(LocalAddress pipeName) -> do
        hpipe <- Win32.connect pipeName
                   (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE )
                   Win32.fILE_SHARE_NONE
                   Nothing
                   Win32.oPEN_EXISTING
                   Win32.fILE_FLAG_OVERLAPPED
                   Nothing
        associateWithIOManager ioManager (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        return (LocalSocket hpipe (LocalAddress pipeName) (LocalAddress pipeName))
    , connect  = \_ _ -> pure ()

    -- Bind and listen are no-op.
    , bind     = \_ _ -> pure ()
    , listen   = \_ -> pure ()

    , accept   = \sock@(LocalSocket hpipe addr _) -> pure $ Accept $ do
          Win32.Async.connectNamedPipe hpipe
          return (Accepted sock addr, acceptNext 0 addr)

      -- Win32.closeHandle is not interruptible
    , close    = Win32.closeHandle . getLocalHandle
    }
  where
    acceptNext :: Word64 -> LocalAddress -> Accept IO LocalSocket LocalAddress
    acceptNext !cnt addr = Accept (acceptOne `catch` handleIOException)
      where
        handleIOException
          :: IOException
          -> IO ( Accepted  LocalSocket LocalAddress
                , Accept IO LocalSocket LocalAddress
                )
        handleIOException err =
          pure ( AcceptFailure (toException err)
               , acceptNext (succ cnt) addr
               )

        acceptOne
          :: IO ( Accepted  LocalSocket LocalAddress
                , Accept IO LocalSocket LocalAddress
                )
        acceptOne =
          bracketOnError
            (Win32.createNamedPipe
                 (getFilePath addr)
                 (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                 (Win32.pIPE_TYPE_BYTE     .|. Win32.pIPE_READMODE_BYTE)
                 Win32.pIPE_UNLIMITED_INSTANCES
                 65536    -- outbound pipe size
                 16384    -- inbound pipe size
                 0        -- default timeout
                 Nothing) -- default security
             Win32.closeHandle
             $ \hpipe -> do
              associateWithIOManager ioManager (Left hpipe)
              Win32.Async.connectNamedPipe hpipe
              -- InboundGovernor/Server requires a unique address for the
              -- remote end one in order to track connections.
              -- So to differentiate clients we use a simple counter as the
              -- remote end's address.
              --
              let addr' = LocalAddress $ getFilePath addr ++ "@" ++ show cnt
              return (Accepted (LocalSocket hpipe addr addr') addr', acceptNext (succ cnt) addr)

-- local snocket on unix
#else

localSnocket ioManager =
    Snocket {
        getLocalAddr  = fmap toLocalAddress . Socket.getSocketName . getLocalHandle
      , getRemoteAddr = fmap toLocalAddress . Socket.getPeerName . getLocalHandle
      , addrFamily    = LocalFamily
      , connect       = \(LocalSocket s) addr ->
          Socket.connect s (fromLocalAddress addr)
      , bind          = \(LocalSocket fd) addr -> Socket.bind fd (fromLocalAddress addr)
      , listen        = flip Socket.listen 8 . getLocalHandle
      , accept        = fmap (bimap LocalSocket toLocalAddress)
                      . berkeleyAccept ioManager
                      . getLocalHandle
      , open          = openSocket
      , openToConnect = openSocket . LocalFamily
      , close         = uninterruptibleMask_ . Socket.close . getLocalHandle
      }
  where
    toLocalAddress :: SockAddr -> LocalAddress
    toLocalAddress (SockAddrUnix path) = LocalAddress path
    toLocalAddress (SockAddrInet {})   = error "localSnocket.toLocalAddress: saw IPV4 socket"
    toLocalAddress (SockAddrInet6 {})  = error "localSnocket.toLocalAddress: saw IPV6 socket"

    fromLocalAddress :: LocalAddress -> SockAddr
    fromLocalAddress = SockAddrUnix . getFilePath

    openSocket :: AddressFamily LocalAddress -> IO LocalSocket
    openSocket (LocalFamily _addr) = do
      sd <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      associateWithIOManager ioManager (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associateWithIOManager' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return (LocalSocket sd)
#endif

localAddressFromPath :: FilePath -> LocalAddress
localAddressFromPath = LocalAddress

-- | Socket file descriptor.
--
newtype FileDescriptor = FileDescriptor { getFileDescriptor :: Int }
  deriving Generic
  deriving Show via Quiet FileDescriptor

-- | We use 'unsafeFdSocket' but 'FileDescriptor' constructor is not exposed.
-- This forbids any usage of 'FileDescriptor' (at least in a straightforward
-- way) using any low level functions which operate on file descriptors.
--
socketFileDescriptor :: Socket -> IO FileDescriptor
socketFileDescriptor = fmap (FileDescriptor . fromIntegral) . Socket.unsafeFdSocket

localSocketFileDescriptor :: LocalSocket -> IO FileDescriptor
#if defined(mingw32_HOST_OS)
localSocketFileDescriptor =
  \(LocalSocket fd _ _) -> case ptrToIntPtr fd of
    IntPtr i -> return (FileDescriptor i)
#else
localSocketFileDescriptor = socketFileDescriptor . getLocalHandle
#endif

-- | invalidFileDescriptor - when we need something for testing/simulation
invalidFileDescriptor :: FileDescriptor
invalidFileDescriptor = FileDescriptor (-1)
