{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}

{- TODO: Snocket should be changed to a better name, and we already have two Socket modules, three if 
         we count network-mux and at least three Socket data types. -}
module Ouroboros.Network.Snocket where

import           Control.Monad (when)
import qualified Network.Socket as Socket hiding (recv)
#if defined(mingw32_HOST_OS)
import           Data.Bits
import           System.IO
import           System.Win32
import           System.Win32.NamedPipes
#endif

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Bearer.Socket as Mx
import qualified Network.Mux.Bearer.Pipe as Mx

-- | Abstract communication interface that can be used by more than Socket.Socket.
data Snocket channel addr ptcl = Snocket {
    createClient        :: IO channel
  , createServer        :: IO channel
  , getLocalAddr        :: channel -> IO addr
  , getRemoteAddr       :: channel -> IO addr
  , close               :: channel -> IO ()
  , connect             :: channel -> addr -> IO ()
  , toBearer            :: channel -> IO (MuxBearer ptcl IO)
  , bind                :: channel -> addr -> IO ()
  , listen              :: channel -> IO ()
  , accept              :: channel -> IO (channel, addr)
  }

-- | Create a Snocket for the given Socket.Family
socketSnocket
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Socket.Family
  -> Snocket Socket.Socket Socket.SockAddr ptcl
socketSnocket family = Snocket {
      createClient = create
    , createServer = create
    , getLocalAddr = Socket.getSocketName
    , getRemoteAddr = Socket.getPeerName
    , close = Socket.close
    , accept = Socket.accept
    , connect = Socket.connect
    , bind = (\sd a -> do
        when (family == Socket.AF_INET ||
              family == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
        Socket.bind sd a)
    , listen = (\s -> Socket.listen s 10 )
    , toBearer = Mx.socketAsMuxBearer
    }
  where
    create = Socket.socket family Socket.Stream Socket.defaultProtocol
      
#if defined(mingw32_HOST_OS)
-- XXX Not tested, not even compiled!

-- | Create a Windows Named Pipe Snocket.
namedPipeSnocket
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => String
  -> Snocket Handle String ptcl
namedPipeSnocket name = Snocket {
      createClient = do
        h <- createFile name
                       (gENERIC_READ .|. gENERIC_WRITE)
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_ATTRIBUTE_NORMAL
                       Nothing
        pipeToHandle h name ReadWriteMode
    , createServer = do
        h <- createNamedPipe name
                           pIPE_ACCESS_DUPLEX
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           512
                           512
                           0
                           Nothing
        pipeToHandle h name ReadWriteMode
    , getLocalAddr = \_ -> return name
    , getRemoteAddr = \_ -> return name
    , close = hClose
    , accept = (\h -> do
        --h' <- pipeToHandle h name ReadWriteMode
        return (h, name))
    , connect = (\_ _ -> return ())
    , bind = \_ _ -> return ()
    , listen = (\_ -> return ())
    , toBearer = (\h -> Mx.pipeAsMuxBearer h h)
    }

#endif
