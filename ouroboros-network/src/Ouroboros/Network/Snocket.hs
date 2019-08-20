{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}

{- TODO: Snocket should be changed to a better name, and we already have two Socket modules, three if 
         we count network-mux and at least three Socket data types. -}
module Ouroboros.Network.Snocket where

import           Control.Monad (when)
import qualified Network.Socket as Socket hiding (recv)
#if defined(mingw32_HOST_OS)
import           Data.Bits
import qualified System.Win32 as Win32
import qualified System.Win32.NamedPipes as Win32.NamedPipes
#endif

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Bearer.Socket as Mx

-- | Abstract communication interface that can be used by more than Socket.Socket.
data Snocket channel addr ptcl = Snocket {
    getLocalAddr  :: channel -> IO addr
  , getRemoteAddr :: channel -> IO addr
  , close         :: channel -> IO ()
  , connect       :: Either
                      (addr -> IO channel)
                      (channel -> addr -> IO channel)
    -- ^ either named pipe connect style function or Berkeley socket connect
    -- style
  , toBearer      :: channel -> IO (MuxBearer ptcl IO)
  , bind          :: channel -> addr -> IO ()
  , listen        :: channel -> IO ()
  , accept        :: channel -> IO (channel, addr)
  }

-- | Create a Snocket for the given Socket.Family
socketSnocket
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Socket.Family
  -> Snocket Socket.Socket Socket.SockAddr ptcl
socketSnocket family = Snocket {
      getLocalAddr   = Socket.getSocketName
    , getRemoteAddr  = Socket.getPeerName
    , close          = Socket.close
    , accept         = Socket.accept
    , connect        = Right $ \s a -> s <$ Socket.connect s a
    , bind = \sd a -> do
        when (family == Socket.AF_INET ||
              family == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
        Socket.bind sd a
    , listen = \s -> Socket.listen s 10
    , toBearer = Mx.socketAsMuxBearer
    }

rawSocketSnocket
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Snocket Socket.Socket Socket.SockAddr ptcl
rawSocketSnocket = Snocket {
    getLocalAddr  = Socket.getSocketName
  , getRemoteAddr = Socket.getPeerName
  , close         = Socket.close
  , connect       = Right $ \s a -> s <$ Socket.connect s a
  , accept        = Socket.accept
  , bind          = Socket.bind
  , listen        = flip Socket.listen 10
  , toBearer      = Mx.socketAsMuxBearer
  }
      
#if defined(mingw32_HOST_OS)
-- | Create a Windows Named Pipe Snocket.
--
namedPipeSnocket
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => String
  -> Snocket Win32.HANDLE String ptcl
namedPipeSnocket name = Snocket {
      getLocalAddr  = \_ -> return name
    , getRemoteAddr = \_ -> return name
    , close    = Win32.NamedPipes.closePipe
    , accept   = \_ -> do
        hpipe <- Win32.NamedPipes.createNamedPipe
                  name
                  Win32.NamedPipes.pIPE_ACCESS_DUPLEX
                  (Win32.NamedPipes.pIPE_TYPE_BYTE
                  .|. Win32.NamedPipes.pIPE_READMODE_BYTE)
                  Win32.NamedPipes.pIPE_UNLIMITED_INSTANCES
                  maxBound
                  maxBound
                  0
                  Nothing
        Win32.NamedPipes.connectNamedPipe hpipe Nothing
        pure (hpipe, name)
    , connect  = Left $ \pipeName ->
        Win32.createFile pipeName
                         (Win32.gENERIC_READ
                           .|. Win32.gENERIC_WRITE)
                         Win32.fILE_SHARE_NONE
                         Nothing
                         Win32.oPEN_EXISTING
                         Win32.fILE_ATTRIBUTE_NORMAL
                         Nothing
    , bind     = \_ _ -> pure ()
    , listen   = \h -> Win32.NamedPipes.connectNamedPipe h Nothing
    -- TODO
    , toBearer = \_ -> error "TODO"
    }

#endif
