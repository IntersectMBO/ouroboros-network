{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}

{- TODO: Snocket should be changed to a better name, and we already have two Socket modules, three if 
         we count network-mux and at least three Socket data types. -}
module Ouroboros.Network.Snocket where

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM
import qualified Network.Socket as Socket
#if defined(mingw32_HOST_OS)
import           Data.Bits
import           System.IO
import           System.Win32
import           System.Win32.NamedPipes
#endif

import Text.Printf

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Bearer.Socket as Mx
import qualified Network.Mux.Bearer.Pipe as Mx

-- | Abstract communication interface that can be used by more than Socket.Socket.
data Snocket channel addr ptcl = Snocket {
    createClient        :: IO channel
  , createServer        :: IO channel
  , setupClient         :: channel -> Maybe addr -> IO ()
  , setupServer         :: channel -> Maybe addr -> IO ()
  , getLocalAddr        :: channel -> IO addr
  , getRemoteAddr       :: channel -> IO addr
  , close               :: channel -> IO ()
  , connect             :: channel -> addr -> IO ()
  , toBearer            :: channel -> IO (MuxBearer ptcl IO)
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
    , setupClient = (\sd addr_m -> do
        case addr_m of
             Nothing   -> pure ()
             Just addr -> bind sd addr
        return ())
    , setupServer = (\sd addr_m -> do
        case addr_m of
             Nothing   -> pure ()
             Just addr -> do
                 bind sd addr
                 Socket.listen sd 10
        return ())
    , getLocalAddr = Socket.getSocketName
    , getRemoteAddr = Socket.getPeerName
    , close = Socket.close
    , accept = Socket.accept
    , connect = Socket.connect
    , toBearer = Mx.socketAsMuxBearer
    }
  where
    create = Socket.socket family Socket.Stream Socket.defaultProtocol
    bind sd a= do
        when (family == Socket.AF_INET ||
              family == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
        Socket.bind sd a

      
#if defined(mingw32_HOST_OS)

data NamedPipe = NamedPipeMature HANDLE HANDLE
               | NamedPipeLarva  (TMVar IO (HANDLE, HANDLE))

-- | Create a Windows Named Pipe Snocket.
namedPipeSnocket
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => String
  -> String
  -> Snocket NamedPipe String ptcl
namedPipeSnocket readName writeName = Snocket {
      createClient = do
        printf "snocket create client\n"
        r <- createFile readName
                       (gENERIC_READ .|. gENERIC_WRITE)
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_ATTRIBUTE_NORMAL
                       Nothing
        w <- createFile writeName
                       (gENERIC_READ .|. gENERIC_WRITE)
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_ATTRIBUTE_NORMAL
                       Nothing
        return $ NamedPipeMature r w
    , createServer = do
        printf "snocket create server\n"
        (r, w) <- create
        np <- newTMVarM (r, w)
        return $ NamedPipeLarva np
    , setupClient = (\_ _ -> return ())
    , setupServer = (\_ _ -> return ())
    , getLocalAddr = \_ -> return $ readName ++ writeName
    , getRemoteAddr = \_ -> return $ readName ++ writeName
    , close = (\case
      NamedPipeMature rh wh -> do
        printf "snocket closing mature\n"
        rh' <- pipeToHandle rh readName ReadWriteMode
        hClose rh'
        wh' <- pipeToHandle wh writeName ReadWriteMode
        hClose wh'
        printf "snocket mature closed\n"
      NamedPipeLarva v -> do
          printf "snocket closing larval\n"
          (r, w) <- atomically $ takeTMVar v
          rh' <- pipeToHandle r readName ReadWriteMode
          hClose rh'
          wh' <- pipeToHandle w writeName ReadWriteMode
          hClose wh'
          printf "snocket larval closed\n"
     )
    , accept = (\case
      NamedPipeMature _ _ -> error "Don't accept on connected named pipes" -- XXX
      NamedPipeLarva v -> do
        printf "snocket accept\n"
        (rh, wh) <- atomically $ takeTMVar v
        connectNamedPipe rh Nothing
        connectNamedPipe wh Nothing
        (r, w) <- create
        atomically $ putTMVar v (r, w)
        return (NamedPipeMature rh wh, readName ++ writeName))
    , connect = (\_ _ -> return ())
    , toBearer = (\case
        NamedPipeMature rh wh -> do
          r <- pipeToHandle rh readName ReadWriteMode
          w <- pipeToHandle wh writeName ReadWriteMode
          Mx.pipeAsMuxBearer r w
        NamedPipeLarva _ -> error "toBearer on larval NamedPipe")
    }
  where
    create = do
        !r <- createNamedPipe readName
                           pIPE_ACCESS_DUPLEX
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           512
                           512
                           0
                           Nothing
        !w <- createNamedPipe writeName
                           pIPE_ACCESS_DUPLEX
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           512
                           512
                           0
                           Nothing
        return (r, w)

#endif
