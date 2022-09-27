{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- `withLocalSocket` has some constraints that are only required on Windows.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Network.Diffusion.Utils
  ( withSockets
  , withLocalSocket
  ) where


import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer, traceWith)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Snocket (FileDescriptor, Snocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Diffusion.Common

--
-- Socket utility functions
--

withSockets :: forall m ntnFd ntnAddr ntcAddr a.
               ( MonadThrow m
               , Typeable ntnAddr
               , Show     ntnAddr
               )
            => Tracer m (DiffusionTracer ntnAddr ntcAddr)
            -> Snocket m ntnFd ntnAddr
            -> (ntnFd -> ntnAddr -> m ()) -- ^ configure a socket
            -> (ntnFd -> ntnAddr -> m ()) -- ^ configure a systemd socket
            -> [Either ntnFd ntnAddr]
            -> (NonEmpty ntnFd -> NonEmpty ntnAddr -> m a)
            -> m a
withSockets tracer sn
            configureSocket
            configureSystemdSocket
            addresses k = go [] addresses
  where
    go !acc (a : as) = withSocket a (\sa -> go (sa : acc) as)
    go []   []       = throwIO (NoSocket :: Failure ntnAddr)
    go !acc []       =
      let acc' = NonEmpty.fromList (reverse acc)
      in (k $! (fst <$> acc')) $! (snd <$> acc')

    withSocket :: Either ntnFd ntnAddr
               -> ((ntnFd, ntnAddr) -> m a)
               -> m a
    withSocket (Left sock) f =
      bracket
        (pure sock)
        (Snocket.close sn)
        $ \_sock -> do
          !addr <- Snocket.getLocalAddr sn sock
          configureSystemdSocket sock addr
          f (sock, addr)
    withSocket (Right addr) f =
      bracket
        (do traceWith tracer (CreatingServerSocket addr)
            Snocket.open sn (Snocket.addrFamily sn addr))
        (Snocket.close sn)
        $ \sock -> do
          traceWith tracer $ ConfiguringServerSocket addr
          configureSocket sock addr
          Snocket.bind sn sock addr
          traceWith tracer $ ListeningServerSocket addr
          Snocket.listen sn sock
          traceWith tracer $ ServerSocketUp addr
          f (sock, addr)


withLocalSocket :: forall ntnAddr ntcFd ntcAddr m a.
                   ( MonadThrow m
                     -- Win32 only constraints:
                   , Typeable ntnAddr
                   , Show     ntnAddr
                   )
                => Tracer m (DiffusionTracer ntnAddr ntcAddr)
                -> (ntcFd -> m FileDescriptor)
                -> Snocket m ntcFd ntcAddr
                -> Either ntcFd ntcAddr
                -> (ntcFd -> m a)
                -> m a
withLocalSocket tracer getFileDescriptor sn localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> traceWith tracer (UnsupportedReadySocketCase
                                       :: DiffusionTracer ntnAddr ntcAddr)
                >> throwIO (UnsupportedReadySocket :: Failure ntnAddr)
#else
         Left sd -> do
             addr <- Snocket.getLocalAddr sn sd
             traceWith tracer (UsingSystemdSocket addr)
             return (Left sd)
#endif
         Right addr -> do
             traceWith tracer $ CreateSystemdSocketForSnocketPath addr
             sd <- Snocket.open sn (Snocket.addrFamily sn addr)
             traceWith tracer $ CreatedLocalSocket addr
             return (Right (sd, addr))
    )
    -- We close the socket here, even if it was provided to us.
    (\case
      Right (sd, _) -> Snocket.close sn sd
      Left   sd     -> Snocket.close sn sd
    )
    $ \case
      -- unconfigured socket
      Right (sd, addr) -> do
        traceWith tracer . ConfiguringLocalSocket addr
           =<< getFileDescriptor sd
        Snocket.bind sn sd addr
        traceWith tracer . ListeningLocalSocket addr
           =<< getFileDescriptor sd
        Snocket.listen sn sd
        traceWith tracer . LocalSocketUp addr
           =<< getFileDescriptor sd
        k sd

      -- pre-configured systemd socket
      Left sd -> k sd

