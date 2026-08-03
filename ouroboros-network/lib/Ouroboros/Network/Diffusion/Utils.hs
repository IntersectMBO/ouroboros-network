{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- `withLocalSocket` has some constraints that are only required on Windows.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Network.Diffusion.Utils
  ( withSockets
  , withLocalSocket
  , readIPAndPort
  ) where


import Control.Applicative ((<|>))
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first)
import Data.IP (IP (..), IPv4, IPv6)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Typeable (Typeable)
import Network.Socket (PortNumber)
import Options.Applicative (ReadM, eitherReader)
import Text.Read (readMaybe)

import Ouroboros.Network.Snocket (FileDescriptor, Snocket)
import Ouroboros.Network.Snocket qualified as Snocket

import Ouroboros.Network.Diffusion.Types


-- | optparse-applictive parser for `IPv4:Port` or `IPv6:Port`.
--
-- note: `Read` instances for `IP`, `IPv4`, `IPv6` expect no trailing characters
-- after the address, thus we need custom parser which finds the split position
-- first.
readIPAndPort :: ReadM (IP, PortNumber)
readIPAndPort = (first IPv4 <$> readIPv4AndPort)
            <|> (first IPv6 <$> readIPv6AndPort)
  where
    readIPv4AndPort :: ReadM (IPv4, PortNumber)
    readIPv4AndPort =
      eitherReader $ \s -> do
        case splitWith ':' s of
          Nothing -> Left s
          Just (addrStr, portStr) ->
            maybe (Left s) Right $
            (,) <$> readMaybe addrStr
                <*> readMaybe portStr


    -- parse IPv6 address and port in a form `[::1]:3001` or a UNIX file path
    readIPv6AndPort :: ReadM (IPv6, PortNumber)
    readIPv6AndPort =
      eitherReader $ \s ->
        case s of
          ('[':s') ->
             case splitWith ']' s' of
               Just (addrStr, ':' : portStr) ->
                 maybe (Left s) Right $
                 (,) <$> readMaybe addrStr
                     <*> readMaybe portStr
               _ -> Left s
          _ -> Left s

    splitWith :: Char -> String -> Maybe (String, String)
    splitWith c = go ""
        where
          go _ []
            = Nothing
          go !acc (a:as)
            | a == c
            = Just (reverse acc, as)
          go !acc (a:as)
            = go (a:acc) as

--
-- Socket utility functions
--

withSockets :: forall m ntnFd ntnAddr ntcAddr a.
               ( MonadCatch m
               , Typeable ntnAddr
               , Show     ntnAddr
               )
            => Tracer m (DiffusionTracer ntnAddr ntcAddr)
            -> Snocket m ntnFd ntnAddr
            -> (ntnFd -> ntnAddr -> m ()) -- ^ configure a socket
            -> (ntnFd -> ntnAddr -> m ()) -- ^ configure a systemd socket
            -> Either (NonEmpty ntnFd) (NonEmpty ntnAddr)
            -> (NonEmpty ntnFd -> NonEmpty ntnAddr -> m a)
            -> m a

-- create a socket for each address
withSockets tracer
            sn
            configureSocket
            _configureSystemdSocket
            (Right addresses) k
            =
            go [] (NonEmpty.toList addresses)
  where
    go !acc (a : as) = withSocket a (\sa -> go (sa : acc) as)
    go []   []       = throwIO NoSocket
    go !acc []       =
      let acc' = NonEmpty.fromList (reverse acc)
      in (k $! (fst <$> acc')) $! (snd <$> acc')

    withSocket :: ntnAddr
               -> ((ntnFd, ntnAddr) -> m a)
               -> m a
    withSocket addr f =
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

-- systemd activated socket
withSockets _tracer
            sn
            _configureSocket
            configureSystemdSocket
            (Left addresses) k
            =
            go [] (NonEmpty.toList addresses)
  where
    go !acc (a : as) = withSocket a (\sa -> go (sa : acc) as)
    go []   []       = throwIO NoSocket
    go !acc []       =
      let acc' = NonEmpty.fromList (reverse acc)
      in (k $! (fst <$> acc')) $! (snd <$> acc')

    withSocket :: ntnFd
               -> ((ntnFd, ntnAddr) -> m a)
               -> m a
    withSocket sock f =
      do !addr <- Snocket.getLocalAddr sn sock
         configureSystemdSocket sock addr
         f (sock, addr)
      `onException` Snocket.close sn sock


withLocalSocket :: forall ntnAddr ntcFd ntcAddr m a.
                   ( MonadThrow m
                     -- Win32 only constraints:
                   , Typeable ntnAddr
                   , Show     ntnAddr
                   )
                => Tracer m (DiffusionTracer ntnAddr ntcAddr)
                -> (ntcFd -> m FileDescriptor)
                -> (ntcAddr -> m ())
                -- ^ configure the local socket file.
                -> Snocket m ntcFd ntcAddr
                -> Either ntcFd ntcAddr
                -> (ntcFd -> m a)
                -> m a
withLocalSocket tracer
                getFileDescriptor
                configureSocketFile
                sn localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> traceWith tracer (UnsupportedReadySocketCase
                                       :: DiffusionTracer ntnAddr ntcAddr)
                >> throwIO UnsupportedReadySocket
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
      -- not configured socket
      Right (sd, addr) -> do
        fd <- getFileDescriptor sd
        traceWith tracer (ConfiguringLocalSocket addr fd)
        Snocket.bind sn sd addr
        configureSocketFile addr
        traceWith tracer (ConfiguredLocalSocket addr fd)
        traceWith tracer (ListeningLocalSocket addr fd)
        Snocket.listen sn sd
        traceWith tracer (LocalSocketUp addr fd)
        k sd

      -- pre-configured systemd socket
      Left sd -> do
        addr <- Snocket.getLocalAddr sn sd
        configureSocketFile addr
        k sd

