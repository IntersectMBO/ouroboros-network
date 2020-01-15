{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Mux
  ( AppType (..)
  , OuroborosApplication (..)
  , MuxPeer (..)
  , runMuxPeer
  , simpleInitiatorApplication
  , simpleResponderApplication

  , toApplication
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception)
import           Control.Tracer (Tracer)
import           Data.Void (Void)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.Driver.Simple (driverSimple)

import           Network.Mux.Interface
import           Network.Mux.Types ( MiniProtocolInitiatorControl
                                   , MiniProtocolResponderControl)

import           Ouroboros.Network.Channel


-- |  Like 'MuxApplication' but using a more polymorphic 'Channel' type.
--
data OuroborosApplication (appType :: AppType) peerid ptcl m bytes a b where
     OuroborosInitiatorApplication
       :: ((ptcl -> MiniProtocolInitiatorControl m a) -> m ())
       -- ^ Used to start and fetch the result of each @ptcl@, see @MiniProtocolInitiatorControl@
       --  and @simpleInitiatorControl@.
       -> (peerid -> ptcl -> Channel m bytes -> m (a, Maybe bytes))
       -- ^ An initiator application per @ptcl@, see @runPeer'@.
       -> OuroborosApplication InitiatorApp peerid ptcl m bytes a Void

     OuroborosResponderApplication
       :: ((ptcl -> MiniProtocolResponderControl m a) -> m ())
       -- ^ Used to fetch the result of each @ptcl@, see @MiniProtocolResponderControl@ and
       -- @'simpleResponderControl'@.
       -> (peerid -> ptcl -> Channel m bytes -> m (a, Maybe bytes))
       -- ^ A responder application per @ptcl@, see @runPeer'@.
       -> OuroborosApplication ResponderApp peerid ptcl m bytes Void a

     OuroborosInitiatorAndResponderApplication
       :: ((ptcl -> MiniProtocolInitiatorControl m a) -> m ())
       -> (peerid -> ptcl -> Channel m bytes -> m (a, Maybe bytes))
       -> ((ptcl -> MiniProtocolResponderControl m b) -> m ())
       -> (peerid -> ptcl -> Channel m bytes -> m (b, Maybe bytes))
       -> OuroborosApplication InitiatorAndResponderApp peerid ptcl m bytes a b


toApplication :: Functor m
              => Applicative m
              => OuroborosApplication appType peerid ptcl m LBS.ByteString a b
              -> MuxApplication appType peerid ptcl m a b
toApplication (OuroborosInitiatorApplication i f) =
    MuxInitiatorApplication
      i
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
toApplication (OuroborosResponderApplication r f) =
    MuxResponderApplication
      r
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
toApplication (OuroborosInitiatorAndResponderApplication i f r g) =
    MuxInitiatorAndResponderApplication
      i
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
      r
      (\peerid ptcl channel -> g peerid ptcl (fromChannel channel))


-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
--
data MuxPeer peerid failure m bytes a where
    MuxPeer :: Tracer m (TraceSendRecv ps peerid failure)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer peerid failure m bytes a

    MuxPeerPipelined
            :: Tracer m (TraceSendRecv ps peerid failure)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer peerid failure m bytes a


-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer peerid failure m bytes a
  -> peerid
  -> Channel m bytes
  -> m (a, Maybe bytes)
runMuxPeer (MuxPeer tracer codec peer) peerid channel =
    runPeerWithDriver driver peer (startDState driver)
  where
    driver = driverSimple tracer peerid codec channel

runMuxPeer (MuxPeerPipelined tracer codec peer) peerid channel =
    runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = driverSimple tracer peerid codec channel


-- |
-- Smart constructor for @'MuxInitiatorApplication'@.  It is a simple client, since
-- none of the applications requires resource handling to run in the monad @m@.
-- Each one is simply run either by @'runPeer'@ or @'runPipelinedPeer'@.
--
simpleInitiatorApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => ((ptcl -> MiniProtocolInitiatorControl m a) -> m ())
  -> (ptcl -> MuxPeer peerid failure m bytes a)
  -> OuroborosApplication InitiatorApp peerid ptcl m bytes a Void
simpleInitiatorApplication ci fn = OuroborosInitiatorApplication ci $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleResponderApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => Monoid bytes
  => ((ptcl -> MiniProtocolResponderControl m a) -> m ())
  -> (ptcl -> MuxPeer peerid failure m bytes a)
  -> OuroborosApplication ResponderApp peerid ptcl m bytes Void a
simpleResponderApplication cr fn = OuroborosResponderApplication cr $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel
