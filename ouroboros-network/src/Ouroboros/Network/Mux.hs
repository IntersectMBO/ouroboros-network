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

import           Network.Mux.Interface

import           Ouroboros.Network.Channel


-- |  Like 'MuxApplication' but using a more polymorphic 'Channel' type.
--
data OuroborosApplication (appType :: AppType) peerid ptcl m bytes a b where
     OuroborosInitiatorApplication
       :: (peerid -> ptcl -> Channel m bytes -> m a)
       -> OuroborosApplication InitiatorApp peerid ptcl m bytes a Void

     OuroborosResponderApplication
       :: (peerid -> ptcl -> Channel m bytes -> m a)
       -> OuroborosApplication ResponderApp peerid ptcl m bytes Void a

     OuroborosInitiatorAndResponderApplication
       :: (peerid -> ptcl -> Channel m bytes -> m a)
       -> (peerid -> ptcl -> Channel m bytes -> m b)
       -> OuroborosApplication InitiatorAndResponderApp peerid ptcl m bytes a b


toApplication :: OuroborosApplication appType peerid ptcl m LBS.ByteString a b
              -> MuxApplication appType peerid ptcl m a b
toApplication (OuroborosInitiatorApplication f) =
    MuxInitiatorApplication
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
toApplication (OuroborosResponderApplication f) =
    MuxResponderApplication
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
toApplication (OuroborosInitiatorAndResponderApplication f g) =
    MuxInitiatorAndResponderApplication
      (\peerid ptcl channel -> f peerid ptcl (fromChannel channel))
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
  :: ( MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer peerid failure m bytes a
  -> peerid
  -> Channel m bytes
  -> m a
runMuxPeer (MuxPeer tracer codec peer) peerid channel =
    runPeer tracer codec peerid channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) peerid channel =
    runPipelinedPeer tracer codec peerid channel peer


-- |
-- Smart constructor for @'MuxInitiatorApplication'@.  It is a simple client, since
-- none of the applications requires resource handling to run in the monad @m@.
-- Each one is simply run either by @'runPeer'@ or @'runPipelinedPeer'@.
--
simpleInitiatorApplication
  :: MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer peerid failure m bytes a)
  -> OuroborosApplication InitiatorApp peerid ptcl m bytes a Void
simpleInitiatorApplication fn = OuroborosInitiatorApplication $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleResponderApplication
  :: MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer peerid failure m bytes a)
  -> OuroborosApplication ResponderApp peerid ptcl m bytes Void a
simpleResponderApplication fn = OuroborosResponderApplication $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel
