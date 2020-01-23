{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Mux
  ( AppType (..)
  , OuroborosApplication (..)
  , MuxPeer (..)
  , ProtocolEnum (..)
  , MiniProtocolLimits (..)
  , MiniProtocolNum (..)
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
import           Data.Int (Int64)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined

import           Network.Mux.Types hiding (MiniProtocolLimits(..))
import qualified Network.Mux.Types as Mux

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

--TODO: the OuroborosApplication type needs to be updated to follow the new
-- structure of the MuxApplication, which no longer uses the bounded enumeration
-- idiom. For now, toApplication converts from the bounded enumeration style to
-- the simple list style that MuxApplication now uses.

class ProtocolEnum ptcl where
    fromProtocolEnum :: ptcl -> MiniProtocolNum

class MiniProtocolLimits ptcl where
    maximumMessageSize :: ptcl -> Int64
    maximumIngressQueue :: ptcl -> Int64

toApplication :: (Enum ptcl, Bounded ptcl,
                  ProtocolEnum ptcl, MiniProtocolLimits ptcl)
              => OuroborosApplication appType peerid ptcl m LBS.ByteString a b
              -> peerid
              -> MuxApplication appType m a b
toApplication (OuroborosInitiatorApplication f) peerid =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumMessageSize  = maximumMessageSize ptcl,
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            InitiatorProtocolOnly
              (\channel -> f peerid ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]

toApplication (OuroborosResponderApplication f) peerid =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumMessageSize  = maximumMessageSize ptcl,
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            ResponderProtocolOnly
              (\channel -> f peerid ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]

toApplication (OuroborosInitiatorAndResponderApplication f g) peerid =
    MuxApplication
      [ MuxMiniProtocol {
          miniProtocolNum    = fromProtocolEnum ptcl,
          miniProtocolLimits = Mux.MiniProtocolLimits {
                                 Mux.maximumMessageSize  = maximumMessageSize ptcl,
                                 Mux.maximumIngressQueue = maximumIngressQueue ptcl
                               },
          miniProtocolRun    =
            InitiatorAndResponderProtocol
              (\channel -> f peerid ptcl (fromChannel channel))
              (\channel -> g peerid ptcl (fromChannel channel))
        }
      | ptcl <- [minBound..maxBound] ]


-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
--
data MuxPeer failure m bytes a where
    MuxPeer :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer failure m bytes a

    MuxPeerPipelined
            :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer failure m bytes a


-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer failure m bytes a
  -> Channel m bytes
  -> m a
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer


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
  => (ptcl -> MuxPeer failure m bytes a)
  -> OuroborosApplication InitiatorApp peerid ptcl m bytes a Void
simpleInitiatorApplication fn = OuroborosInitiatorApplication $ \_peerid ptcl channel ->
  runMuxPeer (fn ptcl) channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleResponderApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m bytes a)
  -> OuroborosApplication ResponderApp peerid ptcl m bytes Void a
simpleResponderApplication fn = OuroborosResponderApplication $ \_peerid ptcl channel ->
  runMuxPeer (fn ptcl) channel
