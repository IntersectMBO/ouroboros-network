{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Mux
  ( AppType (..)
  , OuroborosApplication (..)
  , MiniProtocol (..)
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
  , RunMiniProtocol (..)
  , MuxPeer (..)
  , toApplication

    -- * Re-exports
    -- | from "Network.Mux"
  , MuxError(..)
  , MuxErrorType(..)
  , HasInitiator
  , HasResponder
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception)
import           Control.Tracer (Tracer)

import           Data.Void (Void)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import qualified Network.Mux as Mux
import           Network.Mux
                   ( AppType(..), HasInitiator, HasResponder
                   , MiniProtocolNum, MiniProtocolLimits(..)
                   , MuxError(..), MuxErrorType(..) )

import           Ouroboros.Network.Channel
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
newtype OuroborosApplication (appType :: AppType) addr bytes m a b =
      OuroborosApplication (ConnectionId addr -> [MiniProtocol appType bytes m a b])

data MiniProtocol (appType :: AppType) bytes m a b =
     MiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolLimits :: !MiniProtocolLimits,
       miniProtocolRun    :: !(RunMiniProtocol appType bytes m a b)
     }

data RunMiniProtocol (appType :: AppType) bytes m a b where
     InitiatorProtocolOnly
       :: MuxPeer bytes m a
       -> RunMiniProtocol InitiatorApp bytes m a Void

     ResponderProtocolOnly
       :: MuxPeer bytes m b
       -> RunMiniProtocol ResponderApp bytes m Void b

     InitiatorAndResponderProtocol
       :: MuxPeer bytes m a
       -> MuxPeer bytes m b
       -> RunMiniProtocol InitiatorAndResponderApp bytes m a b

data MuxPeer bytes m a where
    MuxPeer :: Exception failure
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerPipelined
            :: Exception failure
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerRaw
           :: (Channel m bytes -> m a)
           -> MuxPeer bytes m a

toApplication :: (MonadCatch m, MonadAsync m)
              => ConnectionId addr
              -> OuroborosApplication appType addr LBS.ByteString m a b
              -> Mux.MuxApplication appType m a b
toApplication connectionId (OuroborosApplication app) =
  Mux.MuxApplication
    [ Mux.MuxMiniProtocol {
        Mux.miniProtocolNum    = miniProtocolNum ptcl,
        Mux.miniProtocolLimits = miniProtocolLimits ptcl,
        Mux.miniProtocolRun    = toMuxRunMiniProtocol (miniProtocolRun ptcl)
      }
    | ptcl <- app connectionId ]
  where

toMuxRunMiniProtocol :: forall appType m a b.
                        (MonadCatch m, MonadAsync m)
                     => RunMiniProtocol appType LBS.ByteString m a b
                     -> Mux.RunMiniProtocol appType m a b
toMuxRunMiniProtocol (InitiatorProtocolOnly i) =
  Mux.InitiatorProtocolOnly (runMuxPeer i . fromChannel)
toMuxRunMiniProtocol (ResponderProtocolOnly r) =
  Mux.ResponderProtocolOnly (runMuxPeer r . fromChannel)
toMuxRunMiniProtocol (InitiatorAndResponderProtocol i r) =
  Mux.InitiatorAndResponderProtocol (runMuxPeer i . fromChannel)
                                    (runMuxPeer r . fromChannel)

-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     )
  => MuxPeer bytes m a
  -> Channel m bytes
  -> m a
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer

runMuxPeer (MuxPeerRaw action) channel =
    action channel
