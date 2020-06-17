{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Mux
  ( MuxMode (..)
  , OuroborosApplication (..)
  , MiniProtocol (..)
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
  , RunMiniProtocol (..)
  , MuxPeer (..)
  , toApplication
  , RunOrStop (..)
  , ScheduledStop
  , neverStop

    -- * Re-exports
    -- | from "Network.Mux"
  , MuxError(..)
  , MuxErrorType(..)
  , HasInitiator
  , HasResponder
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception)
import           Control.Tracer (Tracer)

import           Data.Void (Void)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import qualified Network.Mux.Compat as Mux
import           Network.Mux
                   ( MuxMode(..), HasInitiator, HasResponder
                   , MiniProtocolNum, MiniProtocolLimits(..)
                   , MuxError(..), MuxErrorType(..) )

import           Ouroboros.Network.Channel
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver


data RunOrStop = Run | Stop

-- |  'ScheduleStop' should depend on `muxMode` (we only need to shedule stop
-- for intiator side).  This is not done only because this would break tests,
-- bue once the old api is removed it should be possible.
type ScheduledStop m = STM m RunOrStop

neverStop :: Applicative (STM m)
          => proxy m
          -> ScheduledStop m
neverStop _ = pure Run


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
newtype OuroborosApplication (mode :: MuxMode) addr bytes m a b =
        OuroborosApplication
          (ConnectionId addr
            -> STM m RunOrStop
            -> [MiniProtocol mode bytes m a b])

data MiniProtocol (mode :: MuxMode) bytes m a b =
     MiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolLimits :: !MiniProtocolLimits,
       miniProtocolRun    :: !(RunMiniProtocol mode bytes m a b)
     }

data RunMiniProtocol (mode :: MuxMode) bytes m a b where
     InitiatorProtocolOnly
       :: MuxPeer bytes m a
       -> RunMiniProtocol InitiatorMode bytes m a Void

     ResponderProtocolOnly
       :: MuxPeer bytes m b
       -> RunMiniProtocol ResponderMode bytes m Void b

     InitiatorAndResponderProtocol
       :: MuxPeer bytes m a
       -> MuxPeer bytes m b
       -> RunMiniProtocol InitiatorResponderMode bytes m a b

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
           :: (Channel m bytes -> m (a, Maybe bytes))
           -> MuxPeer bytes m a

toApplication :: (MonadCatch m, MonadAsync m)
              => ConnectionId addr
              -> ScheduledStop m
              -> OuroborosApplication mode addr LBS.ByteString m a b
              -> Mux.MuxApplication mode m a b
toApplication connectionId scheduleStop (OuroborosApplication ptcls) =
  Mux.MuxApplication
    [ Mux.MuxMiniProtocol {
        Mux.miniProtocolNum    = miniProtocolNum ptcl,
        Mux.miniProtocolLimits = miniProtocolLimits ptcl,
        Mux.miniProtocolRun    = toMuxRunMiniProtocol (miniProtocolRun ptcl)
      }
    | ptcl <- ptcls connectionId scheduleStop ]
  where

toMuxRunMiniProtocol :: forall mode m a b.
                        (MonadCatch m, MonadAsync m)
                     => RunMiniProtocol mode LBS.ByteString m a b
                     -> Mux.RunMiniProtocol mode m a b
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
  :: ( MonadCatch m
     , MonadAsync m
     )
  => MuxPeer bytes m a
  -> Channel m bytes
  -> m (a, Maybe bytes)
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer

runMuxPeer (MuxPeerRaw action) channel =
    action channel
