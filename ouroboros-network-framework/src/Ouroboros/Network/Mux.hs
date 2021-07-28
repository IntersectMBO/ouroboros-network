{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Mux
  ( MuxMode (..)
  , OuroborosApplication (..)
  , MiniProtocol (..)
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
  , RunMiniProtocol (..)
  , MuxPeer (..)
  , toApplication
  , ControlMessage (..)
  , ControlMessageSTM
  , continueForever
  , timeoutWithControlMessage

    -- * Re-exports
    -- | from "Network.Mux"
  , MuxError(..)
  , MuxErrorType(..)
  , HasInitiator
  , HasResponder

    -- * For Consensus ThreadNet Tests
  , runMuxPeer
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Data.Void (Void)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Pipelined

import qualified Network.Mux.Compat as Mux
import           Network.Mux
                   ( MuxMode(..), HasInitiator, HasResponder
                   , MiniProtocolNum, MiniProtocolLimits(..)
                   , MuxError(..), MuxErrorType(..) )

import           Ouroboros.Network.Channel
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)


-- | Control signal sent to a mini-protocol.  expected to exit, on 'Continue' it
-- should continue its operation
--
data ControlMessage =
    -- | Continue operation.
      Continue

    -- | Hold on, e.g. do not sent messages until resumed.  This is not used for
    -- any hot protocol.
    --
    | Quiesce

    -- | The client is expected to terminate as soon as possible.
    --
    | Terminate
  deriving (Eq, Show)

-- |  'ControlMessageSTM' should depend on `muxMode` (we only need to shedule
-- stop for intiator side).  This is not done only because this would break
-- tests, but once the old api is removed it should be possible.
--
type ControlMessageSTM m = STM m ControlMessage

continueForever :: Applicative (STM m)
          => proxy m
          -> ControlMessageSTM m
continueForever _ = pure Continue


-- | First to finish synchronisation between 'Terminate' state of
-- 'ControlMessage' and an stm action.
--
-- This should return @STM m (Maybe a)@ but 'STM' is a non-injective type
-- family, and we would need to pass @Proxy m@ to fix an ambiuous type (or use
-- 'AllowAmbiguousTypes' extension).
--
timeoutWithControlMessage :: MonadSTM m
                          => ControlMessageSTM m
                          -> STM m a
                          -> m (Maybe a)
timeoutWithControlMessage controlMessageSTM stm =
    atomically $
      do
        cntrlMsg <- controlMessageSTM
        case cntrlMsg of
          Terminate -> return Nothing
          Continue  -> retry
          Quiesce   -> retry
      `orElse` (Just <$> stm)


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
newtype OuroborosApplication (mode :: MuxMode) addr bytes m a b =
        OuroborosApplication
          (ConnectionId addr -> ControlMessageSTM m -> [MiniProtocol mode bytes m a b])

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
    MuxPeer :: forall (pr :: PeerRole) ps (st :: ps) failure bytes m a.
               ( Show failure
               , forall (st' :: ps). Show (ClientHasAgency st')
               , forall (st' :: ps). Show (ServerHasAgency st')
               , ShowProxy ps
               )
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerPipelined
             :: forall (pr :: PeerRole) ps (st :: ps) failure bytes m a.
               ( Show failure
               , forall (st' :: ps). Show (ClientHasAgency st')
               , forall (st' :: ps). Show (ServerHasAgency st')
               , ShowProxy ps
               )
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerRaw
           :: (Channel m bytes -> m (a, Maybe bytes))
           -> MuxPeer bytes m a

toApplication :: (MonadCatch m, MonadAsync m)
              => ConnectionId addr
              -> ControlMessageSTM m
              -> OuroborosApplication mode addr LBS.ByteString m a b
              -> Mux.MuxApplication mode m a b
toApplication connectionId controlMessageSTM (OuroborosApplication ptcls) =
  Mux.MuxApplication
    [ Mux.MuxMiniProtocol {
        Mux.miniProtocolNum    = miniProtocolNum ptcl,
        Mux.miniProtocolLimits = miniProtocolLimits ptcl,
        Mux.miniProtocolRun    = toMuxRunMiniProtocol (miniProtocolRun ptcl)
      }
    | ptcl <- ptcls connectionId controlMessageSTM ]

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
