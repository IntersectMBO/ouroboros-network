{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Inbound protocol governor state.
--
module Ouroboros.Network.InboundGovernor.State
  ( -- * Internals
    InboundGovernorState (..)
  , ConnectionState (..)
  , InboundGovernorCounters (..)
  , inboundGovernorCounters
  , unregisterConnection
  , updateMiniProtocol
  , RemoteState (.., RemoteEstablished)
  , updateRemoteState
  , mapRemoteState
  , MiniProtocolData (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadThrow hiding (handle)

import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Network.Mux qualified as Mux

import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.Mux


-- | 'InboundGovernorState', which consist of pure part, and a mutable part.
-- The mutable part can be observable from outside.  Future version could
-- contain additional statistics on the peers.
--
data InboundGovernorState muxMode initiatorCtx peerAddr m a b =
    InboundGovernorState {
        -- | Map of connections state.  Modifying 'igsConnections' outside of
        -- 'inboundGovernorLoop' is not safe.
        --
        igsConnections   :: !(Map (ConnectionId peerAddr)
                                  (ConnectionState muxMode initiatorCtx peerAddr m a b)),

        -- | 'InboundGovernorCounters' counters cache. Allows to only trace
        -- values when necessary.
        igsCountersCache :: !(Cache InboundGovernorCounters)
      }

-- | Counters for tracing and analysis purposes
--
data InboundGovernorCounters = InboundGovernorCounters {
      coldPeersRemote :: !Int,
      -- ^ the number of remote peers which are in 'RemoteCold' state
      idlePeersRemote :: !Int,
      -- ^ the number of remote peers which are in 'RemoteIdle' state
      warmPeersRemote :: !Int,
      -- ^ the number of remote peers which are in 'RemoteWarm' state (a close
      -- approximation of peers that have the node as a warm peer)
      hotPeersRemote  :: !Int
      -- ^ the number of remote peers which are in 'RemoteHot' state (a close
      -- approximation of peers that have the node as a hot peer)
    }
  deriving (Eq, Ord, Show)

instance Semigroup InboundGovernorCounters where
    InboundGovernorCounters c i w h <> InboundGovernorCounters c' i' w' h' =
      InboundGovernorCounters (c + c') (i + i') (w + w') (h + h')

instance Monoid InboundGovernorCounters where
    mempty = InboundGovernorCounters 0 0 0 0


inboundGovernorCounters :: InboundGovernorState muxMode initiatorCtx peerAddr m a b
                        -> InboundGovernorCounters
inboundGovernorCounters InboundGovernorState { igsConnections } =
    foldMap (\ConnectionState { csRemoteState } ->
              case csRemoteState of
                RemoteCold    -> InboundGovernorCounters 1 0 0 0
                RemoteIdle {} -> InboundGovernorCounters 0 1 0 0
                RemoteWarm    -> InboundGovernorCounters 0 0 1 0
                RemoteHot     -> InboundGovernorCounters 0 0 0 1
            )
            igsConnections


data MiniProtocolData muxMode initiatorCtx peerAddr m a b = MiniProtocolData {
    -- | Static 'MiniProtocol' description.
    --
    mpdMiniProtocol     :: !(MiniProtocol muxMode initiatorCtx (ResponderContext peerAddr) ByteString m a b),

    mpdResponderContext :: !(ResponderContext peerAddr),

    -- | Static mini-protocol temperature.
    --
    mpdMiniProtocolTemp :: !ProtocolTemperature
  }


-- | Per connection state tracked by /inbound protocol governor/.
--
data ConnectionState muxMode initiatorCtx peerAddr m a b = ConnectionState {
      -- | Mux interface.
      --
      csMux             :: !(Mux.Mux muxMode m),

      -- | Connection data flow.
      --
      csDataFlow        :: !DataFlow,

      -- | All supported mini-protocols and respective
      -- 'ProtocolTemperature'
      --
      csMiniProtocolMap :: !(Map MiniProtocolNum
                                 (MiniProtocolData muxMode initiatorCtx peerAddr m a b)),

      -- | Map of all running mini-protocol completion STM actions.
      --
      csCompletionMap   :: !(Map MiniProtocolNum
                                 (STM m (Either SomeException b))),

      -- | State of the connection.
      --
      csRemoteState     :: !(RemoteState m)

    }


--
-- State management functions
--


-- | Remove connection from 'InboundGovernorState'.
--
unregisterConnection :: Ord peerAddr
                     => ConnectionId peerAddr
                     -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
                     -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
unregisterConnection connId state =
    state { igsConnections =
              assert (connId `Map.member` igsConnections state) $
              Map.delete connId (igsConnections state)
          }


-- | Update a mini-protocol in 'ConnectionState'.  Once a mini-protocol was
-- restarted we put the new completion action into 'csCompletionMap'.
--
updateMiniProtocol :: Ord peerAddr
                   => ConnectionId peerAddr
                   -> MiniProtocolNum
                   -> STM m (Either SomeException b)
                   -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
                   -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
updateMiniProtocol connId miniProtocolNum completionAction state =
    state { igsConnections =
              Map.adjust (\connState@ConnectionState { csCompletionMap } ->
                           connState {
                             csCompletionMap =
                               assert (miniProtocolNum `Map.member` csCompletionMap) $
                               Map.insert miniProtocolNum
                                          completionAction
                                          csCompletionMap
                            }
                         )
                         connId
                         (igsConnections state)
          }


-- | Each inbound connection is either in 'RemoteIdle', 'RemoteCold' or
-- 'RemoteEstablished' state.  We only need to support
-- @PromotedToWarm^{Duplex}_{Remote}@,
-- @DemotedToCold^{Duplex}_{Remote}@ and
-- @DemotedToCold^{Unidirectional}_{Remote}@ transitions.
--
data RemoteState m
    -- | After @PromotedToWarm^{dataFlow}_{Remote}@ a connection is in
    -- 'RemoteWarm' state.
    --
    = RemoteWarm

    -- | In this state all established and hot mini-protocols are running and
    -- none of the warm mini-protocols is running.
    --
    | RemoteHot

    -- | After @DemotedToCold^{dataFlow}_{Remote}@ is detected.  This state
    -- corresponds to 'InboundIdleState'. In this state we are checking
    -- if the responder protocols are idle during protocol idle timeout
    -- (represented by an 'STM' action)
    --
    -- 'RemoteIdle' is the initial state of an accepted a connection.
    --
    | RemoteIdle !(STM m ())

    -- | The 'RemoteCold' state for 'Duplex' connections allows us to have
    -- responders started using the on-demand strategy.  This assures that once
    -- the remote peer start using the connection the local side will be ready
    -- to serve it.
    --
    -- For a 'Duplex' connection: a 'RemoteIdle' connection transitions to
    -- 'RemoteCold' state after all responders being idle for
    -- @protocolIdleTimeout@. This triggers 'unregisterInboundConnection'.
    --
    -- For a 'Unidreictional' connection: after all responders terminated.
    --
    | RemoteCold


remoteEstablished :: RemoteState m -> Maybe (RemoteState m)
remoteEstablished a@RemoteWarm = Just a
remoteEstablished a@RemoteHot  = Just a
remoteEstablished _            = Nothing

pattern RemoteEstablished :: RemoteState m
pattern RemoteEstablished <- (remoteEstablished -> Just _)

{-# COMPLETE RemoteEstablished, RemoteIdle, RemoteCold #-}


-- | Set 'csRemoteState' for a given connection.
--
updateRemoteState :: Ord peerAddr
                  => ConnectionId peerAddr
                  -> RemoteState m
                  -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
                  -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
updateRemoteState connId csRemoteState state =
    state {
      igsConnections =
        Map.adjust
          (\connState -> connState { csRemoteState })
          connId
          (igsConnections state)
    }

mapRemoteState :: Ord peerAddr
               => ConnectionId peerAddr
               -> (RemoteState m -> RemoteState m)
               -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
               -> InboundGovernorState muxMode initiatorCtx peerAddr m a b
mapRemoteState connId fn state =
    state {
      igsConnections =
        Map.adjust
          (\connState@ConnectionState { csRemoteState } ->
            connState { csRemoteState = fn csRemoteState })
          connId
          (igsConnections state)
    }
