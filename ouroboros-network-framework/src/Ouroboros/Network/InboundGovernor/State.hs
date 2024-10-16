{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Inbound protocol governor state.
--
-- The module should be imported qualified.
--
module Ouroboros.Network.InboundGovernor.State
  ( PublicState (..)
    -- * Internals
  , mkPublicState
  , State (..)
  , ConnectionState (..)
  , Counters (..)
  , counters
  , unregisterConnection
  , updateMiniProtocol
  , RemoteState (.., RemoteEstablished)
  , RemoteSt (..)
  , mkRemoteSt
  , updateRemoteState
  , mapRemoteState
  , MiniProtocolData (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad.Class.MonadTime.SI

import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.OrdPSQ as OrdPSQ

import Network.Mux qualified as Mux

import Ouroboros.Network.Context
import Ouroboros.Network.Mux


-- | Public inbound governor state.
--
data PublicState peerAddr versionData = PublicState {
      -- | A map of mature inbound duplex connections. These peers are used for
      -- light peer sharing (e.g. bootstrapping peer sharing).
      --
      inboundDuplexPeers :: !(Map peerAddr versionData),

      -- | Map of `RemoteSt`.
      --
      -- It is lazy on purpose.  It is created from `igsConnections`, so it
      -- should not point to any thunks that should be GC-ed leading to a memory
      -- leak.
      --
      remoteStateMap     :: Map (ConnectionId peerAddr) RemoteSt

    }


-- | Smart constructor for `PublicState`.
--
-- NOTE: we assume that all inbound connections share the same address.  This
-- is true in P2P mode since all outbound connections (which also can end up in
-- the `State`) bind the server address.  This allows us to use
-- `Map.mapKeysMonotonic`.
--
mkPublicState
  :: forall muxMode initatorCtx versionData peerAddr m a b.
     State muxMode initatorCtx peerAddr versionData m a b
  -> PublicState peerAddr versionData
mkPublicState
    State { connections, matureDuplexPeers }
    =
    PublicState {
      inboundDuplexPeers = matureDuplexPeers,
      remoteStateMap     = Map.map (mkRemoteSt . csRemoteState) connections
    }

-- | 'State', which consist of pure part, and a mutable part. The mutable part
-- can be observable from outside.  Future version could
-- contain additional statistics on the peers.
--
data State muxMode initiatorCtx peerAddr versionData m a b =
    State {
        -- | Map of connections state.  Modifying 'igsConnections' outside of
        -- 'inboundGovernorLoop' is not safe.
        --
        connections   :: !(Map (ConnectionId peerAddr)
                                  (ConnectionState muxMode initiatorCtx peerAddr versionData m a b)),

        -- | Map of mature duplex peers.
        --
        -- We don't add new duplex connections immediately, but only after
        -- a 15mins delay (defined in `inboundMaturePeerDelay`.
        --
        matureDuplexPeers :: !(Map peerAddr versionData),

        -- | Queue of fresh duplex connections ordered by time of arrival.  Only
        -- connections younger than than 15mins are kept in this data structure.
        --
        freshDuplexPeers :: !(OrdPSQ peerAddr Time versionData),

        -- | 'Counters' counters cache. Allows to only trace values when
        -- necessary.
        countersCache :: !(Cache Counters)
      }

-- | Counters for tracing and analysis purposes
--
data Counters = Counters {
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

instance Semigroup Counters where
    Counters c i w h <> Counters c' i' w' h' =
      Counters (c + c') (i + i') (w + w') (h + h')

instance Monoid Counters where
    mempty = Counters 0 0 0 0


counters :: State muxMode initiatorCtx peerAddr versionData m a b
         -> Counters
counters State { connections } =
    foldMap (\ConnectionState { csRemoteState } ->
              case csRemoteState of
                RemoteCold    -> Counters 1 0 0 0
                RemoteIdle {} -> Counters 0 1 0 0
                RemoteWarm    -> Counters 0 0 1 0
                RemoteHot     -> Counters 0 0 0 1
            )
            connections


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
data ConnectionState muxMode initiatorCtx peerAddr versionData m a b = ConnectionState {
      -- | Mux interface.
      --
      csMux             :: !(Mux.Mux muxMode m),

      -- | version data negotiated on the connection.
      --
      csVersionData     :: !versionData,

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


-- | Remove connection from 'State'.
--
unregisterConnection :: Ord peerAddr
                     => ConnectionId peerAddr
                     -> State muxMode initiatorCtx peerAddr versionData m a b
                     -> State muxMode initiatorCtx peerAddr versionData m a b
unregisterConnection connId state =
    state { connections =
              assert (connId `Map.member` connections state) $
              Map.delete connId (connections state),

            matureDuplexPeers =
              Map.delete (remoteAddress connId) (matureDuplexPeers state),

            freshDuplexPeers =
              OrdPSQ.delete (remoteAddress connId) (freshDuplexPeers state)
          }


-- | Update a mini-protocol in 'ConnectionState'.  Once a mini-protocol was
-- restarted we put the new completion action into 'csCompletionMap'.
--
updateMiniProtocol :: Ord peerAddr
                   => ConnectionId peerAddr
                   -> MiniProtocolNum
                   -> STM m (Either SomeException b)
                   -> State muxMode initiatorCtx peerAddr versionData m a b
                   -> State muxMode initiatorCtx peerAddr versionData m a b
updateMiniProtocol connId miniProtocolNum completionAction state =
    state { connections =
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
                         (connections state)
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
                  -> State muxMode initiatorCtx peerAddr versionData m a b
                  -> State muxMode initiatorCtx peerAddr versionData m a b
updateRemoteState connId csRemoteState state =
    state {
      connections =
        Map.adjust
          (\connState -> connState { csRemoteState })
          connId
          (connections state)
    }

mapRemoteState :: Ord peerAddr
               => ConnectionId peerAddr
               -> (RemoteState m -> RemoteState m)
               -> State muxMode initiatorCtx peerAddr versionData m a b
               -> State muxMode initiatorCtx peerAddr versionData m a b
mapRemoteState connId fn state =
    state {
      connections =
        Map.adjust
          (\connState@ConnectionState { csRemoteState } ->
            connState { csRemoteState = fn csRemoteState })
          connId
          (connections state)
    }


-- | Remote connection state tracked by inbound protocol governor.
--
-- This type is used for tracing.
--
data RemoteSt = RemoteWarmSt
              | RemoteHotSt
              | RemoteIdleSt
              | RemoteColdSt
  deriving (Eq, Show)


mkRemoteSt :: RemoteState m -> RemoteSt
mkRemoteSt  RemoteWarm    = RemoteWarmSt
mkRemoteSt  RemoteHot     = RemoteHotSt
mkRemoteSt (RemoteIdle _) = RemoteIdleSt
mkRemoteSt  RemoteCold    = RemoteColdSt
