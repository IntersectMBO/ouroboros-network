{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Inbound protocol governor state.
--
module Ouroboros.Network.InboundGovernor.State
  ( InboundGovernorObservableState (..)
  , newObservableStateVar
  , newObservableStateVarIO
  , newObservableStateVarFromSeed
  -- * Internals
  , InboundGovernorState (..)
  , ConnectionState (..)
  , unregisterConnection
  , updateMiniProtocol
  , RemoteState (..)
  , updateRemoteState
  ) where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)

import           Data.ByteString.Lazy (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.Random (StdGen)
import qualified System.Random as Rnd

import qualified Network.Mux as Mux

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Mux hiding (ControlMessage)


-- | Currently only 'StdGen', but in the future this will be extended to
-- a record which contains some useful statistics about peers to support more
-- advances prune strategies (see. 'PruneStrategy').
--
newtype InboundGovernorObservableState = InboundGovernorObservableState {
      igosPrng :: StdGen
    }

-- | Create new observable state 'StrictTVar'.
--
newObservableStateVar
    :: MonadSTM m
    => StdGen
    -> m (StrictTVar m InboundGovernorObservableState)
newObservableStateVar prng =
    newTVarIO (InboundGovernorObservableState prng)


-- | Using the global 'StdGen'.
--
newObservableStateVarIO
    :: IO (StrictTVar IO InboundGovernorObservableState)
newObservableStateVarIO = do
    g <- Rnd.getStdGen
    let (g', igsPrng) = Rnd.split g
    Rnd.setStdGen g'
    newObservableStateVar igsPrng


-- | Useful for testing, is is using 'Rnd.mkStdGen'.
--
newObservableStateVarFromSeed
    :: MonadSTM m
    => Int
    -> m (StrictTVar m InboundGovernorObservableState)
newObservableStateVarFromSeed = newObservableStateVar . Rnd.mkStdGen


-- | 'InboundGovernorState', which consist of pure part, and a mutable part.
-- The mutable part can be observable from outside.  Future version could
-- contain additional statistics on the peers.
--
data InboundGovernorState muxMode peerAddr m a b =
    InboundGovernorState {
        -- | Map of connections state.  Modifying 'igsConnections' outside of
        -- 'inboundGovernorLoop' is not safe.
        --
        igsConnections   :: !(Map (ConnectionId peerAddr)
                                  (ConnectionState muxMode peerAddr m a b)),

        -- | PRNG available to 'PrunePolicy'.
        --
        igsObservableVar :: !(StrictTVar m InboundGovernorObservableState)
      }


-- | Per connection state tracked by /inbound protocol governor/.
--
data ConnectionState muxMode peerAddr m a b = ConnectionState {
      -- | Mux interface.
      --
      csMux             :: !(Mux.Mux muxMode m),

      -- | Connection data flow.
      --
      csDataFlow        :: !DataFlow,

      -- | All supported mini-protocols.
      --
      csMiniProtocolMap :: !(Map MiniProtocolNum
                                (MiniProtocol muxMode ByteString m a b)),

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
                     -> InboundGovernorState muxMode peerAddr m a b
                     -> InboundGovernorState muxMode peerAddr m a b
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
                   -> InboundGovernorState muxMode peerAddr m a b
                   -> InboundGovernorState muxMode peerAddr m a b
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
    -- 'RemoteEstablished' state.
    --
    = RemoteEstablished

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


-- | Set 'csRemoteState' for a given connection.
--
updateRemoteState :: Ord peerAddr
                  => ConnectionId peerAddr
                  -> RemoteState m
                  -> InboundGovernorState muxMode peerAddr m a b
                  -> InboundGovernorState muxMode peerAddr m a b
updateRemoteState connId csRemoteState state =
    state {
      igsConnections =
        Map.adjust
          (\connState -> connState { csRemoteState })
          connId
          (igsConnections state)
    }
