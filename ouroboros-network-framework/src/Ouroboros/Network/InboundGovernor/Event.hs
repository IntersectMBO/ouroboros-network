{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Internals of inbound protocol governor.  This module provide 'Event' type,
-- which enumerates external events and stm action which block until these
-- events fire.
--
module Ouroboros.Network.InboundGovernor.Event
  ( Event (..)
  , firstMuxToFinish
  , Terminated (..)
  , firstMiniProtocolToFinish
  , firstPeerPromotedToWarm
  , firstPeerDemotedToCold
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Monoid.Synchronisation

import qualified Network.Mux as Mux
import           Network.Mux.Types ( MiniProtocolStatus (..),
                                     MiniProtocolDir (..))

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.Mux hiding (ControlMessage)
import           Ouroboros.Network.InboundGovernor.State
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as ControlChannel


-- | Edge triggered events to which the /inbound protocol governor/ reacts.
--
data Event (muxMode :: MuxMode) peerAddr m a b
    -- | A request to start mini-protocol bundle, either from the server or from
    -- connection manager after a duplex connection was negotiated.
    --
    = NewConnection !(ControlChannel.NewConnection peerAddr
                        (Handle muxMode peerAddr ByteString m a b))

    -- | A multiplexer exited.
    --
    | MuxFinished            !(ConnectionId peerAddr) !(Maybe SomeException)

    -- | A mini-protocol terminated either cleanly or abruptly.
    --
    | MiniProtocolTerminated !(Terminated muxMode peerAddr m a b)

    -- | Transition from 'RemoteEstablished' to 'RemoteIdle'.
    --
    | WaitIdleRemote         !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' to 'RemoteCold'.
    --
    | CommitRemote           !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' or 'RemoteCold' to 'RemoteEstablished'.
    --
    | AwakeRemote            !(ConnectionId peerAddr)


--
-- STM transactions which detect 'Event's
--


-- | A mux stopped.  If mux exited cleanly no error is attached.
--
firstMuxToFinish
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr, Maybe SomeException)
firstMuxToFinish InboundGovernorState { igsConnections } =
      runFirstToFinish
    . Map.foldMapWithKey
        (\connId ConnectionState { csMux } ->
          FirstToFinish $ (connId,) <$> Mux.muxStopped csMux
        )
    $ igsConnections


-- | When a mini-protocol terminates we take 'Terminated' out of 'ConnectionState
-- and pass it to the main loop.  This is just enough to decide if we need to
-- restart a mini-protocol and to do the restart.
--
data Terminated muxMode peerAddr m a b = Terminated {
    tConnId       :: !(ConnectionId peerAddr),
    tMux          :: !(Mux.Mux muxMode m),
    tMiniProtocol :: !(MiniProtocol muxMode ByteString m a b),
    tDataFlow     :: !DataFlow,
    tResult       :: !(Either SomeException b)
  }


-- | Detect when one of the mini-protocols terminated.
--
-- /triggers:/ 'MiniProtocolTerminated'.
--
firstMiniProtocolToFinish
    :: Alternative (STM m)
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (Terminated muxMode peerAddr m a b)
firstMiniProtocolToFinish InboundGovernorState { igsConnections } = runFirstToFinish $
    Map.foldMapWithKey
      (\connId ConnectionState { csMux,
                                 csMiniProtocolMap,
                                 csDataFlow,
                                 csCompletionMap
                               } ->
        Map.foldMapWithKey
          (\miniProtocolNum completionAction ->
                (\tResult -> Terminated {
                      tConnId       = connId,
                      tMux          = csMux,
                      tMiniProtocol = csMiniProtocolMap Map.! miniProtocolNum,
                      tDataFlow     = csDataFlow,
                      tResult
                    }
                )
            <$> FirstToFinish completionAction
          )
          csCompletionMap
      )
      igsConnections


-- | Detect when one of the peers was promoted to warm, e.g.
-- @PromotedToWarm^{Duplex}_{Remote}@ or
-- @PromotedToWarm^{Unidirectional}_{Remote}@.
--
-- /triggers:/ 'PromotedToWarm'
--
-- Note: The specification only describes @PromotedToWarm^{Duplex}_{Remote}@
-- transition, but here we don't make a distinction on @Duplex@ and
-- @Unidirectional@ connections.
--
firstPeerPromotedToWarm
    :: forall muxMode peerAddr m a b. MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr)
firstPeerPromotedToWarm InboundGovernorState { igsConnections } = runFirstToFinish $
     Map.foldMapWithKey
       (\connId ConnectionState { csMux,
                                  csRemoteState
                                } ->
         case csRemoteState of
           -- the connection is already in 'RemoteEstablished' state.
           RemoteEstablished -> mempty

           -- If the connection is in 'RemoteCold' state we do first to finish
           -- synchronisation to detect incoming traffic on any of the responder
           -- mini-protocols.
           --
           -- This works for both duplex and unidirectional connections (e.g. p2p
           -- \/ non-p2p nodes), for which protocols are started eagerly, unlike
           -- for p2p nodes for which we start all mini-protocols on demand.
           -- Using 'miniProtocolStatusVar' is ok for unidirectional connection,
           -- as we never restart the protocols for them.  They transition to
           -- 'RemoteWarm' as soon the connection is accepted.  This is because
           -- for eagerly started mini-protocols mux puts them in 'StatusRunning'
           -- as soon as mini-protocols are set in place by 'runMiniProtocol'.
           RemoteCold ->
             Map.foldMapWithKey
               (fn connId)
               (Mux.miniProtocolStateMap csMux)

           -- We skip it here; this case is done in 'firstPeerDemotedToCold'.
           RemoteIdle {} -> mempty
       )
       igsConnections
  where
    fn :: ConnectionId peerAddr
       -> (MiniProtocolNum, MiniProtocolDir)
       -> STM m MiniProtocolStatus
       -> FirstToFinish (STM m) (ConnectionId peerAddr)
    fn connId = \(_miniProtocolNum, miniProtocolDir) miniProtocolStatus ->
      case miniProtocolDir of
        InitiatorDir -> mempty

        ResponderDir ->
          FirstToFinish $
            miniProtocolStatus >>= \case
              StatusIdle          -> retry
              StatusStartOnDemand -> retry
              StatusRunning       -> return connId



-- | Await for first peer demoted to cold, i.e. detect the
-- @DemotedToCold^{Duplex}_{Remote}@.
--
-- /triggers:/ 'DemotedToColdRemote'
--
firstPeerDemotedToCold
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (Event muxMode peerAddr m a b)
firstPeerDemotedToCold InboundGovernorState { igsConnections } = runFirstToFinish $
    Map.foldMapWithKey
      (\connId
        ConnectionState {
          csMux,
          csRemoteState
        } ->
        case csRemoteState of
          -- the connection is already in 'RemoteCold' state
          RemoteCold -> mempty

          -- Responders are started using 'StartOnDemand' strategy. We detect
          -- when all of the responders are in 'StatusIdle' or
          -- 'StatusStartOnDemand' and subsequently put the connection in
          -- 'RemoteIdle' state.
          --
          -- In compat mode, when established mini-protocols terminate they will
          -- not be restarted.
          RemoteEstablished ->
                fmap (const (WaitIdleRemote connId))
              . lastToFirstM
              $ (Map.foldMapWithKey
                  (\(_, miniProtocolDir) miniProtocolStatus ->
                    case miniProtocolDir of
                      InitiatorDir -> mempty

                      ResponderDir ->
                        LastToFinishM $ do
                          miniProtocolStatus >>= \case
                            StatusIdle          -> return ()
                            StatusStartOnDemand -> return ()
                            StatusRunning       -> retry
                  )
                  (Mux.miniProtocolStateMap csMux)
                )

          -- Possible for both 'Unidirectional' and 'Duplex' connections.  Wait
          -- for first of:
          --
          -- 1. timeout, in which case we will transition to 'RemoteCold',
          -- 2. one of mini-protocols to wake up, in which case we transition
          --    to 'RemoteWarm';
          -- 3. mux stopped;
          --
          -- This is done to solve a situation where one of the mini-protocols
          -- terminates quickly while other mini-protocols are inactive
          -- (e.g. their initial messages are still in flight).
          RemoteIdle timeoutSTM ->
              -- At this stage we know that all mini-protocols terminated, and
              -- we wait for a period of idleness.
                    Map.foldMapWithKey
                      (\(_, miniProtocolDir) miniProtocolStatus ->
                          case miniProtocolDir of
                            InitiatorDir -> mempty

                            ResponderDir ->
                              FirstToFinish $ do
                                miniProtocolStatus >>= \case
                                 StatusIdle          -> retry
                                 StatusStartOnDemand -> retry
                                 StatusRunning       -> return (AwakeRemote connId)
                      )
                      (Mux.miniProtocolStateMap csMux)
                  <> FirstToFinish (timeoutSTM $> CommitRemote connId)
      )
      igsConnections
