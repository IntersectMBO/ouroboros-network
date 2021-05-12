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
  , firstPeerPromotedToHot
  , firstPeerDemotedToCold
  ) where

import           Control.Applicative (Alternative (..), (<|>))
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

    -- | We track remote @warm → hot@ transitions.  The @hot → warm@ transition
    -- is tracked by 'MiniProtocolTerminated' when
    -- a any hot mini-protocol terminates.
    --
    | RemotePromotedToHot    !(ConnectionId peerAddr)

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
    Map.foldrWithKey
      (\connId ConnectionState { csMux } outer ->
            (connId,) <$> Mux.muxStopped csMux
        <|> outer)
      empty
      igsConnections


-- | When a mini-protocol terminates we take 'Terminated' out of 'ConnectionState
-- and pass it to the main loop.  This is just enough to decide if we need to
-- restart a mini-protocol and to do the restart.
--
data Terminated muxMode peerAddr m a b = Terminated {
    tConnId           :: !(ConnectionId peerAddr),
    tMux              :: !(Mux.Mux muxMode m),
    tMiniProtocolData :: !(MiniProtocolData muxMode m a b),
    tDataFlow         :: !DataFlow,
    tResult           :: !(Either SomeException b)
  }


-- | Detect when one of the mini-protocols terminated.
--
-- /triggers:/ 'MiniProtocolTerminated'.
--
firstMiniProtocolToFinish
    :: Alternative (STM m)
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (Terminated muxMode peerAddr m a b)
firstMiniProtocolToFinish InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId ConnectionState { csMux,
                                 csMiniProtocolMap,
                                 csDataFlow,
                                 csCompletionMap
                               }
        outer ->
        Map.foldrWithKey
          (\miniProtocolNum completionAction inner ->
                (\tResult -> Terminated {
                      tConnId           = connId,
                      tMux              = csMux,
                      tMiniProtocolData = csMiniProtocolMap Map.! miniProtocolNum,
                      tDataFlow         = csDataFlow,
                      tResult
                    }
                )
            <$> completionAction
            <|> inner
          )
          outer
          csCompletionMap
      )
      empty
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
firstPeerPromotedToWarm InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId ConnectionState { csMux,
                                 csRemoteState
                               } outer ->
        case csRemoteState of
          -- the connection is already in 'RemoteEstablished' state.
          RemoteEstablished -> outer

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
            Map.foldrWithKey
              (fn connId)
              outer
              (Mux.miniProtocolStateMap csMux)

          -- We skip it here; this case is done in 'firstPeerDemotedToCold'.
          RemoteIdle {} -> outer
      )
      retry
      igsConnections
  where
    fn :: ConnectionId peerAddr
       -> (MiniProtocolNum, MiniProtocolDir)
       -> STM m MiniProtocolStatus
       -> STM m (ConnectionId peerAddr)
       -> STM m (ConnectionId peerAddr)
    fn connId =
      \(_miniProtocolNum, miniProtocolDir)
      miniProtocolStatus
      inner ->
        case miniProtocolDir of
          InitiatorDir -> inner

          ResponderDir ->
                (do status <- miniProtocolStatus
                    case status of
                      StatusIdle          -> retry
                      StatusStartOnDemand -> retry
                      StatusRunning       -> return connId
                )
            <|> inner


-- | Detect when a first warm peer is promoted to hot (all established and hot
-- mini-protocols run running).
--
firstPeerPromotedToHot
    :: forall muxMode peerAddr m a b. MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr)
firstPeerPromotedToHot InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId connState@ConnectionState { csRemoteState } outer ->
        case csRemoteState of
          RemoteHot     -> outer
          RemoteWarm    ->
                Map.foldr
                  (fn connId)
                  (return connId)
                  (hotMiniProtocolStateMap connState)
            <|> outer
          RemoteCold    -> 
              Map.foldr
                (fn connId)
                (return connId)
                (hotMiniProtocolStateMap connState)
            <|> outer
          RemoteIdle {} -> outer
      )
      empty
      igsConnections
  where
    -- only hot or established mini-protocols;
    hotMiniProtocolStateMap :: ConnectionState muxMode peerAddr m a b
                            -> Map (MiniProtocolNum, MiniProtocolDir)
                                   (STM m MiniProtocolStatus)
    hotMiniProtocolStateMap ConnectionState { csMux, csMiniProtocolMap } =
       Mux.miniProtocolStateMap csMux
       `Map.restrictKeys`
       ( Set.map (,ResponderDir)
       . Map.keysSet
       . Map.filter
           (\MiniProtocolData { mpdMiniProtocolTemp } ->
                mpdMiniProtocolTemp == Hot
             || mpdMiniProtocolTemp == Established
           )
       $ csMiniProtocolMap
       )

    fn :: ConnectionId peerAddr
       -> STM m MiniProtocolStatus
       -> STM m (ConnectionId peerAddr)
       -> STM m (ConnectionId peerAddr)
    fn connId miniProtocolStatus inner =
         inner
      >> miniProtocolStatus >>= \case
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
firstPeerDemotedToCold InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId
        ConnectionState {
          csMux,
          csRemoteState
        }
        outer ->
        case csRemoteState of
          -- the connection is already in 'RemoteCold' state
          RemoteCold -> outer

          -- Responders are started using 'StartOnDemand' strategy. We detect
          -- when all of the responders are in 'StatusIdle' or
          -- 'StatusStartOnDemand' and subsequently put the connection in
          -- 'RemoteIdle' state.
          --
          -- In compat mode, when established mini-protocols terminate they will
          -- not be restarted.
          RemoteEstablished ->
                outer
            <|> (Map.foldlWithKey'
                  (\inner (_, miniProtocolDir) miniProtocolStatus ->
                    case miniProtocolDir of
                      InitiatorDir -> inner

                      ResponderDir ->
                           inner
                        >> miniProtocolStatus >>= \case
                             StatusIdle          -> return ()
                             StatusStartOnDemand -> return ()
                             StatusRunning       -> retry
                  )
                  (return ())
                  (Mux.miniProtocolStateMap csMux)
                ) $> WaitIdleRemote connId

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
              -- In non-compat mode, at this stage we know that all
              -- mini-protocols terminated, and we wait for a period of
              -- idleness.  Note that the meaning of 'timeoutSTM' is
              -- different from the compat mode case.
                  outer
              <|> Map.foldlWithKey'
                    (\inner (_, miniProtocolDir) miniProtocolStatus ->
                        case miniProtocolDir of
                          InitiatorDir -> inner

                          ResponderDir ->
                               (miniProtocolStatus >>= \case
                                 StatusIdle          -> retry
                                 StatusStartOnDemand -> retry
                                 StatusRunning       -> return (AwakeRemote connId)
                               )
                            <|> inner
                    )
                    (
                          timeoutSTM $> CommitRemote connId
                    )
                    (Mux.miniProtocolStateMap csMux)
      )
      retry
      igsConnections
