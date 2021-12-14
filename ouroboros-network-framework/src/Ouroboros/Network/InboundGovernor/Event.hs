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
  , EventSignal
  , firstMuxToFinish
  , Terminated (..)
  , firstMiniProtocolToFinish
  , firstPeerPromotedToWarm
  , firstPeerPromotedToHot
  , firstPeerDemotedToWarm
  , firstPeerDemotedToCold
  , firstPeerCommitRemote
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid.Synchronisation
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

    -- | A remote @warm → hot@ transition.  It is scheduled as soon as all hot
    -- mini-protocols are running.
    --
    | RemotePromotedToHot    !(ConnectionId peerAddr)

    -- | A @hot → warm@ transition.  It is scheduled as soon as any hot
    -- mini-protocol terminates.
    --
    | RemoteDemotedToWarm    !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' to 'RemoteCold'.
    --
    | CommitRemote           !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' or 'RemoteCold' to 'RemoteEstablished'.
    --
    | AwakeRemote            !(ConnectionId peerAddr)


--
-- STM transactions which detect 'Event's (signals)
--


-- | A signal which returns an 'Event'.  Signals are combined together and
-- passed used to fold the current state map.
--
type EventSignal (muxMode :: MuxMode) peerAddr m a b =
        ConnectionId peerAddr
     -> ConnectionState muxMode peerAddr m a b
     -> FirstToFinish (STM m) (Event muxMode peerAddr m a b)

-- | A mux stopped.  If mux exited cleanly no error is attached.
--
firstMuxToFinish :: MonadSTM m
                 => EventSignal muxMode peerAddr m a b
firstMuxToFinish connId ConnectionState { csMux } =
    FirstToFinish $ MuxFinished connId <$> Mux.muxStopped csMux


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
firstMiniProtocolToFinish :: MonadSTM m
                          => EventSignal muxMode peerAddr m a b
firstMiniProtocolToFinish
    connId
    ConnectionState { csMux,
                      csMiniProtocolMap,
                      csDataFlow,
                      csCompletionMap
                    }
    = Map.foldMapWithKey
        (\miniProtocolNum completionAction ->
              (\tResult -> MiniProtocolTerminated $ Terminated {
                    tConnId           = connId,
                    tMux              = csMux,
                    tMiniProtocolData = csMiniProtocolMap Map.! miniProtocolNum,
                    tDataFlow         = csDataFlow,
                    tResult
                  }
              )
          <$> FirstToFinish completionAction
        )
        csCompletionMap
      

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
firstPeerPromotedToWarm :: forall muxMode peerAddr m a b.
                           MonadSTM m
                        => EventSignal muxMode peerAddr m a b
firstPeerPromotedToWarm
    connId
    ConnectionState { csMux, csRemoteState }
    = case csRemoteState of
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
            fn
            (Mux.miniProtocolStateMap csMux)

        -- We skip it here; this case is done in 'firstPeerDemotedToCold'.
        RemoteIdle {} ->
          Map.foldMapWithKey
            fn
            (Mux.miniProtocolStateMap csMux)
  where
    fn :: (MiniProtocolNum, MiniProtocolDir)
       -> STM m MiniProtocolStatus
       -> FirstToFinish (STM m) (Event muxMode peerAddr m a b)
    fn = \(_miniProtocolNum, miniProtocolDir) miniProtocolStatus ->
      case miniProtocolDir of
        InitiatorDir -> mempty

        ResponderDir ->
          FirstToFinish $
            miniProtocolStatus >>= \case
              StatusIdle          -> retry
              StatusStartOnDemand -> retry
              StatusRunning       -> return $ AwakeRemote connId


-- | Detect when a first warm peer is promoted to hot (all hot mini-protocols
-- run running).
--
firstPeerPromotedToHot :: forall muxMode peerAddr m a b.
                          MonadSTM m
                       => EventSignal muxMode peerAddr m a b
firstPeerPromotedToHot 
   connId connState@ConnectionState { csRemoteState }
   = case csRemoteState of
       RemoteHot     -> mempty
       RemoteWarm    ->
           lastToFirstM
         . fmap (const $ RemotePromotedToHot connId)
         $ foldMap fn
             (hotMiniProtocolStateMap connState)
       RemoteCold    ->
           lastToFirstM
         . fmap (const $ RemotePromotedToHot connId)
         $ foldMap fn
             (hotMiniProtocolStateMap connState)
       RemoteIdle {} -> mempty
  where
    -- only hot mini-protocols;
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
              case mpdMiniProtocolTemp of
                Hot -> True
                _   -> False
           )
       $ csMiniProtocolMap
       )

    fn :: STM m MiniProtocolStatus
       -> LastToFinishM (STM m) ()
    fn miniProtocolStatus =
      LastToFinishM $
        miniProtocolStatus >>= \case
          StatusIdle          -> retry
          StatusStartOnDemand -> retry
          StatusRunning       -> return ()


-- | Detect when a first hot mini-protocols terminates, which triggers the
-- `RemoteHot → RemoteWarm` transition.
--
firstPeerDemotedToWarm :: forall muxMode peerAddr m a b.
                          MonadSTM m
                       => EventSignal muxMode peerAddr m a b
firstPeerDemotedToWarm 
    connId connState@ConnectionState { csRemoteState }
    = case csRemoteState of
        RemoteHot ->
              const (RemoteDemotedToWarm connId)
          <$> foldMap fn (hotMiniProtocolStateMap connState)

        _  -> mempty
  where
    -- only hot mini-protocols;
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
                case mpdMiniProtocolTemp of
                  Hot -> True
                  _   -> False
           )
       $ csMiniProtocolMap
       )

    fn :: STM m MiniProtocolStatus
       -> FirstToFinish (STM m) ()
    fn miniProtocolStatus =
      FirstToFinish $
        miniProtocolStatus >>= \case
          StatusIdle          -> return ()
          StatusStartOnDemand -> return ()
          StatusRunning       -> retry


-- | Await for first peer demoted to cold, i.e. detect the
-- @DemotedToCold^{Duplex}_{Remote}@.
--
-- /triggers:/ 'DemotedToColdRemote'
--
firstPeerDemotedToCold :: MonadSTM m
                       => EventSignal muxMode peerAddr m a b
firstPeerDemotedToCold
    connId
    ConnectionState {
      csMux,
      csRemoteState
    }
    = case csRemoteState of
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
              fmap (const $ WaitIdleRemote connId)
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

        RemoteIdle {} -> mempty


-- | First peer for which the 'RemoteIdle' timeout expires.
--
firstPeerCommitRemote :: MonadSTM m
                      => EventSignal muxMode peerAddr m a b
firstPeerCommitRemote
    connId ConnectionState { csRemoteState }
    = case csRemoteState of
        -- the connection is already in 'RemoteCold' state
        RemoteCold            -> mempty
        RemoteEstablished     -> mempty
        RemoteIdle timeoutSTM -> FirstToFinish (timeoutSTM $> CommitRemote connId)
