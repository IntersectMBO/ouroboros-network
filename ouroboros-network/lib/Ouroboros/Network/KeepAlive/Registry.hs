{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive.Registry
  ( KeepAliveRegistry (..)
  , newKeepAliveRegistry
  , bracketKeepAliveClient
  , bracketKeepAliveClientWithRtt
  , KeepAliveRtt (..)
  , KeepAliveRttReader
  , lookupKeepAliveRtt
  , readKeepAliveRtt
  , KeepAliveClientContext
  , keepAliveGsvContext
  , publishKeepAliveRtt
  , readPeerGSVs
  ) where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set.Strict qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI

import Ouroboros.Network.DeltaQ

-- | The latest successfully completed KeepAlive exchange, before smoothing or
-- consumer-specific headroom. Published as one observation, independently of
-- tracing. Rationale: @docs/keepalive-rtt.md#observation@.
data KeepAliveRtt = KeepAliveRtt {
       rttDuration    :: !DiffTime,
       -- ^ Raw elapsed seconds between starting the probe and its successful
       -- response continuation. This includes local channel/driver overhead.
       rttCompletedAt :: !Time
       -- ^ Monotonic time observed on entering the successful continuation.
     }
  deriving (Eq, Show)

-- | Internal lifetime state. Retirement is permanent, including for a producer
-- context retained after its bracket has ended.
data KeepAliveRttState = RttActive !(Maybe KeepAliveRtt) | RttRetired

-- | Read-only handle pinned to one KeepAlive registration. It never follows a
-- replacement registration at the same peer key. Reads return 'Nothing' before
-- the first success and after retirement.
-- Rationale: @docs/keepalive-rtt.md#connection-lifetime@.
newtype KeepAliveRttReader m =
    KeepAliveRttReader (StrictTVar m KeepAliveRttState)

-- | Producer context allocated by 'bracketKeepAliveClientWithRtt'. Its RTT cell
-- shares that registration's lifetime; the GSV map retains its existing
-- BlockFetch coordination semantics.
data KeepAliveClientContext peer m = KeepAliveClientContext {
       keepAliveGsvContext :: !(StrictTVar m (Map peer PeerGSV)),
       -- ^ Existing shared GSV state used by the KeepAlive client.
       keepAliveRttState   :: !(StrictTVar m KeepAliveRttState)
       -- ^ Private observation/lifetime cell; consumers receive only a reader.
     }

-- | Capture the reader for a currently registered peer. 'Nothing' means that
-- there is no live KeepAlive registration. Capture once in the connection's
-- consumer context; use 'readKeepAliveRtt' for subsequent cached reads.
lookupKeepAliveRtt :: (MonadSTM m, Ord peer)
                  => KeepAliveRegistry peer m
                  -> peer
                  -> STM m (Maybe (KeepAliveRttReader m))
lookupKeepAliveRtt KeepAliveRegistry { rttRegistry } peer =
  Map.lookup peer <$> readTVar rttRegistry

-- | Read cached raw data without waiting for another probe. A retired reader
-- stays empty even when another connection reuses its former registry key.
readKeepAliveRtt :: MonadSTM m
                 => KeepAliveRttReader m
                 -> STM m (Maybe KeepAliveRtt)
readKeepAliveRtt (KeepAliveRttReader stateVar) = do
  state <- readTVar stateVar
  pure $ case state of
    RttActive sample -> sample
    RttRetired       -> Nothing

-- | Publish one successful observation, in the same STM transaction as its GSV
-- update. A retired producer cannot revive an old reader or affect a new one.
publishKeepAliveRtt :: MonadSTM m
                    => KeepAliveClientContext peer m
                    -> KeepAliveRtt
                    -> STM m ()
publishKeepAliveRtt KeepAliveClientContext { keepAliveRttState } sample = do
  state <- readTVar keepAliveRttState
  case state of
    RttActive _ -> writeTVar keepAliveRttState (RttActive (Just sample))
    RttRetired  -> pure ()

-- | A registry which keeps `PeerGSV` information based on `keep-alive`
-- measurements and independent connection-scoped raw RTT readers.
--
data KeepAliveRegistry peer m = KeepAliveRegistry {
       dqRegistry
         :: !(StrictTVar  m (Map peer PeerGSV)),
       -- ^ Existing smoothed GSV estimates, retained through BlockFetch cleanup.
       keepRegistry
         :: !(StrictTVar  m (Map peer (ThreadId m, StrictTMVar m ()))),
       -- ^ BlockFetch clients coordinated with KeepAlive registration cleanup.
       dyingRegistry
         :: !(StrictTVar m (Set peer)),
       -- ^ Peers whose KeepAlive cleanup is waiting for BlockFetch to stop.
       rttRegistry
         :: !(StrictTVar m (Map peer (KeepAliveRttReader m)))
       -- ^ Readers for live KeepAlive registrations; independent of hot status.
     }

newKeepAliveRegistry :: MonadSTM m
                     => m (KeepAliveRegistry peer m)
newKeepAliveRegistry = KeepAliveRegistry <$> newTVarIO Map.empty
                                         <*> newTVarIO Map.empty
                                         <*> newTVarIO Set.empty
                                         <*> newTVarIO Map.empty

bracketKeepAliveClient :: forall m a peer.
                              (MonadSTM m, MonadFork m, MonadMask m, Ord peer)
                       => KeepAliveRegistry peer m
                       -> peer
                       -> (StrictTVar m (Map peer PeerGSV) -> m a)
                       -> m a
bracketKeepAliveClient registry peer action =
  bracketKeepAliveClientWithRtt registry peer (action . keepAliveGsvContext)

-- | Register a fresh raw-observation cell alongside the existing GSV entry.
-- Retire it when the client exits, before potentially waiting for BlockFetch
-- cleanup. A new registration never inherits observations at the same peer key.
-- Rationale: @docs/keepalive-rtt.md#connection-lifetime@.
bracketKeepAliveClientWithRtt
    :: forall m a peer.
       (MonadSTM m, MonadFork m, MonadMask m, Ord peer)
    => KeepAliveRegistry peer m
    -> peer
    -> (KeepAliveClientContext peer m -> m a)
    -> m a
bracketKeepAliveClientWithRtt
    KeepAliveRegistry { dqRegistry, keepRegistry, dyingRegistry, rttRegistry }
    peer action = bracket register unregister action
  where
    -- the keepAliveClient will register a PeerGSV and the block fetch client will wait on it.
    register :: m (KeepAliveClientContext peer m)
    register =
      atomically $ do
        -- Wait for previous keep alive client to cleanup
        dr <- readTVar dqRegistry
        check (peer `Map.notMember` dr)

        modifyTVar dqRegistry $ \m ->
          assert (peer `Map.notMember` m) $
          Map.insert peer defaultGSV m
        stateVar <- newTVar (RttActive Nothing)
        modifyTVar rttRegistry $ Map.insert peer (KeepAliveRttReader stateVar)
        pure (KeepAliveClientContext dqRegistry stateVar)

    -- It is possible for the keepAlive client to keep running even without a fetch client, but
    -- a fetch client shouldn't run without a keepAlive client.
    unregister :: KeepAliveClientContext peer m -> m ()
    unregister KeepAliveClientContext { keepAliveRttState } = uninterruptibleMask_ $ do
      fetchclient_m <- atomically $ do
        writeTVar keepAliveRttState RttRetired
        modifyTVar rttRegistry (Map.delete peer)
        fetchclients <- readTVar keepRegistry
        case Map.lookup peer fetchclients of
             Nothing -> do
               -- If the fetch client is already dead we remove PeerGSV ourself directly.
               modifyTVar dqRegistry $ \m ->
                 assert (peer `Map.member` m) $
                 Map.delete peer m
               return Nothing
             Just rc -> do
               -- Prevent a new fetchclient from starting while we are killing the old one.
               modifyTVar dyingRegistry $ \s ->
                 assert (peer `Set.notMember` s) $
                 Set.insert peer s
               return $ Just rc
      case fetchclient_m of
           Nothing -> return ()
           Just (tid, doneVar) -> do
             -- Cancel the fetch client.
             throwTo tid AsyncCancelled
             atomically $ do
               -- wait for fetch client to exit.
               readTMVar doneVar
               modifyTVar dqRegistry $ \m ->
                 assert (peer `Map.member` m) $
                 Map.delete peer m
               modifyTVar dyingRegistry $ \s ->
                 assert (peer `Set.member` s) $
                 Set.delete peer s

-- | A read-only 'STM' action to get the 'PeerGSV's for all fetch
-- clients in the 'FetchClientRegistry'.
--
readPeerGSVs :: forall m peer.
                ( MonadSTM m, Ord peer)
             => KeepAliveRegistry peer m
             -> STM m (Map peer PeerGSV)
readPeerGSVs KeepAliveRegistry { dqRegistry, keepRegistry } = do
  dr <- readTVar dqRegistry
  kr <- readTVar keepRegistry
  -- The intersection gives us only the currently hot peers
  return $ Map.intersection dr kr
