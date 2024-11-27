{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Network.PeerSelection.Governor.Event where

import Data.Kind
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Concurrent.Class.MonadSTM
import Control.Monad.Reader
import FRP.Rhine.ClSF
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Select
import Control.Monad.Class.MonadTime.SI qualified as SI
import Control.Monad.Class.MonadTimer.SI
import Data.TimeDomain
import Data.OrdPSQ qualified as PSQ
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Control.Monad.State ( evalStateT, StateT )
import Ouroboros.Network.PeerSelection.Governor.Types
import FRP.Rhine.Reactimation.Combinators qualified as Rhine
import FRP.Rhine.Schedule
import FRP.Rhine.Type
import FRP.Rhine.Clock.Proxy

data DummyUniqKey = DummyUniqKey

infixl 3 |@|
(|@|) ::
  ( Monad m
  , Clock m clL
  , Clock m clR
  , Clock m (Out clL)
  , Clock m (Out clR)
  , GetClockProxy clL
  , GetClockProxy clR
  , Time clL ~ Time (Out clL)
  , Time clR ~ Time (Out clR)
  , Time clL ~ Time (In clL)
  , Time clR ~ Time (In clR)
  , Time clL ~ Time clR
  ) =>
  Rhine m                clL      a b ->
  Rhine m                    clR  a b ->
  Rhine m (ParallelClock clL clR) a b
(|@|) = (Rhine.|@|)

instance Eq DummyUniqKey where
  _ == _ = False

instance Ord DummyUniqKey where
  compare _ _ = LT

-- | Events come thru a TChan in two flavors. Left is for delayed actions,
-- and Right is for actions that fire now. The TChan is wrapped as otherwise
-- a Clock instance can't be derived for a TChan type family
--
-- type ControlChannel m event = ReaderT (TChanWrapper m (Either (SI.Time, event) event)) m
-- newtype TChanWrapper m event = TChanWrapper (TChan m event)
type ControlChannel m event = ReaderT (TChanWrapper m (Either (SI.Time, event) event)) m
newtype TChanWrapper m event = TChanWrapper (TChan m event)

data PeerSelectionEvent = PeerSelectionCardano CardanoEvent
                        | PeerSelectionEvent PeerSelectionGeneral
                        | CardanoDelay

data GovernorClock event = GovernorClock
type CardanoClock = SelectClock (GovernorClock PeerSelectionEvent) CardanoEvent
type PeerSelectionClock = SelectClock (GovernorClock PeerSelectionEvent) PeerSelectionGeneral

data CardanoEvent = LedgerChange !LedgerStateJudgement | BPChange !UseBootstrapPeers
data PeerSelectionGeneral = Churn | Known | Established | Job

instance ( MonadTimer m
         , Alternative (STM m)
         ) => Clock (ControlChannel m event) (GovernorClock event) where
  type Time (GovernorClock event) = SI.Time
  type Tag (GovernorClock event)  = event
  initClock _ = do
    initialTime <- SI.getMonotonicTime
    let runningClock = filterS $ feedback PSQ.empty $ arrM \((), q) -> do
          TChanWrapper chan <- ask
          now <- SI.getMonotonicTime
          case PSQ.minView q of
            Just (_k, wakeupTime, enqueuedEvent, q') -> do
                let wakeupIn = SI.diffTime wakeupTime now
                (readTimeout, cancelTimeout) <- registerDelayCancellable wakeupIn
                let waitEnqueued =
                      readTimeout >>= \case
                        TimeoutPending -> retry
                        _              -> return (Right enqueuedEvent, q')
                    externalRequest = do
                      scheduleEvent <- readTChan chan
                      return (scheduleEvent, q)
                (scheduleEvent, q'') <- atomically (externalRequest <|> waitEnqueued)
                cancelTimeout
                now' <- SI.getMonotonicTime
                case scheduleEvent of
                  Left (wakeupTime', event) ->
                    let q''' = PSQ.insert DummyUniqKey wakeupTime' event q''
                    in return (Nothing, q''')
                  Right event ->
                    return (Just (now', event), q'')
            Nothing -> do
              scheduleEvent <- atomically $ readTChan chan
              case scheduleEvent of
                Left (wakeupTime, event) ->
                  let q' = PSQ.insert DummyUniqKey wakeupTime event q
                  in return (Nothing, q')
                Right event -> return (Just (now, event), q)

    return (runningClock, initialTime)

instance TimeDomain SI.Time where
  type Diff SI.Time = Double

runControlChannel :: (MonadSTM m)
                  => ControlChannel m event a
                  -> m a
runControlChannel a = do
  chan <- atomically newTChan
  runReaderT a (TChanWrapper chan)

cardanoClock :: CardanoClock
cardanoClock = SelectClock GovernorClock selector
  where
    selector (PeerSelectionCardano event) = Just event
    selector _ = Nothing

churnClock :: PeerSelectionClock
churnClock = SelectClock GovernorClock selector
  where
    selector (PeerSelectionEvent Churn) = Just Churn
    selector _ = Nothing

knownPeersClock :: PeerSelectionClock
knownPeersClock = SelectClock GovernorClock selector
  where
    selector (PeerSelectionEvent Known) = Just Known
    selector _ = Nothing

jobArrivedClock :: PeerSelectionClock
jobArrivedClock = SelectClock GovernorClock selector
  where
    selector (PeerSelectionEvent Job) = Just Job
    selector _ = Nothing

tickGovernor :: (MonadSTM m) => Either (SI.Time, event) event -> ControlChannel m event ()
tickGovernor event = do
  TChanWrapper chan <- ask
  atomically $ writeTChan chan event

ticksGovernor :: (MonadSTM m) => [Either (SI.Time, event) event] -> ControlChannel m event ()
ticksGovernor events = do
  TChanWrapper chan <- ask
  atomically $ mapM_ (writeTChan chan) events

tickGovernorS :: (MonadSTM m) => ClSF (ControlChannel m event) cl (Either (SI.Time, event) event) ()
tickGovernorS = arrMCl tickGovernor

ticksGovernorS :: (MonadSTM m) => ClSF (ControlChannel m event) cl [Either (SI.Time, event) event] ()
ticksGovernorS = arrMCl ticksGovernor
