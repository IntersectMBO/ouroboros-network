{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ < 904
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn
  ( peerChurnGovernor
  , ChurnCounters (..)
  ) where

import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)
import System.Random

import Control.Applicative (Alternative)
import Data.Functor (($>))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Ouroboros.Network.BlockFetch (FetchMode (..))
import Ouroboros.Network.Diffusion.Policies (churnEstablishConnectionTimeout,
           closeConnectionTimeout, deactivateTimeout)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.PeerMetric
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..))


type ModifyPeerSelectionTargets = PeerSelectionTargets -> PeerSelectionTargets
type CheckPeerSelectionCounters = PeerSelectionCounters -> PeerSelectionTargets -> Bool

data ChurnCounters = ChurnCounter ChurnAction Int

-- | Churn governor.
--
-- At every churn interval decrease active peers for a short while (1s), so that
-- we can pick new ones. Then we churn non-active peers.
--
-- On startup the churn governor gives a head start to local root peers over
-- root peers.
--
peerChurnGovernor :: forall m peeraddr.
                     ( MonadDelay m
                     , Alternative (STM m)
                     , MonadTimer m
                     , MonadCatch m
                     )
                  => Tracer m (TracePeerSelection peeraddr)
                  -> Tracer m ChurnCounters
                  -> DiffTime
                  -- ^ the base for churn interval in the deadline mode.
                  -> DiffTime
                  -- ^ the base for churn interval in the bulk sync mode.
                  -> DiffTime
                  -- ^ the timeout for outbound governor to find new (thus
                  -- cold) peers through peer sharing mechanism.
                  -> PeerMetrics m peeraddr
                  -> StrictTVar m ChurnMode
                  -> StdGen
                  -> STM m FetchMode
                  -> PeerSelectionTargets
                  -- ^ base targets; set in a configuration file
                  -> StrictTVar m PeerSelectionTargets
                  -> STM m PeerSelectionCounters
                  -> STM m UseBootstrapPeers
                  -> STM m HotValency
                  -> m Void
peerChurnGovernor tracer churnTracer
                  deadlineChurnInterval bulkChurnInterval requestPeersTimeout
                  _metrics churnModeVar inRng getFetchMode base
                  peerSelectionVar readCounters
                  getUseBootstrapPeers getLocalRootHotTarget = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  (mode, ubp, ltt) <- atomically getExtState
  traceWith tracer $ TraceChurnMode mode
  atomically $ do
    modifyTVar peerSelectionVar ( increaseActivePeers mode ltt
                                . increaseEstablishedPeers mode ubp )
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= churnLoop

  where

    getExtState :: STM m (ChurnMode, UseBootstrapPeers, HotValency)
    getExtState = do
        cm <- updateChurnMode
        bp <- getUseBootstrapPeers
        ltt <- getLocalRootHotTarget
        return (cm, bp, ltt)

    updateChurnMode :: STM m ChurnMode
    updateChurnMode = do
        fm <- getFetchMode
        let mode = case fm of
                     FetchModeDeadline -> ChurnModeNormal
                     FetchModeBulkSync -> ChurnModeBulkSync
        writeTVar churnModeVar mode
        return mode

    -- | Update the targets to a given value, and block until they are reached.
    -- The time we are blocked is limited by a timeout.
    --
    updateTargets
      :: ChurnAction
      -- ^ churn actions for tracing
      -> (PeerSelectionCounters -> Int)
      -- ^ counter getter
      -> DiffTime
      -- ^ timeout
      -> ModifyPeerSelectionTargets
      -- ^ update counters function
      -> CheckPeerSelectionCounters
      -- ^ check counters
      -> m ()
    updateTargets churnAction getCounter timeoutDelay modifyTargets checkCounters = do
      -- update targets, and return the new targets
      startTime <- getMonotonicTime
      (c, targets) <- atomically $
        (,) <$> (getCounter <$> readCounters)
            <*> stateTVar peerSelectionVar ((\a -> (a, a)) . modifyTargets)

      -- create timeout and block on counters
      bracketOnError (registerDelayCancellable timeoutDelay)
                     (\(_readTimeout, cancelTimeout) -> cancelTimeout)
                     (\( readTimeout, cancelTimeout) -> do
                         -- block until counters reached the targets, or the timeout fires
                         a <- atomically $ do
                                counters <- readCounters
                                runFirstToFinish $
                                  FirstToFinish (check (checkCounters counters targets) $> (Right $ getCounter counters ))
                                  <>
                                  FirstToFinish (readTimeout >>= \case TimeoutPending -> retry
                                                                       _              -> pure (Left $ getCounter counters))
                         case a of
                           Right c' -> do
                             let r = c' - c
                             endTime <- getMonotonicTime
                             traceWith tracer (TraceChurnAction (endTime `diffTime` startTime) churnAction r)
                             traceWith churnTracer (ChurnCounter churnAction r)
                           Left c' -> do
                             endTime <- getMonotonicTime
                             cancelTimeout
                             let r = c' - c
                             traceWith tracer (TraceChurnTimeout (endTime `diffTime` startTime) churnAction r)
                             traceWith churnTracer (ChurnCounter churnAction r)
                     )

    --
    -- Functions to modify `PeerSelectionTargets` and check
    -- `PeerSelectionCounters`.
    --

    -- TODO: #3396 revisit the policy for genesis
    increaseActivePeers :: ChurnMode
                        -> HotValency
                        -> ModifyPeerSelectionTargets
    increaseActivePeers
      mode (HotValency ltt) targets
      =
      targets {
        targetNumberOfActivePeers =
          case mode of
            ChurnModeNormal  ->
              targetNumberOfActivePeers base
            ChurnModeBulkSync ->
              min ((max 1 ltt) + 1) (targetNumberOfActivePeers base)
      }

    checkActivePeersIncreased :: CheckPeerSelectionCounters
    checkActivePeersIncreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
      numberOfActivePeers >= targetNumberOfActivePeers


    decreaseActivePeers :: ChurnMode
                        -> HotValency
                        -> ModifyPeerSelectionTargets
    decreaseActivePeers mode (HotValency ltt) targets =
      targets {
        targetNumberOfActivePeers =
          case mode of
            ChurnModeNormal ->
              decrease $ targetNumberOfActivePeers base
            ChurnModeBulkSync ->
              min (max 1 ltt) (targetNumberOfActivePeers base - 1)
      }

    checkActivePeersDecreased :: CheckPeerSelectionCounters
    checkActivePeersDecreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
         numberOfActivePeers
      <= targetNumberOfActivePeers


    increaseEstablishedPeers
      :: ChurnMode -> UseBootstrapPeers
      -> ModifyPeerSelectionTargets
    increaseEstablishedPeers
      mode ubp targets
      =
      targets {
        targetNumberOfEstablishedPeers =
          case (mode, ubp) of
            (ChurnModeBulkSync, UseBootstrapPeers _) ->
              min (targetNumberOfActivePeers targets + 1)
                  (targetNumberOfEstablishedPeers base)
            _  -> targetNumberOfEstablishedPeers base
      }

    checkEstablishedPeersIncreased :: CheckPeerSelectionCounters
    checkEstablishedPeersIncreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      >= targetNumberOfEstablishedPeers


    increaseEstablishedBigLedgerPeers
      :: ModifyPeerSelectionTargets
    increaseEstablishedBigLedgerPeers
      targets
      =
      targets { targetNumberOfEstablishedBigLedgerPeers = targetNumberOfEstablishedBigLedgerPeers base }

    checkEstablishedBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters
    checkEstablishedBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfEstablishedBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfEstablishedBigLedgerPeers }
      =
      numberOfEstablishedBigLedgerPeers >= targetNumberOfEstablishedBigLedgerPeers


    decreaseEstablishedPeers
      :: ChurnMode -> UseBootstrapPeers
      -> ModifyPeerSelectionTargets
    decreaseEstablishedPeers mode ubp targets =
      targets {
        targetNumberOfEstablishedPeers =
          case (mode, ubp) of
             (ChurnModeBulkSync, UseBootstrapPeers _) ->
                 min (targetNumberOfActivePeers targets)
                     (targetNumberOfEstablishedPeers base - 1)
             _ -> decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                    + targetNumberOfActivePeers base
      }

    checkEstablishedPeersDecreased
      :: CheckPeerSelectionCounters
    checkEstablishedPeersDecreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      <= targetNumberOfEstablishedPeers


    increaseActiveBigLedgerPeers
      :: ChurnMode -> ModifyPeerSelectionTargets
    increaseActiveBigLedgerPeers mode targets =
      targets {
        -- TODO: when chain-skipping will be implemented and chain-sync client
        -- will take into account big ledger peers, we don't need pattern
        -- match on the churn mode, but use
        -- `targetNumberOfActiveBigLedgerPeers` (issue #4609).
        targetNumberOfActiveBigLedgerPeers =
          case mode of
            ChurnModeNormal ->
              targetNumberOfActiveBigLedgerPeers base
            ChurnModeBulkSync ->
              min 1 (targetNumberOfActiveBigLedgerPeers base)
      }

    checkActiveBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters
    checkActiveBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
      numberOfActiveBigLedgerPeers >= targetNumberOfActiveBigLedgerPeers


    decreaseActiveBigLedgerPeers
      :: ChurnMode
      -> ModifyPeerSelectionTargets
    decreaseActiveBigLedgerPeers mode targets =
      targets {
        targetNumberOfActiveBigLedgerPeers =
          case mode of
            ChurnModeNormal ->
              decrease $ targetNumberOfActiveBigLedgerPeers base
            ChurnModeBulkSync ->
              min 1 (targetNumberOfActiveBigLedgerPeers base)
      }

    checkActiveBigLedgerPeersDecreased
      :: CheckPeerSelectionCounters
    checkActiveBigLedgerPeersDecreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
         numberOfActiveBigLedgerPeers
      <= targetNumberOfActiveBigLedgerPeers


    decreaseEstablishedBigLedgerPeers
      :: ModifyPeerSelectionTargets
    decreaseEstablishedBigLedgerPeers targets =
      targets {
        targetNumberOfEstablishedBigLedgerPeers =
          decrease (targetNumberOfEstablishedBigLedgerPeers base -
                    targetNumberOfActiveBigLedgerPeers base)
          + targetNumberOfActiveBigLedgerPeers base
      }

    checkEstablishedBigLedgerPeersDecreased
      :: CheckPeerSelectionCounters
    checkEstablishedBigLedgerPeersDecreased
      PeerSelectionCounters { numberOfEstablishedBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfEstablishedBigLedgerPeers }
      =
         numberOfEstablishedBigLedgerPeers
      <= targetNumberOfEstablishedBigLedgerPeers


    decreaseKnownPeers
      :: ModifyPeerSelectionTargets
    decreaseKnownPeers targets =
      targets {
          targetNumberOfRootPeers =
            decrease (targetNumberOfRootPeers base - targetNumberOfEstablishedPeers base)
            + targetNumberOfEstablishedPeers base
        , targetNumberOfKnownPeers =
            decrease (targetNumberOfKnownPeers base - targetNumberOfEstablishedPeers base)
            + targetNumberOfEstablishedPeers base
        }

    checkKnownPeersDecreased
      :: PeerSelectionCounters -> PeerSelectionTargets -> Bool
    checkKnownPeersDecreased
      PeerSelectionCounters { numberOfKnownPeers }
      PeerSelectionTargets { targetNumberOfKnownPeers }
      =
        -- note: we are not checking target root peers, since it is a one-sided
        -- target
         numberOfKnownPeers <= targetNumberOfKnownPeers

    decreaseKnownBigLedgerPeers
      :: ModifyPeerSelectionTargets
    decreaseKnownBigLedgerPeers targets =
      targets {
          targetNumberOfKnownBigLedgerPeers =
            decrease (targetNumberOfKnownBigLedgerPeers base -
                      targetNumberOfEstablishedBigLedgerPeers base)
            + targetNumberOfEstablishedBigLedgerPeers base
        }

    checkKnownBigLedgerPeersDecreased
      :: PeerSelectionCounters -> PeerSelectionTargets -> Bool
    checkKnownBigLedgerPeersDecreased
      PeerSelectionCounters { numberOfKnownBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfKnownBigLedgerPeers }
      = numberOfKnownBigLedgerPeers <= targetNumberOfKnownBigLedgerPeers


    increaseKnownPeers
      :: ModifyPeerSelectionTargets
    increaseKnownPeers targets =
      targets {
          targetNumberOfRootPeers = targetNumberOfRootPeers base
        , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
        }

    checkKnownPeersIncreased
      :: CheckPeerSelectionCounters
    checkKnownPeersIncreased
      PeerSelectionCounters { numberOfRootPeers,
                              numberOfKnownPeers }
      PeerSelectionTargets { targetNumberOfRootPeers,
                             targetNumberOfKnownPeers }
      =
         numberOfRootPeers >= targetNumberOfRootPeers
      && numberOfKnownPeers >= targetNumberOfKnownPeers


    increaseKnownBigLedgerPeers
      :: ModifyPeerSelectionTargets
    increaseKnownBigLedgerPeers targets =
      targets {
          targetNumberOfKnownBigLedgerPeers = targetNumberOfKnownBigLedgerPeers base
        }

    checkKnownBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters
    checkKnownBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfKnownBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfKnownBigLedgerPeers }
      =
      numberOfKnownBigLedgerPeers >= targetNumberOfKnownBigLedgerPeers


    --
    -- Main loop
    --

    churnLoop :: StdGen -> m Void
    churnLoop !rng = do
      startTs <- getMonotonicTime

      (churnMode, ubp, ltt) <- atomically getExtState

      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peers.
      updateTargets DecreasedActivePeers
                    numberOfActivePeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    (decreaseActivePeers churnMode ltt)
                    checkActivePeersDecreased

      -- Pick new active peers.
      updateTargets IncreasedActivePeers
                    numberOfActivePeers
                    shortTimeout
                    (increaseActivePeers churnMode ltt)
                    checkActivePeersIncreased

      -- Purge the worst active big ledger peers.
      updateTargets DecreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    (decreaseActiveBigLedgerPeers churnMode)
                    (checkActiveBigLedgerPeersDecreased)

      -- Pick new active big ledger peers.
      updateTargets IncreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    shortTimeout
                    (increaseActiveBigLedgerPeers churnMode)
                    checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established peers.
      updateTargets DecreasedEstablishedPeers
                    numberOfEstablishedPeers
                    (1 + closeConnectionTimeout)
                    (decreaseEstablishedPeers churnMode ubp)
                    (checkEstablishedPeersDecreased)

      -- Forget the worst performing established big ledger peers.
      updateTargets DecreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    (1 + closeConnectionTimeout)
                    decreaseEstablishedBigLedgerPeers
                    checkEstablishedBigLedgerPeersDecreased

      -- Forget the worst performing known peers (root peers, ledger peers)
      updateTargets DecreasedKnownPeers
                    numberOfKnownPeers
                    shortTimeout
                    decreaseKnownPeers
                    checkKnownPeersDecreased

      -- Pick new known peers
      updateTargets IncreasedKnownPeers
                    numberOfKnownPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    increaseKnownPeers
                    checkKnownPeersIncreased

      -- Forget the worst performing known big ledger peers.
      updateTargets DecreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    shortTimeout
                    decreaseKnownBigLedgerPeers
                    checkKnownBigLedgerPeersDecreased

      -- Pick new known big ledger peers
      updateTargets IncreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    increaseKnownBigLedgerPeers
                    checkKnownBigLedgerPeersIncreased

      -- Pick new non-active peers
      updateTargets IncreasedEstablishedPeers
                    numberOfEstablishedPeers
                    churnEstablishConnectionTimeout
                    (increaseEstablishedPeers churnMode ubp)
                    checkEstablishedPeersIncreased

      -- Pick new non-active big ledger peers
      updateTargets IncreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    churnEstablishConnectionTimeout
                    increaseEstablishedBigLedgerPeers
                    checkEstablishedBigLedgerPeersIncreased

      endTs <- getMonotonicTime

      fuzzyDelay rng (endTs `diffTime` startTs) >>= churnLoop


    --
    -- Auxiliary functions and constants
    --


    -- Randomly delay between churnInterval and churnInterval + maxFuzz seconds.
    fuzzyDelay :: StdGen -> DiffTime -> m StdGen
    fuzzyDelay rng execTime = do
      mode <- atomically getFetchMode
      case mode of
           FetchModeDeadline -> longDelay rng execTime
           FetchModeBulkSync -> shortDelay rng execTime


    fuzzyDelay' :: DiffTime -> Double -> StdGen -> DiffTime -> m StdGen
    fuzzyDelay' baseDelay maxFuzz rng execTime = do
      let (fuzz, rng') = randomR (0, maxFuzz) rng
          delay = realToFrac fuzz + baseDelay - execTime
      traceWith tracer $ TraceChurnWait delay
      threadDelay delay
      return rng'


    longDelay :: StdGen -> DiffTime -> m StdGen
    longDelay = fuzzyDelay' deadlineChurnInterval 600


    shortDelay :: StdGen -> DiffTime -> m StdGen
    shortDelay = fuzzyDelay' bulkChurnInterval 60

    shortTimeout :: DiffTime
    shortTimeout = 1

    -- Replace 20% or at least one peer every churnInterval.
    decrease :: Int -> Int
    decrease v = max 0 $ v  - max 1 (v `div` 5)
