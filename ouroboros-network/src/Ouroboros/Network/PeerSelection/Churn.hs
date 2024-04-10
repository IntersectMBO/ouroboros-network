{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn (peerChurnGovernor) where

import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)
import System.Random

import Control.Applicative (Alternative)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Ouroboros.Network.BlockFetch (FetchMode (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.PeerMetric


type ModifyPeerSelectionTargets = PeerSelectionTargets -> PeerSelectionTargets
type CheckPeerSelectionCounters = PeerSelectionCounters -> PeerSelectionTargets -> Bool

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
                  -> StrictTVar m PeerSelectionTargets
                  -> StrictTVar m PeerSelectionCounters
                  -> STM m UseBootstrapPeers
                  -> m Void
peerChurnGovernor tracer
                  deadlineChurnInterval bulkChurnInterval requestPeersTimeout
                  _metrics churnModeVar inRng getFetchMode base
                  peerSelectionVar countersVar
                  getUseBootstrapPeers = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  (mode, ubp) <- atomically ((,) <$> updateChurnMode
                                 <*> getUseBootstrapPeers)
  atomically $ do
    modifyTVar peerSelectionVar ( increaseActivePeers mode
                                . increaseEstablishedPeers mode ubp )
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= churnLoop

  where

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
      :: [ChurnAction]
      -- ^ churn actions for tracing
      -> DiffTime
      -- ^ timeout
      -> ModifyPeerSelectionTargets
      -- ^ update counters function
      -> CheckPeerSelectionCounters
      -- ^ check counters
      -> m ()
    updateTargets churnActions timeoutDelay modifyTargets checkCounters = do
      -- update targets, and return the new targets
      targets <- atomically $ stateTVar peerSelectionVar ((\a -> (a, a)) . modifyTargets)

      -- create timeout and block on counters
      bracketOnError (registerDelayCancellable timeoutDelay)
                     (\(_readTimeout, cancelTimeout) -> cancelTimeout)
                     (\( readTimeout, cancelTimeout) -> do
                         -- block until counters reached the targets, or the timeout fires
                         a <- atomically $ runFirstToFinish $
                                FirstToFinish ((readTVar countersVar >>= check . flip checkCounters targets) $> True)
                                <>
                                FirstToFinish (readTimeout >>= \case TimeoutPending -> retry
                                                                     _              -> pure False)
                         if a
                           then cancelTimeout
                             >> traverse_ (traceWith tracer . TraceChurnAction) churnActions
                           else traverse_ (traceWith tracer . TraceChurnTimeout) churnActions
                     )

    --
    -- Functions to modify `PeerSelectionTargets` and check
    -- `PeerSelectionCounters`.
    --

    -- TODO: #3396 revisit the policy for genesis
    increaseActivePeers :: ChurnMode
                        -> ModifyPeerSelectionTargets
    increaseActivePeers
      mode targets
      =
      targets {
        targetNumberOfActivePeers =
          case mode of
            ChurnModeNormal  ->
              targetNumberOfActivePeers base
            ChurnModeBulkSync ->
              min 2 (targetNumberOfActivePeers base)
      }

    checkActivePeersIncreased :: CheckPeerSelectionCounters
    checkActivePeersIncreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
      numberOfActivePeers >= targetNumberOfActivePeers


    decreaseActivePeers :: ChurnMode -> ModifyPeerSelectionTargets
    decreaseActivePeers mode targets =
      targets {
        targetNumberOfActivePeers =
          case mode of
            ChurnModeNormal ->
              decrease $ targetNumberOfActivePeers base
            ChurnModeBulkSync ->
              min 1 (targetNumberOfActivePeers base - 1)
      }

    checkActivePeersDecreased :: CheckPeerSelectionCounters
    checkActivePeersDecreased
      PeerSelectionCounters { numberOfActivePeers, numberOfActivePeersDemotions }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
         numberOfActivePeers
       - numberOfActivePeersDemotions
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
      PeerSelectionCounters { numberOfEstablishedPeers,
                              numberOfColdPeersPromotions }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
       + numberOfColdPeersPromotions
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
      PeerSelectionCounters { numberOfEstablishedPeers,
                              numberOfWarmPeersDemotions }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
       - numberOfWarmPeersDemotions
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
      PeerSelectionCounters { numberOfActiveBigLedgerPeers,
                              numberOfActiveBigLedgerPeersDemotions }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
         numberOfActiveBigLedgerPeers
       - numberOfActiveBigLedgerPeersDemotions
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
      PeerSelectionCounters { numberOfEstablishedBigLedgerPeers,
                              numberOfWarmBigLedgerPeersDemotions }
      PeerSelectionTargets { targetNumberOfEstablishedBigLedgerPeers }
      =
         numberOfEstablishedBigLedgerPeers
       - numberOfWarmBigLedgerPeersDemotions
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
        , targetNumberOfKnownBigLedgerPeers =
            decrease (targetNumberOfKnownBigLedgerPeers base -
                      targetNumberOfEstablishedBigLedgerPeers base)
            + targetNumberOfEstablishedBigLedgerPeers base
        }

    checkKnownPeersDecreased
      :: PeerSelectionCounters -> PeerSelectionTargets -> Bool
    checkKnownPeersDecreased
      PeerSelectionCounters { numberOfKnownPeers,
                              numberOfKnownBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfKnownPeers,
                             targetNumberOfKnownBigLedgerPeers }
      =
        -- note: we are not checking target root peers, since it is a one-sided
        -- target
         numberOfKnownPeers <= targetNumberOfKnownPeers
      && numberOfKnownBigLedgerPeers <= targetNumberOfKnownBigLedgerPeers


    increaseKnownPeers
      :: ModifyPeerSelectionTargets
    increaseKnownPeers targets =
      targets {
          targetNumberOfRootPeers = targetNumberOfRootPeers base
        , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
        , targetNumberOfKnownBigLedgerPeers = targetNumberOfKnownBigLedgerPeers base
        }

    checkKnownPeersIncreased
      :: CheckPeerSelectionCounters
    checkKnownPeersIncreased
      PeerSelectionCounters { numberOfRootPeers,
                              numberOfKnownPeers,
                              numberOfKnownBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfRootPeers,
                             targetNumberOfKnownPeers,
                             targetNumberOfKnownBigLedgerPeers }
      =
         numberOfRootPeers >= targetNumberOfRootPeers
      && numberOfKnownPeers >= targetNumberOfKnownPeers
      && numberOfKnownBigLedgerPeers >= targetNumberOfKnownBigLedgerPeers


    --
    -- Main loop
    --

    churnLoop :: StdGen -> m Void
    churnLoop !rng = do
      startTs <- getMonotonicTime

      (churnMode, ubp) <- atomically ((,) <$> updateChurnMode
                                          <*> getUseBootstrapPeers)
      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peers.
      updateTargets [DecreasedActivePeers]
                    shortTimeout
                    (decreaseActivePeers churnMode) checkActivePeersDecreased

      -- Pick new active peers.
      updateTargets [IncreasedActivePeers]
                    shortTimeout
                    (increaseActivePeers churnMode)
                    checkActivePeersIncreased

      -- Purge the worst active big ledger peers.
      --
      -- Note: we could decrease active big ledger peers, together with
      -- increasing the active peers.  We do that in a separate step to be able
      -- to test that decreasing any targets by churn never timeouts.
      updateTargets [DecreasedActiveBigLedgerPeers]
                    shortTimeout
                    (decreaseActiveBigLedgerPeers churnMode)
                    (checkActiveBigLedgerPeersDecreased)

      -- Pick new active big ledger peers.
      updateTargets [IncreasedActiveBigLedgerPeers]
                    shortTimeout
                    (increaseActiveBigLedgerPeers churnMode)
                     checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established peers.
      updateTargets [DecreasedEstablishedPeers,
                     DecreasedEstablishedBigLedgerPeers]
                    (1 + closeConnectionTimeout)
                    ( decreaseEstablishedPeers churnMode ubp
                    . decreaseEstablishedBigLedgerPeers )
                    (\a b -> checkEstablishedPeersDecreased a b
                          && checkEstablishedBigLedgerPeersDecreased a b)

      -- Forget the worst performing known peers (root peers, ledger peers & big
      -- ledger peers)
      updateTargets [DecreasedKnownPeers]
                    shortTimeout
                    decreaseKnownPeers
                    checkKnownPeersDecreased

      -- Pick new known peers
      updateTargets [IncreasedKnownPeers]
                    requestPeersTimeout
                    increaseKnownPeers
                    checkKnownPeersIncreased

      -- Pick new non-active peers
      updateTargets [IncreasedEstablishedPeers,
                     IncreasedEstablishedBigLedgerPeers]
                    shortTimeout
                    ( increaseEstablishedPeers churnMode ubp
                    . increaseEstablishedBigLedgerPeers )
                    (\a b -> checkEstablishedPeersIncreased a b
                          && checkEstablishedBigLedgerPeersIncreased a b)

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
