{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn (peerChurnGovernor) where

import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)
import System.Random

import Control.Applicative (Alternative)
import Data.Functor (void)
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Ouroboros.Network.BlockFetch (FetchMode (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.PeerMetric


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
                     )
                  => Tracer m (TracePeerSelection peeraddr)
                  -> DiffTime
                  -- ^ the base for churn interval in the deadline mode.
                  -> DiffTime
                  -- ^ the base for churn interval in the bulk sync mode.
                  -> PeerMetrics m peeraddr
                  -> StrictTVar m ChurnMode
                  -> StdGen
                  -> STM m FetchMode
                  -> PeerSelectionTargets
                  -> StrictTVar m PeerSelectionTargets
                  -> StrictTVar m PeerSelectionCounters
                  -> STM m UseBootstrapPeers
                  -> m Void
peerChurnGovernor tracer deadlineChurnInterval bulkChurnInterval
                  _metrics churnModeVar inRng getFetchMode base peerSelectionVar
                  countersVar getUseBootstrapPeers = do
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
    increaseActivePeers mode
    increaseEstablishedPeers mode ubp
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= go

  where

    updateChurnMode :: STM m ChurnMode
    updateChurnMode = do
        fm <- getFetchMode
        let mode = case fm of
                        FetchModeDeadline -> ChurnModeNormal
                        FetchModeBulkSync -> ChurnModeBulkSync
        writeTVar churnModeVar mode
        return mode

    -- TODO: #3396 revisit the policy for genesis
    increaseActivePeers :: ChurnMode -> STM m ()
    increaseActivePeers mode =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   ChurnModeNormal  ->
                       targetNumberOfActivePeers base
                   ChurnModeBulkSync ->
                       min 2 (targetNumberOfActivePeers base)
        })

    decreaseActivePeers :: ChurnMode -> STM m ()
    decreaseActivePeers mode =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   ChurnModeNormal ->
                       decrease $ targetNumberOfActivePeers base
                   ChurnModeBulkSync ->
                       min 1 (targetNumberOfActivePeers base - 1)
        })

    increaseEstablishedPeers :: ChurnMode -> UseBootstrapPeers -> STM m ()
    increaseEstablishedPeers mode ubp =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              case (mode, ubp) of
                   (ChurnModeBulkSync, UseBootstrapPeers _) ->
                       min (targetNumberOfActivePeers targets + 1)
                           (targetNumberOfEstablishedPeers base)
                   _  -> targetNumberOfEstablishedPeers base
        })

    decreaseEstablished :: ChurnMode -> UseBootstrapPeers -> STM m ()
    decreaseEstablished mode ubp =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              case (mode, ubp) of
                   (ChurnModeBulkSync, UseBootstrapPeers _) ->
                       min (targetNumberOfActivePeers targets) (targetNumberOfEstablishedPeers base - 1)
                   _ -> decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                          + targetNumberOfActivePeers base
        })

    increaseActiveBigLedgerPeers :: ChurnMode -> STM m ()
    increaseActiveBigLedgerPeers mode =
        modifyTVar peerSelectionVar (\targets -> targets {
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
        })

    decreaseActiveBigLedgerPeers :: ChurnMode -> STM m ()
    decreaseActiveBigLedgerPeers mode =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              case mode of
                   ChurnModeNormal ->
                       decrease $ targetNumberOfActiveBigLedgerPeers base
                   ChurnModeBulkSync ->
                       min 1 (targetNumberOfActiveBigLedgerPeers base)
        })

    decreaseEstablishedBigLedgerPeers :: STM m ()
    decreaseEstablishedBigLedgerPeers =
        modifyTVar peerSelectionVar (\targets -> targets {
            targetNumberOfEstablishedBigLedgerPeers =
              decrease (targetNumberOfEstablishedBigLedgerPeers base -
                        targetNumberOfActiveBigLedgerPeers base)
              + targetNumberOfActiveBigLedgerPeers base
        })

    modifyPeersBasedOnCounter :: STM m ()
                              -> (PeerSelectionCounters -> a)
                              -> (a -> a -> Bool)
                              -> a
                              -> STM m TimeoutState
                              -> STM m (Maybe (TracePeerSelection peeraddr))
    modifyPeersBasedOnCounter peerOperation counterFunction f target readTimeout = do
      counters <- readTVar countersVar
      let currentCounter = counterFunction counters
      -- Wait for the counters to reach the new targets
      r <- runFirstToFinish $
            FirstToFinish (Just <$> check (f currentCounter target))
            <>
            FirstToFinish (readTimeout >>= (\case TimeoutPending -> retry
                                                  _              -> pure Nothing))
      peerOperation
      return $ case r of
        Just _  -> Nothing
        Nothing -> Just TraceChurnTimeoutFired

    withDelayCancellable stm = do
      (readTimeout, cancelTimeout) <- registerDelayCancellable closeConnectionTimeout
      (targets, mbTr) <- atomically $ do
        mbTr <- stm readTimeout
        t <- readTVar peerSelectionVar
        return (t, mbTr)
      case mbTr of
        Nothing -> cancelTimeout
        Just tr -> traceWith tracer tr
      return targets

    go :: StdGen -> m Void
    go !rng = do
      startTs <- getMonotonicTime

      (churnMode, ubp) <- atomically ((,) <$> updateChurnMode
                                          <*> getUseBootstrapPeers)
      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peer(s).
      targets1 <- atomically $ do
        decreaseActivePeers churnMode
        -- Return current targets
        readTVar peerSelectionVar

      -- Only increase the targets after the governor notices the previous
      -- change
      targets2 <- withDelayCancellable $ do
        let targetActive =
                targetNumberOfActivePeers targets1
              + targetNumberOfActiveBigLedgerPeers targets1

        modifyPeersBasedOnCounter
          (do -- Pick new active peer(s).
              increaseActivePeers churnMode
              -- Purge  worst active big ledger peer(s).
              decreaseActiveBigLedgerPeers churnMode
          )
          hotPeers
          (<=)
          targetActive

      -- Only increase the targets after the governor notices the previous
      -- change
      targets3 <- withDelayCancellable $ do
        let targetActiveBigLedger =
              targetNumberOfActiveBigLedgerPeers targets2

        -- Pick new active peer(s).
        modifyPeersBasedOnCounter
          (increaseActiveBigLedgerPeers churnMode)
          hotBigLedgerPeers
          (<=)
          targetActiveBigLedger

      -- Only decrease the targets after the governor notices the previous
      -- change
      targets4 <- withDelayCancellable $ do
        let targetActiveBigLedger =
              targetNumberOfActiveBigLedgerPeers targets3

        -- Forget the worst performing established peers.
        modifyPeersBasedOnCounter
          (do decreaseEstablished churnMode ubp
              decreaseEstablishedBigLedgerPeers
          )
          hotBigLedgerPeers
          (>=)
          targetActiveBigLedger

      -- Only forget about the worst performing known peers after the governor
      -- has noticed the previous change
      targets5 <- withDelayCancellable $ do
        let targetEstablished =
              targetNumberOfEstablishedPeers targets4
            targetEstablishedBigLedger =
              targetNumberOfEstablishedBigLedgerPeers targets4

        -- Wait for the counters to reach the new targets
        modifyPeersBasedOnCounter
          (modifyTVar peerSelectionVar (\targets -> targets {
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
          }))
          ((,) <$> warmPeers <*> warmBigLedgerPeers)
          (\(wpc, wblpc) (te, tebl) -> wpc <= te && wblpc <= tebl)
          (targetEstablished, targetEstablishedBigLedger)

      -- Only pick new known peers after the governor has noticed the previous change
      targets6 <- withDelayCancellable $ do
        let targetKnown =
              targetNumberOfKnownPeers targets5
            targetKnownBigLedger =
              targetNumberOfKnownBigLedgerPeers targets5

        modifyPeersBasedOnCounter
          (modifyTVar peerSelectionVar (\targets -> targets {
            targetNumberOfRootPeers = targetNumberOfRootPeers base
          , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
          , targetNumberOfKnownBigLedgerPeers = targetNumberOfKnownBigLedgerPeers base
          }))
          (\c ->
            let counterKnown =
                    coldPeers c
                  + warmPeers c
                  + hotPeers c
                  - counterKnownBigLedger

                counterKnownBigLedger =
                    coldBigLedgerPeers c
                  + warmBigLedgerPeers c
                  + hotBigLedgerPeers c
             in (counterKnown, counterKnownBigLedger)
          )
          (\(ck, ckbl) (tk, tkbl) -> ck <= tk && ckbl <= tkbl)
          (targetKnown, targetKnownBigLedger)

      -- Only pick new non-active peers after the governor has noticed the
      -- previous change
      void $ withDelayCancellable $ do
        let targetKnown =
              targetNumberOfKnownPeers targets6
            targetKnownBigLedger =
              targetNumberOfKnownBigLedgerPeers targets6

        modifyPeersBasedOnCounter
          (do increaseEstablishedPeers churnMode ubp
              modifyTVar peerSelectionVar (\targets -> targets {
                targetNumberOfEstablishedBigLedgerPeers = targetNumberOfEstablishedBigLedgerPeers base
              })
          )
          (\c ->
            let counterKnown =
                    coldPeers c
                  + warmPeers c
                  + hotPeers c
                  - counterKnownBigLedger

                counterKnownBigLedger =
                    coldBigLedgerPeers c
                  + warmBigLedgerPeers c
                  + hotBigLedgerPeers c
             in (counterKnown, counterKnownBigLedger)
          )
          (\(ck, ckbl) (tk, tkbl) -> ck >= tk && ckbl >= tkbl)
          (targetKnown, targetKnownBigLedger)

      endTs <- getMonotonicTime

      fuzzyDelay rng (endTs `diffTime` startTs) >>= go

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

    -- Replace 20% or at least one peer every churnInterval.
    decrease :: Int -> Int
    decrease v = max 0 $ v  - max 1 (v `div` 5)



