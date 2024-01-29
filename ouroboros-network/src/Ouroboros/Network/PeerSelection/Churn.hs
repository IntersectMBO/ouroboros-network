{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn (peerChurnGovernor) where

import           Data.Void (Void)

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer (..), traceWith)
import           System.Random

import           Ouroboros.Network.BlockFetch (FetchMode (..))
import           Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout)
import           Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import           Ouroboros.Network.PeerSelection.PeerMetric


-- | Churn governor.
--
-- At every churn interval decrease active peers for a short while (1s), so that
-- we can pick new ones. Then we churn non-active peers.
--
-- On startup the churn governor gives a head start to local root peers over
-- root peers.
--
peerChurnGovernor :: forall m peeraddr.
                     ( MonadSTM m
                     , MonadDelay m
                     )
                  => Tracer m (TracePeerSelection peeraddr)
                  -> DiffTime
                  -- ^ the base for churn interval in the deadline mode.
                  -> DiffTime
                  -- ^ the base for churn interval in the bulk sync mode.
                  -> DiffTime
                  -- ^ the max peersharing timeout
                  -> PeerMetrics m peeraddr
                  -> StrictTVar m ChurnMode
                  -> StdGen
                  -> STM m FetchMode
                  -> PeerSelectionTargets
                  -> StrictTVar m PeerSelectionTargets
                  -> m Void
peerChurnGovernor tracer deadlineChurnInterval bulkChurnInterval psOverallTimeout
                  _metrics churnModeVar inRng getFetchMode base peerSelectionVar = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  mode <- atomically updateChurnMode
  atomically $ increaseActivePeers mode
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
    increaseActivePeers mode =  do
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   ChurnModeNormal  ->
                       targetNumberOfActivePeers base
                   ChurnModeBulkSync ->
                       min 2 (targetNumberOfActivePeers base)
        })

    decreaseActivePeers :: ChurnMode -> STM m ()
    decreaseActivePeers mode =  do
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              case mode of
                   ChurnModeNormal ->
                       decrease $ targetNumberOfActivePeers base
                   ChurnModeBulkSync ->
                       min 1 (targetNumberOfActivePeers base - 1)
        })

    increaseActiveBigLedgerPeers :: ChurnMode -> STM m ()
    increaseActiveBigLedgerPeers mode =  do
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
    decreaseActiveBigLedgerPeers mode =  do
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              case mode of
                   ChurnModeNormal ->
                       decrease $ targetNumberOfActiveBigLedgerPeers base
                   ChurnModeBulkSync ->
                       min 1 (targetNumberOfActiveBigLedgerPeers base)
        })


    go :: StdGen -> m Void
    go !rng = do
      startTs <- getMonotonicTime

      churnMode <- atomically updateChurnMode
      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peer(s).
      atomically $ decreaseActivePeers churnMode

      -- Short delay, we may have no active peers right now
      threadDelay 1

      atomically $ do
        -- Pick new active peer(s).
        increaseActivePeers churnMode

        -- Purge the worst active big ledger peer(s).
        decreaseActiveBigLedgerPeers churnMode

      -- Short delay, we may have no active big ledger peers right now
      threadDelay 1

      -- Pick new active peer(s).
      atomically $ increaseActiveBigLedgerPeers churnMode

      -- Give the promotion process time to start
      threadDelay 1

      -- Forget the worst performing non-active peers.
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfRootPeers =
            decrease (targetNumberOfRootPeers base - targetNumberOfEstablishedPeers base)
            + targetNumberOfEstablishedPeers base
        , targetNumberOfKnownPeers =
            decrease (targetNumberOfKnownPeers base - targetNumberOfEstablishedPeers base)
            + targetNumberOfEstablishedPeers base
        , targetNumberOfEstablishedPeers =
            decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
            + targetNumberOfActivePeers base
        , targetNumberOfKnownBigLedgerPeers =
            decrease (targetNumberOfKnownBigLedgerPeers base -
                      targetNumberOfEstablishedBigLedgerPeers base)
            + targetNumberOfEstablishedBigLedgerPeers base
        , targetNumberOfEstablishedBigLedgerPeers =
            decrease (targetNumberOfEstablishedBigLedgerPeers base -
                      targetNumberOfActiveBigLedgerPeers base)
            + targetNumberOfActiveBigLedgerPeers base
        })

      -- Give the governor time to properly demote them.
      threadDelay $ 1 + closeConnectionTimeout

      -- Pick new known peers
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfRootPeers = targetNumberOfRootPeers base
        , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
        , targetNumberOfKnownBigLedgerPeers = targetNumberOfKnownBigLedgerPeers base
        })

      -- Give the governor time to find some new peers
      threadDelay $ 1 + psOverallTimeout

      -- Pick new non-active peers
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers = targetNumberOfEstablishedPeers base
        , targetNumberOfEstablishedBigLedgerPeers = targetNumberOfEstablishedBigLedgerPeers base
        })
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



