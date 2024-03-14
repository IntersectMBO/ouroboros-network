{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn (peerChurnGovernor) where

import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)
import System.Random

import Ouroboros.Network.BlockFetch (FetchMode (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerStateJudgement (..))
import Ouroboros.Network.PeerSelection.PeerMetric

-- | Tag indicating churning approach
-- There are three syncing methods that networking layer supports, the legacy
-- method with or without bootstrap peers, and the Genesis method that relies
-- on chain skipping optimization courtesy of consensus, which also provides
-- probabilistic ledger guarantees.
-- Default - churn peers normally when syncing up in Genesis, or
--  when we are caught up.
-- ChurnLegacySync - mode used when we are behind and we are not using bootstrap
--  peers.ActivityType
-- ChurnBootstrapSync - same as above but with bootstrap peers
--
data ChurnRegime = Default | ChurnLegacySync | ChurnBootstrapSync deriving (Eq, Ord)

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
                  -> TargetsSelector
                  -> StrictTVar m PeerSelectionTargets
                  -> STM m UseBootstrapPeers
                  -> StrictTMVar m LedgerStateJudgement
                  -- ^ Peer selection govnr chooses the ledger state that churn
                  -- govnr should use for setting targets to keep everything in sync
                  -> Bool
                  -> m Void
peerChurnGovernor tracer deadlineChurnInterval bulkChurnInterval psOverallTimeout
                  _metrics churnModeVar inRng getFetchMode targetsSelector peerSelectionVar
                  getUseBootstrapPeers mutexPeerSelection useGenesis = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  (_, regime) <- atomically $ pickChurnRegime <$> updateChurnMode
                                              <*> getUseBootstrapPeers
  -- peer selection governor starts in TooOld state, and unless bootstrap peers
  -- are enabled, waits for initial peer targets set here, therefore to keep
  -- things consistent, TooOld is used here to ensure that the right targets
  -- are selected from the configuration.
  let base = targetsSelector TooOld useGenesis
  -- TODO: verify if #3396 is resolved
  atomically $ do
    increaseActivePeers regime base
    increaseEstablishedPeers regime base

  -- Put back TooOld tag in the mutex to indicate to peer selection that we are done.
  -- only the peer selection governor is responsible for switching this flag
  -- on the basis of ledger state judgement to keep things organized.
  atomically $ putTMVar mutexPeerSelection TooOld
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= go

  where

    pickChurnRegime :: ChurnMode -> UseBootstrapPeers -> (ChurnMode, ChurnRegime)
    pickChurnRegime mode ubp =
      (mode,) if | useGenesis -> Default
                 | mode == ChurnModeBulkSync, UseBootstrapPeers _ <- ubp -> ChurnBootstrapSync
                 | mode == ChurnModeBulkSync -> ChurnLegacySync
                 | otherwise -> Default

    updateChurnMode :: STM m ChurnMode
    updateChurnMode = do
        fm <- getFetchMode
        let mode = case fm of
                        FetchModeDeadline -> ChurnModeNormal
                        FetchModeBulkSync -> ChurnModeBulkSync
        writeTVar churnModeVar mode
        return mode

    increaseActivePeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseActivePeers mode base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              if mode == Default
              then targetNumberOfActivePeers base
              else min 2 (targetNumberOfActivePeers base) })

    decreaseActivePeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseActivePeers mode base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              if mode == Default
              then decrease $ targetNumberOfActivePeers base
              else min 1 (targetNumberOfActivePeers base - 1) })

    increaseEstablishedPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseEstablishedPeers mode base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              if mode == ChurnBootstrapSync
              then min (targetNumberOfActivePeers targets + 1)
                       (targetNumberOfEstablishedPeers base)
              else targetNumberOfEstablishedPeers base })

    decreaseEstablished :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseEstablished mode base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              if mode == ChurnBootstrapSync
              then min (targetNumberOfActivePeers targets)
                       (targetNumberOfEstablishedPeers base - 1)
              else decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                   + targetNumberOfActivePeers base })

    increaseActiveBigLedgerPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseActiveBigLedgerPeers mode base =
      modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              if mode >= ChurnLegacySync
              then min 1 (targetNumberOfActiveBigLedgerPeers base)
              else targetNumberOfActiveBigLedgerPeers base })

    decreaseActiveBigLedgerPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseActiveBigLedgerPeers mode base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              if mode >= ChurnLegacySync
              then min 1 (targetNumberOfActiveBigLedgerPeers base)
              else decrease $ targetNumberOfActiveBigLedgerPeers base })

    decreaseEstablishedBigLedgerPeers :: PeerSelectionTargets -> STM m ()
    decreaseEstablishedBigLedgerPeers base =
        modifyTVar peerSelectionVar (\targets -> targets {
            targetNumberOfEstablishedBigLedgerPeers =
              decrease (targetNumberOfEstablishedBigLedgerPeers base -
                        targetNumberOfActiveBigLedgerPeers base)
              + targetNumberOfActiveBigLedgerPeers base
        })


    go :: StdGen -> m Void
    go !rng = do
      startTs <- getMonotonicTime

      lsj <- atomically $ do
        lsj <- takeTMVar mutexPeerSelection
        -- this is critical to keep targets in sync with peer selection
        -- in case the ledger state judgement has changed in the period
        -- since last churn cycle
        writeTVar peerSelectionVar (targetsSelector lsj useGenesis)
        return lsj

      let base = targetsSelector lsj useGenesis
      (churnMode, regime) <- atomically (pickChurnRegime <$> updateChurnMode
                                                         <*> getUseBootstrapPeers)
      traceWith tracer $ TraceChurnMode churnMode

      atomically $
        -- Purge the worst active peer(s).
        decreaseActivePeers regime base

      -- Short delay, we may have no active peers right now
      threadDelay 1

      atomically $ do
        -- Pick new active peer(s).
        increaseActivePeers regime base

        -- Purge the worst active big ledger peer(s).
        decreaseActiveBigLedgerPeers regime base

      -- Short delay, we may have no active big ledger peers right now
      threadDelay 1

      -- Pick new active peer(s).
      atomically $ increaseActiveBigLedgerPeers regime base

      -- Give the promotion process time to start
      threadDelay 1

      -- Forget the worst performing established peers.
      atomically $ do
        decreaseEstablished regime base
        decreaseEstablishedBigLedgerPeers base

      -- Give the governor time to properly demote them.
      threadDelay $ 1 + closeConnectionTimeout

      -- Forget the worst performing known peers
      atomically $
        modifyTVar peerSelectionVar (\targets -> targets {
            targetNumberOfRootPeers =
              decrease (targetNumberOfRootPeers base - targetNumberOfEstablishedPeers base)
              + targetNumberOfEstablishedPeers base,
            targetNumberOfKnownPeers =
              decrease (targetNumberOfKnownPeers base - targetNumberOfEstablishedPeers base)
              + targetNumberOfEstablishedPeers base,
            targetNumberOfKnownBigLedgerPeers =
              decrease (targetNumberOfKnownBigLedgerPeers base -
                        targetNumberOfEstablishedBigLedgerPeers base)
              + targetNumberOfEstablishedBigLedgerPeers base
          })

      -- Forgetting cold peers should be quick
      threadDelay 1

      -- Pick new known peers
      atomically $ modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfRootPeers = targetNumberOfRootPeers base
        , targetNumberOfKnownPeers = targetNumberOfKnownPeers base
        , targetNumberOfKnownBigLedgerPeers = targetNumberOfKnownBigLedgerPeers base
        })

      -- Give the governor time to find some new peers
      threadDelay $ 1 + psOverallTimeout

      -- Pick new non-active peers
      atomically $ do
        increaseEstablishedPeers regime base
        modifyTVar peerSelectionVar (\targets ->
          targets {
            targetNumberOfEstablishedBigLedgerPeers =
              targetNumberOfEstablishedBigLedgerPeers base })

      atomically $ putTMVar mutexPeerSelection lsj
      endTs <- getMonotonicTime

      fuzzyDelay rng (endTs `diffTime` startTs) >>= go

    -- Randomly delay between churnInterval and churnInterval + maxFuzz seconds.
    fuzzyDelay :: StdGen -> DiffTime -> m StdGen
    fuzzyDelay rng execTime = do
      mode <- atomically getFetchMode
      -- todo: is this right?
      if useGenesis || mode == FetchModeDeadline
      then longDelay rng execTime
      else shortDelay rng execTime

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
