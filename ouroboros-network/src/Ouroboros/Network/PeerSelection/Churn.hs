{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
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
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerStateJudgement (..))
import Ouroboros.Network.PeerSelection.PeerMetric
import Ouroboros.Network.ConsensusMode

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
data ChurnRegime = ChurnDefault | ChurnPraosSync | ChurnBootstrapPraosSync deriving (Eq, Ord)

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
                  -> TargetsSelector
                  -> StrictTVar m PeerSelectionTargets
                  -> StrictTVar m PeerSelectionCounters
                  -> STM m UseBootstrapPeers
                  -> StrictTMVar m ()
                  -- ^ Peer selection govnr chooses the ledger state that churn
                  -- govnr should use for setting targets to keep everything in sync
                  -> ConsensusMode
                  -> m Void
peerChurnGovernor tracer deadlineChurnInterval bulkChurnInterval
                  _metrics churnModeVar inRng getFetchMode staticTargetsSelector peerSelectionVar
                  countersVar getUseBootstrapPeers churnMutex consensusMode = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  (_, regime) <- atomically $ pickChurnRegime <$> updateChurnMode
                                              <*> getUseBootstrapPeers
  -- peer selection governor starts in TooOld state,
  -- so consistency aside, it also probably
  -- doesn't hurt to assume that at initial startup the node
  -- is behind
  let base = staticTargetsSelector TooOld consensusMode
  -- TODO: verify if #3396 is resolved
  atomically $ do
    increaseActivePeers regime base
    increaseEstablishedPeers regime base

  -- This mutex was created empty in the caller. Here we want to
  -- indicate to peer selection governor that it can now respond
  -- to changes in ledger state judgement to set correct targets
  atomically $ putTMVar churnMutex ()
  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= go

  where

    pickChurnRegime :: ChurnMode -> UseBootstrapPeers -> (ChurnMode, ChurnRegime)
    pickChurnRegime mode ubp =
      let regime =
            case (mode, ubp, consensusMode) of
              (_, _, GenesisMode) -> ChurnDefault
              (ChurnModeBulkSync, UseBootstrapPeers _, _) -> ChurnBootstrapPraosSync
              (ChurnModeBulkSync, _, _) -> ChurnPraosSync
              otherwise -> ChurnDefault
      in (mode, regime)

    -- this is used by staticTargetsSelector and
    -- we only have to distinguish the top case, the other
    -- alternatives should use the non-genesis target set
    -- and so it is NOT a true ledger state judgement
    -- for general use
    -- cf ouroboros-consensus 7509fdb for the relationship
    -- between FetchMode, ChurnMode and LedgerStateJudgement
    -- of why this works
    pseudoLedgerStateJudgement :: ConsensusMode -> ChurnMode -> LedgerStateJudgement
    pseudoLedgerStateJudgement GenesisMode ChurnModeBulkSync = TooOld
    pseudoLedgerStateJudgement _ _ = YoungEnough

    updateChurnMode :: STM m ChurnMode
    updateChurnMode = do
        fm <- getFetchMode
        let mode = case fm of
                        FetchModeDeadline -> ChurnModeNormal
                        FetchModeBulkSync -> ChurnModeBulkSync
        writeTVar churnModeVar mode
        return mode

    increaseActivePeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseActivePeers regime base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              if regime == ChurnDefault
              then targetNumberOfActivePeers base
              else min 2 (targetNumberOfActivePeers base) })

    decreaseActivePeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseActivePeers regime base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActivePeers =
              if regime == ChurnDefault
              then decrease $ targetNumberOfActivePeers base
              else min 1 (targetNumberOfActivePeers base - 1) })

    increaseEstablishedPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseEstablishedPeers regime base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              if regime == ChurnBootstrapPraosSync
              then min (targetNumberOfActivePeers targets + 1)
                       (targetNumberOfEstablishedPeers base)
              else targetNumberOfEstablishedPeers base })

    decreaseEstablished :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseEstablished regime base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfEstablishedPeers =
              if regime == ChurnBootstrapPraosSync
              then min (targetNumberOfActivePeers targets)
                       (targetNumberOfEstablishedPeers base - 1)
              else decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                   + targetNumberOfActivePeers base })

    increaseActiveBigLedgerPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    increaseActiveBigLedgerPeers regime base =
      modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              if regime >= ChurnPraosSync
              then min 1 (targetNumberOfActiveBigLedgerPeers base)
              else targetNumberOfActiveBigLedgerPeers base })

    decreaseActiveBigLedgerPeers :: ChurnRegime -> PeerSelectionTargets -> STM m ()
    decreaseActiveBigLedgerPeers regime base =
        modifyTVar peerSelectionVar (\targets -> targets {
          targetNumberOfActiveBigLedgerPeers =
              if regime >= ChurnPraosSync
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

      (lsj, churnMode, regime) <- atomically $ do
        takeTMVar churnMutex
        (churnMode, regime) <- pickChurnRegime <$> updateChurnMode
                                               <*> getUseBootstrapPeers
        
        let lsj = pseudoLedgerStateJudgement consensusMode churnMode
        return (lsj, churnMode, regime)

      let base = staticTargetsSelector lsj consensusMode

      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peer(s).
      targets1 <- atomically $ do
        decreaseActivePeers regime base
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
              increaseActivePeers regime base
              -- Purge  worst active big ledger peer(s).
              decreaseActiveBigLedgerPeers regime base
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
          (increaseActiveBigLedgerPeers regime base)
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
          (do decreaseEstablished regime base
              decreaseEstablishedBigLedgerPeers base
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
              + targetNumberOfEstablishedPeers base,
            targetNumberOfKnownPeers =
              decrease (targetNumberOfKnownPeers base - targetNumberOfEstablishedPeers base)
              + targetNumberOfEstablishedPeers base,
            targetNumberOfKnownBigLedgerPeers =
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
          (do increaseEstablishedPeers regime base
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

      atomically $ putTMVar churnMutex () 
      endTs <- getMonotonicTime

      fuzzyDelay rng (endTs `diffTime` startTs) >>= go

    -- Randomly delay between churnInterval and churnInterval + maxFuzz seconds.
    fuzzyDelay :: StdGen -> DiffTime -> m StdGen
    fuzzyDelay rng execTime = do
      mode <- atomically getFetchMode
      -- todo: is this right?
      case (mode, consensusMode) of
        (FetchModeDeadline, _) -> longDelay rng execTime
        (_, GenesisMode) -> longDelay rng execTime
        otherwise -> shortDelay rng execTime

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
