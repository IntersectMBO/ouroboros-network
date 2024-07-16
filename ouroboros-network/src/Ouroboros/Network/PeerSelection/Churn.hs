{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

#if __GLASGOW_HASKELL__ < 904
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif

-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection.Churn
  ( PeerChurnArgs (..)
  , ChurnCounters (..)
  , peerChurnGovernor
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
import Ouroboros.Network.ConsensusMode (ConsensusMode (..))
import Ouroboros.Network.Diffusion.Policies (churnEstablishConnectionTimeout,
           closeConnectionTimeout, deactivateTimeout)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.PeerMetric
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..))

-- | Tag indicating churning approach
-- There are three syncing methods that networking layer supports, the legacy
-- method with or without bootstrap peers, and the Genesis method that relies
-- on chain skipping optimization courtesy of consensus, which also provides

--
data ChurnRegime = ChurnDefault
                 -- ^ tag to use Praos targets when caught up, or Genesis
                 -- targets when syncing in case that is the consensus mode
                 | ChurnPraosSync
                 -- ^ Praos targets to churn normally when syncing
                 | ChurnBootstrapPraosSync
                 -- ^ Praos targets further reduced to conserve resources
                 -- when syncing

getPeerSelectionTargets :: ConsensusMode -> LedgerStateJudgement -> ConsensusModePeerTargets -> PeerSelectionTargets
getPeerSelectionTargets consensus lsj ConsensusModePeerTargets {
                                        deadlineTargets,
                                        syncTargets } =
  case (consensus, lsj) of
    (GenesisMode, TooOld) -> syncTargets
    _otherwise            -> deadlineTargets

pickChurnRegime :: ConsensusMode -> ChurnMode -> UseBootstrapPeers -> ChurnRegime
pickChurnRegime consensus churn ubp =
  case (churn, ubp, consensus) of
    (ChurnModeNormal, _, _)                     -> ChurnDefault
    (_, _, GenesisMode)                         -> ChurnDefault
    (ChurnModeBulkSync, UseBootstrapPeers _, _) -> ChurnBootstrapPraosSync
    (ChurnModeBulkSync, _, _)                   -> ChurnPraosSync

-- | Facilitates composing updates to various targets via back-to-back pipeline
type ModifyPeerSelectionTargets = PeerSelectionTargets -> PeerSelectionTargets
type CheckPeerSelectionCounters = PeerSelectionCounters -> PeerSelectionTargets -> Bool

data ChurnCounters = ChurnCounter ChurnAction Int

-- | Record of arguments for peer churn governor
--
data PeerChurnArgs m peeraddr = PeerChurnArgs {
  pcaPeerSelectionTracer :: Tracer m (TracePeerSelection peeraddr),
  pcaChurnTracer         :: Tracer m ChurnCounters,
  pcaDeadlineInterval    :: DiffTime,
  pcaBulkInterval        :: DiffTime,
  pcaPeerRequestTimeout  :: DiffTime,
  -- ^ the timeout for outbound governor to find new (thus
  -- cold) peers through peer sharing mechanism.
  pcaMetrics             :: PeerMetrics m peeraddr,
  pcaModeVar             :: StrictTVar m ChurnMode,
  pcaRng                 :: StdGen,
  pcaReadFetchMode       :: STM m FetchMode,
  peerTargets            :: ConsensusModePeerTargets,
  pcaPeerSelectionVar    :: StrictTVar m PeerSelectionTargets,
  pcaReadCounters        :: STM m PeerSelectionCounters,
  pcaReadUseBootstrap    :: STM m UseBootstrapPeers,
  pcaConsensusMode       :: ConsensusMode,
  getLedgerStateCtx      :: LedgerPeersConsensusInterface m,
  getLocalRootHotTarget  :: STM m HotValency }

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
                  => PeerChurnArgs m peeraddr
                  -> m Void
peerChurnGovernor PeerChurnArgs {
                    pcaPeerSelectionTracer = tracer,
                    pcaChurnTracer         = churnTracer,
                    pcaDeadlineInterval    = deadlineChurnInterval,
                    pcaBulkInterval        = bulkChurnInterval,
                    pcaPeerRequestTimeout  = requestPeersTimeout,
                    pcaModeVar             = churnModeVar,
                    pcaRng                 = inRng,
                    pcaReadFetchMode       = getFetchMode,
                    peerTargets,
                    pcaPeerSelectionVar    = peerSelectionVar,
                    pcaReadCounters        = readCounters,
                    pcaReadUseBootstrap    = getUseBootstrapPeers,
                    pcaConsensusMode       = consensusMode,
                    getLedgerStateCtx = LedgerPeersConsensusInterface {
                        lpGetLedgerStateJudgement },
                    getLocalRootHotTarget } = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  atomically $ do
    (churnMode, ledgerStateJudgement, useBootstrapPeers, ltt)
      <- (,,,) <$> updateChurnMode <*> lpGetLedgerStateJudgement <*> getUseBootstrapPeers <*> getLocalRootHotTarget
    let regime  = pickChurnRegime consensusMode churnMode useBootstrapPeers
        targets = getPeerSelectionTargets consensusMode ledgerStateJudgement peerTargets

    modifyTVar peerSelectionVar ( increaseActivePeers regime ltt targets
                                . increaseEstablishedPeers regime ltt targets)

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
      :: ChurnAction
      -- ^ churn actions for tracing
      -> (PeerSelectionCounters -> Int)
      -- ^ counter getter
      -> DiffTime
      -- ^ timeout
      -> (ChurnRegime -> HotValency -> PeerSelectionTargets -> ModifyPeerSelectionTargets)
      -- ^ update counters function
      -> CheckPeerSelectionCounters
      -- ^ check counters
      -> m ()
    updateTargets churnAction getCounter timeoutDelay modifyTargets checkCounters = do
      -- update targets, and return the new targets
      startTime <- getMonotonicTime
      (c, targets) <- atomically $ do
        churnMode <- updateChurnMode
        ltt       <- getLocalRootHotTarget
        lsj       <- lpGetLedgerStateJudgement
        regime <- pickChurnRegime consensusMode churnMode <$> getUseBootstrapPeers
        let targets = getPeerSelectionTargets consensusMode lsj peerTargets

        (,) <$> (getCounter <$> readCounters)
            <*> stateTVar peerSelectionVar ((\a -> (a, a)) . modifyTargets regime ltt targets)

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
    increaseActivePeers :: ChurnRegime
                        -> HotValency
                        -> PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    increaseActivePeers regime (HotValency ltt) base targets =
      targets {
        targetNumberOfActivePeers =
          case regime of
            ChurnDefault -> targetNumberOfActivePeers base
            _otherwise   -> min ((max 1 ltt) + 1) (targetNumberOfActivePeers base) }

    checkActivePeersIncreased :: CheckPeerSelectionCounters
    checkActivePeersIncreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
      numberOfActivePeers >= targetNumberOfActivePeers

    decreaseActivePeers :: ChurnRegime
                        -> HotValency
                        -> PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    decreaseActivePeers regime (HotValency ltt) base targets =
      targets {
        targetNumberOfActivePeers =
          case regime of
            ChurnDefault -> decrease $ targetNumberOfActivePeers base
            _otherwise   -> min (max 1 ltt) (targetNumberOfActivePeers base - 1) }

    checkActivePeersDecreased :: CheckPeerSelectionCounters
    checkActivePeersDecreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
         numberOfActivePeers
      <= targetNumberOfActivePeers

    increaseEstablishedPeers :: ChurnRegime
                             -> HotValency
                             -> PeerSelectionTargets
                             -> ModifyPeerSelectionTargets
    increaseEstablishedPeers regime _ base targets =
      targets {
        targetNumberOfEstablishedPeers =
          case regime of
            ChurnBootstrapPraosSync -> min (targetNumberOfActivePeers targets + 1)
                                           (targetNumberOfEstablishedPeers base)
            _otherwise -> targetNumberOfEstablishedPeers base }

    checkEstablishedPeersIncreased :: CheckPeerSelectionCounters
    checkEstablishedPeersIncreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      >= targetNumberOfEstablishedPeers

    increaseEstablishedBigLedgerPeers
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseEstablishedBigLedgerPeers _ _ base
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
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseEstablishedPeers regime _ base targets =
      targets {
        targetNumberOfEstablishedPeers =
          case regime of
            ChurnBootstrapPraosSync -> min (targetNumberOfActivePeers targets)
                                           (targetNumberOfEstablishedPeers base - 1)
            _otherwise ->   decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                          + targetNumberOfActivePeers base }

    checkEstablishedPeersDecreased
      :: CheckPeerSelectionCounters
    checkEstablishedPeersDecreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      <= targetNumberOfEstablishedPeers

    increaseActiveBigLedgerPeers :: ChurnRegime
                                 -> HotValency
                                 -> PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    increaseActiveBigLedgerPeers regime _ base targets =
      targets {
        targetNumberOfActiveBigLedgerPeers =
          let praosSync = min 1 (targetNumberOfActiveBigLedgerPeers base)
          in case regime of
               ChurnBootstrapPraosSync -> praosSync
               ChurnPraosSync -> praosSync
               ChurnDefault -> targetNumberOfActiveBigLedgerPeers base }

    checkActiveBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters
    checkActiveBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
      numberOfActiveBigLedgerPeers >= targetNumberOfActiveBigLedgerPeers

    decreaseActiveBigLedgerPeers :: ChurnRegime
                                 -> HotValency
                                 -> PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    decreaseActiveBigLedgerPeers regime _ base targets =
      targets {
        targetNumberOfActiveBigLedgerPeers =
          let praosSync = min 1 (targetNumberOfActiveBigLedgerPeers base)
          in case regime of
               ChurnBootstrapPraosSync -> praosSync
               ChurnPraosSync -> praosSync
               ChurnDefault -> decrease $ targetNumberOfActiveBigLedgerPeers base }

    checkActiveBigLedgerPeersDecreased
      :: CheckPeerSelectionCounters
    checkActiveBigLedgerPeersDecreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
         numberOfActiveBigLedgerPeers
      <= targetNumberOfActiveBigLedgerPeers

    decreaseEstablishedBigLedgerPeers :: ChurnRegime
                                      -> HotValency
                                      -> PeerSelectionTargets
                                      -> ModifyPeerSelectionTargets
    decreaseEstablishedBigLedgerPeers _ _ base targets =
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
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownPeers _ _ base targets =
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
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownBigLedgerPeers _ _ targets base =
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
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownPeers _ _ base targets =
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
      :: ChurnRegime
      -> HotValency
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownBigLedgerPeers _ _ base targets =
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

      churnMode <- atomically updateChurnMode
      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peers.
      updateTargets DecreasedActivePeers
                    numberOfActivePeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    decreaseActivePeers
                    checkActivePeersDecreased

      -- Pick new active peers.
      updateTargets IncreasedActivePeers
                    numberOfActivePeers
                    shortTimeout
                    increaseActivePeers
                    checkActivePeersIncreased

      -- Purge the worst active big ledger peers.
      updateTargets DecreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    decreaseActiveBigLedgerPeers
                    (checkActiveBigLedgerPeersDecreased)

      -- Pick new active big ledger peers.
      updateTargets IncreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    shortTimeout
                    increaseActiveBigLedgerPeers
                    checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established peers.
      updateTargets DecreasedEstablishedPeers
                    numberOfEstablishedPeers
                    (1 + closeConnectionTimeout)
                    decreaseEstablishedPeers
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
                    increaseEstablishedPeers
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
      -- todo: is this right?
      case (mode, consensusMode) of
        (FetchModeDeadline, _) -> longDelay rng execTime
        (_, GenesisMode)       -> longDelay rng execTime
        _otherwise             -> shortDelay rng execTime


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

    -- Ideally this would be as low as possible but the governor might be in
    -- the process of promoting/demoting a peer and it will take some time
    -- before it can act on new targets set by churn
    shortTimeout :: DiffTime
    shortTimeout = 60

    -- Replace 20% or at least one peer every churnInterval.
    decrease :: Int -> Int
    decrease v = max 0 $ v  - max 1 (v `div` 5)
