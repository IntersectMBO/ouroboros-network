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

-- | Tag indicating churning approach
-- There are three syncing methods that networking layer supports, the legacy
-- method with or without bootstrap peers, and the Genesis method that relies
-- on chain skipping optimization courtesy of consensus, which also provides
-- probabilistic ledger guarantees.
--
data ChurnRegime = ChurnDefault
                 -- ^ tag to use Praos targets when caught up, or Genesis
                 -- targets when syncing in case that is the consensus mode
                 | ChurnPraosSync
                 -- ^ Praos targets to churn normally when syncing
                 | ChurnBootstrapPraosSync
                 -- ^ Praos targets further reduced to conserve resources
                 -- when syncing

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
  pcaChurnMutex          :: StrictTMVar m (),
  pcaConsensusMode       :: ConsensusMode }

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
                    pcaChurnTracer = churnTracer,
                    pcaDeadlineInterval = deadlineChurnInterval,
                    pcaBulkInterval = bulkChurnInterval,
                    pcaPeerRequestTimeout = requestPeersTimeout,
                    pcaModeVar = churnModeVar,
                    pcaRng = inRng,
                    pcaReadFetchMode = getFetchMode,
                    peerTargets = ConsensusModePeerTargets { praosTargets, genesisSyncTargets },
                    pcaPeerSelectionVar = peerSelectionVar,
                    pcaReadCounters = readCounters,
                    pcaReadUseBootstrap = getUseBootstrapPeers,
                    pcaChurnMutex = churnMutex,
                    pcaConsensusMode = consensusMode } = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  (churnMode, regime) <- atomically $ do
    churnMode <- updateChurnMode
    useBootstrapPeers <- getUseBootstrapPeers
    return (churnMode, pickChurnRegime consensusMode churnMode useBootstrapPeers)

  let base =
        case (consensusMode, churnMode) of
          (GenesisMode, ChurnModeBulkSync) -> genesisSyncTargets
          (GenesisMode, ChurnModeNormal)   -> praosTargets
          (PraosMode, _)                   -> praosTargets

  atomically $ do
    modifyTVar peerSelectionVar ( increaseActivePeers regime base
                                . increaseEstablishedPeers regime base )
    putTMVar churnMutex ()

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
    increaseActivePeers :: ChurnRegime
                        -> PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    increaseActivePeers regime base targets =
      targets {
        targetNumberOfActivePeers =
          case regime of
            ChurnDefault -> targetNumberOfActivePeers base
            _otherwise   -> min 2 (targetNumberOfActivePeers base) }

    checkActivePeersIncreased :: CheckPeerSelectionCounters
    checkActivePeersIncreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
      numberOfActivePeers >= targetNumberOfActivePeers

    decreaseActivePeers :: ChurnRegime
                        -> PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    decreaseActivePeers regime base targets =
      targets {
        targetNumberOfActivePeers =
          case regime of
            ChurnDefault -> decrease $ targetNumberOfActivePeers base
            _otherwise   -> min 1 (targetNumberOfActivePeers base - 1) }

    checkActivePeersDecreased :: CheckPeerSelectionCounters
    checkActivePeersDecreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
         numberOfActivePeers
      <= targetNumberOfActivePeers

    increaseEstablishedPeers :: ChurnRegime
                             -> PeerSelectionTargets
                             -> ModifyPeerSelectionTargets
    increaseEstablishedPeers regime base targets =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseEstablishedBigLedgerPeers base
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
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseEstablishedPeers regime base targets =
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
                                 -> PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    increaseActiveBigLedgerPeers regime base targets =
      targets {
        -- TODO: when chain-skipping will be implemented and chain-sync client
        -- will take into account big ledger peers, we don't need pattern
        -- match on the churn mode, but use
        -- `targetNumberOfActiveBigLedgerPeers` (issue #4609).
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
                                 -> PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    decreaseActiveBigLedgerPeers regime base targets =
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

    decreaseEstablishedBigLedgerPeers :: PeerSelectionTargets
                                      -> ModifyPeerSelectionTargets
    decreaseEstablishedBigLedgerPeers base targets =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownPeers base targets =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownBigLedgerPeers targets base =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownPeers base targets =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownBigLedgerPeers base targets =
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
    churnLoop !rng = bracket_ (atomically $ takeTMVar churnMutex)
                              (atomically $ putTMVar churnMutex ()) $ do
      startTs <- getMonotonicTime

      (base, churnMode, regime) <- atomically $ do
        churnMode <- updateChurnMode
        useBootstrapPeers <- getUseBootstrapPeers
        let regime = pickChurnRegime consensusMode churnMode useBootstrapPeers
            base =
              case (consensusMode, churnMode) of
                (GenesisMode, ChurnModeBulkSync) -> genesisSyncTargets
                (GenesisMode, ChurnModeNormal)   -> praosTargets
                (PraosMode, _)                   -> praosTargets

        return (base, churnMode, regime)

      traceWith tracer $ TraceChurnMode churnMode

      -- Purge the worst active peers.
      updateTargets DecreasedActivePeers
                    numberOfActivePeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    (decreaseActivePeers regime base)
                    checkActivePeersDecreased

      -- Pick new active peers.
      updateTargets IncreasedActivePeers
                    numberOfActivePeers
                    shortTimeout
                    (increaseActivePeers regime base)
                    checkActivePeersIncreased

      -- Purge the worst active big ledger peers.
      updateTargets DecreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    deactivateTimeout -- chainsync might timeout after 5mins
                    (decreaseActiveBigLedgerPeers regime base)
                    (checkActiveBigLedgerPeersDecreased)

      -- Pick new active big ledger peers.
      updateTargets IncreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    shortTimeout
                    (increaseActiveBigLedgerPeers regime base)
                    checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established peers.
      updateTargets DecreasedEstablishedPeers
                    numberOfEstablishedPeers
                    (1 + closeConnectionTimeout)
                    (decreaseEstablishedPeers regime base)
                    (checkEstablishedPeersDecreased)

      -- Forget the worst performing established big ledger peers.
      updateTargets DecreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    (1 + closeConnectionTimeout)
                    (decreaseEstablishedBigLedgerPeers base)
                    checkEstablishedBigLedgerPeersDecreased

      -- Forget the worst performing known peers (root peers, ledger peers)
      updateTargets DecreasedKnownPeers
                    numberOfKnownPeers
                    shortTimeout
                    (decreaseKnownPeers base)
                    checkKnownPeersDecreased

      -- Pick new known peers
      updateTargets IncreasedKnownPeers
                    numberOfKnownPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    (increaseKnownPeers base)
                    checkKnownPeersIncreased

      -- Forget the worst performing known big ledger peers.
      updateTargets DecreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    shortTimeout
                    (decreaseKnownBigLedgerPeers base)
                    checkKnownBigLedgerPeersDecreased

      -- Pick new known big ledger peers
      updateTargets IncreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    (increaseKnownBigLedgerPeers base)
                    checkKnownBigLedgerPeersIncreased

      -- Pick new non-active peers
      updateTargets IncreasedEstablishedPeers
                    numberOfEstablishedPeers
                    churnEstablishConnectionTimeout
                    (increaseEstablishedPeers regime base)
                    checkEstablishedPeersIncreased

      -- Pick new non-active big ledger peers
      updateTargets IncreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    churnEstablishConnectionTimeout
                    (increaseEstablishedBigLedgerPeers base)
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

    shortTimeout :: DiffTime
    shortTimeout = 1

    -- Replace 20% or at least one peer every churnInterval.
    decrease :: Int -> Int
    decrease v = max 0 $ v  - max 1 (v `div` 5)
