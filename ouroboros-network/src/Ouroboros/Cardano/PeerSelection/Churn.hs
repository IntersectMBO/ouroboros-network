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
module Ouroboros.Cardano.PeerSelection.Churn (peerChurnGovernor) where

import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (traceWith)
import System.Random

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Control.Applicative (Alternative)
import Data.Functor (($>))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Ouroboros.Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Churn.ExtraArguments qualified as Churn
import Ouroboros.Cardano.Network.Types (ChurnMode (..))
import Ouroboros.Network.BlockFetch (FetchMode (..), PraosFetchMode (..))
import Ouroboros.Network.Diffusion.Policies (churnEstablishConnectionTimeout,
           closeConnectionTimeout, deactivateTimeout)
import Ouroboros.Network.PeerSelection.Churn (CheckPeerSelectionCounters,
           ChurnCounters (..), ModifyPeerSelectionTargets, PeerChurnArgs (..))
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
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

getPeerSelectionTargets
  :: ConsensusMode
  -> LedgerStateJudgement
  -> PeerSelectionTargets
  -> PeerSelectionTargets
  -> PeerSelectionTargets
getPeerSelectionTargets consensus lsj deadlineTargets syncTargets =
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

-- | Churn governor.
--
-- At every churn interval decrease active peers for a short while (1s), so that
-- we can pick new ones. Then we churn non-active peers.
--
-- On startup the churn governor gives a head start to local root peers over
-- root peers.
--
peerChurnGovernor
  :: forall m extraState extraFlags extraCounters extraPeers peeraddr.
     ( MonadDelay m
     , Alternative (STM m)
     , MonadTimer m
     , MonadCatch m
     )
  => PeerChurnArgs
      m
      (Churn.ExtraArguments m)
      extraState
      extraFlags
      extraPeers
      (Cardano.LedgerPeersConsensusInterface m)
      extraCounters
      peeraddr
  -> m Void
peerChurnGovernor PeerChurnArgs {
                    pcaPeerSelectionTracer = tracer,
                    pcaChurnTracer         = churnTracer,
                    pcaDeadlineInterval    = deadlineChurnInterval,
                    pcaBulkInterval        = bulkChurnInterval,
                    pcaPeerRequestTimeout  = requestPeersTimeout,
                    pcaRng                 = inRng,
                    pcaPeerSelectionVar    = peerSelectionVar,
                    pcaReadCounters        = readCounters,
                    getLedgerStateCtx = LedgerPeersConsensusInterface {
                      lpExtraAPI = Cardano.LedgerPeersConsensusInterface {
                        Cardano.getLedgerStateJudgement
                      }
                    },
                    getLocalRootHotTarget,
                    getOriginalPeerTargets,
                    getExtraArgs = Churn.ExtraArguments {
                      Churn.modeVar             = churnModeVar,
                      Churn.readFetchMode       = getFetchMode,
                      Churn.readUseBootstrap    = getUseBootstrapPeers,
                      Churn.consensusMode       = consensusMode,
                      Churn.genesisPeerTargets
                    }
                  } = do
  -- Wait a while so that not only the closest peers have had the time
  -- to become warm.
  startTs0 <- getMonotonicTime
  -- TODO: revisit the policy once we have local root peers in the governor.
  -- The intention is to give local root peers give head start and avoid
  -- giving advantage to hostile and quick root peers.
  threadDelay 3
  atomically $ do
    (churnMode, ledgerStateJudgement, useBootstrapPeers, ltt)
      <- (,,,) <$> updateChurnMode <*> getLedgerStateJudgement <*> getUseBootstrapPeers <*> getLocalRootHotTarget
    let regime  = pickChurnRegime consensusMode churnMode useBootstrapPeers
        targets = getPeerSelectionTargets consensusMode ledgerStateJudgement getOriginalPeerTargets genesisPeerTargets

    modifyTVar peerSelectionVar ( increaseActivePeers regime ltt targets
                                . increaseEstablishedPeers regime ltt targets)

  endTs0 <- getMonotonicTime
  fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= churnLoop

  where
    updateChurnMode :: STM m ChurnMode
    updateChurnMode = do
        fm <- getFetchMode
        let mode = case fm of
                     PraosFetchMode FetchModeDeadline -> ChurnModeNormal
                     PraosFetchMode FetchModeBulkSync -> ChurnModeBulkSync
                     FetchModeGenesis                 -> ChurnModeBulkSync
        writeTVar churnModeVar mode
        return mode

    -- | Update the targets to a given value, and block until they are reached.
    -- The time we are blocked is limited by a timeout.
    --
    updateTargets
      :: ChurnAction
      -- ^ churn actions for tracing
      -> (PeerSelectionCounters extraCounters -> Int)
      -- ^ counter getter
      -> DiffTime
      -- ^ timeout
      -> (ChurnRegime -> HotValency -> PeerSelectionTargets -> ModifyPeerSelectionTargets)
      -- ^ update counters function
      -> CheckPeerSelectionCounters extraCounters
      -- ^ check counters
      -> m ()
    updateTargets churnAction getCounter timeoutDelay modifyTargets checkCounters = do
      -- update targets, and return the new targets
      startTime <- getMonotonicTime
      (c, targets) <- atomically $ do
        churnMode <- updateChurnMode
        ltt       <- getLocalRootHotTarget
        lsj       <- getLedgerStateJudgement
        regime    <- pickChurnRegime consensusMode churnMode <$> getUseBootstrapPeers
        let targets = getPeerSelectionTargets consensusMode lsj getOriginalPeerTargets genesisPeerTargets

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

    checkActivePeersIncreased :: CheckPeerSelectionCounters extraCounters
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

    checkActivePeersDecreased :: CheckPeerSelectionCounters extraCounters
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
            -- ^ In this mode, we are only connected to a handful of bootstrap peers.
            -- The original churn strategy was to increase the targets by small
            -- fixed amount (e.g. 1). Therefore, we use
            -- targets to calculate the upper bound, ie. active + 1 here.
            _otherwise -> targetNumberOfEstablishedPeers base }

    checkEstablishedPeersIncreased :: CheckPeerSelectionCounters extraCounters
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
      :: CheckPeerSelectionCounters extraCounters
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
            -- ^ In this mode, we are only connected to a handful of bootstrap peers.
            -- The original churn strategy was to decrease the targets by small
            -- fixed amount (e.g. 1) and then increase it back, and to churn out
            -- all warm peers to speed up the time to find the best performers.
            -- That is why we use the number of active peers in current targets
            -- as the upper bound on the number of established peers during this action.
            _otherwise ->   decrease (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
                          + targetNumberOfActivePeers base }

    checkEstablishedPeersDecreased
      :: CheckPeerSelectionCounters extraCounters
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
          let praosSyncTargets = min 1 (targetNumberOfActiveBigLedgerPeers base)
          in case regime of
               ChurnBootstrapPraosSync -> praosSyncTargets
               ChurnPraosSync -> praosSyncTargets
               ChurnDefault -> targetNumberOfActiveBigLedgerPeers base }

    checkActiveBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters extraCounters
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
          let praosSyncTargets = min 1 (targetNumberOfActiveBigLedgerPeers base)
          in case regime of
               ChurnBootstrapPraosSync -> praosSyncTargets
               ChurnPraosSync -> praosSyncTargets
               ChurnDefault -> decrease $ targetNumberOfActiveBigLedgerPeers base }

    checkActiveBigLedgerPeersDecreased
      :: CheckPeerSelectionCounters extraCounters
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
      :: CheckPeerSelectionCounters extraCounters
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
      :: PeerSelectionCounters extraCounters -> PeerSelectionTargets -> Bool
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
    decreaseKnownBigLedgerPeers _ _ base targets =
      targets {
          targetNumberOfKnownBigLedgerPeers =
            decrease (targetNumberOfKnownBigLedgerPeers base -
                      targetNumberOfEstablishedBigLedgerPeers base)
            + targetNumberOfEstablishedBigLedgerPeers base
        }

    checkKnownBigLedgerPeersDecreased
      :: PeerSelectionCounters extraCounters -> PeerSelectionTargets -> Bool
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
      :: CheckPeerSelectionCounters extraCounters
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
      :: CheckPeerSelectionCounters extraCounters
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

      -- Purge the worst active big ledger peers.
      updateTargets DecreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    deactivateTimeout
                    decreaseActiveBigLedgerPeers
                    checkActiveBigLedgerPeersDecreased

      -- Pick new active big ledger peers.
      updateTargets IncreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    shortTimeout
                    increaseActiveBigLedgerPeers
                    checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established big ledger peers.
      updateTargets DecreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    (1 + closeConnectionTimeout)
                    decreaseEstablishedBigLedgerPeers
                    checkEstablishedBigLedgerPeersDecreased

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

      -- Pick new non-active big ledger peers
      updateTargets IncreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    churnEstablishConnectionTimeout
                    increaseEstablishedBigLedgerPeers
                    checkEstablishedBigLedgerPeersIncreased

      -- Purge the worst active peers.
      updateTargets DecreasedActivePeers
                    numberOfActivePeers
                    deactivateTimeout
                    decreaseActivePeers
                    checkActivePeersDecreased

      -- Pick new active peers.
      updateTargets IncreasedActivePeers
                    numberOfActivePeers
                    shortTimeout
                    increaseActivePeers
                    checkActivePeersIncreased

      -- Forget the worst performing established peers.
      updateTargets DecreasedEstablishedPeers
                    numberOfEstablishedPeers
                    (1 + closeConnectionTimeout)
                    decreaseEstablishedPeers
                    checkEstablishedPeersDecreased

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

      -- Pick new non-active peers
      updateTargets IncreasedEstablishedPeers
                    numberOfEstablishedPeers
                    churnEstablishConnectionTimeout
                    increaseEstablishedPeers
                    checkEstablishedPeersIncreased

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
        (PraosFetchMode FetchModeDeadline, _) -> longDelay rng execTime
        (_, GenesisMode)                      -> longDelay rng execTime
        _otherwise                            -> shortDelay rng execTime

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
