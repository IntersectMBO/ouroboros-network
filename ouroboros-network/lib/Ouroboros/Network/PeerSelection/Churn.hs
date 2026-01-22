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
  ( PeerChurnArgs (..)
  , ChurnCounters (..)
  , ModifyPeerSelectionTargets
  , CheckPeerSelectionCounters
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
import Data.Functor (void, ($>))
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Ouroboros.Network.Diffusion.Policies (churnEstablishConnectionTimeout,
           closeConnectionTimeout, deactivateTimeout)
import Ouroboros.Network.PeerSelection.Governor.Types hiding (targets)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..))

-- | Facilitates composing updates to various targets via back-to-back pipeline
type ModifyPeerSelectionTargets = PeerSelectionTargets -> PeerSelectionTargets
type CheckPeerSelectionCounters extraCounters = PeerSelectionCounters extraCounters -> PeerSelectionTargets -> Bool

data ChurnCounters = ChurnCounter ChurnAction Int
  deriving Show

-- | Record of arguments for peer churn governor
--
data PeerChurnArgs m extraArgs extraDebugState extraFlags extraPeers extraAPI extraCounters extraTrace peeraddr = PeerChurnArgs {
  pcaPeerSelectionTracer :: Tracer m (TracePeerSelection extraDebugState extraFlags extraPeers extraTrace peeraddr),
  pcaChurnTracer         :: Tracer m ChurnCounters,
  pcaDeadlineInterval    :: DiffTime,
  pcaBulkInterval        :: DiffTime,
  pcaPeerRequestTimeout  :: DiffTime,
  -- ^ the timeout for outbound governor to find new (thus
  -- cold) peers through peer sharing mechanism.
  pcaRng                 :: StdGen,
  pcaPeerSelectionVar    :: StrictTVar m PeerSelectionTargets,
  pcaReadCounters        :: STM m (PeerSelectionCounters extraCounters),
  getLedgerPeersAPI      :: LedgerPeersConsensusInterface extraAPI m,
  getLocalRootHotTarget  :: STM m HotValency,
  pcaPeerSelectionTargets:: PeerSelectionTargets,
  -- ^ configured peer selection targets
  getExtraArgs           :: extraArgs }

-- | Churn governor.
--
-- At every churn interval decrease active peers for a short while (1s), so that
-- we can pick new ones. Then we churn non-active peers.
--
-- On startup the churn governor gives a head start to local root peers over
-- root peers.
--
peerChurnGovernor :: forall m extraArgs extraDebugState extraFlags extraPeers extraAPI extraCounters extraTrace peeraddr.
                     ( MonadDelay m
                     , Alternative (STM m)
                     , MonadTimer m
                     , MonadCatch m
                     )
                  => PeerChurnArgs m extraArgs extraDebugState extraFlags extraPeers extraAPI extraCounters extraTrace peeraddr
                  -> m Void
peerChurnGovernor
  PeerChurnArgs {
    pcaPeerSelectionTracer  = tracer,
    pcaChurnTracer          = churnTracer,
    pcaBulkInterval         = bulkChurnInterval,
    pcaPeerRequestTimeout   = requestPeersTimeout,
    pcaRng                  = inRng,
    pcaPeerSelectionVar     = peerSelectionVar,
    pcaReadCounters         = readCounters,
    pcaPeerSelectionTargets = peerSelectionTargets
  } = do
    -- Wait a while so that not only the closest peers have had the time
    -- to become warm.
    startTs0 <- getMonotonicTime

    -- Set initial targets.
    atomically $ do
      -- Give a head start to big ledger & local root peers by disabling root
      -- peers.
      let peerSelectionTargets0 = peerSelectionTargets { targetNumberOfRootPeers = 0 }
      writeTVar peerSelectionVar peerSelectionTargets0

    threadDelay 3

    atomically $
      writeTVar peerSelectionVar peerSelectionTargets

    endTs0 <- getMonotonicTime
    fuzzyDelay inRng (endTs0 `diffTime` startTs0) >>= churnLoop
  where
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
      -> (PeerSelectionTargets -> ModifyPeerSelectionTargets)
      -- ^ update counters function
      -> CheckPeerSelectionCounters extraCounters
      -- ^ check counters
      -> m Int
    updateTargets churnAction getCounter timeoutDelay modifyTargets checkCounters = do
      -- update targets, and return the new targets
      startTime <- getMonotonicTime
      (c, targets) <- atomically $ do
        targets <- readTVar peerSelectionVar

        (,) <$> (getCounter <$> readCounters)
            <*> stateTVar peerSelectionVar ((\a -> (a, a)) . modifyTargets targets)

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
                             return $ abs r
                           Left c' -> do
                             endTime <- getMonotonicTime
                             cancelTimeout
                             let r = c' - c
                             traceWith tracer (TraceChurnTimeout (endTime `diffTime` startTime) churnAction r)
                             traceWith churnTracer (ChurnCounter churnAction r)
                             return $ abs r
                     )

    --
    -- Functions to modify `PeerSelectionTargets` and check
    -- `PeerSelectionCounters`.
    --

    -- TODO: #3396 revisit the policy for genesis
    increaseActivePeers :: PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    increaseActivePeers base targets =
      targets {
        targetNumberOfActivePeers = targetNumberOfActivePeers base
      }

    checkActivePeersIncreased :: CheckPeerSelectionCounters extraCounters
    checkActivePeersIncreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
      numberOfActivePeers >= targetNumberOfActivePeers

    decreaseActivePeers :: PeerSelectionTargets
                        -> ModifyPeerSelectionTargets
    decreaseActivePeers base targets =
      targets {
        targetNumberOfActivePeers =
          decrease (targetNumberOfActivePeers base)
      }

    checkActivePeersDecreased :: CheckPeerSelectionCounters extraCounters
    checkActivePeersDecreased
      PeerSelectionCounters { numberOfActivePeers }
      PeerSelectionTargets { targetNumberOfActivePeers }
      =
         numberOfActivePeers
      <= targetNumberOfActivePeers

    increaseEstablishedPeers :: PeerSelectionTargets
                             -> ModifyPeerSelectionTargets
    increaseEstablishedPeers base targets =
      targets {
        targetNumberOfEstablishedPeers = targetNumberOfEstablishedPeers base
      }

    checkEstablishedPeersIncreased :: CheckPeerSelectionCounters extraCounters
    checkEstablishedPeersIncreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      >= targetNumberOfEstablishedPeers

    increaseEstablishedBigLedgerPeers
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseEstablishedBigLedgerPeers base targets =
      targets {
        targetNumberOfEstablishedBigLedgerPeers = targetNumberOfEstablishedBigLedgerPeers base
      }

    checkEstablishedBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters extraCounters
    checkEstablishedBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfEstablishedBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfEstablishedBigLedgerPeers }
      =
      numberOfEstablishedBigLedgerPeers >= targetNumberOfEstablishedBigLedgerPeers

    decreaseEstablishedPeers
      :: Int
      -- ^ Decrease established peers with at least this value
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseEstablishedPeers minDecrease base targets =
      targets {
        targetNumberOfEstablishedPeers =
            decreaseWithMin minDecrease
                            (targetNumberOfEstablishedPeers base - targetNumberOfActivePeers base)
          + targetNumberOfActivePeers base
      }

    checkEstablishedPeersDecreased
      :: CheckPeerSelectionCounters extraCounters
    checkEstablishedPeersDecreased
      PeerSelectionCounters { numberOfEstablishedPeers }
      PeerSelectionTargets { targetNumberOfEstablishedPeers }
      =
         numberOfEstablishedPeers
      <= targetNumberOfEstablishedPeers

    increaseActiveBigLedgerPeers :: PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    increaseActiveBigLedgerPeers base targets =
      targets {
        targetNumberOfActiveBigLedgerPeers = targetNumberOfActiveBigLedgerPeers base
      }

    checkActiveBigLedgerPeersIncreased
      :: CheckPeerSelectionCounters extraCounters
    checkActiveBigLedgerPeersIncreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
      numberOfActiveBigLedgerPeers >= targetNumberOfActiveBigLedgerPeers

    decreaseActiveBigLedgerPeers :: PeerSelectionTargets
                                 -> ModifyPeerSelectionTargets
    decreaseActiveBigLedgerPeers base targets =
      targets {
        targetNumberOfActiveBigLedgerPeers =
          decrease $ targetNumberOfActiveBigLedgerPeers base
      }

    checkActiveBigLedgerPeersDecreased
      :: CheckPeerSelectionCounters extraCounters
    checkActiveBigLedgerPeersDecreased
      PeerSelectionCounters { numberOfActiveBigLedgerPeers }
      PeerSelectionTargets { targetNumberOfActiveBigLedgerPeers }
      =
         numberOfActiveBigLedgerPeers
      <= targetNumberOfActiveBigLedgerPeers

    decreaseEstablishedBigLedgerPeers :: Int
                                      -> PeerSelectionTargets
                                      -> ModifyPeerSelectionTargets
    decreaseEstablishedBigLedgerPeers minDecrease base targets =
      targets {
        targetNumberOfEstablishedBigLedgerPeers =
          decreaseWithMin minDecrease
                          (targetNumberOfEstablishedBigLedgerPeers base -
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
      :: Int
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownPeers minDecrease base targets =
      targets {
          -- we clamp from above to not accidentally actually increase
          -- the number of root peers
          targetNumberOfRootPeers = min (targetNumberOfRootPeers base) $
            decrease (targetNumberOfRootPeers base - targetNumberOfEstablishedPeers base)
            + targetNumberOfEstablishedPeers base
        , targetNumberOfKnownPeers =
            decreaseWithMin minDecrease
                            (targetNumberOfKnownPeers base - targetNumberOfEstablishedPeers base)
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
      :: Int
      -> PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    decreaseKnownBigLedgerPeers minDecrease base targets =
      targets {
        targetNumberOfKnownBigLedgerPeers =
          decreaseWithMin minDecrease
                          (targetNumberOfKnownBigLedgerPeers base -
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownPeers base targets =
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
      :: PeerSelectionTargets
      -> ModifyPeerSelectionTargets
    increaseKnownBigLedgerPeers base targets =
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

      -- Purge the worst active big ledger peers.
      activeBigLedgerDecreased <- updateTargets DecreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    deactivateTimeout
                    decreaseActiveBigLedgerPeers
                    checkActiveBigLedgerPeersDecreased

      -- Pick new active big ledger peers.
      void $ updateTargets IncreasedActiveBigLedgerPeers
                    numberOfActiveBigLedgerPeers
                    shortTimeout
                    increaseActiveBigLedgerPeers
                    checkActiveBigLedgerPeersIncreased

      -- Forget the worst performing established big ledger peers.
      establishedBigLedgerDecreased <- updateTargets DecreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    (1 + closeConnectionTimeout)
                    (decreaseEstablishedBigLedgerPeers activeBigLedgerDecreased)
                    checkEstablishedBigLedgerPeersDecreased

      -- Forget the worst performing known big ledger peers.
      void $ updateTargets DecreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    shortTimeout
                    (decreaseKnownBigLedgerPeers establishedBigLedgerDecreased)
                    checkKnownBigLedgerPeersDecreased

      -- Pick new known big ledger peers
      void $ updateTargets IncreasedKnownBigLedgerPeers
                    numberOfKnownBigLedgerPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    increaseKnownBigLedgerPeers
                    checkKnownBigLedgerPeersIncreased

      -- Pick new non-active big ledger peers
      void $ updateTargets IncreasedEstablishedBigLedgerPeers
                    numberOfEstablishedBigLedgerPeers
                    churnEstablishConnectionTimeout
                    increaseEstablishedBigLedgerPeers
                    checkEstablishedBigLedgerPeersIncreased

      -- Purge the worst active peers.
      activePeersDecreased <- updateTargets DecreasedActivePeers
                    numberOfActivePeers
                    deactivateTimeout
                    decreaseActivePeers
                    checkActivePeersDecreased

      -- Pick new active peers.
      void $ updateTargets IncreasedActivePeers
                    numberOfActivePeers
                    shortTimeout
                    increaseActivePeers
                    checkActivePeersIncreased

      -- Forget the worst performing established peers.
      establishedPeersDecreased <- updateTargets DecreasedEstablishedPeers
                    numberOfEstablishedPeers
                    (1 + closeConnectionTimeout)
                    (decreaseEstablishedPeers activePeersDecreased)
                    checkEstablishedPeersDecreased

      -- Forget the worst performing known peers (root peers, ledger peers)
      void $ updateTargets DecreasedKnownPeers
                    numberOfKnownPeers
                    shortTimeout
                    (decreaseKnownPeers establishedPeersDecreased)
                    checkKnownPeersDecreased

      -- Pick new known peers
      void $ updateTargets IncreasedKnownPeers
                    numberOfKnownPeers
                    (2 * requestPeersTimeout + shortTimeout)
                    increaseKnownPeers
                    checkKnownPeersIncreased

      -- Pick new non-active peers
      void $ updateTargets IncreasedEstablishedPeers
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
    fuzzyDelay rng execTime = shortDelay rng execTime

    fuzzyDelay' :: DiffTime -> Double -> StdGen -> DiffTime -> m StdGen
    fuzzyDelay' baseDelay maxFuzz rng execTime = do
      let (fuzz, rng') = randomR (0, maxFuzz) rng
          delay = realToFrac fuzz + baseDelay - execTime
      traceWith tracer $ TraceChurnWait delay
      threadDelay delay
      return rng'

    shortDelay :: StdGen -> DiffTime -> m StdGen
    shortDelay = fuzzyDelay' bulkChurnInterval 60

    -- Ideally this would be as low as possible but the governor might be in
    -- the process of promoting/demoting a peer and it will take some time
    -- before it can act on new targets set by churn
    shortTimeout :: DiffTime
    shortTimeout = 60

    -- Replace 20% or at least one peer every churnInterval.
    decrease :: Int -> Int
    decrease = decreaseWithMin 1

    -- Replace 20% or at least `u` or at least one peer every churnInterval.
    decreaseWithMin :: Int -> Int -> Int
    decreaseWithMin u v =  max 0 $ v - max u (max 1 (v `div` 5))
