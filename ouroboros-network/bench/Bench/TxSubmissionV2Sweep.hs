{-# LANGUAGE NumericUnderscores #-}

-- | The V2 shared-state sweep and the retention queue at the sizes a relay
-- reaches when the sweep falls behind: a queue of retained transactions, a
-- share of them already past their deadline, with the lookup entry every
-- retained key has in a real state; and the sweep's cost for the active
-- entries and peer references a loaded relay holds.
module Bench.TxSubmissionV2Sweep (benchmarks) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Test.Tasty.Bench

import Ouroboros.Network.TxSubmission.Inbound.V2.Registry (sweepBatch)
import Ouroboros.Network.TxSubmission.Inbound.V2.State (expireSharedStateUpTo,
           sweepSharedState, sweepSharedStateUpTo)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

import Bench.Env (prepareEnv)
import Test.Ouroboros.Network.TxSubmission.TxLogic (swept)
import Test.Ouroboros.Network.TxSubmission.Types ()

now :: Time
now = Time 100_000

-- | @n@ retained entries one millisecond apart, the first @e@ of them past
-- their deadline at 'now'.
retained :: Int -> Int -> [(Int, Word64)]
retained n e =
  [ (k, retainDeadline (addTime (fromIntegral (k - e + 1) * (0.001 :: DiffTime)) now))
  | k <- [0 .. n - 1] ]

-- | The retained entries with their lookup entries, as interning leaves
-- them: txid and key coincide.
retainedState :: Int -> Int -> SharedTxState Int Int
retainedState n e =
  emptySharedTxState
    { sharedRetainedTxs = retainedFromList (retained n e)
    , sharedTxIdToKey   = Map.fromList [ (k, TxKey k) | k <- [0 .. n - 1] ]
    , sharedKeyToTxId   = IntMap.fromList [ (k, k) | k <- [0 .. n - 1] ]
    , sharedNextTxKey   = n
    }

-- | 'retainedState' with @a@ active entries above the retained keys, half
-- of them attempted, each advertised by a peer, as advertised txids are:
-- what the walk examines.
activeState :: Int -> Int -> SharedTxState Int Int
activeState n a = activeStateExpired n a 0

-- | 'activeState' with the first @e@ retained entries past their deadline.
activeStateExpired :: Int -> Int -> Int -> SharedTxState Int Int
activeStateExpired n a e =
  (retainedState n e)
    { sharedTxTable    = IntMap.fromList [ (k, entry k) | k <- active ]
    , sharedTxIdToKey  = Map.fromList [ (k, TxKey k) | k <- [0 .. n + a - 1] ]
    , sharedKeyToTxId  = IntMap.fromList [ (k, k) | k <- [0 .. n + a - 1] ]
    , sharedNextTxKey  = n + a
    }
  where
    active = [n .. n + a - 1]
    entry k = TxEntry {
        txLease = TxClaimable now,
        txAttempt = if even k then 1 else 0,
        txAdvertisers = 1,
        txInSubmission = False,
        currentMaxInflightMultiplicity = 1
      }

-- | One sweep, then expiry-only batches as the counters thread runs them,
-- until none remain.
drain :: SharedTxState Int Int -> SharedTxState Int Int
drain st0 =
  case sweepSharedStateUpTo sweepBatch now sweepStart st0 of
    (st, True, _)  -> go st
    (st, False, _) -> st
  where
    go st = case expireSharedStateUpTo sweepBatch now st of
              (st', True)  -> go st'
              (st', False) -> st'

benchmarks :: Benchmark
benchmarks =
  bgroup "sweep"
    [ bgroup "insert"
        [ env (prepareEnv (retained n 0)) $ \kts ->
            bench ("retainedFromList/" ++ show n) $ nf retainedFromList kts
        | n <- [10_000, 100_000] ]
    , bgroup "unbounded"
        [ env (prepareEnv (retainedState n e)) $ \st ->
            bench (show n ++ "retained/" ++ show e ++ "expired") $
              whnf (sweepSharedState now) st
        | (n, e) <- sizes ]
    , bgroup "active"
        [ env (prepareEnv (activeState n a)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/0expired") $
              whnf (sweepSharedState now) st
        | (n, a) <- [(100_000, 10_000), (100_000, 50_000)] ]
    , bgroup "bounded"
        [ env (prepareEnv (retainedState n e)) $ \st ->
            bench (show n ++ "retained/" ++ show e ++ "expired/" ++ show sweepBatch) $
              whnf (swept . sweepSharedStateUpTo sweepBatch now sweepStart) st
        | (n, e) <- sizes ]
    , bgroup "walk"
        [ env (prepareEnv (activeState n a)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/0expired/" ++ show sweepBatch) $
              whnf (swept . sweepSharedStateUpTo sweepBatch now sweepStart) st
        | (n, a) <- [(100_000, 10_000), (100_000, 50_000)] ]
    , bgroup "expire"
        [ env (prepareEnv (activeStateExpired n a e)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/" ++ show e ++ "expired/" ++ show sweepBatch) $
              whnf (fst . expireSharedStateUpTo sweepBatch now) st
        | (n, a, e) <- [(100_000, 50_000, 0), (100_000, 50_000, 1_000)] ]
    , bgroup "drain"
        [ env (prepareEnv (retainedState n n)) $ \st ->
            bench (show n ++ "retained/allexpired/" ++ show sweepBatch) $ whnf drain st
        | n <- [100_000, 1_000_000] ]
    ]
  where
    sizes = [(100_000, 0), (100_000, 1_000), (100_000, 10_000), (100_000, 100_000)]
