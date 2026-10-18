{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The V2 shared-state sweep and the retention queue at the sizes a relay
-- reaches when the sweep falls behind: a queue of retained transactions, a
-- share of them already past their deadline, with the lookup entry every
-- retained key has in a real state, keyed by a 32-byte hash as a relay's
-- txids are; and the sweep's cost for the active entries a loaded relay
-- holds, abandoned ones among them, and the review of what it doomed.
module Bench.TxSubmissionV2Sweep (benchmarks) where

import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime)
import Data.Bits (shiftR)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as Short
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import System.Random.SplitMix (mkSMGen, nextWord64)
import Test.Tasty.Bench

import Ouroboros.Network.Tx (HasRawTxId (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
           (defaultTxSubmissionConfigV2)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry (doomGrace,
           sweepBatch)
import Ouroboros.Network.TxSubmission.Inbound.V2.State (expireSharedStateUpTo,
           reviewDoomed, sweepSharedState, sweepSharedStateUpTo)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

import Test.Ouroboros.Network.TxSubmission.TxLogic (swept)
import Test.Ouroboros.Network.TxSubmission.Types (unsafeMkTxDecisionPolicy)

-- | The clock the states are built at and swept at.
now :: Time
now = Time 100_000

-- | A clock past 106 days of uptime, where a 'Time', a 'Fixed' of
-- picoseconds, no longer fits a machine word: what the walk's
-- comparisons cost then.
late :: Time
late = Time (200 * 86_400)

-- | A txid as a relay sees one: a 32-byte hash, compared as bytes in the
-- lookup map, which every expiry deletes from by txid.  A key of the
-- machine's own word size would flatter the sweep.
newtype HashTxId = HashTxId ShortByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, NoThunks)

instance HasRawTxId HashTxId where
  type RawTxId HashTxId = ShortByteString
  getRawTxId (HashTxId h) = h

-- | The hash of a key: 32 pseudo-random bytes drawn from the key, so that
-- neighbouring keys' hashes share no prefix, as real hashes do not.
hashOf :: Int -> HashTxId
hashOf k = HashTxId (Short.pack (take 32 (bytes (mkSMGen (fromIntegral k)))))
  where
    bytes g = let (w, g') = nextWord64 g in wordBytes w ++ bytes g'
    wordBytes w = [ fromIntegral (w `shiftR` (8 * i)) :: Word8 | i <- [0 .. 7] ]

-- | @n@ retained entries one millisecond apart, the first @e@ of them past
-- their deadline at the clock.
retained :: Time -> Int -> Int -> [(Int, RetainTime)]
retained at n e =
  [ (k, retainDeadline (addTime (fromIntegral (k - e + 1) * (0.001 :: DiffTime)) at))
  | k <- [0 .. n - 1] ]

-- | The retained entries with their lookup entries, as interning leaves
-- them.
retainedState :: Time -> Int -> Int -> SharedTxState Int HashTxId
retainedState at n e =
  emptySharedTxState
    { sharedRetainedTxs = retainedFromList (retained at n e)
    , sharedTxIdToKey   = Map.fromList [ (getRawTxId (hashOf k), TxKey k) | k <- [0 .. n - 1] ]
    , sharedKeyToTxId   = IntMap.fromList [ (k, hashOf k) | k <- [0 .. n - 1] ]
    , sharedNextTxKey   = n
    }

-- | 'retainedState' with @a@ active entries above the retained keys, half
-- of them attempted, the rest claimable since a grace ago, as abandoned
-- entries are: what the walk examines and dooms.
activeState :: Time -> Int -> Int -> SharedTxState Int HashTxId
activeState at n a = activeStateExpired at n a 0

-- | 'activeState' with the first @e@ retained entries past their deadline.
activeStateExpired :: Time -> Int -> Int -> Int -> SharedTxState Int HashTxId
activeStateExpired at n a e =
  (retainedState at n e)
    { sharedTxTable    = IntMap.fromList [ (k, entry k) | k <- active ]
    , sharedTxIdToKey  = Map.fromList [ (getRawTxId (hashOf k), TxKey k) | k <- [0 .. n + a - 1] ]
    , sharedKeyToTxId  = IntMap.fromList [ (k, hashOf k) | k <- [0 .. n + a - 1] ]
    , sharedNextTxKey  = n + a
    }
  where
    active = [n .. n + a - 1]
    entry k = TxEntry {
        txLease = TxClaimable (if even k then at else abandonedSince at),
        txAttempt = if even k then 1 else 0,
        txDoom = Undoomed,
        txInSubmission = False,
        currentMaxInflightMultiplicity = 1
      }

-- | 'activeState' after a sweep doomed @d@ of its abandoned entries, the
-- lowest keys first; the keys it doomed, with every fourth of them
-- advertised by a live peer, as the review has them.
doomedState :: Time -> Int -> Int -> Int -> (SharedTxState Int HashTxId, IntSet, IntSet)
doomedState at n a d = (st { sharedTxTable = table }, doomed, held)
  where
    st     = activeState at n a
    doomed = IntSet.fromList (take d [ k | k <- [n .. n + a - 1], odd k ])
    held   = IntSet.filter (\k -> k `mod` 4 == 1) doomed
    table  = IntSet.foldl' (\tbl k -> IntMap.adjust (\e -> e { txDoom = Doomed }) k tbl)
                           (sharedTxTable st) doomed

-- | The grace before a claimable entry is doomed, as the counters thread
-- has it under the default policy.
grace :: DiffTime
grace = doomGrace (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)

-- | When an entry claimable for exactly the grace at the clock became so.
abandonedSince :: Time -> Time
abandonedSince = addTime (negate grace)

-- | One sweep, then expiry-only batches as the counters thread runs them,
-- until none remain.
drain :: SharedTxState Int HashTxId -> SharedTxState Int HashTxId
drain st0 =
  case sweepSharedStateUpTo sweepBatch grace now sweepStart st0 of
    (st, True, _, _)  -> go st
    (st, False, _, _) -> st
  where
    go st = case expireSharedStateUpTo sweepBatch now st of
              (st', True)  -> go st'
              (st', False) -> st'

benchmarks :: Benchmark
benchmarks =
  bgroup "sweep"
    [ bgroup "insert"
        [ env (pure (retained now n 0)) $ \kts ->
            bench ("retainedFromList/" ++ show n) $ nf retainedFromList kts
        | n <- [10_000, 100_000] ]
    , bgroup "unbounded"
        [ env (pure (retainedState now n e)) $ \st ->
            bench (show n ++ "retained/" ++ show e ++ "expired") $
              whnf (fst . sweepSharedState grace now) st
        | (n, e) <- sizes ]
    , bgroup "active"
        [ env (pure (activeState now n a)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/0expired") $
              whnf (fst . sweepSharedState grace now) st
        | (n, a) <- [(100_000, 10_000), (100_000, 50_000)] ]
    , bgroup "bounded"
        [ env (pure (retainedState now n e)) $ \st ->
            bench (show n ++ "retained/" ++ show e ++ "expired/" ++ show sweepBatch) $
              whnf (swept . sweepSharedStateUpTo sweepBatch grace now sweepStart) st
        | (n, e) <- sizes ]
    , bgroup "walk"
        [ env (pure (activeState now n a)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/0expired/" ++ show sweepBatch) $
              whnf (swept . sweepSharedStateUpTo sweepBatch grace now sweepStart) st
        | (n, a) <- [(100_000, 10_000), (100_000, 50_000)] ]
    , bgroup "walk/day200"
        [ env (pure (activeState late n a)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/0expired/" ++ show sweepBatch) $
              whnf (swept . sweepSharedStateUpTo sweepBatch grace late sweepStart) st
        | (n, a) <- [(100_000, 50_000)] ]
    , bgroup "expire"
        [ env (pure (activeStateExpired now n a e)) $ \st ->
            bench (show n ++ "retained/" ++ show a ++ "active/" ++ show e ++ "expired/" ++ show sweepBatch) $
              whnf (fst . expireSharedStateUpTo sweepBatch now) st
        | (n, a, e) <- [(100_000, 50_000, 0), (100_000, 50_000, 1_000)] ]
    , bgroup "review"
        [ env (pure (doomedState now n a d)) $ \ ~(st, doomed, held) ->
            bench (show n ++ "retained/" ++ show a ++ "active/" ++ show d ++ "doomed") $
              whnf (fst . reviewDoomed now held doomed) st
        | (n, a, d) <- [(100_000, 50_000, sweepBatch `div` 2), (100_000, 50_000, sweepBatch)] ]
    , bgroup "drain"
        [ env (pure (retainedState now n n)) $ \st ->
            bench (show n ++ "retained/allexpired/" ++ show sweepBatch) $ whnf drain st
        | n <- [100_000, 1_000_000] ]
    ]
  where
    sizes = [(100_000, 0), (100_000, 1_000), (100_000, 10_000), (100_000, 100_000)]
