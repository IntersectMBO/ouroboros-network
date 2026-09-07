-- | Phase-2 mempool + EB-cache state — the per-node state the multi-round experiment
-- carries across rounds (design.md "The multi-round experiment (Phase 2)";
-- mechanics.md §9).  Two byte-capped stores over a shared tx-id universe, ids in global
-- submission order so a tx's id *is* its age (oldest = smallest id):
--
--   * the __mempool__ (@mpSet@) — the churning, lagged, hard-capped queue a producer
--     forges the *tail* of, oldest-first.  Admission is __blocking__ at the cap (a full
--     mempool refuses new txs; the pull stalls upstream — no drop, no eviction),
--     matching the reviewer's rev-16 §4.2 semantics;
--   * the __EB-cache__ (@mpCache@) — the mechanism's memory (rev-17 §4.4 / feedback §4):
--     fetched closures planted here (the byte half of write-back), byte-limited with a
--     coarse LRU-by-id eviction.  @possession = mpSet ∪ mpCache@ is what a node can vote
--     on and what the promotion walk draws the attested head from.
--
-- The forge order is: any __promoted head__ (@mpHead@, front-first, set by the walk from
-- possession) then the mempool tail by age.  With no promotion @mpHead@ is empty and the
-- order is pure mempool age — the promotion-free baseline.  This split is load-bearing:
-- the byte half (planting → possession) and the ordering half (the walk) are two halves of
-- *one* mechanism, and the alignment ratchet needs both (design.md).
module SmallWorld.Mempool
  ( TxId
  , TxSizes
  , txBytes
  , Mempool(..)
  , emptyMempool
  , emptyMempoolC
  , possession
  , admit
  , plantClosure
  , includeClosure
  , expireBelow
  , expireWhere
  , oldestClosure
  , promoteHead
  , setBytes
  , overlapBytes
  ) where

import           Data.List (foldl')
import qualified Data.IntSet as IS
import qualified Data.Vector as V

type TxId = Int

-- | Byte size of every tx in the universe, indexed by 'TxId'.
type TxSizes = V.Vector Int

txBytes :: TxSizes -> TxId -> Int
txBytes sz i = sz V.! (i `mod` max 1 (V.length sz))
-- ^ sizes are uniform (every entry = the tx size), so an id beyond the admitted stream — the
-- synthetic per-producer fronts of `--diverge` — still resolves to the tx size by wrapping.
-- For a real id (< length) `i `mod` length == i`, so existing behaviour is unchanged.

-- | A node's mempool + EB-cache.  See the module header for the roles of the two stores.
data Mempool = Mempool
  { mpCap        :: !Int        -- mempool byte cap C
  , mpBytes      :: !Int        -- current held mempool bytes
  , mpSet        :: !IS.IntSet  -- mempool membership (churning, capped, lagged) — the forge TAIL
  , mpHead       :: ![TxId]     -- promoted/attested head, in forge order (the walk's output)
  , mpCacheCap   :: !Int        -- EB-cache byte cap B (0 ⇒ no cache — the pre-Phase-2 mode)
  , mpCacheBytes :: !Int        -- current cached bytes
  , mpCache      :: !IS.IntSet  -- planted closures (possession beyond the mempool)
  }

-- | What a node possesses = mempool ∪ EB-cache — the set it can vote on, and the set the
-- promotion walk draws the attested head from.
possession :: Mempool -> IS.IntSet
possession mp = mpSet mp `IS.union` mpCache mp

-- | An empty mempool with the given mempool cap and EB-cache cap.
emptyMempoolC :: Int -> Int -> Mempool
emptyMempoolC cap cacheCap = Mempool cap 0 IS.empty [] cacheCap 0 IS.empty

-- | An empty mempool with __no__ EB-cache (cache cap 0) — the pre-converger baseline
-- (@calibrate@, and @rounds@ without planting).
emptyMempool :: Int -> Mempool
emptyMempool cap = emptyMempoolC cap 0

-- | Blocking admission into the mempool: admit the tx iff it fits under the mempool cap,
-- else leave it unchanged (the pull stalls upstream — no drop, no eviction).
admit :: TxSizes -> TxId -> Mempool -> Mempool
admit sz i mp
  | IS.member i (mpSet mp)     = mp
  | mpBytes mp + b <= mpCap mp = mp { mpBytes = mpBytes mp + b
                                    , mpSet   = IS.insert i (mpSet mp) }
  | otherwise                  = mp
  where b = txBytes sz i

-- | Plant a fetched closure's txs into the EB-cache (the byte half of write-back).  Only
-- txs not already possessed are added; when the cache exceeds its cap, evict smallest-id
-- (oldest — a coarse LRU proxy, id being age) until under.  A no-op when the cache cap is
-- 0.  Planting does not touch the mempool, so it never converges the forge *tail* — only
-- possession (voting) and the head the walk can reconstruct.
plantClosure :: TxSizes -> [TxId] -> Mempool -> Mempool
plantClosure sz refs mp
  | mpCacheCap mp <= 0 = mp
  | otherwise          = evict (foldl' add mp fresh)
  where
    fresh   = [ i | i <- refs, not (IS.member i (mpCache mp)), not (IS.member i (mpSet mp)) ]
    add m i = m { mpCache = IS.insert i (mpCache m), mpCacheBytes = mpCacheBytes m + txBytes sz i }
    evict m
      | mpCacheBytes m <= mpCacheCap m = m
      | IS.null (mpCache m)            = m
      | otherwise = let i = IS.findMin (mpCache m)
                    in evict m { mpCache      = IS.delete i (mpCache m)
                               , mpCacheBytes = mpCacheBytes m - txBytes sz i }

-- | A certified closure's txs go on-chain: remove them from the mempool __and__ the cache
-- (no longer pending) and from the promoted head, freeing the bytes they held.  Also the
-- Praos-floor inline-RB drain, applied to the oldest mempool txs.
includeClosure :: TxSizes -> [TxId] -> Mempool -> Mempool
includeClosure sz ids mp =
  let inMp  = filter (`IS.member` mpSet mp)   ids
      inCa  = filter (`IS.member` mpCache mp) ids
      pset  = IS.fromList ids
  in mp { mpBytes      = mpBytes mp      - setBytes sz inMp
        , mpSet        = foldr IS.delete (mpSet mp) inMp
        , mpCacheBytes = mpCacheBytes mp - setBytes sz inCa
        , mpCache      = foldr IS.delete (mpCache mp) inCa
        , mpHead       = filter (not . (`IS.member` pset)) (mpHead mp) }

-- | TTL expiry: drop every tx older than a submission-id threshold from the mempool, the
-- cache, and the promoted head (ids are global submission order, so @id < thresh@ means
-- "submitted before the cutoff").  Unlike inclusion this is age-driven, not chain-driven —
-- the channel that, under a long failing episode, expires the attested closure out from
-- under the ratchet before it can certify (design.md "TTL churn").  A no-op at @thresh ≤ 0@.
expireBelow :: TxSizes -> TxId -> Mempool -> Mempool
expireBelow sz thresh mp
  | thresh <= 0 = mp
  | otherwise   =
      let (expS, keepS) = IS.partition (< thresh) (mpSet mp)
          (expC, keepC) = IS.partition (< thresh) (mpCache mp)
      in mp { mpBytes      = mpBytes mp      - setBytes sz (IS.toList expS)
            , mpSet        = keepS
            , mpCacheBytes = mpCacheBytes mp - setBytes sz (IS.toList expC)
            , mpCache      = keepC
            , mpHead       = filter (>= thresh) (mpHead mp) }

-- | Per-tx-lifetime expiry: drop every tx satisfying @isExpired@ from the mempool, the cache,
-- and the promoted head.  Generalises 'expireBelow' from a single age cutoff to a per-tx
-- predicate — the measured-TTL-mixture case (@--ttl-mixture@, `mainnet_tx_ttl.md`): each id
-- carries its own sampled lifetime (never / hours / 180–360 s), so expiry is no longer a clean
-- id threshold.  Under the mixture the head __thins but cannot evaporate__ — the non-expiring
-- share anchors it — which is the whole point of replacing the uniform lifetime.
expireWhere :: TxSizes -> (TxId -> Bool) -> Mempool -> Mempool
expireWhere sz isExpired mp =
  let (expS, keepS) = IS.partition isExpired (mpSet mp)
      (expC, keepC) = IS.partition isExpired (mpCache mp)
  in mp { mpBytes      = mpBytes mp      - setBytes sz (IS.toList expS)
        , mpSet        = keepS
        , mpCacheBytes = mpCacheBytes mp - setBytes sz (IS.toList expC)
        , mpCache      = keepC
        , mpHead       = filter (not . isExpired) (mpHead mp) }

-- | Forge the closure up to a byte budget: the promoted head first (in head order, kept
-- refs present in __possession__ — the head may be cache-backed), then the tail by age
-- from the __mempool only__ (the fresh local txs).  Empty head ⇒ pure mempool age order
-- (the promotion-free baseline).
oldestClosure :: TxSizes -> Int -> Mempool -> [TxId]
oldestClosure sz budget mp = go 0 (headTxs ++ restTxs)
  where
    poss    = possession mp
    headTxs = filter (`IS.member` poss) (mpHead mp)
    headSet = IS.fromList headTxs
    restTxs = IS.toAscList (mpSet mp `IS.difference` headSet)
    go _   []     = []
    go acc (i:is)
      | acc >= budget = []
      | otherwise     = i : go (acc + txBytes sz i) is

-- | The promotion walk: move the attested closure's kept references to the front of the
-- forge order, in EB order (reviewer feedback §1).  Refs are kept iff present in
-- __possession__ (mempool ∪ cache) — so a node that fetched the closure (planted it) can
-- reconstruct the *whole* attested head, even the txs its churning mempool never held.  A
-- node that ran the walk forges this head first, so if it is the next producer its
-- oldest-first snapshot carries the attested head.
promoteHead :: [TxId] -> Mempool -> Mempool
promoteHead refs mp = mp { mpHead = filter (`IS.member` possession mp) refs }

-- | Total bytes of a set of txs.
setBytes :: TxSizes -> [TxId] -> Int
setBytes sz = sum . map (txBytes sz)

-- | Byte-weighted overlap: the fraction of @closure@ (by bytes) also present in @held@ —
-- pass @possession mp@ for the vote-side quantity, @mpSet mp@ for the mempool-only one.
overlapBytes :: TxSizes -> [TxId] -> IS.IntSet -> Double
overlapBytes sz closure held
  | tot == 0  = 1
  | otherwise = fromIntegral have / fromIntegral tot
  where
    tot  = setBytes sz closure
    have = setBytes sz (filter (`IS.member` held) closure)
