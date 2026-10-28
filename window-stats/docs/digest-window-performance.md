# window-stats: space and time behaviour of the sliding windows

Notes from a review of `window-stats` (October 2026): what was measured,
what was changed, and the decisions that are still open. Intended as a
reference for future work on `Data.Window.DigestTimeBatched` in
particular.

## Setup

Unless stated otherwise the numbers below come from small probe programs
compiled with GHC 9.6.7 `-O2` against `fingertree-0.1.6.3` and
`tdigest-0.3.1`, on one developer machine; treat them as indicative
ratios rather than absolute figures.

* `TimedDigestWindow Time 100` (compression 100), 1 s buckets, 60 bucket
  retention.
* "Steady": 1 kHz samples, i.e. ~1000 samples per bucket and one bucket
  sealed (and, once full, one evicted) per second.
* "Bursty": 0.5 s bursts of 500 samples separated by 30 s idle gaps.
* Residency is `max_live_bytes` from `GHC.Stats`; per-operation figures
  are wall time and allocation divided by the number of operations.

## Changes made

### Space leaks fixed elsewhere

| Site | Problem | Before | After |
|---|---|---|---|
| `Fold.{Timed,Count}.windowScan`, `windowFoldRolling*` | lazy `windowMeasure w'` stored in the `Seq` / passed to the inner fold retains every intermediate window | 94.8 MB (200k samples) | 7.4 MB |
| `Timed.fromListN` | folds the entire input before trimming | 363 MB (1M samples) | 0.1 MB, works on infinite input |
| `Timed.insertMany`, `fromFoldable` | lazy pair in `foldl'`; lazy `tmV` builds an n-long `<>` chain | 57 MB (200k) | 34 MB |
| `Count.insert`, `evictOldest`, `fromListN` | forced the `WindowMeasure` constructor (already guaranteed by `Deep`'s strict field) but not `wmV` | 244 MB (filling a 1M window) | 140 MB |

`Timed.insertMany` still holds all `n` samples until its single final
trim, so its memory is `O(w + n)`; its documentation now says so and
recommends folding `insert` when `n >> w`.

### Fast path in `DigestTimeBatched.evictBefore`

`FT.dropUntil p = snd . split p` returns the original tree only when `p`
is false for the whole tree. Its predicate here, `(> cutoff) . bmFinish`,
looks at the *newest* bucket and so is practically always true: `split`
always runs `splitTree` and returns `x <| deepL r m sf`, a rebuilt spine
whose new root digest `!treeDigest` then forces with several digest
merges, even when nothing is evicted.

`evictBefore` now checks the oldest bucket with `FT.viewl` first and
keeps the tree, and its already evaluated root digest, when that bucket
is still within the window.

| `evictBefore` evicting nothing, 60-bucket window | Time | Allocation |
|---|---|---|
| before | 3.58 ms | 13 MB |
| after | ~1 µs | — |

The cache thunk is still rebuilt, so the next query pays one bucket
merge (~0.35 ms). In steady state every seal evicts a bucket, so the
sealing insert is unaffected (inserts into the open bucket never call
`evictBefore`); the fast path helps while the window fills and for
direct `evictBefore` calls, e.g. a timer ageing out data while no
samples arrive.

(`Timed.evictBefore` does not need this: `takeUntil`'s predicate looks
at the *oldest* sample, so "nothing to evict" already takes `split`'s
`otherwise` branch in O(1).)

## Where the time goes in `DigestTimeBatched`

| Steady, per insert | Time | Allocation |
|---|---|---|
| `DigestTimeBatched.insert` | 4.7 µs | 18 KB |
| `TD.insert` on a single digest | 0.6 µs | 2.6 KB |
| `insert` without forcing the root digest at seal | 0.83 µs | 3.3 KB |

These are averages over all inserts, and the cost is not uniform. An
insert into the open bucket never calls `evictBefore`: it does a
`TD.insert` into the open bucket's digest, an O(1) read of the cached root
measure and the allocation of an unevaluated cache thunk, so it costs
about the same as a plain `TD.insert` (~0.6 µs). The extra cost sits in
the single sealing insert per bucket; with ~1000 samples per bucket that
is roughly 999 cheap inserts and one spike of about 4 ms.

That spike is the work done at each seal: `!treeDigest`
forces roughly eight digest merges (the rebuilt digit measures, which
`fingertree` does not cache, plus `pr <> m <> sf` and `x <| r`). With the
tree-based t-digest a merge inserts the smaller digest's centroids one
by one: merging the window root (~1000 centroids) with a bucket digest
(~450–750 centroids) takes ~0.45 ms and allocates ~2 MB. Digests are
this large because `tdigest` only compresses once a digest exceeds
`max(25 * comp, 1000)` centroids.

The per-seal cost is independent of the sample rate but multiplies with
the number of windows (e.g. one window per peer).

## Open decisions

### 1. Eager or lazy root digest after eviction

`evictBefore` currently forces the post-eviction root digest
(`!treeDigest`), so a query costs at most one merge with the open bucket.
The alternative is to leave it lazy and let `windowDigest` force it.

Leaving it lazy does not build up unbounded thunks: each seal's
`|>`-produced root thunk is discarded unevaluated when `dropUntil`
rebuilds the root, and the measure chains that do form inside the tree
only cover buckets still in the window (eviction rebuilds the part of
the spine it cuts through). The deferred work is therefore bounded by
about one full recomputation of the window, and the total work never
exceeds the eager variant's.

Query latency after `k` seals without a query:

| k | eager | lazy |
|---|---|---|
| 1 | 0.34 ms | 2.9 ms |
| 5 | 0.36 ms | 8.6 ms |
| 20 | 0.33 ms | 11.7 ms |
| 60 | 0.35 ms | 16.5 ms |
| 300 | 0.36 ms | 15.6 ms |

Cost per seal when querying once every `q` seals:

| q | 1 | 5 | 20 | 60 |
|---|---|---|---|---|
| eager | ~4 ms | ~4 ms | ~4 ms | ~4 ms |
| lazy | ~2.9 ms | ~1.7 ms | ~0.6 ms | ~0.27 ms |

Options:

* **Eager** (current): predictable ~0.35 ms queries, ~4 ms per seal
  whether or not anyone queries.
* **Lazy**: total work proportional to queries; worst-case query of about
  one window recomputation (~16 ms here).
* **Periodic**: stay lazy but force the root every `N` seals, capping the
  worst case at the `k = N` latency while removing most of the per-seal
  cost.

What decides it is where queries run: a latency spike is harmless in a
tracer or metrics thread, but matters on a hot path or if the digest is
forced inside an STM transaction. Inserts are expected to outnumber
queries, which favours lazy or periodic.

### 2. Compress bucket digests when sealing

Calling `forceCompress` on a bucket's digest at seal reduces steady
insert cost from 4.8 to 3.1 µs/op (and `query every 100 inserts` from
7.5 to 5.6 µs/op). In one check over the last 60 s of a uniform-ish
stream the quantiles were essentially unchanged (exact p50/p99
486.0/967.0; current 484.7/966.8; compressed 488.6/966.8). The root
digest still holds ~1100 centroids because merges only compress past
the size threshold. It helps both the eager and the lazy variant.

Not adopted for now: it needs an accuracy assessment across input
distributions and compression parameters first.

### 3. `Timed` eviction: `split` versus popping the oldest

| 1 s window, per insert | `split` (current) | pop via `viewr` |
|---|---|---|
| steady (one eviction per insert) | 1716 ns, 3.7 KB | 1120 ns, 2.4 KB |
| bursty (mass eviction after gaps) | 300 ns, 0.9 KB | 626 ns, 2.0 KB |

A hybrid that pops once or twice and falls back to `takeUntil` when more
needs evicting should get the better figure in both cases. Worth doing
only if `Timed` windows are on a hot path.

### 4. Structural alternatives for `DigestTimeBatched`

* **Two-stacks sliding-window aggregation.** Eviction is always
  oldest-first, so the finger tree's arbitrary splits are not needed. A
  two-stacks structure needs about one or two merges per seal (amortised)
  instead of about eight, and a query needs one. This is a rewrite.
* **`Data.TDigest.Vector`.** Inserts are about 40x cheaper than the tree
  variant (13 ns versus 500 ns), but in a quick test its merge was slower
  (1.1 ms versus 0.2 ms) and `forceCompress` did not shrink the digests
  as expected (3325 centroids for the merged root). Inconclusive; needs a
  closer look at how it is meant to be used.

### 5. Smaller follow-ups

* An `evictBefore` that evicts nothing still rebuilds the cache thunk,
  costing one merge at the next query. Reusing the cache would require
  `insert`'s seal path to stop passing a record with a stale
  `tdwCacheDigest` to `evictBefore`.
* `TimedDigestWindow` has no `NFData` instance, although its component
  types do.
* Anything holding a window in a `TVar` must update it strictly
  (`modifyTVar'`), otherwise chains of `insert` thunks build up whatever
  the library does internally.
