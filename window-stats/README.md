# window-stats

Efficient sliding-window statistics for Haskell, backed by a finger
tree, with a cached user-supplied measure that updates incrementally
as the window slides.

## Scope

The package provides two variants of sliding window:

- **Count-based** - keeps the most recent _N_ inserted samples.
- **Time-based** - keeps samples within a configured duration of the
  newest sample's timestamp.

Both are generic in the cached measure: a finger-tree node stores the
combination of an element count (or timestamp bounds) and a
user-supplied monoid `v`. Looking the measure up is `O(1)` regardless
of window size.

Prebuilt sample wrappers and their measures are included for the
common rolling statistics:

| Sample wrapper  | Measure          | Statistic                              |
|-----------------|------------------|----------------------------------------|
| `SumSample`     | `Sum`            | running sum                            |
| `MinMaxSample`  | `MinMaxV`        | running min / max                      |
| `MomentSample`  | `WelfordMeasure` | running mean / sample variance / stddev (Welford) |
| `DigestSample`  | `TDigest comp`   | approximate quantiles (with-tdigest)   |

Time-based windows abstract over the timestamp type via the
`TimeLike` class. Two stock instances are shipped:

- `UTCTime` / `NominalDiffTime` (wall-clock, from `time`)
- `Time` / `DiffTime` (monotonic, from `io-classes:si-timers`)

Monotonic is preferred when sliding-window correctness must not be
perturbed by NTP corrections or wall-clock jumps; wall-clock fits
data that already carries `UTCTime` timestamps.

## Sublibraries

The package is split so that downstream consumers only pay for the
dependencies they need.

```
window-stats              -- core: Window, TimedWindow, prebuilt measures
window-stats:with-tdigest -- approximate-quantile backend via tdigest
window-stats:with-foldl   -- Control.Foldl combinators that drive a window
```

Sublibraries compose at the user's call site: a project depending on
both `with-tdigest` and `with-foldl` automatically gets foldl-driven
t-digest rolling quantiles.

## Complexity

Per-operation costs, for a window of size _w_:

| operation                          | amortised | worst-case |
|------------------------------------|-----------|------------|
| `insert`                           | `O(1)`    | `O(log w)` |
| `insert`'s built-in eviction       | `O(1)`    | `O(log w)` |
| `evictOldest`                      | `O(1)`    | `O(log w)` |
| `evictOldestN`                     | `O(log w)`| `O(log w)` |
| `trimByMeasure`                    | `O(log w)`| `O(log w)` |
| `evictBefore` (Timed)              | `O(log w)`| `O(log w)` |
| `resize` (shrink)                  | `O(log w)`| `O(log w)` |
| `resize` (grow / no-op)            | `O(1)`    | `O(1)`     |
| `windowMeasure` and other queries  | `O(1)`    | `O(1)`     |
| `fromListN` / `fromFoldable`       | `O(n)`    | `O(n log w)` |

t-digest-backed insertion is the one exception: each insert triggers
up to `O(log w)` TDigest `<>` merges along the finger-tree spine, and
each merge is `O(δ)` in the compression parameter - so an effective
`O(δ log w)` per insert.

Space is `O(w)` for the window itself plus whatever the user's
measure adds (typically `O(1)` for `Sum`/`MinMaxV`/`WelfordMeasure`;
`O(δ)` for `TDigest comp`, independent of `w`).

## When to reach for this package

- You need rolling statistics over a continuously moving window.
- You want both count- and time-based windowing in one API.
- You want approximate quantiles in bounded memory (the t-digest
  backend gives `O(δ)` regardless of window size).
- You don't want to commit to a particular streaming library - the
  core types are pure values and you compose them however suits your
  pipeline; `Control.Foldl` integration is optional.
- You want the cached running statistic - mean, variance, sum, a
  digest - as a value, on demand, in `O(1)`.

## Use-case examples

- **Network monitoring**. Rolling latency / throughput / drop-rate
  windows per peer, with cheap `O(1)` reads at each tick.
- **Service-level analytics**. Rolling p50/p95/p99 of request times
  over the last _N_ minutes via the t-digest backend.
- **Budget enforcement**. Variable-cost windows - keep the most
  recent samples whose cumulative weight (a `Sum`) stays below a
  threshold - via `trimByMeasure`.
- **Cross-window comparison**. Short- vs long-period moving averages
  for trend / crossover detection, via the dual-window combinators in
  `Data.Window.Fold.Count`.

## Relation to similar packages

The cleanest split is between packages that compete at the **windowing**
layer (where `window-stats` sits) and those that compete or compose at
the **measure** layer (the `v` in `Window v a`). Packages in the
second group plug in as alternative measures rather than substituting
for the window itself.

### `streamly-statistics`

Provides exact rolling statistics over a streamly stream:
- Exact quantiles via a sorted structure, at `O(w)` memory.
- Tied to streamly's streaming model and types.

`window-stats` is complementary rather than a replacement: it trades
exactness for `O(δ)` memory on quantiles via t-digest, is independent
of any streaming library, and provides a `Window` value you can hold,
query, and pass around - useful when sliding statistics are part of
some larger data structure rather than a one-shot pipeline result.

Use `streamly-statistics` when you need exact quantiles and you're
already on streamly; use `window-stats` when you can tolerate
approximate quantiles, want fixed memory, want time-based as well as
count-based windowing, or aren't already in a streamly context.

### `streaming`'s `slidingWindow`

`Streaming.Prelude.slidingWindow :: Int -> Stream (Of a) m r -> Stream (Of (Seq a)) m r`
yields each window position as a `Seq a`. It does no measure caching:
to compute a statistic at each step you'd traverse the `Seq` from
scratch.

`window-stats` carries the running measure inside the window itself
and updates it incrementally on insertion / eviction. For a stream of
length _n_ with windows of size _w_, computing a rolling sum:

- `streaming` + `slidingWindow`: `O(n · w)` work total (sum each window).
- `window-stats`: `O(n)` work total - each insert updates the cached
  measure in `O(1)` amortised; the read is `O(1)`.

Pick `streaming`'s `slidingWindow` if you genuinely need the window
contents at each step and the windows are small. Pick `window-stats`
if you want an incrementally-maintained statistic and the window
might be large.

### `conduit`'s `slidingVector` / `slidingWindow`

Same shape as `streaming`'s `slidingWindow` - yields each window
position as a `Vector a` / `Seq a` over the conduit pipeline, with no
measure caching. Same complexity trade-off, same advice: reach for it
when you actually need the window contents at each step, not just a
rolling statistic.

### `monoid-statistics`

Complementary, not competing. Provides numerically careful statistical
monoids (`Mean`, `Variance`, `Min`, `Max`, `KBNSum` Kahan-Babuška
summation, etc.). Any of them can be plugged in as the `v` parameter
of `Window v a` to get rolling versions of those statistics. Our
prebuilt `WelfordMeasure` covers the most common case; reach for
`monoid-statistics` when you need its specific numerical guarantees
or one of its less common stats.

### `histogram-fill`

Also complementary. Its `Histogram bin val` is a `Monoid` (bin-wise
addition), so a user can define a `Measured (Histogram bin val) sample`
instance and use `Window (Histogram bin val) sample` directly. The
window then carries a **rolling histogram** as its cached measure,
available in `O(1)` via `windowMeasure`. Cost per insert scales with
the bin count - much like the t-digest backend scales with δ - but
the result is exact rather than approximate.

### `tdigest`

We sit on top of it for the `with-tdigest` sublibrary's approximate
quantiles. If you only need a one-shot quantile over a static dataset
(no sliding-window aspect), use `tdigest` directly. Reach for our
`with-tdigest` when you specifically want approximate quantiles that
*roll* with a moving window in bounded memory.

### `Control.Foldl`-based folds

The `with-foldl` sublibrary integrates with the `foldl` package via
combinators such as `windowFoldRolling`, which feed each step's
window measure into an arbitrary downstream `Fold`. Memory is bounded
by the inner fold's own state, so summarising rolling statistics
across a long stream stays constant-memory.

## A small example

```haskell
import Data.Monoid (Sum)
import Data.Window.Count

-- A window of the last 100 sample sums:
let w0 = empty 100        :: Window (Sum Double) (SumSample Double)
    w1 = insert 1.5 w0
    w2 = insert 2.5 w1
in  (windowSum w2, size w2)
-- => (Just 4.0, 2)
```

t-digest quantiles:

```haskell
import Data.Window.DigestCount
import Data.TDigest qualified as TD

let w   = fromListN 1000 prices :: DigestWindow 100
    p50 = TD.quantile 0.5  (windowMeasure w)
    p99 = TD.quantile 0.99 (windowMeasure w)
in (p50, p99)
```

Time-based, with monotonic timestamps:

```haskell
import Control.Monad.Class.MonadTime.SI (Time, getMonotonicTime)
import Data.Window.Timed

-- 60-second sliding window of running mean
type LatencyWin = TimedWindow Time (WelfordMeasure Double) (MomentSample Double)

step :: Double -> LatencyWin -> IO LatencyWin
step latency w = do
  now <- getMonotonicTime
  return (insert (now, latency) w)
```

## Building and testing

```bash
cabal build all
cabal test window-stats
```

## License

Apache-2.0. See [LICENSE](LICENSE).
