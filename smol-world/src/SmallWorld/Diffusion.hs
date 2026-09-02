-- | Store-and-forward EB diffusion over the topology, with per-node egress
-- fair-share and emergent loss.  This is the model the ΔQ report could not
-- supply.  The per-hop exchange is faithful to Cardano (no load balancing):
--
--   * a server pushes an EB **announce** to its downstream peers — one-way
--     delay (OWD = RTT/2);
--   * a downstream node fetches the **EB body** (the tx references, ≈ n_refs×32
--     bytes, ≤512 kB) from the *first* peer that announced — pull, so it pays a
--     round-trip before bytes plus the transfer;
--   * then it fetches the **EB closure** (the referenced txs; π₁=1 ⇒ all of
--     them) from that same peer — another round-trip + transfer.
--
-- Both fetches contend on the serving node's egress (fair-shared each bin);
-- when a flow's cwnd window exceeds its egress share the excess overflows —
-- emergent loss driving the ported CUBIC/RTO cwnd.  A node can serve/announce
-- downstream once it has *secured* body+closure; it need only *apply* (CPU) the
-- closure to vote (apply cost = α + β·n_txs, the marginal slope — not the mean).
--
-- Time advances in fixed bins (heterogeneous link RTTs preclude a global
-- RTT-bin); each flow's cwnd updates on its own RTT boundaries.
module SmallWorld.Diffusion
  ( DiffParams(..)
  , GE(..)
  , tierGE
  , refLoss
  , defaultDiffParams
  , DiffResult(..)
  , simulateEB
  , diffuseTimes
  , diffuseTimesWith
  , diffuseTimesFrom
  , diffuseTimesGen
  , ebApplyDelay
  , warmIdleSet
  , committeeSeats
  , committeeMembers
  , pCadence
  , BattleResult(..)
  , simulateBattle
  ) where

import           Data.List          (minimumBy, sortBy)
import qualified Data.Foldable as Foldable
import           Data.Ord           (comparing, Down(..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import qualified Data.Vector        as V
import           System.Random      (mkStdGen, randomRs)

import           SmallWorld.Rand    (hashUnit, mix)
import           SmallWorld.TCP
import           SmallWorld.Types

-- | Gilbert–Elliott baseline (non-congestion) loss: a two-state good/bad chain.
-- The **good** state carries a low per-packet rate, the **bad** state a high one.
-- @pGB@/@pBG@ set how often the bad state is entered and how long it lasts, tuning
-- the RTO rate independently of the good-state rate.  The RTO-vs-fast-retransmit
-- split is not keyed to the state — it emerges from the per-window burst count (see
-- `advance`); the bad state RTOs more only because its higher rate yields ≥2 losses
-- per window more often.  Transitions are per RTT-round.
data GE = GE
  { geGood :: !Double   -- good-state per-packet loss (low rate)
  , geBad  :: !Double   -- bad-state per-packet loss (high rate)
  , gePGB  :: !Double   -- per-round good→bad transition probability
  , gePBG  :: !Double   -- per-round bad→good transition probability
  } deriving (Show)

data DiffParams = DiffParams
  { dpEgressBps       :: !Double  -- per-node egress capacity (bits/s)
  , dpClosureB        :: !Int     -- closure bytes per node (π₁=1 → whole EB tx data)
  , dpRefBytes        :: !Int     -- bytes per tx reference in the EB body (CIP: 32)
  , dpBodyCapB        :: !Int     -- EB body size cap (CIP: 512 kB)
  , dpBinS            :: !Double  -- simulation time step (s)
  , dpApplyFixedMs    :: !Double  -- α: fixed per-closure apply overhead (ms)
  , dpApplyMarginalMs :: !Double  -- β: marginal per-tx apply (ms), large-block slope
  , dpAvgTxB          :: !Int     -- average tx size (bytes) → tx count = closure/avgTx
  , dpDeadlineS       :: !Double  -- voter deadline (s)
  , dpQuorum          :: !Double  -- stake fraction needed (0.75)
  , dpRtoThresh       :: !Double  -- windowRate/share above which oversub → RTO (else cut)
  , dpCapS            :: !Double  -- hard time cap (s) to end a stuck run
  , dpWarmIdleFrac    :: !Double  -- fraction of nodes with tcp_slow_start_after_idle=0
                                  --   (keep cwnd warm across idle); Phase-4 lever, default 0
  , dpWarmIdleSeed    :: !Int     -- seed for the (stable) warm-idle node assignment
  , dpCommitteeCoverage :: !Double -- σ_c: cumulative active-stake coverage of the
                                   --   deterministic truncated committee (CIP-0164); 1.0 = all nodes
  , dpRbRate          :: !Double  -- Praos active-slot coefficient (RBs/slot), for cadence
  , dpCadenceSlots    :: !Int     -- CIP rule #3 window: 3·L_hdr + L_vote + L_diff (= 14 slots)
  , dpJitter          :: !Double  -- RTT jitter coefficient σ_j: per-round RTT = base·(1 ± σ_j/2),
                                  --   and RTO RTTVAR = max(0.25, σ_j)·RTT; 0 = no jitter (default)
  , dpReorderP        :: !Double  -- per-round probability a reordering triggers a spurious
                                  --   fast-retransmit (a cwnd cut without real loss); 0 = off
  , dpJitterSeed      :: !Int     -- seed for the deterministic per-round jitter/reorder draws
  , dpSack            :: !Bool    -- SACK fast-recovery: a recoverable GE burst recovers in ~1 RTT (else RTO);
                                  --   off = No-SACK. (No retransmit-loss RTO path is modelled — D20 simplification.)
  , dpLossIntraCluster   :: !GE   -- baseline Gilbert–Elliott loss per RTT tier (D2): a
  , dpLossIntraContinent :: !GE   --   separate good/bad chain for local / intra-continent /
  , dpLossInterContinent :: !GE   --   inter-continent edges (loss rises with the tier)
  } deriving (Show)

defaultDiffParams :: DiffParams
defaultDiffParams = DiffParams
  { dpEgressBps = 1.0e9, dpClosureB = 12000000, dpRefBytes = 32, dpBodyCapB = 512000
  , dpBinS = 0.01, dpApplyFixedMs = 2.1, dpApplyMarginalMs = 0.083, dpAvgTxB = 1000
  , dpDeadlineS = 7.0, dpQuorum = 0.75, dpRtoThresh = 2.0, dpCapS = 30.0
  , dpWarmIdleFrac = 0.0, dpWarmIdleSeed = 1
  , dpCommitteeCoverage = 0.99
  , dpRbRate = 0.05, dpCadenceSlots = 14
  , dpJitter = 0.0, dpReorderP = 0.0, dpJitterSeed = 1, dpSack = False
  -- baseline loss OFF by default (opt-in, like jitter/reorder): the sweep surface
  -- stays pure-contention.  `refLoss` is the profile --loss-scale (and the sweep's
  -- loss slices) dial up from zero.
  , dpLossIntraCluster   = GE 0 0 0 0
  , dpLossIntraContinent = GE 0 0 0 0
  , dpLossInterContinent = GE 0 0 0 0 }

-- | Reference baseline-loss profile per tier (well-provisioned local, rising to a
-- modest intercontinental floor).  Baseline loss is OFF by default; this is what
-- --loss-scale and the sweep's loss slices dial up from zero.  good-state = a low
-- per-packet rate, bad-state = a high one; pGB/pBG set how often the bad state is
-- entered and how long it lasts.  RTO vs fast-retransmit is NOT keyed to the state
-- (see `advance`): it emerges from the per-window burst count — the bad state RTOs
-- more only because its higher rate yields ≥2 losses per window more often.
refLoss :: Tier -> GE
refLoss IntraCluster   = GE 1.0e-6 0.02 0.0005 0.20
refLoss IntraContinent = GE 1.0e-5 0.03 0.0010 0.15
refLoss InterContinent = GE 1.0e-4 0.05 0.0020 0.10

-- | Baseline-loss GE parameters for an RTT tier.
tierGE :: DiffParams -> Tier -> GE
tierGE dp IntraCluster   = dpLossIntraCluster dp
tierGE dp IntraContinent = dpLossIntraContinent dp
tierGE dp InterContinent = dpLossInterContinent dp

-- | Deterministically choose exactly @round(frac·n)@ nodes to keep their cwnd
-- warm across idle (the @tcp_slow_start_after_idle=0@ minority), seeded so the
-- assignment is stable across a multi-round run.  Selection is a seeded random
-- shuffle (rank nodes by a random key, take the top @k@) so the chosen set is an
-- unbiased sample of exactly the requested fraction, not an i.i.d. coin flip that
-- only approximates it.  At @frac=0@ (the default) the set is empty and every
-- connection idle-resets like a stock Linux stack.
warmIdleSet :: Int -> Double -> Int -> IS.IntSet
warmIdleSet n frac seed =
  let k     = max 0 (min n (round (frac * fromIntegral n)))
      keys  = take n (randomRs (0, 1 :: Double) (mkStdGen seed))
      order = map snd (sortBy (comparing fst) (zip keys [0 .. n - 1]))
  in IS.fromList (take k order)

-- | Generic stake-weighted multinomial sampler: draw @nSeats@ nodes with
-- replacement, each ∝ stake (a node's count is Binomial(nSeats, stakeᵢ)), seeded.
-- Used to pick stake-sortitioned *producers* (e.g. a random rival in a slot
-- battle) — NOT the voting committee, which is a deterministic truncation
-- (see 'committeeMembers'; CIP-0164 has no per-EB sortition).
committeeSeats :: Int -> Int -> V.Vector Double -> IM.IntMap Int
committeeSeats nSeats seed stakeV
  | nSeats <= 0 || n == 0 = IM.empty
  | otherwise             = IM.fromListWith (+) [ (find x, 1) | x <- draws ]
  where
    n     = V.length stakeV
    cum   = V.scanl1' (+) stakeV                    -- cumulative stake, last = total
    tot   = cum V.! (n - 1)
    draws = take nSeats (randomRs (0, tot) (mkStdGen seed))
    find x = bsearch 0 (n - 1)                      -- first index with cum ≥ x
      where bsearch lo hi
              | lo >= hi         = lo
              | cum V.! mid >= x = bsearch lo mid
              | otherwise        = bsearch (mid + 1) hi
              where mid = (lo + hi) `div` 2

-- | Node stake vector, indexed by NodeId (stakes sum to 1 = total active stake).
stakesOf :: Topology -> V.Vector Double
stakesOf topo = V.generate (nNodes topo) (\i -> nStake (topoNodes topo V.! i))


-- | The CIP-0164 voting committee: a deterministic stake-based *truncation* of
-- the active-stake distribution — order pools by stake descending and take the
-- top ones until cumulative stake first reaches @coverage@ (σ_c).  Fixed for the
-- epoch: no per-EB sortition, no seed, no randomness (CIP-0164 §Committee
-- Structure).  Every member votes with its own stake; the quorum is a fraction of
-- TOTAL active stake (see 'ebResult').  @coverage >= 1@ ⇒ every node is a member.
committeeMembers :: Double -> V.Vector Double -> IS.IntSet
committeeMembers coverage stakeV =
  let ranked = sortBy (comparing (Down . snd)) (zip [0 ..] (V.toList stakeV))
      go _   acc []            = acc
      go cum acc ((i, s) : xs)
        | cum >= coverage = acc
        | otherwise       = go (cum + s) (IS.insert i acc) xs
  in go 0 IS.empty ranked

data DiffResult = DiffResult
  { drServeQuorumTime :: !(Maybe Double)  -- network only: 75% stake has secured body+closure
  , drQuorumTime      :: !(Maybe Double)  -- + CPU apply: 75% stake has VOTED
  , drQuorumMet       :: !Bool            -- validated quorum within the deadline
  , drReachedDeadline :: !Double          -- stake fraction validated (voted) by deadline
  , drReachedTotal    :: !Double          -- stake fraction secured ever (by cap)
  } deriving (Show)

data FetchPhase = Body | Closure deriving (Eq)

data Flow = Flow
  { flTp    :: !TcpParams   -- for the current phase's byte target
  , flPhase :: !FetchPhase
  , flSrc   :: !NodeId      -- the first peer that announced (fixed for both phases)
  , flFs    :: !FlowState
  , flRtt   :: !Double
  , flNext  :: !Double      -- next RTT boundary
  , flLoss  :: !Bool
  , flReady :: !Double      -- time the first byte can arrive (request RTT paid); no
                            --   bytes and no egress share consumed before this
  , flRound :: !Int         -- RTT-boundary counter (indexes the per-round jitter draw)
  , flTier  :: !Tier        -- RTT/loss tier of this (server → u) edge
  , flGeBad :: !Bool        -- Gilbert–Elliott baseline-loss state (in the bad/bursty state?)
  , flRoundB0 :: !Int       -- fsBytesSent at the current round's start (for the GE-RTO prefix credit)
  }

-- | The diffusion DES core: propagate one EB from @producer@ and return each
-- node's SERVE time (body+closure secured; producer at 0).  Deterministic given
-- (topology, producer, params): emergent loss is threshold-based, not sampled.
diffuseTimes :: DiffParams -> Topology -> NodeId -> IM.IntMap Double
diffuseTimes dp topo producer = diffuseTimesWith dp topo producer (const (dpClosureB dp))

-- | As 'diffuseTimes', but each node @u@ fetches a per-node closure of @closureOf u@
-- bytes (its missing fraction) rather than the uniform @dpClosureB@.  The body (the
-- reference list) stays uniform; only the closure fetch varies per node — the
-- π₁→state bridge the multi-round loop uses (design.md/mechanics §9).
diffuseTimesWith :: DiffParams -> Topology -> NodeId -> (NodeId -> Int) -> IM.IntMap Double
diffuseTimesWith dp topo producer closureOf = diffuseTimesFrom dp topo [producer] closureOf

-- | As 'diffuseTimesWith', but seeded from a SET of sources all present at t=0 — the
-- producer plus any node that already possesses the whole closure (planted it in a prior
-- attempt).  Planted nodes relay, so planting CASCADES: without this the closure floods
-- from the single producer and the tail never secures a cold closure within the gap
-- (design.md, the strict-fetch reading).  @diffuseTimesWith@ is the single-source case.
diffuseTimesFrom :: DiffParams -> Topology -> [NodeId] -> (NodeId -> Int) -> IM.IntMap Double
diffuseTimesFrom dp topo sources closureOf =
  diffuseTimesGen dp topo sources closureOf (const False) (const (dpEgressBps dp))

-- | The general diffusion: as 'diffuseTimesFrom', plus the two carry-over levers the
-- multi-round loop drives between EBs (design.md "Carry-over").  @warm v@ = the sender @v@
-- kept its cwnd warm across the idle gap (@tcp_slow_start_after_idle=0@), so its flows open
-- already-ramped (skip slow-start) instead of cold — a persistent fast minority.  @egressOf
-- v@ = @v@'s available egress this EB, which the loop haircuts by the residual load @v@'s
-- previous-EB fetch tail is still carrying when this EB announces (the load-carry-over
-- hypothesis).  The other entry points fix @warm = const False@, @egressOf = const
-- dpEgressBps@ (cold start, full egress) — so g5/sweep/battle are unchanged.
diffuseTimesGen :: DiffParams -> Topology -> [NodeId] -> (NodeId -> Int)
                -> (NodeId -> Bool) -> (NodeId -> Double) -> IM.IntMap Double
diffuseTimesGen dp topo sources closureOf warm egressOf
  -- Input guard (TCP-audit Finding 3): the bin loop steps `t` by `dpBinS`, so a
  -- non-positive or non-finite step never reaches `dpCapS` and the run hangs.
  | dpBinS dp <= 0 || isNaN (dpBinS dp) || isInfinite (dpBinS dp) =
      errorWithoutStackTrace "--bin-ms must be positive and finite (else the bin loop never advances)"
  | dpCapS dp <= 0 = errorWithoutStackTrace "--cap-s must be positive"
  | otherwise = loop 0 IM.empty (IM.fromList [ (s, 0) | s <- sources ])
  where
    n        = nNodes topo
    srcsOf   = V.generate n (\u -> [ (eTo e, eRttMs e / 1000, eTier e) | e <- topoOut topo V.! u ]) -- rtt in s
    mssB     = 1460 :: Int
    closureB = dpClosureB dp
    nRefs    = closureB `div` max 1 (dpAvgTxB dp)
    bodyB    = min (dpBodyCapB dp) (nRefs * dpRefBytes dp)

    -- `has` maps node -> SERVE time (body+closure secured); producer at 0.
    loop t fetch has
      | IM.size has == n = has
      | t > dpCapS dp    = has
      | otherwise        = let (f', h') = stepBin t fetch has in loop (t + dpBinS dp) f' h'

    stepBin t fetch has =
      let -- announce reaches u from server v at v.serveTime + OWD (= rtt/2)
          announces u = [ (v, rtt, tier, st + rtt / 2)
                        | (v, rtt, tier) <- srcsOf V.! u, Just st <- [IM.lookup v has] ]
          startable = [ u | u <- [0 .. n - 1]
                          , not (IM.member u has), not (IM.member u fetch)
                          , any (\(_, _, _, a) -> a <= t) (announces u) ]
          -- Open each connection at EB start.  A cold source slow-starts (initFlow); a warm
          -- source (kept cwnd across the idle, design.md D12) opens already-ramped — cwnd at
          -- the BDP cap, in CA — so it uses its full egress share from the first RTT rather
          -- than doubling up to it.  Egress is the SENDER v's (carry-over haircut applied).
          startFlow u =
            let (v, rtt, tier, _) = minimumBy (comparing (\(_, _, _, a) -> a)) (announces u) -- first announcer
                tpB = mkTcpParams bodyB rtt 0 (egressOf v)
                fs0 | warm v    = warmFlow tpB   -- already-ramped; epoch built by enterCA
                    | otherwise = initFlow tpB
            in Flow tpB Body v fs0 rtt (t + 2 * rtt) False (t + rtt) 0 tier False 0  -- body req: flReady = 1 RTT to first byte; flNext = grow at the NEXT boundary, not this one (Finding 4)
          fetch1 = Foldable.foldl' (\m u -> IM.insert u (startFlow u) m) fetch startable
          -- only actively-transmitting flows consume egress: a flow waiting on a
          -- request RTT or stalled in an RTO backoff (flReady > t) sends nothing,
          -- so it must not dilute the hub's egress share
          load  = IM.fromListWith (+) [ (flSrc f, 1 :: Int) | f <- IM.elems fetch1, flReady f <= t ]
          shareOf v = egressOf v / fromIntegral (max 1 (IM.findWithDefault 1 v load))
          (fetch2, done) = IM.foldrWithKey (advance t shareOf) (IM.empty, []) fetch1
          has1 = Foldable.foldl' (\m (u, tc) -> IM.insert u tc m) has done
      in (fetch2, has1)

    advance t shareOf u f (accF, accDone)
      | t < flReady f = (IM.insert u f accF, accDone)  -- request in flight: waiting for first byte
      | otherwise =
      let v          = flSrc f
          share      = shareOf v
          fs         = flFs f
          rttBase    = flRtt f
          -- phase salt for every per-round draw below.  flRound resets to 0 at the
          -- body→closure switch, so without a phase discriminator the closure would
          -- reuse the body's round-indexed draws.  It also future-proofs the Phase-4
          -- option of fetching the closure from a different peer than the body
          -- (design.md Phase 4): body and closure keep independent draw streams.
          phaseSalt  = case flPhase f of { Body -> 0; Closure -> 1 }
          -- per-round jitter: RTT varies ±σ_j/2 around the tier base (deterministic,
          -- seeded), clamped to ≥ half the base to stay physical
          uJit       = hashUnit (mix [dpJitterSeed dp, u, flRound f, 0, phaseSalt])
          rtt        = max (0.5 * rttBase) (rttBase * (1 + dpJitter dp * (uJit - 0.5)))
          tp         = flTp f
          -- window rate from the BDP-capped cwnd: CUBIC can grow fsCwnd past the
          -- pipe, but cwnd above the BDP is not on the wire, so the oversub/rate
          -- signal must use the capped window (matches the loss-exposure count).
          windowRate = min (fsCwnd fs) (fromIntegral (tpBdpCapSegs tp)) * fromIntegral mssB * 8 / rtt
          rate       = min windowRate share
          bytesRaw   = rate * dpBinS dp / 8
          -- On a GE-RTO round the flow delivers only up to the first-loss prefix, then
          -- stalls (matching TCP.hs stepRound's firstLoss-1 credit); the packets past
          -- the first loss are retransmitted after the RTO, not booked now.  `flRoundB0`
          -- marks this round's starting byte count, so the cap holds fsBytesSent at
          -- ≤ roundStart + prefix.  The same prefix cap applies to a `capFinal` round — a
          -- recoverable (fast-rtx) loss in the FINAL window, whose fill RTT is exposed (no
          -- following data to overlap it) — so it completes ~1 RTT on, not this round.
          -- Mid-transfer fast-rtx and clean/cut/oversub rounds credit the full bin (M0).
          deliverB   | geRTO || capFinal = max 0 (min (round bytesRaw) (flRoundB0 f + prefixB - fsBytesSent fs))
                     | otherwise         = round bytesRaw
          fsB        = fs { fsBytesSent = min (tpFileBytes tp) (fsBytesSent fs + deliverB) }
          oversub    = windowRate > share * 1.001    -- ε=0.001: float-equality guard at exactly fair share
          -- Tier-dependent baseline (non-congestion) link loss, Gilbert–Elliott.  The
          -- per-packet rate is state-dependent — geGood in the good state (rare
          -- isolated drops), geBad in the bad state (a high-loss regime).  State
          -- (flGeBad) is carried per flow across rounds; the transition is seeded per
          -- round.  The RTO vs fast-retransmit split is NOT gated on the GE state — it
          -- falls out of a real per-window burst count (below): the bad state produces
          -- RTOs only because its high rate yields ≥2 losses in a window.
          geP        = tierGE dp (flTier f)
          nowBad     | flGeBad f = hashUnit (mix [dpJitterSeed dp, u, flRound f, 2, phaseSalt]) >= gePBG geP
                     | otherwise = hashUnit (mix [dpJitterSeed dp, u, flRound f, 2, phaseSalt]) <  gePGB geP
          pLoss      = min 1 (max 0 (if nowBad then geBad geP else geGood geP))   -- per-packet rate, clamped to [0,1]
          qLoss      = 1 - pLoss
          -- packets actually clocked onto the wire this round: the congestion window,
          -- throttled to the egress share at a congested hub (loss applies to what is
          -- sent, not the full cwnd), and capped by what is left to send.
          onWire     = max 1 (ceiling (rate * rtt / (8 * fromIntegral mssB)) :: Int)
          -- remaining packets from the ROUND'S START (flRoundB0), not the running
          -- fsBytesSent: this fixes the round's loss classification (attempt, hence
          -- geRTO/firstLoss) so it cannot drift as delivery accumulates within the
          -- round — otherwise a final window classified as an RTO at round start could
          -- shrink `attempt` to 1 mid-round, flip geRTO false, and complete uncapped.
          remPkts    = max 1 ((tpFileBytes tp - flRoundB0 f + mssB - 1) `div` mssB)
          -- effCwnd and remPkts are round-stable (round-start); onWire still tracks the
          -- per-bin hub share, so when share is the binding term (oversub) a round's GE
          -- classification can rarely reclassify mid-round.  Non-directional (share moves
          -- either way) and non-systematic — well within the extreme-cell sampling noise.
          attempt    = max 1 (minimum [effCwnd, onWire, remPkts])
          -- A real per-window burst.  Sample the loss OUTCOME over `attempt` i.i.d.
          -- packets, matching TCP.hs stepRound's 3-dup-ACK rule analytically (one draw,
          -- no per-packet loop): a first loss with ≥3 trailing packets is *recoverable*
          -- (enough dup-ACKs to clock a fast-retransmit); a first loss in the last 3
          -- packets is a *tail* loss (too few dup-ACKs ⇒ RTO).  a3 = positions whose
          -- first-loss leaves ≥3 trailing, i.e. firstLoss ≤ attempt−3.
          a3         = max 0 (attempt - 3)
          pClean     = qLoss ** fromIntegral attempt                                 -- P(no loss)
          pRecov     = 1 - qLoss ** fromIntegral a3                                  -- P(first loss ≤ attempt−3): recoverable
          pFastRtx   = fromIntegral a3 * pLoss * qLoss ** fromIntegral (attempt - 1) -- P(exactly one such loss)
          -- One draw picks the outcome band.  No-SACK: only a single recoverable loss
          -- fast-retransmits; ≥2 losses in the window (or a tail loss) RTO.  SACK: any
          -- recoverable burst fast-recovers in ~1 RTT; only a tail loss still RTOs.
          uLoss      = hashUnit (mix [dpJitterSeed dp, u, flRound f, 3, phaseSalt])
          cutThresh  = pClean + (if dpSack dp then pRecov else pFastRtx)
          geClean    = uLoss <  pClean
          geLoss     = not geClean
          geRTO      = uLoss >= cutThresh                     -- past the recoverable band ⇒ timeout
          oversubRTO = windowRate > share * dpRtoThresh dp
          -- RFC 5681 §3.2: a fast retransmit needs three dup-ACKs, so a loss in a window of
          -- fewer than four packets cannot be recovered without a timeout.  The GE channel
          -- already encodes this analytically (with a3 = 0 both pRecov and pFastRtx vanish,
          -- so every GE loss becomes a geRTO); the oversub and carried (flLoss) channels did
          -- not.  Gating all of them keeps onCongestionEvent to windows where a real sender
          -- could fast-retransmit — the precondition RFC 9438 §4.6 Figure 5 relies on when it
          -- floors cwnd at 2 SMSS with no guard of its own.
          dupAckable = a3 > 0                                 -- attempt >= 4
          lossNow    = flLoss f || oversub || geLoss
          rtoNow     = oversubRTO || geRTO || not dupAckable
          -- Final-window fast-retransmit: a recoverable (non-RTO) loss in the LAST window
          -- has no following data to overlap the retransmit, so the in-order prefix closes
          -- only ~1 RTT on (the fill lands next round).  Mid-transfer that RTT is absorbed
          -- by continued transmission (M0); at the file tail it is exposed, so `deliverB`
          -- caps this round to the pre-loss prefix (as GE-RTO does) and it completes next
          -- round — the DES analogue of TCP.hs's completing-fastRtx +rtt (faithful fast recovery).
          finalWindow = attempt >= remPkts                   -- the window covers all remaining pkts (attempt == remPkts)
          capFinal    = lossNow && not rtoNow && finalWindow  -- a fast-rtx loss that would otherwise complete the file
          -- first-loss position (truncated geometric over the window, given ≥1 loss)
          -- for the GE-RTO prefix-credit above; only forced when geRTO (pLoss > 0).
          u4         = hashUnit (mix [dpJitterSeed dp, u, flRound f, 4, phaseSalt])
          pWinLoss   = 1 - qLoss ** fromIntegral attempt
          firstLoss  | pLoss <= 0 = attempt
                     | otherwise  = max 1 (min attempt (ceiling (log (1 - u4 * pWinLoss) / log qLoss) :: Int))
          prefixB    = (firstLoss - 1) * mssB
          -- reordering can trigger a spurious fast-retransmit on an otherwise clean round
          reorder    = hashUnit (mix [dpJitterSeed dp, u, flRound f, 1, phaseSalt]) < dpReorderP dp
          boundary   = t + dpBinS dp >= flNext f
          effCwnd    = max 1 (min (floor (fsCwnd fs)) (tpBdpCapSegs tp))   -- fs, not fsB (same cwnd; breaks the deliverB cycle)
          r1         = flRound f + 1
          (fs2, loss2, next2, ready2, round2)
            | not boundary = (fsB, lossNow, flNext f, flReady f, flRound f)
            | lossNow && rtoNow =                               -- RTO: oversub OR a ≥2-loss/tail burst; backed-off stall + HOL + restart
                let rttVar = max 0.25 (dpJitter dp)             -- jitter inflates RTTVAR ⇒ larger RTO
                    rtoBase = rttBase * (1 + 4 * rttVar)
                    rto = min (tpRtoMaxS tp)
                              (max (tpRtoMinS tp) rtoBase * 2 ^ min (fsConsecTO fsB) (30 :: Int))
                    -- did this round put anything on the wire?  If so its originals are
                    -- Karn-legal (RFC 6298 §3) and the sample collapses the backoff; if
                    -- not, the backoff compounds (§5.5).  Covers both RTO channels: a
                    -- GE-RTO delivers its pre-loss prefix, an oversub RTO the whole bin.
                    fs' | fsBytesSent fsB > flRoundB0 f
                        = onDelivery rtt (rtoCollapse tp (fromIntegral effCwnd) fsB)
                        | otherwise
                        = (rtoCollapse tp (fromIntegral effCwnd) fsB)
                            { fsConsecTO = fsConsecTO fsB + 1 }
                in (fs', False, t + rto, t + rto, r1)           -- HOL: no delivery/egress until stall ends
            | lossNow =                                         -- recoverable loss ⇒ fast-retransmit / SACK fast-recovery: graceful cut
                -- fast recovery acks at least three originals by construction, so the round
                -- always yields a Karn-legal sample, which also clears the backoff (§3 and
                -- the note after §5.7)
                (onDelivery rtt (onCongestionEvent tp Loss (fromIntegral effCwnd) fsB), False, flNext f + rtt, flReady f, r1)
            | reorder && dupAckable =                           -- reordering ⇒ spurious fast-retransmit
                -- a spurious fast retransmit needs the same three dup-ACKs; below that the
                -- reordered packet simply arrives late and nothing fires (not a timeout).
                -- The round was in fact clean, so it samples like one.
                (onDelivery rtt (onCongestionEvent tp Loss (fromIntegral effCwnd) fsB), False, flNext f + rtt, flReady f, r1)
            | otherwise =                                       -- clean round: grow, reset RTO backoff
                -- `attempt`, not effCwnd: §4.3 Figure 4 credits W_est by
                -- alpha_cubic * segments_acked / cwnd, and a share-throttled round puts
                -- fewer than cwnd segments on the wire
                let grown = growOnAck tp rtt (fromIntegral attempt) fsB  -- advances t - t_epoch itself (§4.2)
                in (onDelivery rtt grown, False, flNext f + rtt, flReady f, r1)
          rb0'       = if boundary then fsBytesSent fs2 else flRoundB0 f  -- next round starts at this round's delivered total
      in if fsBytesSent fs2 >= tpFileBytes tp
           then if flPhase f == Body
                  -- body secured → fetch the closure from the SAME peer over the
                  -- SAME persistent connection: carry the congestion-control
                  -- state (cwnd, ssthresh, CUBIC epoch) over — reset only the
                  -- byte counter — so the closure does NOT re-slow-start. One RTT
                  -- gap for the closure request (flReady = t + rtt); grow at the next boundary (flNext = t + 2*rtt, Finding 4).
                  then let tpC = mkTcpParams (closureOf u) rttBase 0 (egressOf v)
                           fC  = f { flTp = tpC, flPhase = Closure
                                   , flFs = fs2 { fsBytesSent = 0 }
                                   , flNext = t + 2 * rtt, flLoss = False
                                   , flReady = t + rtt, flRound = 0
                                   , flRoundB0 = 0 }  -- closure req: 1 RTT to first byte; round-start resets
                       in (IM.insert u fC accF, accDone)
                  else (accF, (u, t + dpBinS dp) : accDone)   -- closure secured → served
           else (IM.insert u f { flFs = fs2, flLoss = loss2, flNext = next2
                               , flReady = ready2, flRound = round2, flRoundB0 = rb0'
                               , flGeBad = if boundary then nowBad else flGeBad f } accF, accDone)

-- | Marginal CPU apply delay for the closure (α + β·n_txs), in seconds.
ebApplyDelay :: DiffParams -> Double
ebApplyDelay dp = (dpApplyFixedMs dp + fromIntegral nRefs * dpApplyMarginalMs dp) / 1000
  where nRefs = dpClosureB dp `div` max 1 (dpAvgTxB dp)

-- | The epoch's voting committee (deterministic stake truncation to σ_c coverage).
committeeFor :: DiffParams -> Topology -> IS.IntSet
committeeFor dp topo = committeeMembers (dpCommitteeCoverage dp) (stakesOf topo)

-- | Aggregate a serve-time map into a DiffResult.  Only committee members vote,
-- each with its own stake; certification is the CIP-0164 predicate — the summed
-- stake of members that have voted (secured + applied) by a time reaches
-- τ (dpQuorum) of TOTAL active stake.  Serving/relaying is unchanged (every node
-- fetches and forwards regardless of committee membership).
ebResult :: DiffParams -> Topology -> IM.IntMap Double -> DiffResult
ebResult dp topo has =
  let stakeV    = stakesOf topo
      valDelay  = ebApplyDelay dp
      committee = committeeFor dp topo
      weightOf u = stakeV V.! u                       -- a voter contributes its stake
      voters = [ e | e@(u, _) <- IM.toList has, u `IS.member` committee ]
      firstCross getT = go 0 . sortBy (comparing getT)
        where go acc ((u, vt) : rest)
                | acc + weightOf u >= dpQuorum dp = Just (getT (u, vt))
                | otherwise                       = go (acc + weightOf u) rest
              go _ [] = Nothing
      serveQ = firstCross snd voters
      valQ   = firstCross (\(_, vt) -> vt + valDelay) voters
      reachedBy tt = sum [ weightOf u | (u, vt) <- voters, vt + valDelay <= tt ]
  in DiffResult
       { drServeQuorumTime = serveQ
       , drQuorumTime      = valQ
       , drQuorumMet       = maybe False (<= dpDeadlineS dp) valQ
       , drReachedDeadline = reachedBy (dpDeadlineS dp)
       , drReachedTotal    = sum [ stakeV V.! u | (u, _) <- IM.toList has ]
       }

-- | Simulate one EB from @producer@ (network diffusion → committee vote-quorum).
simulateEB :: DiffParams -> Topology -> NodeId -> DiffResult
simulateEB dp topo producer = ebResult dp topo (diffuseTimes dp topo producer)

-- | P(cadence window satisfied) = P(the next RB lands ≥ @dpCadenceSlots@ after
-- the announcing RB), with RB gaps ~ Exponential(rate @dpRbRate@ per slot):
-- e^(−rate·slots).  At the CIP params (0.05/slot, 14 slots) this is ≈ 0.497.
-- Network-independent — the Praos cadence cap (F5), applied as a factor.
pCadence :: DiffParams -> Double
pCadence dp = exp (negate (dpRbRate dp * fromIntegral (dpCadenceSlots dp)))

-- | Slot battle: two sibling EBs (@a@, @b@) announced in one slot.  Each node
-- commits to the sibling whose announcement it saw FIRST and forwards only that
-- (the CIP rule the user specified), so the slot's single committee partitions
-- by arrival order (Voronoi by first-seen).  The battle *resolves* only if one
-- sibling alone still reaches the quorum; otherwise both fail and the slot is
-- wasted (the next RB is a plain txRB).
--
-- First-seen is approximated by each sibling's *solo* serve time.  This is
-- optimistic on reach — it ignores that a node cannot route its committed EB
-- through peers that committed to the rival — but it captures the committee
-- split, which is the dominant effect.
data BattleResult = BattleResult
  { brWeightA  :: !Double   -- committee weight that committed A and voted by deadline
  , brWeightB  :: !Double
  , brWinner   :: !Double   -- max of the two sibling weights
  , brResolved :: !Bool     -- winner ≥ quorum (else the slot is wasted)
  } deriving (Show)

simulateBattle :: DiffParams -> Topology -> NodeId -> NodeId -> BattleResult
simulateBattle dp topo a b =
  let hasA      = diffuseTimes dp topo a
      hasB      = diffuseTimes dp topo b
      valDelay  = ebApplyDelay dp
      stakeV    = stakesOf topo
      committee = committeeFor dp topo             -- the epoch committee votes the slot
      wOf u     = stakeV V.! u                      -- a voter contributes its stake
      commitA u = case (IM.lookup u hasA, IM.lookup u hasB) of
                    (Just ta, Just tb) -> ta <= tb   -- saw A's announce first
                    (Just _ , Nothing) -> True
                    (Nothing, Just _ ) -> False
                    _                  -> True
      voteOK has u = case IM.lookup u has of
                       Just t  -> t + valDelay <= dpDeadlineS dp
                       Nothing -> False
      wA = sum [ wOf u | u <- IS.toList committee, commitA u,       voteOK hasA u ]
      wB = sum [ wOf u | u <- IS.toList committee, not (commitA u), voteOK hasB u ]
  in BattleResult wA wB (max wA wB) (max wA wB >= dpQuorum dp)
