-- | Per-flow CUBIC + full-RTO transport, a faithful Haskell port of the TCP
-- estimator's @simulate_one_run@ (round-based cwnd dynamics: slow-start / CUBIC
-- congestion avoidance, fast-retransmit vs. RTO, prefix-crediting on timeout).
--
-- This is the single-flow core.  Phase 2 drives it per-round from the diffusion
-- DES with a *per-round loss probability* derived from each serving node's egress
-- contention (rather than a fixed p) and a bandwidth cap from the node's egress
-- fair-share — but the cwnd/RTO arithmetic here is exactly the estimator's.
module SmallWorld.TCP
  ( Phase(..)
  , FlowState(..)
  , TcpParams(..)
  , mkTcpParams
  , initFlow
  , growOnAck
  , cutOnLoss
  , rtoCollapse
  , idleReset
  , stepRound
  , flowTime
  , flowTimeFrom
  , warmFlow
  ) where

import System.Random (RandomGen, randomR)

data Phase = SlowStart | CA deriving (Eq, Show)

-- | Static per-flow parameters (constant over the transfer).
data TcpParams = TcpParams
  { tpFileBytes  :: !Int
  , tpRttS       :: !Double
  , tpLossP      :: !Double   -- per-packet loss this transfer (Phase-2 overrides per round)
  , tpMss        :: !Int
  , tpBdpCapSegs :: !Int      -- window cap = BDP/MSS (bandwidth ceiling)
  , tpCwnd0      :: !Double
  , tpSsthresh0  :: !Double
  , tpCubicC     :: !Double
  , tpCubicBeta  :: !Double
  , tpRtoMinS    :: !Double
  , tpRtoMaxS    :: !Double
  , tpBaseRtoS   :: !Double
  , tpFastConv   :: !Bool
  , tpEnableRto  :: !Bool
  } deriving (Show)

-- | Smart constructor: derive the BDP window cap and initial ssthresh from a
-- capacity (bits/s) and RTT, matching the estimator's auto-sizing (ssthresh at
-- link saturation; base RTO = RTT + 4·RTTVAR with the no-jitter RTTVAR = RTT/4).
mkTcpParams :: Int -> Double -> Double -> Double -> TcpParams
mkTcpParams fileBytes rttS lossP capBps = TcpParams
  { tpFileBytes  = fileBytes
  , tpRttS       = rttS
  , tpLossP      = lossP
  , tpMss        = mss
  , tpBdpCapSegs = bdpCap
  , tpCwnd0      = 10
  , tpSsthresh0  = fromIntegral bdpCap
  , tpCubicC     = 0.4
  , tpCubicBeta  = 0.3
  , tpRtoMinS    = 0.2
  , tpRtoMaxS    = 120
  , tpBaseRtoS   = rttS + 4 * (0.25 * rttS)
  , tpFastConv   = True
  , tpEnableRto  = True
  }
  where
    mss    = 1460
    bdpCap = max 1 (floor (capBps * rttS / 8 / fromIntegral mss))  -- floor matches the reference estimator; inert in-regime (BDP ≥ ~850 segs)

data FlowState = FlowState
  { fsCwnd        :: !Double
  , fsSsthresh    :: !Double
  , fsPhase       :: !Phase
  , fsWMax        :: !Double
  , fsLastWMax    :: !Double
  , fsK           :: !Double
  , fsCaElapsed   :: !Double
  , fsBytesSent   :: !Int
  , fsT           :: !Double
  , fsConsecTO    :: !Int
  } deriving (Show)

initFlow :: TcpParams -> FlowState
initFlow tp = FlowState
  { fsCwnd = tpCwnd0 tp, fsSsthresh = tpSsthresh0 tp, fsPhase = SlowStart
  , fsWMax = 0, fsLastWMax = 0, fsK = 0, fsCaElapsed = 0
  , fsBytesSent = 0, fsT = 0, fsConsecTO = 0 }

-- Loss-free round: slow-start doubling to ssthresh, then CUBIC convex regrowth.
growOnAck :: TcpParams -> FlowState -> FlowState
growOnAck tp fs
  | fsPhase fs == SlowStart =
      let c = fsCwnd fs * 2
      in if c >= fsSsthresh fs
           then let w = max (fsSsthresh fs) (fsCwnd fs)  -- Finding 8: never shrink on a clean round (post-idleReset ssthresh<cwnd corner); no-op when cwnd<ssthresh
                in fs { fsCwnd = w, fsWMax = w
                      , fsLastWMax = 0, fsK = 0   -- no prior loss yet; RFC 8312 §4.6 W_last_max starts unset
                      , fsCaElapsed = 0, fsPhase = CA }
           else fs { fsCwnd = c }
  | otherwise =
      fs { fsCwnd = max 1 (tpCubicC tp * (fsCaElapsed fs - fsK fs) ** 3 + fsWMax fs) }

-- Graceful fast recovery: CUBIC multiplicative decrease + fresh concave epoch.
-- fsBytesSent is untouched — a loss costs a cwnd cut + ~1 RTT, not delivered data
-- (no-round-discard; see tcpcheck's self-check and mechanics.md §4/§5).
cutOnLoss :: TcpParams -> Int -> FlowState -> FlowState
cutOnLoss tp effCwnd fs =
  let beta = tpCubicBeta tp
      cwndAtLoss = fromIntegral effCwnd
      wMax | tpFastConv tp && fsLastWMax fs > 0 && cwndAtLoss < fsLastWMax fs
               = cwndAtLoss * (2 - beta) / 2
           | otherwise = cwndAtLoss
  in fs { fsWMax = wMax
        , fsLastWMax = cwndAtLoss   -- RFC 8312 §4.6: W_last_max = the pre-reduction cwnd-at-loss (may descend)
        , fsCwnd = max 1 (cwndAtLoss * (1 - beta))
        , fsK = (max 0 (wMax - max 1 (cwndAtLoss * (1 - beta))) / tpCubicC tp) ** (1/3)  -- regrow CA from the installed cwnd, not a fast-conv-dipped value (Finding 1; no-op outside the fast-conv corner)
        , fsCaElapsed = 0
        , fsPhase = CA }

-- RTO: collapse to a slow-start restart (cwnd->1, ssthresh = beta_cubic·cwnd).
-- fsBytesSent is untouched — the RTO costs a backoff stall + slow-start re-ramp,
-- not the round; the transfer resumes from where it was.
rtoCollapse :: Int -> FlowState -> FlowState
rtoCollapse effCwnd fs =
  fs { fsSsthresh = max 2 (fromIntegral effCwnd * 0.7)   -- RFC 8312 §4.7: ssthresh = beta_cubic·cwnd (0.7), not Standard-TCP 0.5
     , fsCwnd = 1, fsPhase = SlowStart
     , fsWMax = 0, fsLastWMax = 0, fsK = 0, fsCaElapsed = 0 }  -- Finding 11: clear the high-water mark on RTO (Linux bictcp_reset)

-- | Linux @tcp_slow_start_after_idle@: a connection idle for longer than one RTO
-- restarts its window at the initial cwnd (back into slow-start), keeping
-- ssthresh — so a *reused* connection does not send from a stale warm window.
-- Stacks that disable it (@tcp_slow_start_after_idle=0@, @keepsWarm=True@) retain
-- the warm cwnd across the idle gap.  @idleGapS@ is the inactivity since the
-- connection last sent.  Gaps within one RTO (e.g. the ~1-RTT body→closure
-- request within a single EB) are not idle and stay warm regardless.
--
-- In single-EB diffusion every connection is used once, so this never fires;
-- it is the seam the Phase-4 multi-round driver uses when a connection is reused
-- for the next EB after seconds of inactivity (see design.md D12).
idleReset :: Bool -> Double -> TcpParams -> FlowState -> FlowState
idleReset keepsWarm idleGapS tp fs
  | keepsWarm                 = fs
  | idleGapS <= max (tpRtoMinS tp) (tpBaseRtoS tp) = fs   -- within one (floored) RTO: not idle, stays warm (Finding 7 — the RTO the model charges is floored at tpRtoMinS)
  | otherwise                 = fs { fsCwnd = tpCwnd0 tp, fsPhase = SlowStart
                                   , fsWMax = 0, fsLastWMax = 0, fsK = 0
                                   , fsCaElapsed = 0, fsConsecTO = 0 }   -- Finding 11: clear the high-water mark on idle-reset too

-- | Sample losses in a window of @n@ packets at per-packet rate @p@: returns
-- (loss count, 1-based first-loss position or 0, rng').
sampleLosses :: RandomGen g => Int -> Double -> g -> (Int, Int, g)
sampleLosses n p = go 1 0 0
  where
    go i k firstPos g
      | i > n     = (k, firstPos, g)
      | otherwise = let (u, g') = randomR (0, 1) g
                    in if u < p
                         then go (i+1) (k+1) (if firstPos == 0 then i else firstPos) g'
                         else go (i+1) k firstPos g'

-- | One round (one RTT, or one RTO stall on timeout).  @rtt@ and @lossP@ are
-- passed in so the Phase-2 DES can vary them per round (jitter, egress-derived
-- loss); the isolated single-flow driver just passes the flow's constants.
stepRound :: RandomGen g => TcpParams -> Double -> Double -> FlowState -> g -> (FlowState, Int, g)
stepRound tp rtt lossP fs g =
  let mss     = tpMss tp
      effCwnd = max 1 (min (floor (fsCwnd fs)) (tpBdpCapSegs tp))
      remB    = tpFileBytes tp - fsBytesSent fs
      remPkts = (remB + mss - 1) `div` mss
      attempt = min effCwnd remPkts
      (k, firstLoss, g') = sampleLosses attempt lossP g
      lossRound = k > 0
      fastRtx | lossRound && tpEnableRto tp = k == 1 && attempt - firstLoss >= 3
              | otherwise                   = lossRound
  in if not lossRound
       then let fs1 = (advance attempt rtt fs) { fsConsecTO = 0 }  -- clean round resets RTO backoff
            in (if fsBytesSent fs1 >= tpFileBytes tp then fs1 else growOnAck tp fs1, 0, g')
     else if fastRtx
       then let fs1 = (advance attempt rtt fs) { fsConsecTO = 0 }  -- data delivered ⇒ RTO backoff clears (fresh RTT sample recomputes RTO, RFC 6298 §2.2/2.3 + Karn)
            in if fsBytesSent fs1 >= tpFileBytes tp
                 -- final-window fast-retransmit: mid-transfer the fill RTT is absorbed by the
                 -- next round's continued transmission (M0), but at the file tail nothing rides
                 -- behind the hole, so the in-order prefix closes only when the retransmit lands
                 -- ~1 RTT on — charge it (faithful Linux fast recovery; mechanics.md §4).
                 then (fs1 { fsT = fsT fs1 + rtt }, 1, g')
                 else (cutOnLoss tp effCwnd fs1, 1, g')
     else -- RTO
       let delivered = max 0 (firstLoss - 1)
           backoff   = (2 :: Int) ^ min (fsConsecTO fs) (30 :: Int)
           rto       = min (tpRtoMaxS tp) (max (tpRtoMinS tp) (tpBaseRtoS tp) * fromIntegral backoff)
           fs1 = fs { fsBytesSent = min (tpFileBytes tp) (fsBytesSent fs + delivered * mss)
                    , fsT = fsT fs + rto, fsConsecTO = fsConsecTO fs + 1 }
       in (rtoCollapse effCwnd fs1, 1, g')
  where
    -- advance a delivering round; does NOT reset fsConsecTO itself — the caller
    -- clears it on any data-delivering round (clean or fast-retransmit), since a
    -- successful delivery draws fresh RTT samples and clears the RTO backoff
    -- (a fresh RTT sample recomputes RTO, RFC 6298 §2.2/2.3 + Karn); only back-to-back timeouts keep the backoff.
    advance pkts rtt' f =
      let f1 = f { fsBytesSent = min (tpFileBytes tp) (fsBytesSent f + pkts * tpMss tp)
                 , fsT = fsT f + rtt' }
      in if fsPhase f1 == CA then f1 { fsCaElapsed = fsCaElapsed f1 + rtt' } else f1

-- | Isolated single-flow completion: (download time s, effective loss events).
flowTime :: RandomGen g => TcpParams -> g -> (Double, Int)
flowTime tp = flowTimeFrom tp (initFlow tp)

-- | As 'flowTime' but from an arbitrary initial 'FlowState' — lets a caller start a transfer WARM (cwnd
-- already ramped, e.g. a continuous gossip link or a warm-idle node) instead of cold slow-start.
flowTimeFrom :: RandomGen g => TcpParams -> FlowState -> g -> (Double, Int)
flowTimeFrom tp fs0 g0
  | tpFileBytes tp <= 0 = (0, 0)
  | otherwise           = loop fs0 { fsBytesSent = 0, fsT = 0 } 0 g0 (0 :: Int)
  where
    loop fs nLoss g rounds
      | fsBytesSent fs >= tpFileBytes tp = (fsT fs, nLoss)
      | rounds > 10000000                = (1/0, nLoss)
      | otherwise =
          let (fs', dl, g') = stepRound tp (tpRttS tp) (tpLossP tp) fs g
          in loop fs' (nLoss + dl) g' (rounds + 1)

-- | A warm flow at (near-)steady cwnd: ramped through slow-start to ssthresh and into congestion
-- avoidance, so a WARM transfer skips the cold slow-start penalty.  (Losses still cut it via CUBIC.)
warmFlow :: TcpParams -> FlowState
warmFlow tp = (initFlow tp) { fsCwnd = fromIntegral (tpBdpCapSegs tp), fsSsthresh = fromIntegral (tpBdpCapSegs tp)
                            , fsPhase = CA, fsWMax = fromIntegral (tpBdpCapSegs tp) }
